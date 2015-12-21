%%% The MIT License
%%%
%%% Copyright (C) 2014-2015 by Tatsuya Kawano <tatsuya@hibaridb.org>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

-module(h2leveldb).

-behaviour(gen_server).

-export([start_link/1,
         stop/0]).

-export([create_db/1,
         create_db/2,
         get_db/1,
         get_db/2,
         close_db/1,
         destroy_db/1,
         repair_db/1,
         repair_db/2,
         backup_db/2
        ]).

%% Not implemented.
%% -export([info_memory/1,
%%          info_size/1
%%         ]).

-export([delete/2,
         delete/3,
         get/2,
         get/3,
         put/3,
         put/4
        ]).

%% Not implemented.
%% -export([get_many/3,
%%          get_many/4,
%%          get_many/5
%%         ]).

-export([write/2,
         write/3,
         new_write_batch/0,
         is_empty_batch/1,
         add_put/3,
         add_delete/2,
         make_put/2,
         make_delete/1
        ]).

-export([first_key/1,
         first_key/2,
         last_key/1,
         last_key/2,
         next_key/2,
         next_key/3,
         prev_key/2,
         prev_key/3
        ]).

-export([fold_keys/6,
         fold_keys/7,
         fold_kvs/6,
         fold_kvs/7
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).


%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%% @TODO Move types to an .hrl file.
-export_type([db/0,
              write_batch/0
             ]).

-type db() :: term().

-type option() :: term(). %% @TODO underspec

-type read_option() :: verifi_checksums | {verify_checksums, boolean()}
                     | fill_cache | {fill_cache, boolean()}.

-type write_option() :: sync | {sync, boolean()}.

-type key() :: binary().
-type value() :: binary().
-type put_op() :: tuple().
-type delete_op() :: tuple().
-type write_batch() :: {batch, term()}.
-type write_batch_list() :: write_batch() | [put_op() | delete_op()].
-type from() :: tuple().

-record(state, {
          open_db_count=0   :: non_neg_integer(),
          repair_db_count=0 :: non_neg_integer()   %% not used (reserved)
         }).
-type state() :: #state{}.


-define(SERVER_REG_NAME,           h2leveldb).
-define(ETS_REG_NAME,              h2leveldb).
-define(TIMEOUT,                   60 * 1000).  %% for gen_server:call/3.

-define(H2LDB_BADARG,              16#00).
-define(H2LDB_NOT_IMPLEMENTED,     16#01).
-define(H2LDB_TRUE,                16#02).
-define(H2LDB_FALSE,               16#03).
-define(H2LDB_KEY_NOT_EXIST,       16#04).
-define(H2LDB_END_OF_TABLE,        16#05).
-define(H2LDB_BINARY,              16#06).

-define(H2LDB_OPEN_DB2,            16#00).
-define(H2LDB_DESTROY_DB1,         16#01).
-define(H2LDB_REPAIR_DB2,          16#02).
-define(H2LDB_CLOSE_DB1,           16#03).
-define(H2LDB_INFO_MEMORY1,        16#04).
-define(H2LDB_INFO_SIZE1,          16#05).

-define(H2LDB_DELETE3,             16#06).
-define(H2LDB_FIRST2,              16#07).
-define(H2LDB_GET3,                16#08).
-define(H2LDB_LAST2,               16#09).
-define(H2LDB_NEXT3,               16#0A).
-define(H2LDB_PREV3,               16#0B).
-define(H2LDB_PUT4,                16#0C).
-define(H2LDB_WRITE_BATCH3,        16#0D).
-define(H2LDB_BACKUP_DB,           16#0E).

-define(H2LDB_BATCH_PUT,           16#00).
-define(H2LDB_BATCH_DELETE,        16#01).


%%%----------------------------------------------------------------------
%%% API - Application
%%%----------------------------------------------------------------------

-spec start_link([]) -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link([]) ->
    gen_server:start_link({local, ?SERVER_REG_NAME}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER_REG_NAME, stop, ?TIMEOUT).

%%%----------------------------------------------------------------------
%%% API - DB Operations
%%%----------------------------------------------------------------------

%% - +compressed+ If this option is present, the table data will be
%%   stored in a compressed format.
%%
%% - +async+ If this option is present, the emulator\'s async thread
%%   pool will be used when accessing the table data. The default is
%%   +true+
%%
%% Valid LevelDB database properties for +db_opts()+ are:
%%
%% - +create_if_missing | {create_if_missing, boolean()}+ If +true+,
%%   the database will be created if it is missing.  The default is
%%   +false+.
%%
%% - +error_if_exists | {error_if_exists, boolean()}+ If +true+, an
%%   error is raised if the database already exists. The default is
%%   +false+.
%%
%% - +paranoid_checks | {paranoid_checks, boolean()}+ If +true+, the
%%   implementation will do aggressive checking of the data it is
%%   processing and will stop early if it detects any errors. The
%%   default is +false+.
%%
%% - +{write_buffer_size, pos_integer()}+ The default is 4MB.
%%
%% - +{max_open_files, pos_integer()}+ The default is 1000.
%%
%% - +{block_cache_size, pos_integer()}+ The default is 8MB.
%%
%% - +{block_size, pos_integer()}+ The default is 4K.
%%
%% - +{block_restart_interval, pos_integer()}+ The default is 16.
%%
%% - +{filter_policy, no | {bloom, pos_integer()}}+ The default is +no+.
%%
%% Valid LevelDB read properties for +db_read_opts()+ are:
%%
%% - +verify_checksums | {verify_checksums, boolean()}+ If +true+, all
%%   data read from underlying storage will be verified against
%%   corresponding checksums. The default is +false+.
%%
%% - +fill_cache | {fill_cache, boolean()}+ If +true+, the data read
%%   should be cached in memory. The default is +true+.
%%
%% Valid LevelDB write properties for +db_write_opts()+ are:
%%
%% - +sync | {sync, boolean()}+ If +true+, the write will be flushed
%%   from the operating system buffer cache before the write is
%%   considered complete. The default is +false+.

%% Options:      [{async, true}, {compression, no}, {create_if_missing, true}]
%% ReadOptions:  [{verify_checksums, true}]
%% WriteOptions: [{sync, false}]


-spec create_db(Path::file:path()) -> {ok, db()} | {error, io_error}.
create_db(Path) ->
    create_db(Path, []).

-spec create_db(Path::file:path(), Options::[option()]) ->
                       {ok, db()} | {error, io_error}.
create_db(Path, Options) ->
    gen_server:call(?SERVER_REG_NAME, {create_db, Path, Options}, ?TIMEOUT).

%% @doc Returns the reference to an existing database at the
%% given path. If it isn't opened, this function tries to open it.
%% Returns {error, not_found} if the database doesn't exist at
%% the path.
-spec get_db(Path::file:path()) -> {ok, db()} | {error, io_error}.
get_db(Path) ->
    get_db(Path, []).

-spec get_db(Path::file:path(), Options::[option()]) -> {ok, db()} | {error, io_error}.
get_db(Path, Options) ->
    case ets:lookup(?ETS_REG_NAME, Path) of
        [{Path, DB}] ->
            {ok, DB};
        [] ->
            gen_server:call(?SERVER_REG_NAME, {open_db, Path, Options}, ?TIMEOUT)
    end.

-spec close_db(Path::file:path()) -> ok | {error, not_opened} | {error, io_error}.
close_db(Path) ->
    gen_server:call(?SERVER_REG_NAME, {close_db, Path}, ?TIMEOUT).

-spec destroy_db(Path::file:path()) -> ok | {error, not_closed} | {error, io_error}.
destroy_db(Path) ->
    gen_server:call(?SERVER_REG_NAME, {destroy_db, Path}, ?TIMEOUT).

%% @TODO ENHANCEME: repair_db/2 may take some time to finish and
%% gen_server will be blocked until it finishes. (may cause timeout)
-spec repair_db(Path::file:path()) -> ok | {error, not_closed} | {error, io_error}.
repair_db(Path) ->
    repair_db(Path, []).

-spec repair_db(Path::file:path(), Options::[option()]) ->
                       ok | {error, not_closed} | {error, io_error}.
repair_db(Path, Options) ->
    gen_server:call(?SERVER_REG_NAME, {repair_db, Path, Options}, ?TIMEOUT).

-spec backup_db(db(),binary()) -> ok | {error, io_error}.
backup_db(DB,Name) ->
  call(DB,{?H2LDB_BACKUP_DB,Name}).

%% @TODO: Not implemented.
%% -spec info_memory(DB::db()) -> term().
%% info_memory(DB) ->
%%     call(DB, {?H2LDB_INFO_MEMORY1}).

%% @TODO: Not implemented.
%% -spec info_size(DB::db()) -> term().
%% info_size(DB) ->
%%     call(DB, {?H2LDB_INFO_SIZE1}).


%%%----------------------------------------------------------------------
%%% API - Single Key-Value Operations (delete, get, and put)
%%%----------------------------------------------------------------------

%% Key-Value Operations

-spec delete(db(), key()) -> ok | {error, io_error}.
delete(DB, Key) ->
    delete(DB, Key, []).

-spec delete(db(), key(), [write_option()]) -> ok | {error, io_error}.
delete(DB, Key, WriteOptions) ->
    call(DB, {?H2LDB_DELETE3, Key, WriteOptions}).

-spec get(db(), key()) -> {ok, value()} | key_not_exist | {error, io_error}.
get(DB, Key) ->
    get(DB, Key, []).

-spec get(db(), key(), [read_option()]) ->
                 {ok, value()} | key_not_exist | {error, io_error}.
get(DB, Key, ReadOptions) ->
    call(DB, {?H2LDB_GET3, Key, ReadOptions}).

-spec put(db(), key(), value()) -> ok | {error, io_error}.
put(DB, Key, Value) ->
    put(DB, Key, Value, []).

-spec put(db(), key(), value(), [write_option()]) -> ok | {error, io_error}.
put(DB, Key, Value, WriteOptions) ->
    call(DB, {?H2LDB_PUT4, Key, Value, WriteOptions}).

%%%----------------------------------------------------------------------
%%% API - Key-Value Operation (get_many)
%%%----------------------------------------------------------------------

%% NOTE: [StartKey .. EndKey]    inclusive .. inclusive
%% -spec get_many(db(), StartKey::key(), EndKeyOrLimit::key() | non_neg_integer()) -> ?.
%% get_many(DB, StartKey, EndKeyOrLimit) ->
%%     get_many(DB, StartKey, EndKeyOrLimit, []).

%% -spec get_many(db(), StartKey::key(), EndKey::key(), Limit::non_neg_integer()) -> ?;
%%               (db(), StartKey::key(), EndKey::key(), [read_option()]) -> ?;
%%               (db(), StartKey::key(), Limit::non_neg_integer(), [read_option()]) -> ?.
%% get_many(DB, StartKey, EndKey, Limit)
%%   when is_binary(EndKey), is_integer(Limit) ->
%%     get_many(DB, StartKey, EndKey, Limit, []);
%% get_many(DB, StartKey, EndKey, ReadOptions)
%%   when is_binary(EndKey), is_list(ReadOptions) ->
%%     get_many(DB, StartKey, EndKey, undefined, ReadOptions);
%% get_many(DB, StartKey, Limit, ReadOptions)
%%   when is_integer(Limit), is_list(ReadOptions) ->
%%     get_many(DB, StartKey, undefined, Limit, ReadOptions).

%% -spec get_many(db(),
%%                StartKey::key(),
%%                EndKey::key() | undefined,
%%                Limit::non_neg_integer() | undefined,
%%                [read_option()]) -> ?.
%% get_many(_DB, _StartKey, _EndKey, _Limit, _ReadOptions) ->
%%     %% call(DB, {?H2LDB_GET_MANY5, StartKey, EndKey, Limit, ReadOptions}).
%%     error(not_implemented).


%%%----------------------------------------------------------------------
%%% API - Write Batch Operation
%%%----------------------------------------------------------------------

-spec write(db(), write_batch_list()) -> ok.
write(DB, WriteBatch) ->
    write(DB, WriteBatch, []).

-spec write(db(), write_batch_list(), [write_option()]) -> ok.
write(_DB, [], _WriteOptions) ->
    true;
write(DB, WriteBatch, WriteOptions) when is_list(WriteBatch) ->
    call(DB, {?H2LDB_WRITE_BATCH3, WriteOptions, WriteBatch});
write(DB, {batch, Q}, WriteOptions) ->
    write(DB, queue:to_list(Q), WriteOptions).

-spec new_write_batch() -> write_batch().
new_write_batch() ->
    {batch, queue:new()}.

-spec is_empty_batch(write_batch_list()) -> boolean().
is_empty_batch([]) ->
    true;
is_empty_batch({batch, Q}) ->
    queue:is_empty(Q);
is_empty_batch(_) ->
    false.

-spec add_put(key(), value(), write_batch()) -> ok.
add_put(Key, Value, {batch, Q}) ->
    {batch, queue:in(make_put(Key, Value), Q)}.

-spec add_delete(key(), write_batch()) -> ok.
add_delete(Key, {batch, Q}) ->
    {batch, queue:in(make_delete(Key), Q)}.

-spec make_put(key(), value()) -> put_op().
make_put(Key, Value) ->
    {?H2LDB_BATCH_PUT, Key, Value}.

-spec make_delete(key()) -> delete_op().
make_delete(Key) ->
    {?H2LDB_BATCH_DELETE, Key}.


%%%-----------------------------------------------------------------------------
%%% API - Key Iteration Operations (first_key, last_key, next_key, and prev_key)
%%%-----------------------------------------------------------------------------

-spec first_key(db()) -> {ok, key()} | end_of_table | {error, io_error}.
first_key(DB) ->
    first_key(DB, []).

-spec first_key(db(), [read_option()]) ->
                       {ok, key()} | end_of_table | {error, io_error}.
first_key(DB, ReadOptions) ->
    call(DB, {?H2LDB_FIRST2, ReadOptions}).

-spec last_key(db()) -> {ok, key()} | end_of_table | {error, io_error}.
last_key(DB) ->
    last_key(DB, []).

-spec last_key(db(), [read_option()]) ->
                      {ok, key()} | end_of_table | {error, io_error}.
last_key(DB, ReadOptions) ->
    call(DB, {?H2LDB_LAST2, ReadOptions}).

-spec next_key(db(), key()) -> {ok, key()} | end_of_table | {error, io_error}.
next_key(DB, Key) ->
    next_key(DB, Key, []).

-spec next_key(db(), key(), [read_option()]) ->
                      {ok, key()} | end_of_table | {error, io_error}.
next_key(DB, Key, ReadOptions) ->
    call(DB, {?H2LDB_NEXT3, Key, ReadOptions}).

-spec prev_key(db(), key()) -> {ok, key()} | end_of_table | {error, io_error}.
prev_key(DB, Key) ->
    prev_key(DB, Key, []).

-spec prev_key(db(), key(), [read_option()]) ->
                      {ok, key()} | end_of_table | {error, io_error}.
prev_key(DB, Key, ReadOptions) ->
    call(DB, {?H2LDB_PREV3, Key, ReadOptions}).


%%%-----------------------------------------------------------------------------
%%% API - Key-Value Iteration Operations (fold_kvs, fold_keys)
%%%-----------------------------------------------------------------------------

%% Start, inclusive
%% End, inclusive
-spec fold_keys(db(), fun((key(), AccIn::term()) -> AccOut::term()),
                Acc0::term(),
                StartKey::key() | undefined, StopKey::key() | undefined,
                NumItems::non_neg_integer()) ->
                       {ok, Acc1::term(), IsTruncated::boolean()}
                           | {error, io_error}.
fold_keys(DB, Fun, Acc0, Start, Stop, NumItems) ->
    fold_keys(DB, Fun, Acc0, Start, Stop, NumItems, []).

%% Start, inclusive
%% End, inclusive
-spec fold_keys(db(), fun((key(), AccIn::term()) -> AccOut::term()),
                Acc0::term(),
                StartKey::key() | undefined, StopKey::key() | undefined,
                NumItems::non_neg_integer(),
                [read_option()]) ->
                       {ok, Acc1::term(), IsTruncated::boolean()}
                           | {error, io_error}.
fold_keys(DB, Fun, Acc0, Start, Stop, NumItems, ReadOptions) ->
    case find_first_key_for_fold(DB, Start, ReadOptions) of
        {ok, _}=Key ->
            fold_keys1(DB, Fun, Acc0, Key, Stop, NumItems, ReadOptions);
        end_of_table ->
            {ok, Acc0, false};
        {error, _}=Err ->
            Err
    end.

%% Start, inclusive
%% End, inclusive
-spec fold_kvs(db(), fun(({key(), value()}, AccIn::term()) -> AccOut::term()),
               Acc0::term(),
               StartKey::key() | undefined, StopKey::key() | undefined,
               NumItems::non_neg_integer()) ->
                      {ok, Acc1::term(), IsTruncated::boolean()}
                          | {error, io_error}.
fold_kvs(DB, Fun, Acc0, Start, Stop, NumItems) ->
    fold_kvs(DB, Fun, Acc0, Start, Stop, NumItems, []).

%% Start, inclusive
%% End, inclusive
-spec fold_kvs(db(), fun(({key(), value()}, AccIn::term()) -> AccOut::term()),
               Acc0::term(),
               StartKey::key() | undefined, StopKey::key() | undefined,
               NumItems::non_neg_integer(),
               [read_option()]) ->
                      {ok, Acc1::term(), IsTruncated::boolean()}
                          | {error, io_error}.
fold_kvs(DB, Fun, Acc0, Start, Stop, NumItems, ReadOptions) ->
    case find_first_key_for_fold(DB, Start, ReadOptions) of
        {ok, _}=Key ->
            fold_kvs1(DB, Fun, Acc0, Key, Stop, NumItems, ReadOptions);
        end_of_table ->
            {ok, Acc0, false};
        {error, _}=Err ->
            Err
    end.


%% ====================================================================
%% gen_server callbacks
%% ====================================================================

init([]) ->
    process_flag(priority, high),
    %% ServerRegName = proplists:get_value(name, PropList, h2leveldb),
    %% Assuming read (get_db/1) will be far more common than write (create_db/2 etc.)
    ets:new(?ETS_REG_NAME, [set,
                            protected,
                            named_table,
                            {write_concurrency, false},
                            {read_concurrency, true}]),
    {ok, #state{}}.

-spec handle_call({create_db, file:path(), [option()]}, from(), state()) ->
                         {reply, {ok, db()}, state()}
                             | {reply, {error, io_error}, state()};
                 ({open_db, file:path(), [option()]}, from(), state()) ->
                         {reply, {ok, db()}, state()}
                             | {reply, {error, io_error}, state()};
                 ({close_db, file:path(), [option()]}, from(), state()) ->
                         {reply, ok, state()}
                             | {reply, {error, not_opened}, state()}
                             | {reply, {error, io_error}, state()};
                 ({destroy_db, file:path()}, from(), state()) ->
                         {reply, ok, state()}
                             | {reply, {error, not_closed}, state()}
                             | {reply, {error, io_error}, state()};
                 ({repair_db, file:path(), [option()]}, from(), state()) ->
                         {reply, ok, state()}
                             | {reply, {error, not_closed}, state()}
                             | {reply, {error, io_error}, state()}.
handle_call({create_db, Path, Options}, _From, #state{open_db_count=Count}=State) ->
    try
        DB = init(),
        case call(DB, {?H2LDB_OPEN_DB2, list_to_binary(Path),
                       [create_if_missing, error_if_exists | Options]}) of
            ok ->
                true = ets:insert(?ETS_REG_NAME, {Path, DB}),
                {reply, {ok, DB}, State#state{open_db_count=Count + 1}};
            {error, _}=Err ->
                %% @TODO ENHANCEME: Need more informative error (currently io_error only)
                {reply, Err, State}
        end
    catch
        error:Err1 ->
            %% @TODO ENHANCEME: Need more informative error.
            {reply, Err1, State}
    end;
handle_call({open_db, Path, Options}, _From, #state{open_db_count=Count}=State) ->
    case ets:lookup(?ETS_REG_NAME, Path) of
        [{Path, DB}] ->
            %% Race condition. Two client processes may have called get_db/1
            %% at almost same time.
            {reply, {ok, DB}, State};
        [] ->
            try
                DB = init(),
                %% @TODO Remove create_if_missing
                case call(DB, {?H2LDB_OPEN_DB2, list_to_binary(Path),
                               [{create_if_missing, false} | Options]}) of
                    ok ->
                        true = ets:insert(?ETS_REG_NAME, {Path, DB}),
                        {reply, {ok, DB}, State#state{open_db_count=Count + 1}};
                    {error, _}=Err ->
                        %% @TODO ENHANCEME: Need more informative error (currently io_error only)
                        {reply, Err, State}
                end
            catch
                error:Err1 ->
                    {reply, Err1, State}
            end
    end;
handle_call({close_db, Path}, _From, #state{open_db_count=Count}=State) ->
    case ets:lookup(?ETS_REG_NAME, Path) of
        [] ->
            {reply, {error, not_opened}, State};
        [{Path, DB}] ->
            case do_close_db(Path, DB) of
                ok ->
                    case Count - 1 of
                        Count1 when Count1 =:= 0 ->
                            %% If there is no open DB, unload the driver.
                            _ = erl_ddll:unload(h2leveldb_impl_drv),
                            {reply, ok, State#state{open_db_count=0}};
                        Count1 when Count1 > 0 ->
                            {reply, ok, State#state{open_db_count=Count1}};
                        Count1 ->
                            erlang:error({bug, negative_db_count, Count1})
                    end;
                {error, _}=Err ->
                    {reply, Err, State}
            end
    end;
handle_call({destroy_db, Path}, _From, State) ->
    case ets:lookup(?ETS_REG_NAME, Path) of
        [{Path, _DB}] ->
            {reply, {error, not_closed}, State};
        [] ->
            try
                DB = init(),
                case call(DB, {?H2LDB_DESTROY_DB1, list_to_binary(Path), []}) of
                    ok ->
                        _ = port_close(DB),
                        {reply, ok, State};
                    {error, _}=Err ->
                        %% @TODO ENHANCEME: Need more informative error (currently io_error only)
                        {reply, Err, State}
                end
            catch
                error:Err1 ->
                    {reply, {error, Err1}, State}
            end
    end;
handle_call({repair_db, Path, Options}, _From, State) ->
    case ets:lookup(?ETS_REG_NAME, Path) of
        [{Path, _DB}] ->
            {reply, {error, not_closed}, State};
        [] ->
            try
                DB = init(),
                case call(DB, {?H2LDB_REPAIR_DB2, list_to_binary(Path), Options}) of
                    ok ->
                        _ = port_close(DB),
                        {reply, ok, State};
                    {error, Err} ->
                        %% @TODO ENHANCEME: Need more informative error (currently io_error only)
                        {reply, Err, State}
                end
            catch
                error:Err1 ->
                    {reply, {error, Err1}, State}
            end
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

-spec handle_cast(tuple(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(tuple(), state()) -> {noreply, state()}.
handle_info(_Msg, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{open_db_count=0}) ->
    ok;
terminate(_Reason, _State) ->
    _Count = ets:foldl(
               fun({Path, DB}, Acc) ->
                       try do_close_db(Path, DB) of
                           ok ->
                               Acc + 1;
                           _Err ->  %% ignore {error, _} response.
                               Acc
                       catch
                           error:_=_Err1 ->  %% ignore erlang:error.
                               Acc
                       end
               end, 0, ?ETS_REG_NAME),
    %% Unload the driver.
    _ = erl_ddll:unload(h2leveldb_impl_drv),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

-spec init() -> db().
init() ->
    Path =
        case code:priv_dir(h2leveldb) of
            {error, bad_name} ->
                "./priv/lib";
            Dir ->
                filename:join([Dir, "lib"])
        end,
    case erl_ddll:load_driver(Path, h2leveldb_impl_drv) of
        ok ->
            ok;
        {error, already_loaded} ->
            ok;
        {error, permanent} ->
            ok;
        {error, {open_error, _}=Err} ->
            FormattedErr = erl_ddll:format_error(Err),
            error_logger:error_msg("Failed to load the driver library h2leveldb_impl_drv. "
                                   ++ "Error: ~p, Path: ~p~n",
                                   [FormattedErr,
                                    filename:join(Path, h2leveldb_impl_drv)
                                   ]),
            erlang:error({Err, FormattedErr});
        {error, _}=OtherErr ->
            erlang:error(OtherErr)
    end,
    open_port({spawn, "h2leveldb_impl_drv"}, [binary]).

-spec do_close_db(file:path(), db()) -> ok | {error, io_error}.
do_close_db(Path, DB) ->
    try call(DB, {?H2LDB_CLOSE_DB1}) of
        ok ->
            _ = port_close(DB),
            true = ets:delete(?ETS_REG_NAME, Path),
            ok;
        {error, _}=Err ->
            %% @TODO ENHANCEME: Need more informative error (currently io_error only)
            Err
    catch
        error:_=Err1->
            Err1
    end.

-spec call(db(), tuple()) -> ok | {ok, binary()} | key_not_exist | end_of_table | no_return().
call(DB, Tuple) ->
    Data = term_to_binary(Tuple),
    port_command(DB, Data),
    receive
        {DB, ?H2LDB_BINARY, Reply} ->
            {ok, Reply};
        {DB, ?H2LDB_TRUE} ->
            ok;
        %% {DB, ?H2LDB_FALSE} ->
        %%     false;   %% Cunnernt implementation never returns false.
        {DB, ?H2LDB_KEY_NOT_EXIST} ->
            key_not_exist;
        {DB, ?H2LDB_END_OF_TABLE} ->
            end_of_table;
        {DB, ?H2LDB_BADARG} ->
            %% erlang:error(badarg, [DB]);
            {error, io_error};
        {_DB, ?H2LDB_NOT_IMPLEMENTED} ->
            erlang:error(not_implemented)
    end.

find_first_key_for_fold(DB, undefined, ReadOptions) ->
    first_key(DB, ReadOptions);
find_first_key_for_fold(DB, Key, ReadOptions) ->
    case get(DB, Key, ReadOptions) of
        {ok, _} ->
            {ok, Key};
        key_not_exist ->
            next_key(DB, Key, ReadOptions);
        {error, _}=Err ->
            Err
    end.

fold_keys1(_DB, _Fun, _AccIn, {error, _}=Err, _Stop, _NumItems, _ReadOptions) ->
    Err;
fold_keys1(_DB, _Fun, AccIn, end_of_table, _Stop, _NumItems, _ReadOptions) ->
    {ok, lists:reverse(AccIn), false};
fold_keys1(_DB, _Fun, AccIn, _Key, _Stop, NumItems, _ReadOptions) when NumItems =< 0 ->
    {ok, lists:reverse(AccIn), true};
fold_keys1(_DB, _Fun, AccIn, {ok, Key}, Stop,
         _NumItems, _ReadOptions) when Stop =/= undefined, Key > Stop ->
    {ok, lists:reverse(AccIn), false};
fold_keys1(DB, Fun, AccIn, {ok, Key}, Stop, NumItems, ReadOptions) ->
    AccOut = Fun(Key, AccIn),
    NextKey = next_key(DB, Key, ReadOptions),
    fold_keys1(DB, Fun, AccOut, NextKey, Stop, NumItems - 1, ReadOptions).

fold_kvs1(_DB, _Fun, _AccIn, {error, _}=Err, _Stop, _NumItems, _ReadOptions) ->
    Err;
fold_kvs1(_DB, _Fun, AccIn, end_of_table, _Stop, _NumItems, _ReadOptions) ->
    {ok, lists:reverse(AccIn), false};
fold_kvs1(_DB, _Fun, AccIn, _Key, _Stop, NumItems, _ReadOptions) when NumItems =< 0 ->
    {ok, lists:reverse(AccIn), true};
fold_kvs1(_DB, _Fun, AccIn, {ok, Key}, Stop,
         _NumItems, _ReadOptions) when Stop =/= undefined, Key > Stop ->
    {ok, lists:reverse(AccIn), false};
fold_kvs1(DB, Fun, AccIn, {ok, Key}, Stop, NumItems, ReadOptions) ->
    case get(DB, Key, ReadOptions) of
        {ok, Value} ->
            AccOut = Fun({Key, Value}, AccIn),
            NextKey = next_key(DB, Key, ReadOptions),
            fold_kvs1(DB, Fun, AccOut, NextKey, Stop, NumItems - 1, ReadOptions);
        key_not_exist ->
            %% Race condition: The Key has been deleted by other process
            NextKey = next_key(DB, Key, ReadOptions),
            fold_kvs1(DB, Fun, AccIn, NextKey, Stop, NumItems, ReadOptions);
        {error, _}=Err ->
            Err
    end.

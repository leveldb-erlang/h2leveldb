%%% The MIT License
%%%
%%% Copyright (C) 2014 by Tatsuya Kawano <tatsuya@hibaridb.org>
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

-module(h2leveldb_test).

-include_lib("eunit/include/eunit.hrl").

-define(H2LEVELDB, h2leveldb).

db_crud_test() ->
    {ok, _} = h2leveldb:start_link([]),
    DBPath = make_temp_db_path(),
    %% ?debugVal(DBPath),

    %% create, get and close
    ?LET(Res, ?H2LEVELDB:create_db(DBPath),
         begin
             ?assertMatch({ok, _}, Res),
             {ok, DB} = Res,
             ?assertMatch({ok, DB}, ?H2LEVELDB:get_db(DBPath)),
             ?assertMatch(ok, ?H2LEVELDB:close_db(DBPath))
         end),

    %% create again (Need a more informative error.)
    ?assertMatch({error, io_error}, ?H2LEVELDB:create_db(DBPath)),

    %% get, delete and get again
    ?LET(Res, ?H2LEVELDB:get_db(DBPath),
         begin
             ?assertMatch({ok, _}, Res),
             %% It's not allowed to destroy while the DB is open.
             ?assertMatch({error, not_closed}, ?H2LEVELDB:destroy_db(DBPath)),
             ?assertMatch(ok, ?H2LEVELDB:close_db(DBPath)),
             ?assertMatch(ok, ?H2LEVELDB:destroy_db(DBPath)),

             %% @TODO: CHECKME This seems to create the DB again?
             ?assertMatch({error, io_error}, ?H2LEVELDB:get_db(DBPath))
         end),

    h2leveldb:stop(). %% @TODO: Call at teardown.

db_repair_test() ->
    {ok, _} = h2leveldb:start_link([]),
    DBPath = make_temp_db_path(),
    %% ?debugVal(DBPath),

    %% create, get and close
    ?LET(Res, ?H2LEVELDB:create_db(DBPath),
         begin
             ?assertMatch({ok, _}, Res),
             {ok, DB} = Res,
             ?assertMatch({ok, DB}, ?H2LEVELDB:get_db(DBPath)),
             %% It's not allowed to repair while the DB is open.
             ?assertMatch({error, not_closed}, ?H2LEVELDB:repair_db(DBPath)),
             ?assertMatch(ok, ?H2LEVELDB:close_db(DBPath)),
             ?assertMatch(ok, ?H2LEVELDB:repair_db(DBPath))
         end),

    %% @TODO CHECKME: This doesn't seem to delete the *repaired* DB?
    ?assertMatch(ok, ?H2LEVELDB:destroy_db(DBPath)),
    h2leveldb:stop(). %% @TODO: Call at teardown.

db_not_found_test() ->
    {ok, _} = h2leveldb:start_link([]),
    DBPath = make_temp_db_path(),
    %% ?debugVal(DBPath),

    ?assertMatch({error, not_opened}, ?H2LEVELDB:close_db(DBPath)),

    %% @TODO: Need more informative errors.
    ?assertMatch({error, io_error}, ?H2LEVELDB:get_db(DBPath)),

    %% @TODO: CHECKME Destroy will succeed even if the DB doesn't exist?
    ?assertMatch(ok, ?H2LEVELDB:destroy_db(DBPath)),

    %% @TODO: CHECKME Repair will succeed even if the DB doesn't exist?
    ?assertMatch(ok, ?H2LEVELDB:repair_db(DBPath)),
    %% and repair seems to create the DB?
    ?assertMatch({ok, _}, ?H2LEVELDB:get_db(DBPath)),
    ?assertMatch(ok, ?H2LEVELDB:close_db(DBPath)),
    ?assertMatch(ok, ?H2LEVELDB:destroy_db(DBPath)),

    h2leveldb:stop(). %% @TODO: Call at teardown.

crud_test() ->
    {ok, _} = h2leveldb:start_link([]),
    DBPath = make_temp_db_path(),
    %% ?debugVal(DBPath),

    {ok, DB} = ?H2LEVELDB:create_db(DBPath),
    try
        ?assertEqual(ok, ?H2LEVELDB:put(DB, <<"key1">>, <<"value1">>)),
        ?assertEqual(ok, ?H2LEVELDB:put(DB, <<"key2">>, <<"value2">>, [sync])),
        ?assertEqual({ok, <<"value1">>}, ?H2LEVELDB:get(DB, <<"key1">>)),
        ?assertEqual({ok, <<"value2">>}, ?H2LEVELDB:get(DB, <<"key2">>)),
        ?assertEqual(key_not_exist,      ?H2LEVELDB:get(DB, <<"key3">>)),

        ?assertEqual(ok, ?H2LEVELDB:delete(DB, <<"key1">>)),
        ?assertEqual(ok, ?H2LEVELDB:delete(DB, <<"key2">>, [sync])),
        %% You won't get error for deleting a non-existing key.
        ?assertEqual(ok, ?H2LEVELDB:delete(DB, <<"key3">>))
    after
        catch ?H2LEVELDB:close_db(DBPath),
        ?H2LEVELDB:destroy_db(DBPath)
    end,
    h2leveldb:stop(). %% @TODO: Call at teardown.

batch_write1_test() ->
    {ok, _} = h2leveldb:start_link([]),
    Batch = [?H2LEVELDB:make_delete(<<"key1">>),
             ?H2LEVELDB:make_put(<<"key2">>, <<"value2">>)],

    DBPath = make_temp_db_path(),
    %% ?debugVal(DBPath),
    {ok, DB} = ?H2LEVELDB:create_db(DBPath),
    try
        ?assertEqual(ok, ?H2LEVELDB:put(DB, <<"key1">>, <<"value1">>)),
        ?assertEqual({ok, <<"value1">>}, ?H2LEVELDB:get(DB, <<"key1">>)),

        ?assertEqual(ok, ?H2LEVELDB:write(DB, Batch, [sync])),
        ?assertEqual(key_not_exist,      ?H2LEVELDB:get(DB, <<"key1">>)),
        ?assertEqual({ok, <<"value2">>}, ?H2LEVELDB:get(DB, <<"key2">>))
    after
        catch ?H2LEVELDB:close_db(DBPath),
        ?H2LEVELDB:destroy_db(DBPath)
    end,
    h2leveldb:stop(). %% @TODO: Call at teardown.

batch_write2_test() ->
    KeyMin = 10,
    KeyMax = 990,

    {ok, _} = h2leveldb:start_link([]),
    DBPath = make_temp_db_path(),
    %% ?debugVal(DBPath),
    Batch = lists:foldl(fun(N, B) ->
                                ?H2LEVELDB:add_put(<<N:64>>, <<"value", N:64>>, B)
                        end,
                        ?H2LEVELDB:new_write_batch(), lists:seq(KeyMin, KeyMax)),

    ?LET({ok, DB}, ?H2LEVELDB:create_db(DBPath),
         try
             ?assertEqual(ok, ?H2LEVELDB:write(DB, Batch, [sync]))
         after
             catch ?H2LEVELDB:close_db(DBPath)
         end),

    ?LET({ok, DB}, ?H2LEVELDB:get_db(DBPath),
         try
             lists:foreach(fun(N) when KeyMin =< N, N =< KeyMax ->
                                   ?assertEqual({ok, <<"value", N:64>>},
                                                ?H2LEVELDB:get(DB, <<N:64>>));
                              (N) ->
                                   %% ?debugVal(N),
                                   ?assertEqual(key_not_exist,
                                                ?H2LEVELDB:get(DB, <<N:64>>))
                           end, lists:seq(KeyMin - 10, KeyMax + 10))
         after
             catch ?H2LEVELDB:close_db(DBPath),
             ?H2LEVELDB:destroy_db(DBPath)
         end),
    h2leveldb:stop(). %% @TODO: Call at teardown.

next_key_test() ->
    KeyMin = 10,
    KeyMax = 490,

    {ok, _} = h2leveldb:start_link([]),
    DBPath = make_temp_db_path(),
    %% ?debugVal(DBPath),
    Batch = lists:foldl(fun(N, B) ->
                                ?H2LEVELDB:add_put(<<N:64>>, <<"value", N:64>>, B)
                        end,
                        ?H2LEVELDB:new_write_batch(), lists:seq(KeyMin, KeyMax)),

    ?LET({ok, DB}, ?H2LEVELDB:create_db(DBPath),
         try
             ?assertEqual(ok, ?H2LEVELDB:write(DB, Batch, [sync]))
         after
             catch ?H2LEVELDB:close_db(DBPath)
         end),

    ?LET({ok, DB}, ?H2LEVELDB:get_db(DBPath),
         try
             FirstKey = ?H2LEVELDB:first_key(DB),
             ?assertEqual(KeyMax - KeyMin + 1,
                          read_and_next(DB, FirstKey, KeyMin, KeyMax, 0))
         after
             catch ?H2LEVELDB:close_db(DBPath),
             ?H2LEVELDB:destroy_db(DBPath)
         end),
    h2leveldb:stop(). %% @TODO: Call at teardown.

read_and_next(_, end_of_table, _, _, Count) ->
    Count;
read_and_next(DB, {ok, <<N:64>>=Key}, KeyMin, KeyMax, Count) when KeyMin =< N, N =< KeyMax ->
    %% ?debugVal(N),
    ?assertEqual({ok, <<"value", N:64>>}, ?H2LEVELDB:get(DB, Key)),
    case ?H2LEVELDB:next_key(DB, Key) of
        {error, Err} ->
            error(Err);
        NextKey ->
            read_and_next(DB, NextKey, KeyMin, KeyMax, Count + 1)
    end;
read_and_next(_, Key, _, _, _) ->
    error({key, Key, must_not_exist}).

prex_key_test() ->
    KeyMin = 10,
    KeyMax = 490,

    {ok, _} = h2leveldb:start_link([]),
    DBPath = make_temp_db_path(),
    %% ?debugVal(DBPath),
    Batch = lists:foldl(fun(N, B) ->
                                ?H2LEVELDB:add_put(<<N:64>>, <<"value", N:64>>, B)
                        end,
                        ?H2LEVELDB:new_write_batch(), lists:seq(KeyMin, KeyMax)),

    ?LET({ok, DB}, ?H2LEVELDB:create_db(DBPath),
         try
             ?assertEqual(ok, ?H2LEVELDB:write(DB, Batch, [sync]))
         after
             catch ?H2LEVELDB:close_db(DBPath)
         end),

    ?LET({ok, DB}, ?H2LEVELDB:get_db(DBPath),
         try
             LastKey = ?H2LEVELDB:last_key(DB),
             ?assertEqual(KeyMax - KeyMin + 1,
                          read_and_prev(DB, LastKey, KeyMin, KeyMax, 0))
         after
             catch ?H2LEVELDB:close_db(DBPath),
             ?H2LEVELDB:destroy_db(DBPath)
         end),
    h2leveldb:stop(). %% @TODO: Call at teardown.

read_and_prev(_, end_of_table, _, _, Count) ->
    Count;
read_and_prev(DB, {ok, <<N:64>>=Key}, KeyMin, KeyMax, Count) when KeyMin =< N, N =< KeyMax ->
    %% ?debugVal(N),
    ?assertEqual({ok, <<"value", N:64>>}, ?H2LEVELDB:get(DB, Key)),
    case ?H2LEVELDB:prev_key(DB, Key) of
        {error, Err} ->
            error(Err);
        PrevKey ->
            read_and_prev(DB, PrevKey, KeyMin, KeyMax, Count + 1)
    end;
read_and_prev(_, Key, _, _, _) ->
    error({key, Key, must_not_exist}).

make_temp_db_path() ->
    {MegaSecs, Secs, MicroSecs} = erlang:now(),
    FileName = io_lib:format("h2leveldb-test-~w-~w-~w.leveldb",
                             [MegaSecs, Secs, MicroSecs]),
    filename:join("/tmp", FileName).

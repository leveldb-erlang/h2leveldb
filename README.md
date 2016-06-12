
## h2leveldb - HyperLevelDB bindings for Erlang/OTP [![wercker status](https://app.wercker.com/status/754e6bbdcdb482588b775193ae9314c2/s "wercker status")](https://app.wercker.com/project/bykey/754e6bbdcdb482588b775193ae9314c2)

Copyright (c) 2014-2016 by Tatsuya Kawano

**Authors:** Tatsuya Kawano ([`tatsuya@hibaridb.org`](mailto:tatsuya@hibaridb.org)).

Also see the [Credits](#credits) chapter. h2leveldb borrowed port
driver C/C++ codes from LETS.

**h2leveldb** is Erlang bindings to HyperLevelDB embedded key-value
store that can be used in any Erlang or Elixir application. It is open
sourced under the MIT license. It was originally developed for
[Hibari DB](https://github.com/hibari/hibari) and that is why it was
named as an abbreviation of Hibari HyperLevelDB. Despite the name
suggests, it does not depend on any components of Hibari DB.

**The goals of h2leveldb** are to provide straightforward bindings to
HyperLevelDB and to expose its native C++ API as much as possible to
Erlang application. While this will help developers to take
performance advantage of HyperLevelDB's native API, it will also ask
the developers to learn the new API and a bit of the internals of
HyperLevelDB.

If you are looking for an easier option to work with, I would
recommend to take a look at other projects such as
[LETS](https://github.com/norton/lets), which is a drop-in replacement
of ETS (Erlang Term Storage) using LevelDB or HyperLevelDB as the
storage implementation.

### Notes About The Implementation

h2leveldb is implemented as an Erlang port driver (not NIFs). It
takes advantage of the async thread pool in an Erlang VM to perform
concurrent writes and reads against HyperLevelDB without blocking the
scheduler threads in the VM. It also exposes HyperLevelDB's batch
write API to improve performance, especially for synchronous writes.

The port driver is statically linked to specific versions of
HyperLevelDB and Snappy compression library. Therefore, you do not
have to separately install and manage these products as dynamic shared
libraries in a system path.

### What is HyperLevelDB?

[**HyperLevelDB**](https://github.com/rescrv/HyperLevelDB) is a fork
of [Google LevelDB](https://code.google.com/p/leveldb/) intended to
meet the needs of HyperDex while remaining compatible with LevelDB.

HyperLevelDB improves on LevelDB in two key ways:

- **Improved parallelism:** HyperLevelDB uses more fine-grained
  locking internally to provide higher throughput for multiple
  writer threads.

- **Improved compaction:** HyperLevelDB uses a different method of
  compaction that achieves higher throughput for write-heavy
  workloads, even as the database grows.

HyperLevelDB is written in C++0x and open sourced under the New BSD
License.

For more information, please see the following articles:

- [Inside HyperLevelDB](http://hackingdistributed.com/2013/06/17/hyperleveldb/)
- [HyperLevelDB Performance Benchmarks](http://hyperdex.org/performance/leveldb/)

### What is Snappy?

[**Google Snappy**](https://code.google.com/p/snappy/) is a
compression/decompression library used by LevelDB and HyperLevelDB to
compress on-disk data blocks. It aims for very high speeds with
reasonable compression ratio. Snappy is written in C++ and open
sourced under the New BSD License.

For more information, please see the
[README file](https://code.google.com/p/snappy/source/browse/trunk/README)
of Snappy.

## Usage Examples

Here are some basic usage examples written in Erlang. If you are using
Elixir, do not forget to replace char list `"/tmp/ldb.leveldb1"` in
Erlang with `'/tmp/ldb.leveldb1'` in Elixir, and binary `<<"key1">>`
with `"key1"`.


### Basic CRUD Operations

```erlang
    _ = h2leveldb:start_link([]),
    DBPath = "/tmp/ldb.leveldb1",

    %% create a new database,
    {ok, DB} = h2leveldb:create_db(DBPath),
    %% or open an existing database.
    %% {ok, DB} = h2leveldb:get_db(DBPath),

    try
        ok = h2leveldb:put(DB, <<"key1">>, <<"value1">>),
        ok = h2leveldb:put(DB, <<"key2">>, <<"value2">>, [sync]),
        {ok, <<"value1">>} = h2leveldb:get(DB, <<"key1">>),
        {ok, <<"value2">>} = h2leveldb:get(DB, <<"key2">>),
        key_not_exist =      h2leveldb:get(DB, <<"key3">>),

        ok = h2leveldb:delete(DB, <<"key1">>),
        ok = h2leveldb:delete(DB, <<"key2">>, [sync]),
        %% You won't get error for deleting a non-existing key.
        ok = h2leveldb:delete(DB, <<"key3">>)
    after
        catch ok = h2leveldb:close_db(DBPath)
    end
```

### Batch Write

#### Option 1: Use make_put/2 and make_delete/1

```erlang
    _ = h2leveldb:start_link([]),

    Batch = [h2leveldb:make_delete(<<"key1">>),
             h2leveldb:make_put(<<"key2">>, <<"value2">>)],

    DBPath = "/tmp/ldb.leveldb2",
    {ok, DB} = h2leveldb:create_db(DBPath),
    try
        ok = h2leveldb:put(DB, <<"key1">>, <<"value1">>),
        {ok, <<"value1">>} = h2leveldb:get(DB, <<"key1">>),

        ok = h2leveldb:write(DB, Batch, [sync]),
        key_not_exist =      h2leveldb:get(DB, <<"key1">>),
        {ok, <<"value2">>} = h2leveldb:get(DB, <<"key2">>)
    after
        catch ok = h2leveldb:close_db(DBPath)
    end
```

#### Option 2: Use lists:foldl/3 with add_put/3 and add_delete/2

```erlang
    _ = h2leveldb:start_link([]),
    Puts = [{<<"key1">>, <<"value1">>},
            {<<"key2">>, <<"value2">>},
            {<<"key3">>, <<"value3">>}],
    Batch = lists:foldl(fun({K, V}, B) ->
                                h2leveldb:add_put(K, V, B)
                        end,
                        h2leveldb:new_write_batch(), Puts),

    DBPath = "/tmp/ldb.leveldb2",
    {ok, DB} = h2leveldb:create_db(DBPath),
    try
        ok = h2leveldb:write(DB, Batch, [sync]),
    after
        catch ok = h2leveldb:close_db(DBPath)
    end
```

### Iteration: Priv, Next and First and Last

See the [eunit test cases](https://github.com/leveldb-erlang/h2leveldb/blob/master/test/eunit/h2leveldb_test.erl).


## System Requirements

### Erlang/OTP 18 and 17

Recently tested with the following Erlang/OTP releases:

- Erlang/OTP 18.3.3 (64 bit)
- Erlang/OTP 17.5.6.8 (64 bit)

If you want to build it on an older releases such as R16B03-1 (64
bit), please let me know by creating
[a GitHub issue](https://github.com/leveldb-erlang/h2leveldb/issues).

### Unix like OS such as Linux, BSD and illumos[1]

h2leveldb will run on any Unix like operating systems such as Linux,
BSD and illumos. Although I have not tested, it will run on OS X too
with a little change in h2leveldb/c_src/build_deps.sh script.

- **1**: [illumos](http://wiki.illumos.org/display/illumos/About+illumos)
  is the open source fork of Sun's OpenSolaris. OmniOS and SmartOS
  will be good options for illumos based servers.

**Recently tested with:**

I am developing h2leveldb and regularly testing it on the following
platforms:

| OS    |          | Release         | Arch   | Compiler    | Erlang/OTP       | File System |
|:------|:---------|:----------------|:-------|:------------|------------------|:------------|
| Linux | CentOS 7 | 7.2.1511        | x86_64 | GCC 4.8.5   | 18.3.3, 17.5.6.8 | XFS         |
| Linux | CentOS 6 | 6.8             | x86_64 | GCC 4.4.7   | 18.3.3, 17.5.6.8 | EXT4        |
| BSD   | FreeBSD  | 10.3-RELEASE-p5 | amd64  | Clang 3.4.1 | 18.3.3, 17.5.6.8 | ZFS         |

**Other platforms:**

I once tried an older version of h2leveldb on the following platforms:

| OS      |         | Release      | Arch  | Compiler  | Erlang/OTP | File System |
|:--------|:--------|:-------------|:------|:----------|:-----------|:------------|
| illumos | SmartOS | pkgin 2014Q1 | amd64 | GCC 4.7.3 | R16B02     | ZFS         |
| illumos | OmniOS  | r151008j-r1  | amd64 | GCC 4.8.1 | 17.0       | ZFS         |


## Adding h2leveldb to Your Erlang or Elixir Project

### Erlang Based Project

1. Add h2leveldb to your rebar.config:

   ```erlang
   {deps, [{h2leveldb, "", {git, "git://github.com/leveldb-erlang/h2leveldb",
                            {branch, "master"}}}

           %% other deps ...
           ]}.
   ```

1. Install [build dependencies](#installing-build-dependencies).

1. Build and run your project:

   ```sh
   $ rebar get-deps
   $ rebar compile
   $ rebar shell
   ```


### Elixir Based Project

1. Add h2leveldb and override its dependencies to your mix.exs:

   ```elixir
   defp deps do
    [{:h2leveldb, git: "https://github.com/leveldb-erlang/h2leveldb.git",
      tag: "master"},
     {:HyperLevelDB, git: "https://github.com/leveldb-erlang/HyperLevelDB.git",
      compile: false, app: false, override: true},
     {:snappy, git: "https://github.com/leveldb-erlang/snappy.git",
      compile: false, app: false, override: true},

     # other deps ...
    ]
  end
  ```

1. Install [build dependencies](#installing-build-dependencies).

1. Build and run your project:

   ```sh
   $ mix deps.get
   $ mix compile
   $ iex -S mix
   ```

## Installing Build Dependencies

h2leveldb requires **GNU Autotools** and their dependencies. It also
requires **git** to download HyperLevelDB and Snappy source codes. Of
course, you need **Erlang/OTP 17 or newer**.

- [Arch Linux](#linux---arch-linux)
- [Debian and Ubuntu](#linux---debian-and-ubuntu)
- [Fedora, RHEL 7 and CentOS 7](#linux---fedora-rhel-7-and-centos-7)
- [RHEL 6 and CentOS 6](#linux---rhel-6-and-centos-6)
- [FreeBSD](#freebsd)
- [SmartOS](#illumos---smartos)
- [OmniOS](#illumos---omnios)


### Linux - Arch Linux

```sh
$ sudo pacman -S gcc make autoconf automake libtool git
```

### Linux - Debian and Ubuntu

```sh
$ sudo apt-get install gcc g++ make autoconf automake libtool git
```

### Linux - Fedora, RHEL 7 and CentOS 7

```sh
$ sudo yum install gcc gcc-c++ make autoconf automake libtool git
```

### Linux - RHEL 6 and CentOS 6

```sh
$ sudo yum install gcc gcc-c++ make libtool git wget
```

autoconf and automake in RHEL 6/Cent OS 6 yum repositories will be too
old to build Snappy. Build and install recent versions from their
source codes.

**autoconf**

```sh
$ cd /tmp
$ wget http://ftp.gnu.org/gnu/autoconf/autoconf-2.69.tar.gz
$ tar xvf autoconf-2.69.tar.gz
$ cd autoconf-2.69
$ ./configure --prefix=/usr
$ make
$ sudo make install
$ cd ..
$ rm -rf autoconf-2.69*
```

**automake**

```sh
$ cd /tmp
$ wget http://ftp.gnu.org/gnu/automake/automake-1.15.tar.gz
$ tar xvf automake-1.15.tar.gz
$ cd automake-1.15
$ ./configure --prefix=/usr
$ make
$ sudo make install
$ cd ..
$ rm -rf automake-1.15*
```

### FreeBSD

For FreeBSD 10.0-RELEASE or newer, you can use pkgng to install
pre-built packages.

```sh
$ sudo pkg install bash gmake autoconf automake libtool git
```

I have not tested h2leveldb on older FreeBSD releases, and I think
build will fail because h2leveldb's reber.config assumes that you are
using Clang as C/C++ compiler and its standard C++ library is
available. This may not be true on an older FreeBSD release. You could
solve this by replacing `-lc++` in reber.config with `-lstdc++`.


### illumos - SmartOS

Running h2leveldb in the global zone is not supported. Create a
SmartOS zone by `vmadm` and run the following commands inside the
zone:

```sh
$ sudo pkgin in gcc47 gmake autoconf automake libtool git
```

I installed Erlang/OTP (R16B02) from pkgin.

```sh
$ sudo pkgin in erlang
```

- **TODO**: Build and install Erlang
  * e.g http://christophermeiklejohn.com/ruby/smartos/2013/10/15/erlang-on-smartos.html


### illumos - OmniOS

Erlang/OTP in the official package repository is too old and cannot be
used for h2leveldb. You need to build and install R16B03-1 or 17.x
using Kerl.

Install develop essentials.

```sh
$ sudo pkg install developer/gcc48
$ sudo pkg install \
   developer/build/gnu-make \
   developer/library/lint \
   developer/linker \
   developer/object-file \
   system/header \
   system/library/math/header-math
```

Install Erlang/OTP dependencies.

```sh
$ sudo pkg install \
   archiver/gnu-tar \
   developer/build/autoconf \
   library/ncurses \
   library/security/openssl \
   network/netcat \
   web/ca-bundle
```

Set up [Kerl](https://github.com/spawngrid/kerl), and build Erlang/OTP
with it. I was not able to build 17.0 with `--disable-hipe` due to a
compile error. So I used `--enable-hipe` option but I do not know if
this is a right thing to do.

```sh
$ echo 'KERL_CONFIGURE_OPTIONS="--enable-hipe --enable-smp-support --enable-threads --enable-kernel-poll" ' > ~/.kerlrc
$ kerl update releases

$ export PATH=$PATH:/opt/gcc-4.8.1/bin
$ kerl build 17.0 17.0_hipe
$ kerl install 17.0_hipe ~/erlang/17.0_hipe

$ . ~/erlang/17.0_hipe/activate
$ kerl status
```

Install the remaining build dependencies for h2leveldb.

```sh
$ sudo pkg install \
   developer/build/automake \
   developer/build/libtool \
   developer/versioning/git
```


## Performance Tips

### File System Tuning

#### XFS

- `inode64` mount option must be specified for a block device larger
  than 2 TB, otherwise you will hit a serious performance degradation
  after you randomly delete many files.
- As a general rule, having more allocation groups at format time will
  improve writes/reads concurrency. This will not be true if you
  create only one database because all store-files for a database will
  be stored in one folder belonging to one allocation group. But if
  you plan to create more than one databases, those store-files will
  be stored in separate folders in different allocation groups,
  therefore you will likely get improved concurrency.

#### ZFS

- To improve random read performance, make the block size smaller to
  match the HyperLevelDB's block size. The default for ZFS dataset is
  128 KB and for HyperLevelDB is approximately 4 KB when uncompressed.
  So having 4 KB block will be a good idea.
  * On SmartOS, you need to create a ZFS dataset and delegate it to
    your zone to achieve this.
- h2leveldb uses Snappy compression by default, you might want to
  turn off compression at ZFS dataset.
- As a general rule, adding a separate spindle (HDD) for ZIL and an
  SSD partition for L2ARC will improve performance.


## Credits

Many thanks to Joseph Wayne Norton who authored and open sourced
[LETS](https://github.com/norton/lets), an alternative ETS (Erlang
Term Storage) using LevelDB as the storage implementation. h2leveldb
borrowed port driver C/C++ codes form LETS.


## TODO

- [ ] Add `get_many/5` operation to move the load for partial or full DB
  scan from Erlang land to C++ land. I found repeating `next/3` and
  `get/3` calls hogs the CPU so I believe this will be better handled
  in C++ land.
- [ ] More informative error messages. Right now, it will only return
  `{error, io_error}` for any kind of errors occurred in HyperLevelDB.
- [ ] Add bloom filter policy (See: Performance -> Filter section of the
  [LevelDB document](http://leveldb.googlecode.com/svn/trunk/doc/index.html).)
- [ ] Add QuickCheck test cases.

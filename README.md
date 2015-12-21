
## h2leveldb - HyperLevelDB bindings for Erlang/OTP

Copyright (c) 2014-2015 by Tatsuya Kawano

**Authors:** Tatsuya Kawano ([`tatsuya@hibaridb.org`](mailto:tatsuya@hibaridb.org)), Mark Steele ([`mark@control-alt-del.org`](mailto:mark@control-alt-del.org)).
Also see the [Credits](#credits) chapter. h2leveldb borrowed port
driver C/C++ codes from LETS.

**h2leveldb** is Erlang bindings to HyperLevelDB embedded key-value
store that can be used in any Erlang application. It is open sourced
under the MIT license. It was originally developed for
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

### Notes About Implementation

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

### Hot database backups

To initiate a hot backup, use the `h2leveldb:backup_db/2` function:
```
[root@dev1 h2leveldb]# rebar shell
==> h2leveldb (shell)
Erlang/OTP 17 [erts-6.4] [source-2e19e2f] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.4  (abort with ^G)
1>  h2leveldb:start_link([]).
{ok,<0.43.0>}
2>  DBPath = "/tmp/ldb.leveldb1".
"/tmp/ldb.leveldb1"
3> h2leveldb:repair_db(DBPath).
ok
4> {ok, DB} = h2leveldb:get_db(DBPath).
{ok,#Port<0.4965>}
5> timer:tc(fun() -> [ h2leveldb:put(DB, integer_to_binary(erlang:phash2(erlang:now())), crypto:strong_rand_bytes(1000)) || X <- lists:seq(1,10000)] end).
{2663985,
 [ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,
  ok,ok,ok,ok,ok,ok,ok,ok|...]}
6> h2leveldb:backup_db(DB,<<"foo">>).
ok
7> q().
ok
[root@dev1 h2leveldb]# ls -alh /tmp/ldb.leveldb1/
total 148M
drwxr-xr-x.  4 root root 4.0K Dec 20 23:48 .
drwxrwxrwt. 17 root root 4.0K Dec 20 23:03 ..
-rw-r--r--.  3 root root 8.9M Dec 20 23:32 000150.sst
-rw-r--r--.  3 root root 8.9M Dec 20 23:32 000151.sst
-rw-r--r--.  3 root root 5.6M Dec 20 23:32 000152.sst
-rw-r--r--.  3 root root 3.5M Dec 20 23:32 000154.sst
-rw-r--r--.  3 root root 8.3M Dec 20 23:39 000180.sst
-rw-r--r--.  3 root root 6.3M Dec 20 23:39 000181.sst
-rw-r--r--.  1 root root    0 Dec 20 23:48 000190.log
-rw-r--r--.  2 root root 8.2M Dec 20 23:47 000191.sst
-rw-r--r--.  2 root root 8.1M Dec 20 23:47 000192.sst
-rw-r--r--.  2 root root 8.1M Dec 20 23:47 000193.sst
-rw-r--r--.  2 root root 2.6M Dec 20 23:47 000194.sst
-rw-r--r--.  1 root root   16 Dec 20 23:47 CURRENT
-rw-r--r--.  1 root root    0 Dec 20 22:55 LOCK
-rw-r--r--.  1 root root 1.5K Dec 20 23:48 LOG
-rw-r--r--.  1 root root 2.7K Dec 20 23:41 LOG.old
-rw-r--r--.  1 root root  29K Dec 20 23:48 MANIFEST-000188
drwxr-xr-x.  2 root root 4.0K Dec 20 23:40 backup-foo
```

The parameters for the function are the port reference, and the name for this backup as a binary. A backup folder will be created in the database folder called `"backup-<NAME>"`

### Iteration: Priv, Next and First and Last

See the [eunit test cases](https://github.com/leveldb-erlang/h2leveldb/blob/master/test/eunit/h2leveldb_test.erl).


## Adding h2leveldb to Your Project

### Requirements

#### Erlang/OTP 18, 17 and R16B

Recently tested with:

- Erlang/OTP 18.0 (64 bit)
- Erlang/OTP 17.5 (64 bit)
- Erlang/OTP R16B03-1 (64 bit)

#### Unix like OS such as GNU/Linux, BSD and illumos(*1)

h2leveldb will run on virtually any Unix like operating systems such
as GNU/Linux, BSD and illumos. Although I have not tested, it will run
on Mac OS X too with a little change in h2leveldb/c_src/build_deps.sh
script.

- **1:** illumos derives from OpenSolaris. OmniOS and SmartOS will be
  good options for illumos based servers.

**Recently tested with:**

I am developing and regularly testing h2leveldb on the following
platforms.

| OS        |            | Release          | Arch   | Compiler    | Erlang/OTP           | File System |
|:----------|:-----------|:-----------------|:-------|:------------|----------------------|:------------|
| GNU/Linux | Arch Linux | 2015.07.01       | x86_64 | GCC 5.2.0   | 18.0, 17.5, R16B03-1 | XFS         |
| BSD       | FreeBSD    | 10.1-RELEASE-p16 | amd64  | Clang 3.4.1 | 18.0, 17.5, R16B03-1 | ZFS         |
| illumos   | SmartOS    | pkgin 2014Q1     | amd64  | GCC 4.7.3   | R16B02               | ZFS         |

**Other platforms:**

In addition to above, I tried the following platforms to see if
I can build and run h2leveldb.

| OS        |            | Release      | Arch   | Compiler  | Erlang/OTP     | File System |
|:----------|:-----------|:-------------|:-------|:----------|:---------------|:------------|
| GNU/Linux | CentOS 7   | 7.1.1503     | x86_64 | GCC 4.8.3 | 18.0, 17.5     | XFS         |
| GNU/Linux | CentOS 6   | 6.6          | x86_64 | GCC 4.4.7 | 18.0, 17.5     | EXT4        |
| illumos   | OmniOS     | r151008j-r1  | amd64  | GCC 4.8.1 | 17.0           | ZFS         |

- (**TODO**) Try Ubuntu "Trusty" 14.04 LTS x86_64.

### Install Build Dependencies

h2leveldb requires **GNU Autotools** and their dependencies. It also
requires **git** to download HyperLevelDB and Snappy source codes. Of
course, you need **Erlang/OTP R16B or newer**.

#### GNU/Linux - Fedora, RHEL 7, CentOS 7

```sh
$ sudo yum install gcc gcc-c++ make autoconf automake libtool git
```

#### GNU/Linux - RHEL 6 and CentOS 6

```sh
$ sudo yum install gcc gcc-c++ make libtool git wget
```

autoconf and automake in RHEL 6/Cent OS 6 yum repositories will be too
old to build Snappy. Build and install the latest version from source.

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

#### GNU/Linux - Debian, Ubuntu

```sh
$ sudo apt-get install gcc g++ make autoconf automake libtool git
```

#### GNU/Linux - Arch Linux

```sh
$ sudo pacman -S gcc make autoconf automake libtool git
```

#### FreeBSD

For FreeBSD 10.0-RELEASE or newer, you can use pkgng to install
pre-built packages.

```sh
$ sudo pkg install bash gmake autoconf automake libtool git
```

I have not tested h2leveldb with older FreeBSD releases, and I think
build will fail because h2leveldb's reber.config assumes that you are
using Clang as C/C++ compiler and its standard C++ library is
available. This may not be true on an older FreeBSD release. You could
solve this by replacing `-lc++` in reber.config with `-lstdc++`.

#### illumos - SmartOS

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

#### illumos - OmniOS

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

### Build Your Project with h2leveldb

To build your project with h2leveldb, run the following commands:

```sh
$ ./rebar get-deps
$ ./rebar compile
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

- Add QuickCheck test cases.
- Add `get_many/5` operation to move the load for partial or full DB
  scan from Erlang land to C++ land. I found repeating `next/3` and
  `get/3` calls hogs the CPU so I believe this will be better handled
  in C++ land.
- More informative error messages. Right now, it will only return
  `{error, io_error}` for any kind of errors occurred in HyperLevelDB.
- Add bloom filter policy (See: Performance -> Filter section of the
  [LevelDB document](http://leveldb.googlecode.com/svn/trunk/doc/index.html).)
- Perhaps add DTrace provider?

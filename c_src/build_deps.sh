#!/usr/bin/env bash

# The MIT License
#
# Copyright (C) 2014-2016 by Tatsuya Kawano <tatsuya@hibaridb.org>
# Copyright (C) 2011-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

set -e -o pipefail

# Snappy 1.1.2
SNAPPY_VSN=1ff9be9b8fafc8528ca9e055646f5932aa5db9c4

# HyperLevelDB Sep 11, 2014
LEVELDB_VSN=40ce80173a8d72443c5f92e3c072a54ed910bab9

PLATFORM='unknown'
unamestr=`uname`
case "$unamestr" in
    Linux)
        PLATFORM='linux'
        ;;
    FreeBSD)
        PLATFORM='freebsd'
        ;;
    SunOS)
        PLATFORM='solaris'
        ;;
    darwin)
        PLATFORM='macosx'
        ;;
esac

case "$PLATFORM" in
    freebsd)
        MAKE=gmake
        CONFENV=''
        TAR=tar
        ;;
    solaris)
        # Only tested with OmniOS and SmartOS
        MAKE=gmake
        CONFENV='CC=gcc CXX=g++'
        TAR=gtar
        ;;
    *)
        MAKE=make
        CONFENV=''
        TAR=tar
        ;;
esac

if [ `basename $PWD` != "c_src" ]; then
    case "$PLATFORM" in
        freebsd|solaris)
            cd c_src
            ;;
        *)
            pushd c_src > /dev/null 2>&1
            ;;
    esac
fi

BASEDIR="$PWD"

case "$1" in
    clean)
        rm -rf snappy snappy-*
        rm -rf HyperLevelDB HyperLevelDB-*
        ;;

    get_deps)
        # snappy
        if [ ! -d $REBAR_DEPS_DIR/snappy ]; then
            git clone git://github.com/leveldb-erlang/snappy.git $REBAR_DEPS_DIR/snappy
        fi
        # HyperLevelDB
        if [ ! -d $REBAR_DEPS_DIR/HyperLevelDB ]; then
             git clone git://github.com/leveldb-erlang/HyperLevelDB.git $REBAR_DEPS_DIR/HyperLevelDB
        fi
        ;;

    update_deps)
        # snappy
        if [ ! -d $REBAR_DEPS_DIR/snappy ]; then
            echo "$REBAR_DEPS_DIR/snappy not found. Please run ./rebar get-deps first."
            echo
            exit 8
        fi
        (cd $REBAR_DEPS_DIR/snappy && git checkout master && git pull origin master)

        # HyperLevelDB
        if [ ! -d $REBAR_DEPS_DIR/HyperLevelDB ]; then
            echo "$REBAR_DEPS_DIR/HyperLevelDB not found. Please run ./rebar get-deps first."
            echo
            exit 8
        fi
        (cd $REBAR_DEPS_DIR/HyperLevelDB && git checkout master && git pull origin master)
        ;;

    *)
        # snappy
        if [ ! -f $BASEDIR/snappy/lib/libsnappy.a ]; then
            LIBTOOLIZE=libtoolize
            ($LIBTOOLIZE --version) < /dev/null > /dev/null 2>&1 || {
                LIBTOOLIZE=glibtoolize
                ($LIBTOOLIZE --version) < /dev/null > /dev/null 2>&1 || {
                    echo
                    echo "You must have GNU Autotools installed to compile h2leveldb."
                    echo "Please see README.md for more information."
                    echo
                    exit 8
                }
            }

            (cd $REBAR_DEPS_DIR/snappy && git checkout $SNAPPY_VSN && \
                git archive --format=tar --prefix=snappy-$SNAPPY_VSN/ $SNAPPY_VSN) \
                | $TAR xf -
            (cd snappy-$SNAPPY_VSN && \
                sed -ibak1 '/^AC_ARG_WITH.*$/, /^fi$/d' configure.ac && \
                perl -ibak2 -pe 's/LT_INIT/AM_PROG_AR\nLT_INIT/' configure.ac
            )
            (cd snappy-$SNAPPY_VSN && \
                rm -rf autom4te.cache && \
                aclocal -I m4 && \
                autoheader && \
                $LIBTOOLIZE --copy && \
                automake --add-missing --copy && \
                autoconf)
            (cd snappy-$SNAPPY_VSN && \
                env $CONFENV ./configure $CONFFLAGS \
                --enable-static \
                --disable-shared \
                --with-pic \
                --prefix=$BASEDIR/snappy &&  \
                $MAKE install)
        fi

        # HyperLevelDB
        if [ ! -f $BASEDIR/HyperLevelDB/lib/libhyperleveldb.a ]; then
            (cd $REBAR_DEPS_DIR/HyperLevelDB && git checkout $LEVELDB_VSN && \
                git archive --format=tar --prefix=HyperLevelDB-$LEVELDB_VSN/ $LEVELDB_VSN) \
                | $TAR xf -
            (cd HyperLevelDB-$LEVELDB_VSN && \
                autoreconf -i && \
                env $CONFENV \
                    CXXFLAGS="$CXXFLAGS -fPIC -O2 -DNDEBUG -DSNAPPY -I$BASEDIR/snappy/include" \
                    LDFLAGS="$LDFLAGS -L$BASEDIR/snappy/lib -lsnappy" ./configure && \
                $MAKE && \
                $MAKE .libs/libhyperleveldb.a && \
                mkdir -p $BASEDIR/HyperLevelDB/include/hyperleveldb && \
                install include/hyperleveldb/*.h $BASEDIR/HyperLevelDB/include/hyperleveldb && \
                mkdir -p $BASEDIR/HyperLevelDB/lib && \
                install .libs/libhyperleveldb.a $BASEDIR/HyperLevelDB/lib/)
        fi
        ;;
esac

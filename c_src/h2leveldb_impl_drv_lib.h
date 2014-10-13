// The MIT License
//
// Copyright (C) 2014 by Tatsuya Kawano <tatsuya@hibaridb.org>
// Copyright (C) 2011-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

#ifndef H2LDB_DRV_LIB_H
#define H2LDB_DRV_LIB_H

#include <string>

#include "hyperleveldb/db.h"
#include "hyperleveldb/cache.h"
#include "hyperleveldb/slice.h"
#include "hyperleveldb/write_batch.h"
#include "hyperleveldb/filter_policy.h"

#include "h2leveldb_impl_drv.h"

#ifdef __cplusplus
extern "C" {
#endif

    typedef struct {
        long buflen;
        char buf[1];
    } Binary;

    enum Type {
        SET         = 0x0,
        ORDERED_SET = 0x1
    };

    enum PrivacyType {
        PRIVATE     = 0x0,
        PROTECTED   = 0x1,
        PUBLIC      = 0x2
    };

    enum DBOpType {
        OPEN        = 0x0,
        DESTROY     = 0x1,
        REPAIR      = 0x2
    };

    typedef struct
    {
        bool async;
        bool alive;
        std::string* name;
        leveldb::Options db_options;
        size_t db_block_cache_size;
        leveldb::Cache* db_block_cache;
        int db_filter_policy_bloom_bits_per_key;
        const leveldb::FilterPolicy* db_filter_policy;
        leveldb::DB* db;
        ErlDrvUInt64 db_memory;
        ErlDrvUInt64 db_size;
    } h2ldb_impl;

    // prototypes
    extern bool h2ldb_impl_drv_lib_init();

    extern bool h2ldb_init(h2ldb_impl& impl, const char* name, const size_t namelen);
    extern bool h2ldb_create(h2ldb_impl& impl, const char op);

    extern bool h2ldb_parse_options(h2ldb_impl& impl, const char* buf, ErlDrvSizeT len);
    extern bool h2ldb_parse_read_options(leveldb::ReadOptions& read_options,
                                         const char* buf, ErlDrvSizeT len);
    extern bool h2ldb_parse_write_options(leveldb::WriteOptions& write_options,
                                          const char* buf, ErlDrvSizeT len);

    extern void h2ldb_print_options(h2ldb_impl impl);

    // helpers
    extern int ei_inspect_atom(const char *buf, int *index, char *p);
    extern int ei_inspect_binary(const char *buf, int *index, void **p, long *lenp);

#ifdef __cplusplus
}
#endif

#endif /* H2LDB_DRV_LIB_H */

// The MIT License
//
// Copyright (C) 2014-2015 by Tatsuya Kawano <tatsuya@hibaridb.org>
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

#include "h2leveldb_impl_drv.h"
#include "h2leveldb_impl_drv_lib.h"

#include <stdio.h>


#if 1
#define GOTO_BADARG { fprintf(stderr, "GOTOBADARG %s:%d\n", __FILE__, __LINE__); goto badarg; }
#else
#define GOTOBADARG { goto badarg; }
#endif

#if 1
#define GOTO_NOT_IMPLEMENTED { fprintf(stderr, "GOTO_NOT_IMPLEMENTED %s:%d\n", __FILE__, __LINE__); goto not_implemented; }
#else
#define GOTO_NOT_IMPLEMENTED { goto not_implemented; }
#endif


#define H2LDB_BADARG              0x00
#define H2LDB_NOT_IMPLEMENTED     0x01
#define H2LDB_TRUE                0x02
#define H2LDB_FALSE               0x03
#define H2LDB_KEY_NOT_EXIST       0x04
#define H2LDB_END_OF_TABLE        0x05
#define H2LDB_BINARY              0x06

#define H2LDB_OPEN_DB2            0x00
#define H2LDB_DESTROY_DB1         0x01
#define H2LDB_REPAIR_DB2          0x02
#define H2LDB_CLOSE_DB1           0x03
#define H2LDB_INFO_MEMORY1        0x04
#define H2LDB_INFO_SIZE1          0x05

#define H2LDB_DELETE3             0x06
#define H2LDB_FIRST2              0x07
#define H2LDB_GET3                0x08
#define H2LDB_LAST2               0x09
#define H2LDB_NEXT3               0x0A
#define H2LDB_PREV3               0x0B
#define H2LDB_PUT4                0x0C
#define H2LDB_WRITE_BATCH3        0x0D
#define H2LDB_BACKUP_DB           0x0E

#define H2LDB_BATCH_PUT           0x00
#define H2LDB_BATCH_DELETE        0x01

// DrvData
typedef struct {
    ErlDrvPort port;
    h2ldb_impl impl;
} DrvData;

struct DrvAsync {
    DrvData* drvdata;
    ErlDrvTermData caller;
    int command;

    // outputs
    ErlDrvBinary* binary;
    int reply;

    // inputs
    leveldb::ReadOptions db_read_options;
    leveldb::WriteOptions db_write_options;
    leveldb::WriteBatch batch;

    DrvAsync(DrvData* d, ErlDrvTermData c, int cmd) :
        drvdata(d), caller(c), command(cmd), binary(NULL), reply(H2LDB_TRUE) {
    }
    DrvAsync(DrvData* d, ErlDrvTermData c, int cmd, leveldb::ReadOptions r_options) :
        drvdata(d), caller(c), command(cmd), binary(NULL), reply(H2LDB_TRUE),
        db_read_options(r_options) {
    }
    DrvAsync(DrvData* d, ErlDrvTermData c, int cmd, leveldb::WriteOptions w_options) :
        drvdata(d), caller(c), command(cmd), binary(NULL), reply(H2LDB_TRUE),
        db_write_options(w_options) {
    }
    DrvAsync(DrvData* d, ErlDrvTermData c, int cmd, const char* key, int keylen) :
        drvdata(d), caller(c), command(cmd), binary(NULL), reply(H2LDB_TRUE) {
        binary = driver_alloc_binary(keylen);
        assert(binary);
        memcpy(binary->orig_bytes, key, binary->orig_size);
    }
    DrvAsync(DrvData* d, ErlDrvTermData c, int cmd, const char* key, int keylen,
             leveldb::ReadOptions r_options) :
        drvdata(d), caller(c), command(cmd), binary(NULL), reply(H2LDB_TRUE),
        db_read_options(r_options)
    {
        binary = driver_alloc_binary(keylen);
        assert(binary);
        memcpy(binary->orig_bytes, key, binary->orig_size);
    }

    ~DrvAsync() {
        if (binary) {
            driver_free_binary(binary);
        }
    }

    void put(const char* key, int keylen, const char* blob, int bloblen) {
        leveldb::Slice skey(key, keylen);
        leveldb::Slice sblob(blob, bloblen);
        batch.Put(skey, sblob);
    }

    void del(const char* key, int keylen) {
        leveldb::Slice skey(key, keylen);
        batch.Delete(skey);
    }
};

static void h2ldb_output_create_db2(const char op, DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items);
static void h2ldb_async_create_db2(void* async_data);
static void h2ldb_output_open_db2(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items);
static void h2ldb_output_destroy_db1(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items);
static void h2ldb_output_repair_db2(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items);
static void h2ldb_output_close_db1(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items);
static void h2ldb_async_close_db1(void* async_data);

static void h2ldb_output_backup_db(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items);

static void h2ldb_output_delete3(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items);
static void h2ldb_async_delete3(void* async_data);
static void h2ldb_output_get3(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items);
static void h2ldb_async_get3(void* async_data);
static void h2ldb_output_first2(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items);
static void h2ldb_async_first2(void* async_data);
static void h2ldb_output_last2(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items);
static void h2ldb_async_last2(void* async_data);
static void h2ldb_output_next3(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items);
static void h2ldb_async_next3(void* async_data);
static void h2ldb_output_prev3(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items);
static void h2ldb_async_prev3(void* async_data);
static void h2ldb_output_put4(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items);
static void h2ldb_async_put4(void* async_data);
static void h2ldb_output_write_batch3(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items);
static void h2ldb_async_write_batch3(void* async_data);

static void
driver_send_int(DrvData* d, const int i, ErlDrvTermData caller=0)
{
    ErlDrvTermData port = driver_mk_port(d->port);
    ErlDrvTermData spec[] = {
        ERL_DRV_PORT, port,
        ERL_DRV_INT, (ErlDrvTermData) i,
        ERL_DRV_TUPLE, 2,
    };
    if (!caller) {
        caller = driver_caller(d->port);
    }
    erl_drv_send_term(port, caller, spec, sizeof(spec) / sizeof(spec[0]));
}

static void
driver_send_binary(DrvData* d, ErlDrvBinary* bin, ErlDrvTermData caller=0)
{
    ErlDrvTermData port = driver_mk_port(d->port);
    ErlDrvTermData spec[] = {
        ERL_DRV_PORT, port,
        ERL_DRV_INT, H2LDB_BINARY,
        ERL_DRV_BINARY, (ErlDrvTermData) bin, (ErlDrvTermData) bin->orig_size, 0,
        ERL_DRV_TUPLE, 3,
    };
    if (!caller) {
        caller = driver_caller(d->port);
    }
    erl_drv_send_term(port, caller, spec, sizeof(spec) / sizeof(spec[0]));
}

static void
driver_send_buf(DrvData* d, const char *buf, const ErlDrvUInt len, ErlDrvTermData caller=0)
{
    ErlDrvTermData port = driver_mk_port(d->port);
    ErlDrvTermData spec[] = {
        ERL_DRV_PORT, port,
        ERL_DRV_INT, H2LDB_BINARY,
        ERL_DRV_BUF2BINARY, (ErlDrvTermData) buf, len,
        ERL_DRV_TUPLE, 3,
    };
    if (!caller) {
        caller = driver_caller(d->port);
    }
    erl_drv_send_term(port, caller, spec, sizeof(spec) / sizeof(spec[0]));
}


//
// Callbacks
//
static ErlDrvEntry drv_driver_entry = {
    drv_init,
    drv_start,
    drv_stop,
    drv_output,
    NULL,
    NULL,
    (char*) "h2leveldb_impl_drv",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    drv_ready_async,
    NULL,
    NULL,
    NULL,
    (int) ERL_DRV_EXTENDED_MARKER,
    (int) ERL_DRV_EXTENDED_MAJOR_VERSION,
    (int) ERL_DRV_EXTENDED_MINOR_VERSION,
    (int) ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL,
    NULL,
    NULL,
    NULL   // emergency_close
};

DRIVER_INIT (h2leveldb_impl_drv) // must match name in driver_entry
{
    return &drv_driver_entry;
}

int
drv_init()
{
    if (!h2ldb_impl_drv_lib_init()) {
        return -1;
    }

    return 0;
}

ErlDrvData
drv_start(ErlDrvPort port, char* command)
{
    (void) command;

    DrvData* d;

    if (port == NULL) {
        return ERL_DRV_ERROR_GENERAL;
    }

    if ((d = (DrvData*) driver_alloc(sizeof(DrvData))) == NULL) {
        errno = ENOMEM;
        return ERL_DRV_ERROR_ERRNO;
    } else {
        memset(d, 0, sizeof(DrvData));
    }

    // port
    d->port = port;

    return (ErlDrvData) d;
}

void
drv_stop(ErlDrvData handle)
{
    DrvData* d = (DrvData*) handle;

    // alive
    d->impl.alive = 0;

    // db
    delete d->impl.db;

    // db_block_cache
    delete d->impl.db_block_cache;

    // db_filter_policy
    delete d->impl.db_filter_policy;

    // name
    delete d->impl.name;

    driver_free(handle);
}

void
drv_output(ErlDrvData handle, char* buf, ErlDrvSizeT len)
{
    DrvData* d = (DrvData*) handle;
    int ng, index, version, items;
    char command;

#if 0
    {
        int term_type, size_needed;

        index = 0;
        ng = ei_decode_version(buf, &index, &version);
        if (ng) GOTO_BADARG;

        ng = ei_get_type(buf, &index, &term_type, &size_needed);
        if (ng) GOTO_BADARG;
        fprintf(stderr, "DEBUG %c %d: ", term_type, (int) size_needed);

        index = 0;
        ng = ei_decode_version(buf, &index, &version);
        if (ng) GOTO_BADARG;

        ei_print_term(stderr, buf, &index);
        fprintf(stderr, "\n");
    }
#endif

    index = 0;
    ng = ei_decode_version(buf, &index, &version);
    if (ng) GOTO_BADARG;

    ng = ei_decode_tuple_header(buf, &index, &items);
    if (ng) GOTO_BADARG;
    ng = (items < 1);
    if (ng) GOTO_BADARG;

    ng = ei_decode_char(buf, &index, &command);
    if (ng) GOTO_BADARG;

    items--;
    switch (command) {
    case H2LDB_OPEN_DB2:
        ng = (items != 2);
        if (ng) GOTO_BADARG;
        h2ldb_output_open_db2(d, buf, len, &index, items);
        break;
    case H2LDB_DESTROY_DB1:
        ng = (items != 2);
        if (ng) GOTO_BADARG;
        h2ldb_output_destroy_db1(d, buf, len, &index, items);
        break;
    case H2LDB_REPAIR_DB2:
        ng = (items != 2);
        if (ng) GOTO_BADARG;
        h2ldb_output_repair_db2(d, buf, len, &index, items);
        break;
    case H2LDB_CLOSE_DB1:
        ng = (items != 0 || !d->impl.alive);
        if (ng) GOTO_BADARG;
        h2ldb_output_close_db1(d, buf, len, &index, items);
        break;
    case H2LDB_BACKUP_DB:
        ng = (items != 1 || !d->impl.alive);
        if (ng) GOTO_BADARG;
        h2ldb_output_backup_db(d, buf, len, &index, items);
        break;
    case H2LDB_DELETE3:
        ng = (items != 2 || !d->impl.alive);
        if (ng) GOTO_BADARG;
        h2ldb_output_delete3(d, buf, len, &index, items);
        break;
    case H2LDB_GET3:
        ng = (items != 2 || !d->impl.alive);
        if (ng) GOTO_BADARG;
        h2ldb_output_get3(d, buf, len, &index, items);
        break;
    case H2LDB_FIRST2:
        ng = (items != 1 || !d->impl.alive);
        if (ng) GOTO_BADARG;
        h2ldb_output_first2(d, buf, len, &index, items);
        break;
    case H2LDB_LAST2:
        ng = (items != 1 || !d->impl.alive);
        if (ng) GOTO_BADARG;
        h2ldb_output_last2(d, buf, len, &index, items);
        break;
    case H2LDB_NEXT3:
        ng = (items != 2 || !d->impl.alive);
        if (ng) GOTO_BADARG;
        h2ldb_output_next3(d, buf, len, &index, items);
        break;
    case H2LDB_PREV3:
        ng = (items != 2 || !d->impl.alive);
        if (ng) GOTO_BADARG;
        h2ldb_output_prev3(d, buf, len, &index, items);
        break;
    case H2LDB_PUT4:
        ng = (items != 3 || !d->impl.alive);
        if (ng) GOTO_BADARG;
        h2ldb_output_put4(d, buf, len, &index, items);
        break;
    case H2LDB_WRITE_BATCH3:
        ng = (items != 2 || !d->impl.alive);
        if (ng) GOTO_BADARG;
        h2ldb_output_write_batch3(d, buf, len, &index, items);
        break;
    case H2LDB_INFO_MEMORY1:
        ng = (items != 0 || !d->impl.alive);
        if (ng) GOTO_BADARG;
        GOTO_NOT_IMPLEMENTED;
        break;
    case H2LDB_INFO_SIZE1:
        ng = (items != 0 || !d->impl.alive);
        if (ng) GOTO_BADARG;
        GOTO_NOT_IMPLEMENTED;
        break;
    default:
        GOTO_BADARG;
    }
    return;

 badarg:
    driver_send_int(d, H2LDB_BADARG);
    return;

 not_implemented:
    driver_send_int(d, H2LDB_NOT_IMPLEMENTED);
    return;
}

void
drv_ready_async(ErlDrvData handle, ErlDrvThreadData async_data)
{
    DrvData* d = (DrvData*) handle;
    DrvAsync* a = (DrvAsync*) async_data;
    assert(a != NULL);

    switch (a->reply) {
    case H2LDB_BADARG:
        driver_send_int(d, H2LDB_BADARG, a->caller);
        break;
    case H2LDB_TRUE:
        driver_send_int(d, H2LDB_TRUE, a->caller);
        break;
    case H2LDB_FALSE:
        driver_send_int(d, H2LDB_FALSE, a->caller);
        break;
    case H2LDB_KEY_NOT_EXIST:
        driver_send_int(d, H2LDB_KEY_NOT_EXIST, a->caller);
        break;
    case H2LDB_END_OF_TABLE:
        driver_send_int(d, H2LDB_END_OF_TABLE, a->caller);
        break;
    case H2LDB_BINARY:
        driver_send_binary(d, a->binary, a->caller);
        break;
    default:
        driver_send_int(d, H2LDB_BADARG, a->caller);
    }

    delete a;
}

void
drv_async_free(void* async_data)
{
    DrvAsync* a = (DrvAsync*) async_data;
    delete a;
}


//
// Commands
//

static void
h2ldb_output_create_db2(const char op, DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items)
{
    (void) len;
    (void) items;

    DrvAsync* drv_async = NULL;
    char *name;
    long namelen;
    char* options;
    int options_len;
    // char* read_options;
    // int read_options_len;

    int ng, term_type, size_needed;

    // db_path::binary()
    ng = ei_inspect_binary(buf, index, (void**) &name, &namelen);
    if (ng) GOTO_BADARG;
    if (!namelen) GOTO_BADARG;

    // options::[option()]
    ng = ei_get_type(buf, index, &term_type, &size_needed);
    if (ng) GOTO_BADARG;
    if (!(term_type == ERL_LIST_EXT || term_type == ERL_NIL_EXT)) GOTO_BADARG;
    options = buf + *index;
    options_len = size_needed;
    ng = ei_skip_term(buf, index);
    if (ng) GOTO_BADARG;

    if (!h2ldb_init(d->impl, name, namelen)) {
        GOTO_BADARG;
    }

    if (!h2ldb_parse_options(d->impl, options, options_len)) {
        GOTO_BADARG;
    }

    if (d->impl.async) {
        drv_async = new DrvAsync(d, driver_caller(d->port), op);
        if (!drv_async) {
            GOTO_BADARG;
        }
        driver_async(d->port, NULL, h2ldb_async_create_db2, drv_async, drv_async_free);
    } else {
        if (!h2ldb_create(d->impl, op)) {
            GOTO_BADARG;
        }
        driver_send_int(d, H2LDB_TRUE);
    }
    return;

 badarg:
    if (drv_async) { delete drv_async; }
    driver_send_int(d, H2LDB_BADARG);
    return;
}

static void
h2ldb_async_create_db2(void* async_data)
{
    DrvAsync* a = (DrvAsync*) async_data;
    assert(a != NULL);
    DrvData* d = a->drvdata;

    if (!h2ldb_create(d->impl, a->command)) {
        a->reply = H2LDB_BADARG;
    } else {
        a->reply = H2LDB_TRUE;
    }
}

static void
h2ldb_output_open_db2(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items)
{
    h2ldb_output_create_db2(OPEN, d, buf, len, index, items);
}

static void
h2ldb_output_destroy_db1(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items)
{
    h2ldb_output_create_db2(DESTROY, d, buf, len, index, items);
}

static void
h2ldb_output_repair_db2(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items)
{
    h2ldb_output_create_db2(REPAIR, d, buf, len, index, items);
}


static void
h2ldb_output_close_db1(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items)
{
    (void) buf;
    (void) len;
    (void) index;
    (void) items;

    DrvAsync* drv_async = NULL;
    leveldb::WriteOptions db_write_options;
    leveldb::WriteBatch batch;
    leveldb::Status status;

    // alive
    d->impl.alive = 0;

    if (d->impl.async) {
        drv_async = new DrvAsync(d, driver_caller(d->port), H2LDB_CLOSE_DB1);
        if (!drv_async) {
            GOTO_BADARG;
        }
        driver_async(d->port, NULL, h2ldb_async_close_db1, drv_async, drv_async_free);
    } else {
        db_write_options.sync = true;
        status = d->impl.db->Write(db_write_options, &batch);
        if (!status.ok()) {
            GOTO_BADARG;
        }

        // @TBD This is quite risky ... need to re-consider.
        // delete d->impl.db;
        // d->impl.db = NULL;

        driver_send_int(d, H2LDB_TRUE);
    }
    return;

 badarg:
    if (drv_async) { delete drv_async; }
    driver_send_int(d, H2LDB_BADARG);
    return;
}

static void
h2ldb_async_close_db1(void* async_data)
{
    DrvAsync* a = (DrvAsync*) async_data;
    assert(a != NULL);
    DrvData* d = a->drvdata;

    leveldb::WriteOptions db_write_options;
    leveldb::WriteBatch batch;
    db_write_options.sync = true;
    leveldb::Status status = d->impl.db->Write(db_write_options, &batch);
    if (!status.ok()) {
        a->reply = H2LDB_BADARG;
    } else {
        // @TBD This is quite risky ... need to re-consider.
        // delete d->impl.db;
        // d->impl.db = NULL;
        a->reply = H2LDB_TRUE;
    }
}

static void
h2ldb_output_delete3(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items)
{
    (void) len;
    (void) items;

    DrvAsync* drv_async = NULL;
    int ng, term_type, size_needed;
    char *key;
    long keylen;
    char* param_write_options;
    int param_write_options_len;
    leveldb::WriteBatch batch;
    leveldb::WriteOptions db_write_options;
    leveldb::Status status;

    // Key::binary()
    ng = ei_inspect_binary(buf, index, (void**) &key, &keylen);
    if (ng) GOTO_BADARG;

    // WriteOptions::[write_option()]
    ng = ei_get_type(buf, index, &term_type, &size_needed);
    if (ng) GOTO_BADARG;
    if (!(term_type == ERL_LIST_EXT || term_type == ERL_NIL_EXT)) GOTO_BADARG;
    param_write_options = buf + *index;
    param_write_options_len = size_needed;
    ng = ei_skip_term(buf, index);
    if (ng) GOTO_BADARG;

    db_write_options = leveldb::WriteOptions();;
    if (!h2ldb_parse_write_options(db_write_options,
                                  param_write_options, param_write_options_len)) {
        GOTO_BADARG;
    }

    if (d->impl.async) {
        drv_async = new DrvAsync(d, driver_caller(d->port), H2LDB_DELETE3, db_write_options);
        if (!drv_async) {
            GOTO_BADARG;
        }
        drv_async->del((const char*) key, keylen);
    } else {
        leveldb::Slice skey((const char*) key, keylen);
        batch.Delete(skey);
    }

    if (drv_async) {
        driver_async(d->port, NULL, h2ldb_async_delete3, drv_async, drv_async_free);
    } else {
        status = d->impl.db->Write(db_write_options, &batch);
        if (!status.ok()) {
            GOTO_BADARG;
        }

        driver_send_int(d, H2LDB_TRUE);
    }
    return;

 badarg:
    if (drv_async) { delete drv_async; }
    driver_send_int(d, H2LDB_BADARG);
    return;
}

static void
h2ldb_output_backup_db(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items)
{
    (void) len;
    (void) items;

    int ng;
    char *name;
    long namelen;

    leveldb::Status st;

    ng = ei_inspect_binary(buf, index, (void**) &name, &namelen);
    if (ng) GOTO_BADARG;


    st = d->impl.db->LiveBackup(name);
    if (!st.ok()) {
      GOTO_BADARG;
    }

    driver_send_int(d, H2LDB_TRUE);
    return;

 badarg:
    driver_send_int(d, H2LDB_BADARG);
    return;
}

static void
h2ldb_async_delete3(void* async_data)
{
    DrvAsync* a = (DrvAsync*) async_data;
    assert(a != NULL);
    DrvData* d = a->drvdata;

    leveldb::Status status = d->impl.db->Write(a->db_write_options, &(a->batch));
    if (!status.ok()) {
        a->reply = H2LDB_BADARG;
    } else {
        a->reply = H2LDB_TRUE;
    }
}

static void
h2ldb_output_get3(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items)
{
    (void) len;
    (void) items;

    DrvAsync* drv_async = NULL;
    int ng, term_type, size_needed;
    char *key;
    long keylen;
    char* param_read_options;
    int param_read_options_len;
    leveldb::ReadOptions db_read_options;

    // Key::key()
    ng = ei_inspect_binary(buf, index, (void**) &key, &keylen);
    if (ng) GOTO_BADARG;

    // ReadOptions::[read_option()]
    ng = ei_get_type(buf, index, &term_type, &size_needed);
    if (ng) GOTO_BADARG;
    if (!(term_type == ERL_LIST_EXT || term_type == ERL_NIL_EXT)) GOTO_BADARG;
    param_read_options = buf + *index;
    param_read_options_len = size_needed;
    ng = ei_skip_term(buf, index);
    if (ng) GOTO_BADARG;

    if (!h2ldb_parse_read_options(db_read_options,
                                 param_read_options, param_read_options_len)) {
        GOTO_BADARG;
    }

    if (d->impl.async) {
        drv_async = new DrvAsync(d, driver_caller(d->port), H2LDB_GET3,
                                 (const char*) key, keylen, db_read_options);
        if (!drv_async) {
            GOTO_BADARG;
        }
        driver_async(d->port, NULL, h2ldb_async_get3, drv_async, drv_async_free);
    } else {
        leveldb::Slice skey((const char*) key, keylen);
        std::string value;
        leveldb::Status status = d->impl.db->Get(db_read_options, skey, &value);
        if (status.IsNotFound()) {
            driver_send_int(d, H2LDB_KEY_NOT_EXIST);
            return;
        } else if (!status.ok()) {
            driver_send_int(d, H2LDB_BADARG);
            return;
        }

        driver_send_buf(d, value.data(), value.size());
    }
    return;

 badarg:
    if (drv_async) { delete drv_async; }
    driver_send_int(d, H2LDB_BADARG);
    return;
}

static void
h2ldb_async_get3(void* async_data)
{
    DrvAsync* a = (DrvAsync*) async_data;
    assert(a != NULL);
    DrvData* d = a->drvdata;

    leveldb::Slice skey((const char*) a->binary->orig_bytes, a->binary->orig_size);
    std::string value;
    leveldb::Status status = d->impl.db->Get(a->db_read_options, skey, &value);
    if (status.IsNotFound()) {
        a->reply = H2LDB_KEY_NOT_EXIST;
        return;
    } else if (!status.ok()) {
        a->reply = H2LDB_BADARG;
        return;
    }

    ErlDrvBinary* binary = driver_realloc_binary(a->binary, value.size());
    if (binary) {
        memcpy(binary->orig_bytes, value.data(), binary->orig_size);
        a->binary = binary;
        a->reply = H2LDB_BINARY;
    } else {
        a->reply = H2LDB_BADARG;
    }
}

static void
h2ldb_output_first2(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items)
{
    (void) buf;
    (void) len;
    (void) index;
    (void) items;

    DrvAsync* drv_async = NULL;
    int ng, term_type, size_needed;
    char* param_read_options;
    int param_read_options_len;
    leveldb::ReadOptions db_read_options;

    // read_options:: []
    ng = ei_get_type(buf, index, &term_type, &size_needed);
    if (ng) GOTO_BADARG;
    if (!(term_type == ERL_LIST_EXT || term_type == ERL_NIL_EXT)) GOTO_BADARG;
    param_read_options = buf + *index;
    param_read_options_len = size_needed;
    ng = ei_skip_term(buf, index);
    if (ng) GOTO_BADARG;

    if (!h2ldb_parse_read_options(db_read_options,
                                 param_read_options, param_read_options_len)) {
        GOTO_BADARG;
    }

    if (d->impl.async) {
        drv_async = new DrvAsync(d, driver_caller(d->port), H2LDB_FIRST2, db_read_options);
        if (!drv_async) {
            GOTO_BADARG;
        }
        driver_async(d->port, NULL, h2ldb_async_first2, drv_async, drv_async_free);
    } else  {
        leveldb::Iterator* it = d->impl.db->NewIterator(db_read_options);
        if (!it) {
            GOTO_BADARG;
        }

        it->SeekToFirst();
        if (!it->Valid()) {
            driver_send_int(d, H2LDB_END_OF_TABLE);
            delete it;
            return;
        }

        driver_send_buf(d, it->key().data(), it->key().size());
        delete it;
    }
    return;

 badarg:
    if (drv_async) { delete drv_async; }
    driver_send_int(d, H2LDB_BADARG);
    return;
}

static void
h2ldb_async_first2(void* async_data)
{
    DrvAsync* a = (DrvAsync*) async_data;
    assert(a != NULL);
    DrvData* d = a->drvdata;

    leveldb::Iterator* it = d->impl.db->NewIterator(a->db_read_options);
    if (!it) {
        a->reply = H2LDB_BADARG;
        return;
    }

    it->SeekToFirst();
    if (!it->Valid()) {
        a->reply = H2LDB_END_OF_TABLE;
        delete it;
        return;
    }

    ErlDrvBinary* binary = driver_alloc_binary(it->key().size());
    if (binary) {
        memcpy(binary->orig_bytes, it->key().data(), binary->orig_size);
        a->binary = binary;
        a->reply = H2LDB_BINARY;
    } else {
        a->reply = H2LDB_BADARG;
    }

    delete it;
}

static void
h2ldb_output_last2(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items)
{
    (void) buf;
    (void) len;
    (void) index;
    (void) items;

    DrvAsync* drv_async = NULL;
    int ng, term_type, size_needed;
    char* param_read_options;
    int param_read_options_len;
    leveldb::ReadOptions db_read_options;

    // read_options:: []
    ng = ei_get_type(buf, index, &term_type, &size_needed);
    if (ng) GOTO_BADARG;
    if (!(term_type == ERL_LIST_EXT || term_type == ERL_NIL_EXT)) GOTO_BADARG;
    param_read_options = buf + *index;
    param_read_options_len = size_needed;
    ng = ei_skip_term(buf, index);
    if (ng) GOTO_BADARG;

    if (!h2ldb_parse_read_options(db_read_options,
                                 param_read_options, param_read_options_len)) {
        GOTO_BADARG;
    }

    if (d->impl.async) {
        drv_async = new DrvAsync(d, driver_caller(d->port), H2LDB_LAST2, db_read_options);
        if (!drv_async) {
            GOTO_BADARG;
        }
        driver_async(d->port, NULL, h2ldb_async_last2, drv_async, drv_async_free);
    } else  {
        leveldb::Iterator* it = d->impl.db->NewIterator(db_read_options);
        if (!it) {
            GOTO_BADARG;
        }

        it->SeekToLast();
        if (!it->Valid()) {
            driver_send_int(d, H2LDB_END_OF_TABLE);
            delete it;
            return;
        }

        driver_send_buf(d, it->key().data(), it->key().size());
        delete it;
    }
    return;

 badarg:
    if (drv_async) { delete drv_async; }
    driver_send_int(d, H2LDB_BADARG);
    return;
}

static void
h2ldb_async_last2(void* async_data)
{
    DrvAsync* a = (DrvAsync*) async_data;
    assert(a != NULL);
    DrvData* d = a->drvdata;

    leveldb::Iterator* it = d->impl.db->NewIterator(a->db_read_options);
    if (!it) {
        a->reply = H2LDB_BADARG;
        return;
    }

    it->SeekToLast();
    if (!it->Valid()) {
        a->reply = H2LDB_END_OF_TABLE;
        delete it;
        return;
    }

    ErlDrvBinary* binary = driver_alloc_binary(it->key().size());
    if (binary) {
        memcpy(binary->orig_bytes, it->key().data(), binary->orig_size);
        a->binary = binary;
        a->reply = H2LDB_BINARY;
    } else {
        a->reply = H2LDB_BADARG;
    }

    delete it;
}

static void
h2ldb_output_next3(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items)
{
    (void) len;
    (void) items;

    DrvAsync* drv_async = NULL;
    int ng, term_type, size_needed;
    char *key;
    long keylen;
    char* param_read_options;
    int param_read_options_len;
    leveldb::ReadOptions db_read_options;

    ng = ei_inspect_binary(buf, index, (void**) &key, &keylen);
    if (ng) GOTO_BADARG;

    // read_options:: []
    ng = ei_get_type(buf, index, &term_type, &size_needed);
    if (ng) GOTO_BADARG;
    if (!(term_type == ERL_LIST_EXT || term_type == ERL_NIL_EXT)) GOTO_BADARG;
    param_read_options = buf + *index;
    param_read_options_len = size_needed;
    ng = ei_skip_term(buf, index);
    if (ng) GOTO_BADARG;

    if (!h2ldb_parse_read_options(db_read_options,
                                 param_read_options, param_read_options_len)) {
        GOTO_BADARG;
    }

    if (d->impl.async) {
        drv_async = new DrvAsync(d, driver_caller(d->port), H2LDB_NEXT3,
                                 (const char*) key, keylen, db_read_options);
        if (!drv_async) {
            GOTO_BADARG;
        }
        driver_async(d->port, NULL, h2ldb_async_next3, drv_async, drv_async_free);
    } else {
        leveldb::Iterator* it = d->impl.db->NewIterator(db_read_options);
        if (!it) {
            GOTO_BADARG;
        }

        leveldb::Slice skey((const char*) key, keylen);
        it->Seek(skey);
        if (!it->Valid()) {
            driver_send_int(d, H2LDB_END_OF_TABLE);
            delete it;
            return;
        }

        if (it->key().compare(skey) == 0) {
            it->Next();
            if (!it->Valid()) {
                driver_send_int(d, H2LDB_END_OF_TABLE);
                delete it;
                return;
            }
        }

        driver_send_buf(d, it->key().data(), it->key().size());
        delete it;
    }
    return;

 badarg:
    if (drv_async) { delete drv_async; }
    driver_send_int(d, H2LDB_BADARG);
}

static void
h2ldb_async_next3(void* async_data)
{
    DrvAsync* a = (DrvAsync*) async_data;
    assert(a != NULL);
    DrvData* d = a->drvdata;

    leveldb::Iterator* it = d->impl.db->NewIterator(a->db_read_options);
    if (!it) {
        a->reply = H2LDB_BADARG;
        return;
    }

    leveldb::Slice skey((const char*) a->binary->orig_bytes, a->binary->orig_size);
    it->Seek(skey);
    if (!it->Valid()) {
        a->reply = H2LDB_END_OF_TABLE;
        delete it;
        return;
    }

    if (it->key().compare(skey) == 0) {
        it->Next();
        if (!it->Valid()) {
            a->reply = H2LDB_END_OF_TABLE;
            delete it;
            return;
        }
    }

    ErlDrvBinary* binary = driver_realloc_binary(a->binary, it->key().size());
    if (binary) {
        memcpy(binary->orig_bytes, it->key().data(), binary->orig_size);
        a->binary = binary;
        a->reply = H2LDB_BINARY;
    } else {
        a->reply = H2LDB_BADARG;
    }

    delete it;
}

static void
h2ldb_output_prev3(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items)
{
    (void) len;
    (void) items;

    DrvAsync* drv_async = NULL;
    int ng, term_type, size_needed;
    char *key;
    long keylen;
    char* param_read_options;
    int param_read_options_len;
    leveldb::ReadOptions db_read_options;

    ng = ei_inspect_binary(buf, index, (void**) &key, &keylen);
    if (ng) GOTO_BADARG;

    // read_options:: []
    ng = ei_get_type(buf, index, &term_type, &size_needed);
    if (ng) GOTO_BADARG;
    if (!(term_type == ERL_LIST_EXT || term_type == ERL_NIL_EXT)) GOTO_BADARG;
    param_read_options = buf + *index;
    param_read_options_len = size_needed;
    ng = ei_skip_term(buf, index);
    if (ng) GOTO_BADARG;

    if (!h2ldb_parse_read_options(db_read_options,
                                 param_read_options, param_read_options_len)) {
        GOTO_BADARG;
    }

    if (d->impl.async) {
        drv_async = new DrvAsync(d, driver_caller(d->port), H2LDB_PREV3,
                                 (const char*) key, keylen, db_read_options);
        if (!drv_async) {
            GOTO_BADARG;
        }
        driver_async(d->port, NULL, h2ldb_async_prev3, drv_async, drv_async_free);
    } else {
        leveldb::Iterator* it = d->impl.db->NewIterator(db_read_options);
        if (!it) {
            GOTO_BADARG;
        }

        leveldb::Slice skey((const char*) key, keylen);
        it->Seek(skey);
        if (!it->Valid()) {
            it->SeekToLast();
        } else {
            it->Prev();
        }

        if (!it->Valid()) {
            driver_send_int(d, H2LDB_END_OF_TABLE);
            delete it;
            return;
        }

        driver_send_buf(d, it->key().data(), it->key().size());
        delete it;
    }
    return;

 badarg:
    if (drv_async) { delete drv_async; }
    driver_send_int(d, H2LDB_BADARG);
}

static void
h2ldb_async_prev3(void* async_data)
{
    DrvAsync* a = (DrvAsync*) async_data;
    assert(a != NULL);
    DrvData* d = a->drvdata;

    leveldb::Iterator* it = d->impl.db->NewIterator(a->db_read_options);
    if (!it) {
        a->reply = H2LDB_BADARG;
        return;
    }

    leveldb::Slice skey((const char*) a->binary->orig_bytes, a->binary->orig_size);
    it->Seek(skey);
    if (!it->Valid()) {
        it->SeekToLast();
    } else {
        it->Prev();
    }

    if (!it->Valid()) {
        a->reply = H2LDB_END_OF_TABLE;
        delete it;
        return;
    }

    ErlDrvBinary* binary = driver_realloc_binary(a->binary, it->key().size());
    if (binary) {
        memcpy(binary->orig_bytes, it->key().data(), binary->orig_size);
        a->binary = binary;
        a->reply = H2LDB_BINARY;
    } else {
        a->reply = H2LDB_BADARG;
    }

    delete it;
}

static void
h2ldb_output_put4(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items)
{
    (void) len;
    (void) items;

    DrvAsync* drv_async = NULL;
    int ng, term_type, size_needed;
    char *key;
    long keylen;
    char *blob;
    long bloblen;
    char* param_write_options;
    int param_write_options_len;
    leveldb::WriteBatch batch;
    leveldb::WriteOptions db_write_options;
    leveldb::Status status;

    // Key::binary()
    ng = ei_inspect_binary(buf, index, (void**) &key, &keylen);
    if (ng) GOTO_BADARG;

    // Blob::binary()
    ng = ei_inspect_binary(buf, index, (void**) &blob, &bloblen);
    if (ng) GOTO_BADARG;

    // WriteOptions::[write_option()]
    ng = ei_get_type(buf, index, &term_type, &size_needed);
    if (ng) GOTO_BADARG;
    if (!(term_type == ERL_LIST_EXT || term_type == ERL_NIL_EXT)) GOTO_BADARG;
    param_write_options = buf + *index;
    param_write_options_len = size_needed;
    ng = ei_skip_term(buf, index);
    if (ng) GOTO_BADARG;

    db_write_options = leveldb::WriteOptions();;
    if (!h2ldb_parse_write_options(db_write_options,
                                  param_write_options, param_write_options_len)) {
        GOTO_BADARG;
    }

    if (d->impl.async) {
        drv_async = new DrvAsync(d, driver_caller(d->port), H2LDB_PUT4, db_write_options);
        if (!drv_async) {
            GOTO_BADARG;
        }
        drv_async->put((const char*) key, keylen, (const char*) blob, bloblen);
    } else {
        leveldb::Slice skey((const char*) key, keylen);
        leveldb::Slice sblob((const char*) blob, bloblen);
        batch.Put(skey, sblob);
    }

    if (drv_async) {
        driver_async(d->port, NULL, h2ldb_async_put4, drv_async, drv_async_free);
    } else {
        status = d->impl.db->Write(db_write_options, &batch);
        if (!status.ok()) {
            GOTO_BADARG;
        }

        driver_send_int(d, H2LDB_TRUE);
    }
    return;

 badarg:
    if (drv_async) { delete drv_async; }
    driver_send_int(d, H2LDB_BADARG);
    return;
}

static void
h2ldb_async_put4(void* async_data)
{
    DrvAsync* a = (DrvAsync*) async_data;
    assert(a != NULL);
    DrvData* d = a->drvdata;

    leveldb::Status status = d->impl.db->Write(a->db_write_options, &(a->batch));
    if (!status.ok()) {
        a->reply = H2LDB_BADARG;
    } else {
        a->reply = H2LDB_TRUE;
    }
}

static void
h2ldb_output_write_batch3(DrvData* d, char* buf, ErlDrvSizeT len, int* index, int items)
{
    (void) len;

    DrvAsync* drv_async = NULL;
    int ng, arity, term_type, size_needed;
    char command;
    char *key;
    long keylen;
    char *blob;
    long bloblen;
    char* param_write_options;
    int param_write_options_len;
    leveldb::WriteBatch batch;
    leveldb::WriteOptions db_write_options;
    leveldb::Status status;

    // WriteOptions::[write_option()]
    ng = ei_get_type(buf, index, &term_type, &size_needed);
    if (ng) GOTO_BADARG;
    if (!(term_type == ERL_LIST_EXT || term_type == ERL_NIL_EXT)) GOTO_BADARG;
    param_write_options = buf + *index;
    param_write_options_len = size_needed;
    ng = ei_skip_term(buf, index);
    if (ng) GOTO_BADARG;

    db_write_options = leveldb::WriteOptions();;
    if (!h2ldb_parse_write_options(db_write_options,
                                  param_write_options, param_write_options_len)) {
        GOTO_BADARG;
    }

    if (!items) {
        driver_send_int(d, H2LDB_TRUE);
        return;
    }

    ng = ei_decode_list_header(buf, index, &items);
    if (ng) GOTO_BADARG;

    if (d->impl.async) {
        drv_async = new DrvAsync(d, driver_caller(d->port), H2LDB_WRITE_BATCH3, db_write_options);
        if (!drv_async) {
            GOTO_BADARG;
        }
    }

    while (items) {
        ng = ei_decode_tuple_header(buf, index, &arity);
        if (ng) GOTO_BADARG;

        ng = ei_decode_char(buf, index, &command);
        if (ng) GOTO_BADARG;

        items--;
        switch (command) {
        case H2LDB_BATCH_PUT:
            ng = (arity != 3);
            if (ng) GOTO_BADARG;

            // Key::binary()
            ng = ei_inspect_binary(buf, index, (void**) &key, &keylen);
            if (ng) GOTO_BADARG;
            // Blob::binary()
            ng = ei_inspect_binary(buf, index, (void**) &blob, &bloblen);
            if (ng) GOTO_BADARG;

            if (drv_async) {
                drv_async->put((const char*) key, keylen, (const char*) blob, bloblen);
            } else {
                leveldb::Slice skey((const char*) key, keylen);
                leveldb::Slice sblob((const char*) blob, bloblen);
                batch.Put(skey, sblob);
            }
            break;
        case H2LDB_BATCH_DELETE:
            ng = (arity != 2);
            if (ng) GOTO_BADARG;

            // Key::binary()
            ng = ei_inspect_binary(buf, index, (void**) &key, &keylen);
            if (ng) GOTO_BADARG;

            if (drv_async) {
                drv_async->del((const char*) key, keylen);
            } else {
                leveldb::Slice skey((const char*) key, keylen);
                batch.Delete(skey);
            }
            break;
        default:
            GOTO_BADARG;
        }
    }

    ng = ei_decode_list_header(buf, index, &items);
    if (ng) GOTO_BADARG;
    ng = (items != 0);
    if (ng) GOTO_BADARG;

    if (drv_async) {
        driver_async(d->port, NULL, h2ldb_async_write_batch3, drv_async, drv_async_free);
    } else {
        status = d->impl.db->Write(db_write_options, &batch);
        if (!status.ok()) {
            GOTO_BADARG;
        }

        driver_send_int(d, H2LDB_TRUE);
    }
    return;

 badarg:
    if (drv_async) { delete drv_async; }
    driver_send_int(d, H2LDB_BADARG);
    return;
}

static void
h2ldb_async_write_batch3(void* async_data)
{
    DrvAsync* a = (DrvAsync*) async_data;
    assert(a != NULL);
    DrvData* d = a->drvdata;

    leveldb::Status status = d->impl.db->Write(a->db_write_options, &(a->batch));
    if (!status.ok()) {
        a->reply = H2LDB_BADARG;
    } else {
        a->reply = H2LDB_TRUE;
    }
}

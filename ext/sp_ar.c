#include "ruby/ruby.h"

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include <string.h>

#if SIZEOF_LONG == SIZEOF_VOIDP
typedef unsigned long st_data_t;
#elif SIZEOF_LONG_LONG == SIZEOF_VOIDP
typedef unsigned LONG_LONG st_data_t;
#else
# error ---->> sp_ar.c requires sizeof(void*) == sizeof(long) or sizeof(LONG_LONG) to be compiled. <<----
#endif

typedef unsigned int spar_index_t;
#define spar_STOP     ST_STOP
#define spar_CONTINUE ST_CONTINUE

#define spar_EMPTY   0

typedef struct spar_entry {
    spar_index_t next;
    spar_index_t key;
    st_data_t value;
} spar_entry;

typedef struct spar_table {
    spar_index_t num_bins;
    spar_index_t num_entries;
    spar_index_t free_pos;
    spar_entry *entries;
} spar_table;

#define spar_EMPTY_TABLE {0, 0, 0, 0};
static void spar_init_table(spar_table *, spar_index_t);
static spar_table *spar_new_table();
static int  spar_insert(spar_table *, spar_index_t, st_data_t);
static int  spar_lookup(spar_table *, spar_index_t, st_data_t *);
static int  spar_delete(spar_table *, spar_index_t, st_data_t *);
static void spar_clear(spar_table *);
static void spar_free_table(spar_table *);
static size_t spar_memsize(const spar_table *);
static void spar_copy_to(spar_table*, spar_table*);
typedef int (*spar_iter_func)(spar_index_t key, st_data_t val, st_data_t arg);

#define SPAR_FOREACH_START_I(table, entry) do { \
    spar_table *T##entry = (table); \
    spar_index_t K##entry; \
    for(K##entry = 0; K##entry < T##entry->num_bins; K##entry++) { \
	spar_entry *entry = T##entry->entries + K##entry; \
	if (entry->next != spar_EMPTY) { \
	    st_data_t value = entry->value
#define SPAR_FOREACH_END() } } } while(0)

#define SPAR_FOREACH_START(table) SPAR_FOREACH_START_I(table, entry)

#define malloc xmalloc
#define calloc xcalloc
#define realloc xrealloc
#define free   xfree

#define spar_table_alloc()          (spar_table*)malloc(sizeof(spar_table))
#define spar_table_xalloc()         (spar_table*)calloc(1, sizeof(spar_table))
#define spar_table_dealloc(table)   free(table)
#define spar_entry_alloc(n)         (spar_entry*)calloc((n), sizeof(spar_entry))
#define spar_entry_dealloc(entries) free(entries)

#define spar_LAST   1
#define spar_OFFSET 2

#define spar_MIN_SIZE 4

static void
spar_init_table(register spar_table *table, spar_index_t num_bins)
{
    if (num_bins) {
        table->num_entries = 0;
        table->entries = spar_entry_alloc(num_bins);
        table->num_bins = num_bins;
        table->free_pos = num_bins;
    }
    else {
        memset(table, 0, sizeof(spar_table));
    }
}

static spar_table*
spar_new_table()
{
    spar_table* table = spar_table_alloc();
    spar_init_table(table, 0);
    return table;
}

static inline spar_index_t
calc_pos(register spar_table* table, spar_index_t key)
{
    /* this formula is empirical */
    /* it has no good avalance, but works well in our case */
    key ^= key >> 16;
    key *= 0x445229;
    return (key + (key >> 16)) % table->num_bins;
}

static void
fix_empty(register spar_table* table)
{
    while(--table->free_pos &&
            table->entries[table->free_pos-1].next != spar_EMPTY);
}

#define FLOOR_TO_4 ((~((spar_index_t)0)) << 2)
static spar_index_t
find_empty(register spar_table* table, register spar_index_t pos)
{
    spar_index_t new_pos = table->free_pos-1;
    spar_entry *entry;
    pos &= FLOOR_TO_4;
    entry = table->entries+pos;

    if (entry->next == spar_EMPTY) { new_pos = pos; }
    else if ((++entry)->next == spar_EMPTY) { new_pos = pos + 1; }
    else if ((++entry)->next == spar_EMPTY) { new_pos = pos + 2; }
    else if ((++entry)->next == spar_EMPTY) { new_pos = pos + 3; }

    if (new_pos+1 == table->free_pos) fix_empty(table);
    return new_pos;
}

static void resize(register spar_table* table);
static int insert_into_chain(register spar_table*, register spar_index_t, st_data_t, spar_index_t pos);
static int insert_into_main(register spar_table*, spar_index_t, st_data_t, spar_index_t pos, spar_index_t prev_pos);

static int
spar_insert(register spar_table* table, register spar_index_t key, st_data_t value)
{
    spar_index_t pos, main_pos;
    register spar_entry *entry;

    if (table->num_bins == 0) {
        spar_init_table(table, spar_MIN_SIZE);
    }

    pos = calc_pos(table, key);
    entry = table->entries + pos;

    if (entry->next == spar_EMPTY) {
        entry->next = spar_LAST;
        entry->key = key;
        entry->value = value;
        table->num_entries++;
        if (pos+1 == table->free_pos) fix_empty(table);
        return 0;
    }

    if (entry->key == key) {
        entry->value = value;
        return 1;
    }

    if (table->num_entries + (table->num_entries >> 2) > table->num_bins) {
        resize(table);
        return spar_insert(table, key, value);
    }

    main_pos = calc_pos(table, entry->key);
    if (main_pos == pos) {
        return insert_into_chain(table, key, value, pos);
    }
    else {
        if (!table->free_pos) {
            resize(table);
            return spar_insert(table, key, value);
        }
        return insert_into_main(table, key, value, pos, main_pos);
    }
}

static int
insert_into_chain(register spar_table* table, register spar_index_t key, st_data_t value, spar_index_t pos)
{
    spar_entry *entry = table->entries + pos, *new_entry;
    spar_index_t new_pos;

    while (entry->next != spar_LAST) {
        pos = entry->next - spar_OFFSET;
        entry = table->entries + pos;
        if (entry->key == key) {
            entry->value = value;
            return 1;
        }
    }

    if (!table->free_pos) {
        resize(table);
        return spar_insert(table, key, value);
    }

    new_pos = find_empty(table, pos);
    new_entry = table->entries + new_pos;
    entry->next = new_pos + spar_OFFSET;

    new_entry->next = spar_LAST;
    new_entry->key = key;
    new_entry->value = value;
    table->num_entries++;
    return 0;
}

static int
insert_into_main(register spar_table* table, spar_index_t key, st_data_t value, spar_index_t pos, spar_index_t prev_pos)
{
    spar_entry *entry = table->entries + pos;
    spar_index_t new_pos = find_empty(table, pos);
    spar_entry *new_entry = table->entries + new_pos;
    spar_index_t npos;

    *new_entry = *entry;

    while((npos = table->entries[prev_pos].next - spar_OFFSET) != pos) {
        prev_pos = npos;
    }
    table->entries[prev_pos].next = new_pos + spar_OFFSET;

    entry->next = spar_LAST;
    entry->key = key;
    entry->value = value;
    table->num_entries++;
    return 0;
}

static spar_index_t
new_size(spar_index_t num_entries)
{
    spar_index_t msb = num_entries;
    msb |= msb >> 1;
    msb |= msb >> 2;
    msb |= msb >> 4;
    msb |= msb >> 8;
    msb |= msb >> 16;
    msb = ((msb >> 4) + 1) << 3;
    return (num_entries & (msb | (msb >> 1))) + (msb >> 1);
}

static void
resize(register spar_table *table)
{
    spar_table tmp_table;
    spar_entry *entry;
    spar_index_t i;

    if (table->num_entries == 0) {
        spar_entry_dealloc(table->entries);
        memset(table, 0, sizeof(spar_table));
        return;
    }

    spar_init_table(&tmp_table, new_size(table->num_entries + (table->num_entries >> 2)));
    entry = table->entries;

    for(i = 0; i < table->num_bins; i++, entry++) {
        if (entry->next != spar_EMPTY) {
            spar_insert(&tmp_table, entry->key, entry->value);
        }
    }
    spar_entry_dealloc(table->entries);
    *table = tmp_table;
}

static int
spar_lookup(register spar_table *table, register spar_index_t key, st_data_t *value)
{
    register spar_entry *entry;

    if (table->num_entries == 0) return 0;

    entry = table->entries + calc_pos(table, key);
    if (entry->next == spar_EMPTY) return 0;

    if (entry->key == key) goto found;
    if (entry->next == spar_LAST) return 0;

    entry = table->entries + (entry->next - spar_OFFSET);
    if (entry->key == key) goto found;

    while(entry->next != spar_LAST) {
        entry = table->entries + (entry->next - spar_OFFSET);
        if (entry->key == key) goto found;
    }
    return 0;
found:
    if (value) *value = entry->value;
    return 1;
}

static void
spar_clear(spar_table *table)
{
    spar_entry_dealloc(table->entries);
    memset(table, 0, sizeof(spar_table));
}

static void
spar_free_table(spar_table *table)
{
    spar_entry_dealloc(table->entries);
    spar_table_dealloc(table);
}

static int
spar_delete(spar_table *table, spar_index_t key, st_data_t *value)
{
    spar_index_t pos, prev_pos = ~0;
    spar_entry *entry;

    if (table->num_entries == 0) goto not_found;

    pos = calc_pos(table, key);
    entry = table->entries + pos;

    if (entry->next == spar_EMPTY) goto not_found;

    do {
        if (entry->key == key) {
            if (value) *value = entry->value;
            if (entry->next != spar_LAST) {
                spar_index_t npos = entry->next - spar_OFFSET;
                *entry = table->entries[npos];
                memset(table->entries + npos, 0, sizeof(spar_entry));
            }
            else {
                memset(table->entries + pos, 0, sizeof(spar_entry));
                if (~prev_pos) {
                    table->entries[prev_pos].next = spar_LAST;
                }
            }
            table->num_entries--;
            if (table->num_entries < table->num_bins / 4) {
                resize(table);
            }
            return 1;
        }
        if (entry->next == spar_LAST) break;
        prev_pos = pos;
        pos = entry->next - spar_OFFSET;
        entry = table->entries + pos;
    } while(1);

not_found:
    if (value) *value = 0;
    return 0;
}

static size_t
spar_memsize(const spar_table *table)
{
    return sizeof(spar_table) + table->num_bins * sizeof(spar_entry);
}

static void
spar_copy_to(spar_table *from, spar_table *to)
{
    spar_entry_dealloc(to->entries);
    *to = *from;
    if (to->num_bins) {
	to->entries = spar_entry_alloc(to->num_bins);
	memcpy(to->entries, from->entries, from->num_bins*sizeof(spar_entry));
    }
}

/******** Ruby Sparse Array binding ********/

static void
sparse_array_mark(void *p)
{
    if (p) {
        spar_table *ar = (spar_table*)p;
        SPAR_FOREACH_START(ar);
            rb_gc_mark((VALUE)value);
        SPAR_FOREACH_END();
    }
}

static void
sparse_array_delete(void *p)
{
    if (p) {
        spar_free_table((spar_table*) p);
    }
}

static size_t
sparse_array_memsize(const void *p)
{
    if (p) {
        return spar_memsize((const spar_table*) p);
    }
    return 0;
}

static const rb_data_type_t SparseArray_data_type = {
	"SparseArrayC",
	{sparse_array_mark, sparse_array_delete, sparse_array_memsize}
};

#define GetSparseArray(value, pointer) \
    TypedData_Get_Struct((value), spar_table, &SparseArray_data_type, (pointer))

static VALUE
sparse_array_alloc(VALUE klass)
{
    spar_table* table = spar_new_table();
    return TypedData_Wrap_Struct(klass, &SparseArray_data_type, table);
}

static VALUE
sparse_array_get(VALUE self, VALUE ri)
{
    spar_table *table;
    spar_index_t i = NUM2UINT(ri);
    st_data_t res = Qnil;
    GetSparseArray(self, table);
    spar_lookup(table, i, &res);
    return (VALUE)res;
}

static VALUE
sparse_array_include(VALUE self, VALUE ri)
{
    spar_table *table;
    spar_index_t i = NUM2UINT(ri);
    st_data_t res = Qnil;
    GetSparseArray(self, table);
    if (spar_lookup(table, i, &res))
        return Qtrue;
    else
        return Qfalse;
}

static VALUE
sparse_array_fetch(VALUE self, VALUE ri, VALUE def)
{
    spar_table *table;
    spar_index_t i = NUM2UINT(ri);
    st_data_t res = Qnil;
    GetSparseArray(self, table);
    if (spar_lookup(table, i, &res))
        return (VALUE)res;
    else
        return def;
}

static VALUE
sparse_array_set(VALUE self, VALUE ri, VALUE val)
{
    spar_table *table;
    spar_index_t i = NUM2UINT(ri);
    GetSparseArray(self, table);
    spar_insert(table, i, val);
    return val;
}

static VALUE
sparse_array_del(VALUE self, VALUE ri)
{
    spar_table *table;
    spar_index_t i = NUM2UINT(ri);
    st_data_t res = Qnil;
    GetSparseArray(self, table);
    if (spar_delete(table, i, &res))
        return (VALUE)res;
    return Qnil;
}

static VALUE
sparse_array_size(VALUE self)
{
    spar_table *table;
    GetSparseArray(self, table);
    return UINT2NUM(table->num_entries);
}

static VALUE
sparse_array_empty_p(VALUE self)
{
    spar_table *table;
    GetSparseArray(self, table);
    return table->num_entries ? Qfalse : Qtrue;
}

static VALUE
sparse_array_clear(VALUE self)
{
    spar_table *table;
    GetSparseArray(self, table);
    spar_clear(table);
    return self;
}

static VALUE
sparse_array_keys(VALUE self)
{
    spar_table *table;
    VALUE res;
    GetSparseArray(self, table);
    res = rb_ary_new2(table->num_entries);
    SPAR_FOREACH_START(table);
    (void)value;
    rb_ary_push(res, UINT2NUM(entry->key));
    SPAR_FOREACH_END();
    return res;
}

static VALUE
sparse_array_values(VALUE self)
{
    spar_table *table;
    VALUE res;
    GetSparseArray(self, table);
    res = rb_ary_new2(table->num_entries);
    SPAR_FOREACH_START(table);
    rb_ary_push(res, (VALUE)value);
    SPAR_FOREACH_END();
    return res;
}

static VALUE
sparse_array_each(VALUE self)
{
    spar_table *table;
    VALUE keys;
    long i, size;
    VALUE *p;
    RETURN_ENUMERATOR(self, 0, 0);
    GetSparseArray(self, table);
    keys = sparse_array_keys(self);
    size = RARRAY_LEN(keys);
    p = RARRAY_PTR(keys);
    for(i=0; i<size; i++) {
        spar_index_t k = NUM2UINT(p[i]);
        st_data_t v = Qnil;
        if (spar_lookup(table, k, &v))
            rb_yield(rb_assoc_new(p[i], (VALUE)v));
    }
    RB_GC_GUARD(keys);
    return self;
}

static VALUE
sparse_array_each_key(VALUE self)
{
    spar_table *table;
    VALUE keys;
    long i, size;
    VALUE *p;
    RETURN_ENUMERATOR(self, 0, 0);
    GetSparseArray(self, table);
    keys = sparse_array_keys(self);
    size = RARRAY_LEN(keys);
    p = RARRAY_PTR(keys);
    for(i=0; i<size; i++) {
        spar_index_t k = NUM2UINT(p[i]);
        st_data_t v = Qnil;
        if (spar_lookup(table, k, &v))
            rb_yield(p[i]);
    }
    RB_GC_GUARD(keys);
    return self;
}

static VALUE
sparse_array_each_value(VALUE self)
{
    spar_table *table;
    VALUE keys;
    long i, size;
    VALUE *p;
    RETURN_ENUMERATOR(self, 0, 0);
    GetSparseArray(self, table);
    keys = sparse_array_keys(self);
    size = RARRAY_LEN(keys);
    p = RARRAY_PTR(keys);
    for(i=0; i<size; i++) {
        spar_index_t k = NUM2UINT(p[i]);
        st_data_t v = Qnil;
        if (spar_lookup(table, k, &v))
            rb_yield(v);
    }
    RB_GC_GUARD(keys);
    return self;
}

static VALUE
sparse_array_init_copy(VALUE self, VALUE copy)
{
    spar_table *table;
    spar_table *copied;
    GetSparseArray(self, table);
    GetSparseArray(copy, copied);
    rb_obj_init_copy(self, copy);
    spar_copy_to(table, copied);
    return copy;
}

static VALUE
sparse_array_inspect_rec(VALUE self, VALUE dummy, int recur)
{
    VALUE str, str2;
    spar_table *table;
    GetSparseArray(self, table);

    if (recur) return rb_usascii_str_new2("<SparseArray ...>");
    str = rb_str_buf_new2("<SparseArray");
    SPAR_FOREACH_START(table);
        rb_str_catf(str, " %u=>", entry->key);
        str2 = rb_inspect(value);
        rb_str_buf_append(str, str2);
        OBJ_INFECT(str, str2);
    SPAR_FOREACH_END();
    rb_str_buf_cat2(str, ">");
    OBJ_INFECT(str, self);

    return str;
}

static VALUE
sparse_array_inspect(VALUE self)
{
    spar_table *table;
    GetSparseArray(self, table);
    if (table->num_entries == 0)
        return rb_usascii_str_new2("<SparseArray>");
    return rb_exec_recursive(sparse_array_inspect_rec, self, 0);
}

void
Init_sparse_array() {
    VALUE cls_sparse_array = rb_define_class("SparseArray", rb_cObject);
    rb_define_alloc_func(cls_sparse_array, sparse_array_alloc);
    rb_define_method(cls_sparse_array, "[]", sparse_array_get, 1);
    rb_define_method(cls_sparse_array, "fetch", sparse_array_fetch, 2);
    rb_define_method(cls_sparse_array, "[]=", sparse_array_set, 2);
    rb_define_method(cls_sparse_array, "delete", sparse_array_del, 1);
    rb_define_method(cls_sparse_array, "clear", sparse_array_clear, 0);
    rb_define_method(cls_sparse_array, "empty?", sparse_array_empty_p, 0);
    rb_define_method(cls_sparse_array, "size", sparse_array_size, 1);
    rb_define_method(cls_sparse_array, "count", sparse_array_size, 1);
    rb_define_method(cls_sparse_array, "include?", sparse_array_include, 1);
    rb_define_method(cls_sparse_array, "has_key?", sparse_array_include, 1);
    rb_define_method(cls_sparse_array, "keys", sparse_array_keys, 0);
    rb_define_method(cls_sparse_array, "values", sparse_array_values, 0);
    rb_define_method(cls_sparse_array, "each", sparse_array_each, 0);
    rb_define_method(cls_sparse_array, "each_pair", sparse_array_each, 0);
    rb_define_method(cls_sparse_array, "each_key", sparse_array_each_key, 0);
    rb_define_method(cls_sparse_array, "each_value", sparse_array_each_value, 0);
    rb_define_method(cls_sparse_array, "inspect", sparse_array_inspect, 0);
    rb_define_method(cls_sparse_array, "initialize_copy", sparse_array_init_copy, 1);
    rb_include_module(cls_sparse_array, rb_mEnumerable);
}

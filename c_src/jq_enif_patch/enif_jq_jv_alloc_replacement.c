
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <erl_nif.h>
#include "jv_alloc.h"
//#include "enif_jq.h"
#include "jv_nomem_handler_code.h"
//#define HASHTABLE_U64 void*
#define HASHTABLE_IMPLEMENTATION
#include "hashtable.h"

static int is_table_init = 0;
static hashtable_t table;
static void init_table_if_needed(){
    if (! is_table_init) {
        hashtable_init( &table, sizeof( int ), 256, NULL );
        is_table_init = 1;
    }
}


void* jv_mem_alloc(size_t sz) {
    init_table_if_needed();
  void* p = enif_alloc(sz);
  if (!p) {
    memory_exhausted();
  }
  printf("ALLOC %p\n", p);
  int true_v = 1;
  hashtable_insert( &table, p, &true_v );
  return p;
}

void* jv_mem_alloc_unguarded(size_t sz) {
  return jv_mem_alloc(sz);
}

void* jv_mem_calloc(size_t nemb, size_t sz) {
  size_t total_size = nemb * sz;
  void *p = jv_mem_alloc(total_size);
  memset(p, 0, total_size);
  printf("CALLOC %p\n", p);
  return p;
}

void* jv_mem_calloc_unguarded(size_t nemb, size_t sz) {
  return jv_mem_calloc(nemb, sz);
}

char* jv_mem_strdup(const char *s) {
  size_t length  = strlen(s);
  char *p = jv_mem_alloc(length + 1);
  if (!p) {
    memory_exhausted();
  }
  strcpy(p, s);
  printf("STRDUP %p\n", p);
  return p;
}

char* jv_mem_strdup_unguarded(const char *s) {
    return jv_mem_strdup(s);
}

void jv_mem_free(void* p) {
  printf("FREE %p\n", p);
  if (p == NULL) {
    return;
  }
  int* res = hashtable_find(&table, p);
  if (res == NULL) {
    printf("A memory block that we did not allocate overself\n");
    abort();
  } else {
    hashtable_remove( &table, p);
    enif_free(p);
  }
}

void* jv_mem_realloc(void* p, size_t sz) {
    init_table_if_needed();
  p = enif_realloc(p, sz);
  if (!p) {
    memory_exhausted();
  }
  printf("REALLOC %p\n", p);
  int true_v = 1;
  hashtable_insert( &table, p, &true_v );
  return p;
}


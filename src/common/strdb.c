/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ const char * strdb_c_rcsid() {return "$Id: strdb.c,v 1.21 2007/11/22 23:28:39 dkc Exp $";}

/*! \file
    C interface into string pools
*/

#include "system.h"
#include "poolmngr.h"
#include "pool.h"
#include "strfunc.h"
#include "strdb.h"
#include "v2k_mem.h"

char defPoolMode[33] = "pcrw";

typedef struct {
   poolRef *array;
   int      size;
} StrSlot;

#define SLOT_INCR 10
#define HASH_MAX  0x10000
#define HASH_MSK  (HASH_MAX -1)

static StrSlot Hash[HASH_MAX];

unsigned int strHashSeg(const char *str,int l)
{
   int h = 0;
#ifndef STR_NOHASH
   while (l-- > 0) h = (h << 4) + *str++ + (h >> 12);
#endif
   return h & HASH_MSK;
}

unsigned int strHash(const char *str)
{
   int h = 0;
#ifndef STR_NOHASH
   while (*str) h = (h << 4) + *str++ + (h >> 12);
#endif
   return h & HASH_MSK;
}

void strRegister(const PoolStr *s,const poolRef ref,int pool_id)
{
   unsigned short  h    = strHash(s->buff);
   StrSlot        *ss   = &Hash[h];
   int             j;

   if (!(j = ss->size)) { /* empty */
     ss->array = (poolRef *)Calloc(ss->size = SLOT_INCR,sizeof(*ss->array));
   } else {
     while (!ss->array[j-1].pool) j--;
     if (j == ss->size) { /* full */
        ss->array = (poolRef *)Realloc(ss->array,
                            (ss->size += SLOT_INCR) * sizeof(*ss->array));
        BZERO(&ss->array[j],SLOT_INCR * sizeof(*ss->array));
     }
   }
   ss->array[j].pool  = ref.pool > 0 ? ref.pool
                                     : pool_id;
   ss->array[j].index = ref.index;
}

poolRef strFindSeg(const char *str,int l)
{
   poolRef         ref;
   unsigned short  h    = strHashSeg(str,l);
   StrSlot        *ss   = &Hash[h];
   int             i    = 0,
                   j    = ss->size,
                   pl   = -1,
                   p,o;

   ref.pool = -1;

   for (; i < j && (p = ss->array[i].pool); i++)
   {
     char    *pool;
     PoolStr *s;

     if (p != pl) { if (pl >= 0) PoolUnlock(pl);
                    PoolLock(pl = p); }

     pool = (char *)PoolAddr(p);
     s    = (PoolStr *)(pool + (o = ss->array[i].index));

     if (0 == strncmp(str,s->buff,l)) {
       ref.pool  = p;
       ref.index = o;
       break;
     }
   }

   if (pl >= 0) PoolUnlock(pl);

   return ref;
}

poolRef strFind(const char *str)
{
   poolRef         ref;
   unsigned short  h    = strHash(str);
   StrSlot        *ss   = &Hash[h];
   int             i    = 0,
                   j    = ss->size,
                   pl   = -1,
                   p,o;

   ref.pool = -1;

   for (; i < j && (p = ss->array[i].pool); i++) {
     char    *pool;
     PoolStr *s;

     if (p != pl) { if (pl >= 0) PoolUnlock(pl);
                    PoolLock(pl = p); }

     pool = (char *)PoolAddr(p);
     s    = (PoolStr *)(pool + (o = ss->array[i].index));

     if (0 == strcmp(str,s->buff)) {
       ref.pool  = p;
       ref.index = o;
       break;
     }
   }

   if (pl >= 0) PoolUnlock(pl);

   return ref;
}

void strForAll(poolStrFn fn)
{
   int     h = HASH_MAX;
   poolRef ref;

   ref.pool = ref.index = 0;
   (*fn)(ref);

   while (h-- > 0) {
      StrSlot *ss   = &Hash[h];
      int      j    = ss->size;

      while (j-- > 0) {
         if (ss->array[j].pool) {
            (*fn)(ss->array[j]);
         }
      }
   }
}

int strRefCmp(const poolRef *a,const poolRef *b)
{
  int diff = b->pool - a->pool;

  if (!diff) {
    diff = b->index - a->index;
    if (!diff) return 0;
  }

  return diff > 0 ? 1
                  : -1;
}

int strRefCmpDbg(const poolRef *a,const poolRef *b)
{
  int diff = b->pool - a->pool;

  fprintf(stderr,"%s ?= %s\n",strDeref(*a),strDeref(*b));

  if (!diff) {
    diff = b->index - a->index;
    if (!diff) return 0;
  }

  return diff > 0 ? 1
                  : -1;
}


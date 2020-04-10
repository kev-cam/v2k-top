/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  v2k_mem_h_rcsid
#define v2k_mem_h_rcsid() {return "$Id: v2k_mem.h,v 1.7 2010/04/17 08:14:40 dkc Exp $";} /* RCS ID */

#ifndef V2K_MEM_H
#define V2K_MEM_H

#include "system.h"

typedef enum {
  ALC_TEMP  = 0,
  ALC_PAD   = 1,
  ALC_PERM  = 2
} eALC;

typedef enum {
  ARENA_NONE = 0,
  ARENA_POOL = 1
} Arena;

#ifdef __cplusplus
extern "C" {
#endif

typedef int (*ArenaCheck)(void *,int,void *,int,U64);

void  Free(void *ptr);
int   queueFree(void *,Arena,int,ArenaCheck,U64);
void  freeQueued(Arena,int);

void *Malloc(size_t size);
void *Calloc(size_t nelem, size_t elsize);
void *Realloc(void *ptr, size_t size);

void *Malloc2(size_t size);
void *Calloc2(size_t nelem, size_t elsize);
void *Realloc2(void *ptr, size_t size);
void *allocPages(U64 *size);
int  freePages(void *ptr,U64 size);
int  resizePages(void *ptr,U64 new_size,U64 *size);

#ifdef __cplusplus
}
#endif

#define MALLOC(t)       ((t *)Malloc(sizeof(t)))
#define MALLOC2(t)      ((t *)Malloc2(sizeof(t)))

#define MALLOC_N(n,t)   ((t *)Malloc((n) * sizeof(t)))
#define MALLOC2_N(n,t)  ((t *)Malloc2((n) * sizeof(t)))

#define CALLOC(n,t)     ((t *)Calloc(n,sizeof(t)))
#define CALLOC2(n,t)    ((t *)Calloc2(n,sizeof(t)))

#define REALLOC(p,n,t)  (p = ((t *)Realloc(p,(n)*sizeof(t))))
#define REALLOC2(p,n,t) (p = ((t *)Realloc2(p,(n)*sizeof(t))))

#define DISPOSE(p)    {if ((void *)(p)) {typeof(p) _p=p; p=0; _p->dispose();}}
#define DESTROY(p)    {if ((void *)(p)) {typeof(p) _p=p; p=0; _p->destroy();}}
#define DELETE(p)     {if ((void *)(p)) {typeof(p) _p=p; p=0; delete(_p);}}
#define DELETE_ARR(p) {if ((void *)(p)) {typeof(p) _p=p; p=0; delete [] (_p);}}
#define FREE(p)       {if ((void *)(p)) {typeof(p) _p=p; p=0; Free((void *)(_p));}}
#define RECYCLE(p)    {if ((void *)(p)) {typeof(p) _p=p; p=0; _p->recycle();}}

#ifndef AllocA
#define AllocA(s)   alloca(s)
#define FreeA(p)
#endif

#ifndef BSD_MEM
# define bzero(p,s)   memset(p,0,s)
# define bcopy(a,b,s) memcpy(b,a,s)
# define bcmp(a,b,s)  memcmp(a,b,s)
#endif

#ifdef BZ_PTR_TYP
# define BZERO(p,s)   bzero((BZ_PTR_TYP)(p),s)
# define BCOPY(f,t,s) bcopy((BZ_PTR_TYP)(f),(BZ_PTR_TYP)(t),s)
# define BCMP(f,t,s)  bcmp((BZ_PTR_TYP)(f),(BZ_PTR_TYP)(t),s)
#else
# define BZERO(p,s)   bzero(p,s)
# define BCOPY(f,t,s) bcopy(f,t,s)
# define BCMP(f,t,s)  bcmp(f,t,s)
#endif

#define BZEROA(s) BZERO(s,sizeof(s))
#define BZEROS(s) BZERO(&(s),sizeof(s))
#define BZEROP(p) BZERO(p,sizeof(*p))

#endif /* V2MEM_H */

/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ const char * mem_c_rcsid() {return "$Id: mem.c,v 1.21 2007/02/01 06:46:49 dkc Exp $";}
 
#include "v2k_mem.h"
#include "error.h"
#include "assertions.h"

#ifdef DEBUG
static PTR_ITYPE MinMax[2] = {~0,0};
# define NEW_PTR(ptr) if (ptr) {PTR_ITYPE p = (PTR_ITYPE)ptr;\
		                if (p > MinMax[1]) MinMax[1] = p;\
                                if (p < MinMax[0]) MinMax[0] = p;}
# define TEST(ptr)    {PTR_ITYPE p = (PTR_ITYPE)ptr;\
		       assert(p <= MinMax[1] && p >= MinMax[0]);}
#else
# define NEW_PTR(ptr)
# define TEST(ptr)
#endif

int cleanup()
{
  return 0;
}

void Free(void *ptr)
{
  if (ptr) {
    TEST(ptr);
    free(ptr);
  }
}

typedef struct free_q {
  struct free_q *next;
  void          *ptr;
  Arena          arena;
  int            id;
  size_t         protected;

} FreeQ;

static FreeQ  *FQ,
             **pFQnxt = &FQ;

void freeQueued(Arena a,int id)
{
  FreeQ **pfq_nxt = &FQ,
         *pfq;
  int     sts;

  while (pfq = *pfq_nxt) {
    if (a  == pfq->arena &&
        id == pfq->id) {
      if (!(*pfq_nxt = pfq->next)) pFQnxt = pfq_nxt;
      if (pfq->protected) {
        sts = freePages(pfq->ptr,pfq->protected);
        ASSERT(0 == sts);
      } else {
        Free(pfq->ptr);
      }
      Free(pfq);
      break;
    }
    pfq_nxt = &pfq->next;
  }
}

int freePages(void *ptr,U64 size)
{
  int sts = munmap(ptr,size);
  ASSERT(0 == sts);
  return sts;
}

int queueFree(void *ptr,Arena a,int id,ArenaCheck chk,U64 protect)
{
  FreeQ **pfq_nxt = &FQ,
         *pfq;
  int     sts     = 0;

  if (chk) while (pfq = *pfq_nxt) {
    if (a == pfq->arena && (*chk)(pfq->ptr,pfq->id,ptr,id,pfq->protected)) {
      if (!(*pfq_nxt = pfq->next)) pFQnxt = pfq_nxt;
      Free(pfq->ptr);
      if (protect) {
        sts = freePages(pfq,protect);
      } else {
        Free(pfq);
      }
    } else {
      pfq_nxt  = &pfq->next;
    }
  }

  if (ptr) {
    pfq        = CALLOC2(1,FreeQ);
    pfq->arena = a;
    pfq->ptr   = ptr;
    pfq->id    = id;
    *pFQnxt    = pfq;
    pFQnxt     = &pfq->next;
    if (pfq->protected = protect) {
      sts = mprotect(ptr, protect, PROT_READ);
    }
  }

  return sts;
}

void *Malloc(size_t size)
{
  void *ptr;

  if (size <= 0) return 0;

  while (!(ptr = malloc(size)) && cleanup());

  NEW_PTR(ptr);
  return ptr;
}

void *Calloc(size_t nelem, size_t elsize)
{
  void *ptr;

  if (elsize <= 0 || nelem <= 0) return 0;

  while (!(ptr = calloc(nelem,elsize)) && cleanup());

  NEW_PTR(ptr);
  return ptr;
}

void *Realloc(void *ptr, size_t size)
{
  void *ptr2;

  if (!ptr) return Malloc(size);

  if (size <= 0) {
    Free(ptr);
    return 0;
  }

  while (!(ptr2 = realloc(ptr,size)) && cleanup());

  NEW_PTR(ptr2);
  return ptr2;
}

void *Malloc2(size_t size)
{
  void *ptr = Malloc(size);

  if (!ptr && size > 0) {
    ExitMsg(S_FATAL(ERROR(errno)),"Out of Memory");
  }

  NEW_PTR(ptr);
  return ptr;
}

void *Calloc2(size_t nelem, size_t elsize)
{
  void *ptr = Calloc(nelem,elsize);

  if (!ptr && (nelem * elsize) > 0) {
    ExitMsg(S_FATAL(ERROR(errno)),"Out of Memory");
  }

  NEW_PTR(ptr);
  return ptr;
}

void *Realloc2(void *ptr, size_t size)
{
  if (!(ptr = Realloc(ptr,size)) && size > 0) {
    ExitMsg(S_FATAL(ERROR(errno)),"Out of Memory");
  }

  NEW_PTR(ptr);
  return ptr;
}

void *allocPages(U64 *size)
{
  int   psz = getpagesize(),
        rem;
  void *ptr = 0;

  if (rem = *size%psz) {
    *size += psz - rem;
  }


  if (*size) {

    ptr = mmap(0, *size, PROT_WRITE|PROT_READ,
                         MAP_PRIVATE|MAP_ANONYMOUS|MAP_NORESERVE, -1, 0);

    if (MAP_FAILED == ptr) {
      ExitMsg(S_FATAL(ERROR(errno)),"Out of Memory/Pages");
    }
  }

  return ptr;
}

int resizePages(void *ptr,U64 new_size,U64 *size)
{
  int   psz  = getpagesize(),
        rem,
        sts  = -1;
  char *base = (char *)ptr;

  if (rem = new_size%psz) {
    new_size += psz - rem;
  }

  if (new_size < *size) {
    sts = munmap(&base[new_size],*size - new_size);
    if (!sts) *size = new_size;
  } else {
    // NIY
  }

  return sts;
}


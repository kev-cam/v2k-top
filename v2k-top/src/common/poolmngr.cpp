/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * poolmngr_cpp_rcsid() {return "$Id: poolmngr.cpp,v 1.36 2009/05/22 21:21:26 dkc Exp $";}
 


#include "system.h"
#include "v2k_mem.h"
#include "assertions.h"
#include "poolmngr.h"
#include "str_pool.h"
#include "template.h"
#define IGNORE_TOK_TABLE
#include "tokpool.h"
#include "modes-a.h"

PoolManager *PoolMngr;
PoolStr      NullStr;

const poolRef NullRef={0,0};
const poolRef BadRef ={-1,-1};

extern "C" int DescribePool(FieldDesc *d)
{
  printf(*d->type ? " %s %s (%d,%d)\n"
                  : "%s%s\n",          d->type,d->name,d->size,d->offset);
  return 0;
}

extern "C" int PoolMpdOwnr(int pid)
{
  int       i = 1;

  if (PoolMngr) {
    for (;i < PoolMngr->Last(); i++) {
      BasePool *pPool = PoolMngr->pool(i);
      if (0 != pPool && (pPool->getMode() & PMF_Mapped)) {
	if (pPool->pid() < 0 || kill(pPool->pid(),0)) {
          pPool->setOwner(pid,-1);
        }
      }
    }
  }

  return -1;
}

PoolManager::PoolManager()
{
  if (PoolMngr) return;

  PoolMngr = (PoolManager *)Calloc2(1,sizeof(PoolManager)
                                      +(MIN_POOLS-1)*sizeof(PlMngrData));
  PoolMngr->top           = MIN_POOLS;
  PoolMngr->last          = LST_RSVRD_PL_ID +1;
  PoolMngr->free          = -1;
  PoolMngr->pools[0].pool = &NullStr;
}

PoolManager::~PoolManager()
{
  if (this == PoolMngr) {
    int i = 1;
    for (;i < last;i++) {
      if (PoolMngr->pools[i].pool) {
        PoolMngr->pool(i)->close_pool(i,PoolMngr->pools[i].data);
      }
    }
  }
}

extern "C" void * PoolAddr(int p)
{
  return PoolMngr->pool(p);
}

extern "C" void * PoolData(int p)
{
  return PoolMngr->data(p);
}

extern "C" const char * PoolName(int p)
{
  return PoolMngr->pool(p)->fileName();
}

extern "C" void PoolLock(int p)
{
#ifdef PTHREADS
   PoolMngr->lock(p);
#endif
}

extern "C" void PoolUnlock(int p)
{
#ifdef PTHREADS
   PoolMngr->unlock(p);
#endif
}

#ifdef PTHREADS

void PoolManager::lock(int i)
{
  pthread_mutex_t *mp = pools[i].mutex;
  pthread_mutex_lock(mp);
}

void PoolManager::unlock(int i)
{
  pthread_mutex_t *mp = pools[i].mutex;
  pthread_mutex_unlock(mp);
}

#endif

int PoolManager::load(char *name,int p,const char *mode)
{
  int i = 1;

  if (0 == p || (i = FIRST_USER_POOL,p >= FIRST_FREE_POOL)) {
    for (;i < last;i++) if (pools[i].pool) {
      if (fileCmp(name,pool(i)->fileName())) {
        return i;
      }
    }
  } else if (pools[i].pool) {
    return i;
  }

  File f(name);
  ContigPool pl(&f,mode,p,-1);
  return pl.mngr_indx;
}

extern "C" int PoolLoad(char *name,int p,const char *mode)
{
  return PoolMngr->load(name,p,mode);
}

void PoolManager::deleteEntry(int i)
{
  PoolManager init;

  assert(i >= 0 && i <= last);

  if (i > 0) {
    intptr_t prv,
             I = i;
#ifdef PTHREADS
    pthread_mutex_destroy(PoolMngr->pools[i].mutex);
    FREE(PoolMngr->pools[i].mutex);
#endif
    if ((prv = PoolMngr->free) > 0) {
      intptr_t nxt = (intptr_t)(PoolMngr->pools[prv].data);
      PoolMngr->pools[nxt].pool = (void     *)I;
      PoolMngr->pools[prv].data = (PMplData *)I;
      PoolMngr->pools[i].pool   = (void     *)prv;
      PoolMngr->pools[i].data   = (PMplData *)nxt;
    } else {
      PoolMngr->pools[i].pool   = (void     *)I;
      PoolMngr->pools[i].data   = (PMplData *)I;
      PoolMngr->free            = i;
    }
  }
}

PMplData *PoolManager::setData(int i,PMplData *data)
{
  PoolManager init;

  assert(i >= 0 && i <= last && PoolMngr->pools[i].pool);

  return PoolMngr->pools[i].data = data;
}

void PoolManager::changeEntry(int i,void *pool,PMplData *data)
{
  PoolManager init;

  assert(i >= 0 && i <= last);

  PoolMngr->pools[i].pool = pool;
  PoolMngr->pools[i].data = data;
}

void PoolManager::changeEntry(int i,void *pool)
{
  PoolManager init;

  assert(i >= 0 && i <= last);

  PoolMngr->pools[i].pool = pool;
}

int PoolManager::addEntry(void *pool,PMplData *data,int index)
{
  PoolManager init;
  int         i = index,
              f;

  if (i >= 0) {
    goto done;
  }

  if ((i = PoolMngr->last) +1 < PoolMngr->top) {
     PoolMngr->last++;
     goto done;
  }

  if ((i = PoolMngr->free) >= 0) {
     goto done;
  }

  PoolMngr->top += POOL_INCR;
  PoolMngr       = (PoolManager *)Realloc2(PoolMngr,sizeof(PoolManager) +
                                         (PoolMngr->top-1)*sizeof(PlMngrData));
  BZERO(&PoolMngr->pools[PoolMngr->last],POOL_INCR * sizeof(PlMngrData));
  i = PoolMngr->last++;
done:
  PlMngrData *pd = &PoolMngr->pools[i];
  if (PoolMngr->pools[i].pool) {
    uintptr_t prv = (intptr_t)(pd->pool),
              nxt = (intptr_t)(pd->data);
    assert(PoolMngr->free >= 0);
    if (prv == nxt) {
      PoolMngr->free = -1;
    } else {
      PoolMngr->free            = prv;
      PoolMngr->pools[prv].data = (PMplData *)nxt;
      PoolMngr->pools[nxt].pool = (void     *)prv;
    }
  }
  pd->pool = pool;
  pd->data = data;
#ifdef PTHREADS
  pthread_mutexattr_init(&pd->ma);
  pthread_mutexattr_settype(&pd->ma,PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(pd->mutex = CALLOC2(1,pthread_mutex_t),&pd->ma);
#endif
  return i;
}

int PoolManager::addEntry(void *pool,PMplData *data)
{
  return addEntry(pool,data,-1);
}

int PoolManager::newPool(ePM typ,const File *f,
                         const char *mode,int id,int def_size)
{
  int pi = -1;
  switch (typ) {
#define PM_ACTION(m) case PM_##m: {m##Pool pl(f,mode,id,def_size);\
                                   pi           = pl.mngr_indx;\
                                   pl.mngr_indx = -1; break;}
#include "poolmode.inc"
  default: assert(0);
  }
  return pi;
}

void clearCache(CchData *pCache)
{
  int b = 1,
      t = pCache->top_block;

  for (; b < t ; b++) {
    FREE(pCache->mapped[b].data);
  }
}

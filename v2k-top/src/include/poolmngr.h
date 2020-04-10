/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  poolmngr_h_rcsid
#define poolmngr_h_rcsid() {return "$Id: poolmngr.h,v 1.23 2009/05/22 21:15:34 dkc Exp $";} /* RCS ID */

 
#ifndef POOLMNGR_H
#define POOLMNGR_H

#ifdef __cplusplus
class File;
class Stream;
class BasePool;
class TemplatePool;
#endif

#include "system.h"
#include "assertions.h"
#include "env.h"
#include "file.h"
#include "pool.h"

#define LST_RSVRD_PL_ID 127  /* max signed byte */
#define MIN_POOLS       1024
#define POOL_INCR       256

#ifdef __cplusplus
extern "C" {
#endif

void       *PoolAddr(int p);
const char *PoolName(int p);
int         PoolLoad(char *,int,const char *);
int         PoolMpdOwnr(int);
void        PoolLock(int);
void        PoolUnlock(int);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus

typedef struct {
  void                *pool;
  PMplData            *data;
#ifdef PTHREADS
  pthread_mutexattr_t  ma;
  pthread_mutex_t     *mutex;
#endif
} PlMngrData;

typedef struct {
  int        last,
             free,
             top;
  PlMngrData pools[200];
} DebugPM;

class PoolManager {
  int   last,
        free,
        top;
public:
  PlMngrData pools[1];

  void      deleteEntry(int i);
  void      changeEntry(int i,void *);
  void      changeEntry(int i,void *,PMplData *);
  PMplData *setData    (int i,PMplData *);
  int       addEntry   (void *,PMplData *);
  int       addEntry   (void *,PMplData *,int);
  int       load       (char *,int ,const char *);

#ifdef PTHREADS
  void      lock       (int);
  void      unlock     (int);
#endif

  inline BasePool *pool(int i) {ASSERT(i >= 0 && i <= last);
                                return (BasePool *)(pools[i].pool);};
  inline PMplData *data(int i) {ASSERT(i >= 0 && i <= last);
                                return pools[i].data;};
  inline int       Top()       {return 0 == this ? -1
				                 : top;};
  inline int       Last()      {return 0 == this ? -1
				                 : last;};

  int newPool(ePM,const File *,const char *,int,int);

  PoolManager();
  ~PoolManager();
};

extern PoolManager *PoolMngr;

class PoolIdentifier {
   int pool_id;
   friend class PoolManager;
public:

   inline PoolIdentifier()      {pool_id = -1;};
   inline PoolIdentifier(int p) {pool_id = p;};

   inline operator  BasePool *() {return PoolMngr->pool(pool_id);};
   inline operator  int       () {return pool_id;};
   inline BasePool *pool()       {return PoolMngr->pool(pool_id);};
   inline int       id()         {return pool_id;};
};

#endif /* __cplusplus */
#endif /* POLMNGR_H */

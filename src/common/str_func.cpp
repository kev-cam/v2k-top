/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * str_func_cpp_rcsid() {return "$Id: str_func.cpp,v 1.25 2012/10/16 22:30:51 cvs Exp $";}
 
#define STR_POOL_DESC

#include "assertions.h"
#include "error.h"
#include "env.h"
#include "str_pool.h"
#include "strdb.h"

FieldDesc *const voidDesc    = 0,
          *const intDesc     = 0,
          *const poolRefDesc = 0,
          *const charDesc    = 0;

typedef struct Loaded_s {
  struct Loaded_s *next;
  int              id;
} Loaded;

static Loaded         *strPools;
static PoolIdentifier  CurrStrPool;

BasePool * CoerceStringPool(PoolIdentifier pi)
{
  BasePool *pool = pi.pool();
  ePM       mode = pool->getMode();

  for (;;) switch (mode & PM_BaseModes) {
  case PM_CtgMppd: mode = PM_Contig;
                   continue;

#define PM_ACTION(m) case PM_##m:   {m##StringPool tmp;\
                                     CopyVirtual(&tmp,pool);\
                                     tmp.self = pool; tmp.Init(); goto done;};
#include "poolmode.inc"
    default:   assert(0);
  }
 done:
  return pool;
}

extern "C" void strReload(int pi)
{
  Loaded *pLdd = strPools;

  for (; pLdd ; pLdd = pLdd->next) {
    if (pi < 0 || pi == pLdd->id) {
      int id;
      CoerceStringPool(id = PoolMngr->pool(pLdd->id)->reload(FM_READ));
      assert(id == pLdd->id);
    }
  }
}

extern "C" void InitStrDB(const char *md)
{
  String mode(md);

  if (CurrStrPool.id() > 0) return;

  mode += "d";

  File f("${" V2K_STRINGS "}","strings",FT_String);

  ContigPool pl(&f,mode);

  CoerceStringPool(CurrStrPool = pl.mngr_indx);

  NullStrRef   = strSaveStr("");
  Loaded *pLdd = CALLOC2(1,Loaded);
  pLdd->id     = pl.mngr_indx;
  pLdd->next   = strPools;
  strPools     = pLdd;
}

extern "C" poolRef strSaveStr(const char *str)
{
  poolRef ret = CurrStrPool.pool()->addString(str);

  if (NULL_REF(ret)) {
    assert("Need new string pool" == 0);
  }

  return ret;
}

extern "C" void strPoolShared(int yes)
{
  static ePM old_mode;

  if (yes) {
    if (0 != CurrStrPool) {
      old_mode = CurrStrPool.pool()->changeMode(PM(PM_Mapped|PMF_Shared));
    }
  } else if (old_mode) {
    CurrStrPool.pool()->changeMode(old_mode);
  }
}

/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * udp_pool_cpp_rcsid() {return "$Id: udp_pool.cpp,v 1.13 2007/02/01 06:49:07 dkc Exp $";}


#define VERILOG3
#define VERILOG4

#include "system.h"
#include "args.h"
#include "env.h"
#include "error.h"
#include "language.h"
#include "poolmngr.h"
#include "strdb.h"
#include "v2k_mem.h"
#include "v2k.h"
#include "tokpool.h"
#include "verilog.h"
#include "dump.h"
#include "mod_pool.h"
#include "udp_pool.h"

BasePool *CoerceUdpPool(PoolIdentifier pi)
{
  BasePool  *pool = pi.pool();
  ePM        mode = PM(pool->getMode() & PM_BaseModes);

  for (;;) switch (mode) {
  case PM_CtgMppd: mode = PM_Contig;
                   continue;

#define PM_ACTION(m) case PM_##m:   {m##UdpPool tmp;\
                                     CopyVirtual(&tmp,pool);\
                                     tmp.self = pool; tmp.Init(); goto done;};
#include "poolmode.inc"

    default:   assert(0);
  }
 done:
  return pool;
}

InMemUdpPool *UdpPool(int id)
{
  return (InMemUdpPool *)(PoolMngr->pool(id));
}

void Prim::createPool(int free_tbl)
{
  File f;

  reposPath(&f,"${" V2K_LIB_REPOS "}/",strDeref(name),"pool",OBJ_Primitive);

  CoerceUdpPool(saved = PoolMngr->newPool(defPoolType,&f,"pcw",
                -1,sizeof(OnDiskUdpPool)));

  UdpPool(saved)->Init(this,free_tbl);

  freeQueued(ARENA_POOL,saved);

  PoolMngr->pool(saved)->syncFile(FM_WRITE);
}

Prim::Prim(int svd)
{
  BZERO(this,sizeof(*this));
  typ   = AO_PRIM;
  saved = svd;
}

int loadPrim(File *f,int lib_indx)
{
  int saved = PoolMngr->newPool(defPoolType,f,"r",
                                -1,sizeof(OnDiskUdpPool));
  if (saved >= 0) {
    CoerceUdpPool(saved);
    Prim *prm     = new Prim(saved);
    prm->lib_indx = lib_indx;
    UdpPool(saved)->Load(prm);
    prm->logGlobal(prm->name,REF_PRIM,prm->prim()->add(prm));
    return 1;
  }

  return 0;
}

void Prim::minimize()
{
  if (saved > 0) {

    bool was         = Expr::CheckInUse;
    Expr::CheckInUse = false;

    deleteLists();

    CoerceUdpPool(saved = PoolMngr->pool(saved)->reload(FM_READ));

    Expr::CheckInUse = was;
  }
}

int Prim::evalIndices(Inst *ip,int c,RngVec *rvp)
{
  if (saved) {
    return UdpPool(saved)->evalIndices(ip,c,rvp);
  } else {
    assert(NIY("Prim::evalIndices"));
  }
}

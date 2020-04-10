/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * mod_pool_cpp_rcsid() {return "$Id: mod_pool.cpp,v 1.54 2009/07/08 08:41:49 dkc Exp $";}

#define VERILOG3
#define VERILOG4
#define VERI_POOL_DESC

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
#include "udp_pool.h"
#include "mod_pool.h"

BasePool *CoerceModulePool(PoolIdentifier pi)
{
  BasePool  *pool = pi.pool();
  ePM        mode = PM(pool->getMode() & PM_BaseModes);

  for (;;) switch (mode) {
  case PM_CtgMppd: mode = PM_Contig;
                   continue;

#define PM_ACTION(m) case PM_##m:   {m##ModulePool tmp;\
                                     CopyVirtual(&tmp,pool);\
                                     tmp.self = pool; tmp.Init(); goto done;};
#include "poolmode.inc"

    default:   assert(("Invalid mode for pool",0));
  }
 done:
  return pool;
}

InMemPoPool *PoPool(int id)
{
  return (InMemPoPool *)(PoolMngr->pool(id));
}

InMemModulePool *ModPool(int id)
{
  InMemModulePool *pm = (InMemModulePool *)(PoolMngr->pool(id));

  return pm;
}

void *VerilogObj::demap(const model *pm)
{
  switch (pm->Type()) {
#define LST(t,b,n,e) case REF_##e: return n##_map(pm->Index());
#define LST_GLOBAL
#include "lists.inc"
  default:;
  }
  return 0;
}

poolRef PrtdObj::stmtLabel(int i)
{
  poolRef ref = NullRef;

  if (saved) {

    switch (typ) {
      case AO_MODULE: return ModPool(saved)->stmtLabel(i);
    }

  } else {
    Stmt *stmt = 0;

    switch (typ) {
      case AO_MODULE: stmt = ((Module *)this)->stmts;
    }

    if (stmt) while (i-- > 0) stmt = stmt->next;
  }

  return ref;
}

poolRef PrtdObj::itemName(eREF eref,int i)
{
  if (saved) switch (typ) {
    case AO_MODULE: return ModPool(saved)->itemName(eref,i);
    case AO_PRIM:   return UdpPool(saved)->itemName(eref,i);
  }

  switch (eref) {
  case REF_PORT:    {Port *prt = port_map(i);        return prt->name;}
  case REF_UNKNOWN: {Unknown *punk = unknown_map(i); return punk->ref;}
  case REF_PARM:    {Attr *pa = parm_map(i);         return pa->name();}
  default:          assert("itemName/typ" == 0);
  }

  return NullRef;
}

void Module::createPool()
{
  File f;

  reposPath(&f,"${" V2K_LIB_REPOS "}",strDeref(name),"pool",OBJ_Module);

  CoerceModulePool(saved = PoolMngr->newPool(defPoolType,&f,"pcw",
                   -1,sizeof(OnDiskModulePool)));

  ModPool(saved)->Init(this);

  freeQueued(ARENA_POOL,saved);

  PoolMngr->pool(saved)->syncFile(FM_WRITE);
}

Module::Module(int svd)
{
  BZERO(this,sizeof(*this));
  ModPool(svd)->Load(this);
  ref_indx = -1;
}

static void *InstCallBack(void *mdl,VStmt *vs)
{
  eSTMT    st;
  poolRef  ref;
  Module  *mod = (Module  *)mdl;
  switch (st = vs->stmt_typ()) {
  case STMT_INST: ref = vs->name();
                  mod->addInstRef(ref);
  default:;
  }

  return 0;
}

int loadModule(File *f,int lib_indx)
{
  int saved = PoolMngr->newPool(defPoolType,f,"r",
                                -1,sizeof(OnDiskModulePool));
  if (saved >= 0) {
    CoerceModulePool(saved);
    Module *mod   = new Module(saved);
    mod->lib_indx = lib_indx;
    mod->logGlobal(mod->name,REF_MODULE,
                   mod->vd()->mod_indx = mod->module()->add(mod));
    mod->pushScope(0,mod->name);
    mod->forAllStmts(InstCallBack,mod,0);
    mod->popScope(0,mod->name);
    mod->clearRefs();
    return 1;
  }

  return 0;
}

void Module::minimize()
{
  if (saved > 0) {

    bool was         = Expr::CheckInUse;
    Expr::CheckInUse = false;

    deleteLists();

    CoerceModulePool(saved = PoolMngr->pool(saved)->reload(FM_READ));

    Expr::CheckInUse = was;
  }
}


int PrtdObj::parmCount()
{
  return saved ? PoPool(saved)->parms
               : parm()->count();
}

poolRef PrtdObj::parmVal(Inst *ip,int idx,BaseExpr *pb,Value *pv)
{
  Attr *attr;

  if (saved) {
     return PoPool(saved)->parmVal(ip,idx,pb,pv);
  } else {
     attr = parm_map(idx);
     assert(0);
  }

  return attr->name();
}

bool PrtdObj::parmRange(Inst *ip,int pidx,int ridx,parmRng *rng_data)
{
  Attr *attr;

  if (saved) {
     return PoPool(saved)->parmRange(ip,pidx,ridx,rng_data);
  } else {
     attr = parm_map(pidx);
     assert(0);
  }

  return false;
}

int PrtdObj::portCount()
{
  return saved ? PoPool(saved)->ports
               : port()->count();
}

PortDir PrtdObj::portMode(int idx)
{
  if (saved) return PoPool(saved)->portMode(0,idx);

  return (PortDir)port_map(idx)->io;
}

eSTMT VStmtSaved::stmt_typ() {return (eSTMT)stmt.Header.typ;}

bool VStmtSaved::set_mod(Inst *ip)
{
  refTup mod_ref;
  int    ok;

  assert(STMT_INST == stmt.Header.typ);

  plStmtInst *ps = (plStmtInst *)&stmt;

  if (ok = ip->findGlobObj(RFF_MP,ps->name,&mod_ref)) {
    switch (mod_ref.typ) {
    case REF_MODULE:  ip->SetSelf(new mod_model(mod_ref.index)); break;
    case REF_PRIM:    ip->SetSelf(new udp_model(mod_ref.index)); break;
    }
  }

  return ok;
}

int VStmtSaved::parm_count()
{
  assert(STMT_INST == stmt.Header.typ);

  plStmtInst *ps  = (plStmtInst *)&stmt;
  int         n   = 0;

  if (ps->udp || ps->prim) {
    return n;
  }

  plExpr *xpr = (plExpr *)ModPool(mod->saved)->deref(IR(ps->param));

  if (xpr) for (;;) {
    n++;
    switch (xpr->typ) {
    case VT_OPERATOR:
      switch (xpr->value.op)
      {
      case OPR_LIST1:
        xpr = (plExpr *)ModPool(mod->saved)->deref(IR(xpr->right));
        continue;
      default:;
      }
    default:;
    }
    break;
  }

  return n;
}

plExpr *VStmtSaved::get_parm(int i)
{
  assert(STMT_INST == stmt.Header.typ);

  plStmtInst *ps  = (plStmtInst *)&stmt;

  if (ps->udp || ps->prim) return 0;

  plExpr *xpr = (plExpr *)ModPool(mod->saved)->deref(IR(ps->param));
  switch (xpr->typ) {
  case VT_OPERATOR:
    for (;;) switch (xpr->value.op)
    {
    case OPR_LIST1:
      if (i--) {
        xpr = (plExpr *)ModPool(mod->saved)->deref(IR(xpr->right));
        continue;
      }
      xpr = (plExpr *)ModPool(mod->saved)->deref(IR(xpr->left));
    default:
      goto done;
    }
    break;
  default:;
  }
done:
  return xpr;
}

poolRef VStmtSaved::parm_name(int i)
{
  poolRef  ref = NullRef;
  plExpr  *xpr = get_parm(i);

  if (xpr) switch (xpr->typ) {
  case OPR_INST:
      xpr = (plExpr *)ModPool(mod->saved)->deref(IR(xpr->left));
      ref = xpr->value.ref;
  default:;
  }
  return ref;
}

int VStmtSaved::parm_val(Inst *ip,int i,Expr *ret,poolRef *pref)
{
  poolRef     ref = NullRef;
  plExpr     *xpr = get_parm(i),
             *xnm;

  if (xpr) {
    if (VT_OPERATOR == xpr->typ) switch (xpr->value.op) {
    case OPR_BIND:
      xnm = (plExpr *)ModPool(mod->saved)->deref(IR(xpr->left));
      ref = xnm->value.ref;
      xpr = (plExpr *)ModPool(mod->saved)->deref(IR(xpr->right));
    default:;
    }
  }

  *pref = ref;

  ModPool(mod->saved)->evalExpr(ip,xpr,(plExpr *)ret);

  return 0;
}

int VStmtSaved::inst_count()
{
  assert(STMT_INST == stmt.Header.typ);

  plStmtInst *ps  = (plStmtInst *)&stmt;
  int         n   = 1;
  plExpr     *xpr = (plExpr *)ModPool(mod->saved)->deref(IR(ps->inst));

  if (xpr) switch (xpr->typ) {
  default:
    assert(0);
    break;
  case VT_OPERATOR:
    for (;;) switch (xpr->value.op)
    {
    case OPR_LIST1:
      xpr = (plExpr *)ModPool(mod->saved)->deref(IR(xpr->right));
      n++;
      continue;
    case OPR_UN_INST:
    case OPR_INST:
      goto done;
    default:
      assert(0);
    }
  }
done:
  return n;
}

poolRef VStmtSaved::inst_name(int i,int *range,int *bx_idx,Inst *ip)
{
  assert(STMT_INST == stmt.Header.typ);

  plStmtInst *ps  = (plStmtInst *)&stmt;
  poolRef     ref = NullRef;
  plExpr     *xpr = (plExpr *)ModPool(mod->saved)->deref(IR(ps->inst)),
             *rhs,
             *lhs;

  if (xpr) switch (xpr->typ) {
  default:
    assert(0);
    break;
  case VT_OPERATOR:
    for (;;) switch (xpr->value.op) {
    case OPR_LIST1:
      if (!i--) {
        xpr = (plExpr *)ModPool(mod->saved)->deref(IR(xpr->left));
      } else {
        xpr = (plExpr *)ModPool(mod->saved)->deref(IR(xpr->right));
      }
      continue;
    case OPR_INST:
      *bx_idx = ModPool(mod->saved)->index(xpr->right);
      lhs     = (plExpr *)ModPool(mod->saved)->deref(IR(xpr->left));
      switch (lhs->typ) {
      case VT_OPERATOR: 
        xpr = lhs;
        switch (xpr->value.op) {  
        case OPR_INDEX:
          rhs      = (plExpr *)ModPool(mod->saved)->deref(IR(xpr->right)),
          range[0] = -1;
          xpr      = (plExpr *)ModPool(mod->saved)->deref(IR(xpr->left));
          if (VT_OPERATOR == rhs->typ) switch (rhs->value.op) {
	  case OPR_RANGE:
            ref = xpr->value.ref;
            lhs = (plExpr *)ModPool(mod->saved)->deref(IR(rhs->left));
            rhs = (plExpr *)ModPool(mod->saved)->deref(IR(rhs->right));
            range[0] = 1;
	    if (lhs->xtra & VTX_CONST) {
              range[1] = ModPool(mod->saved)->eval_i32(lhs);
	    } else {
              range[1] = ModPool(mod->saved)->eval_i32(lhs,ip);
	    }
            if (rhs->xtra & VTX_CONST) {
              range[2] = ModPool(mod->saved)->eval_i32(rhs);
	    } else {
              range[2] = ModPool(mod->saved)->eval_i32(rhs,ip);
	    }
	    goto done;
          default:;
          }
          goto use_value;
        case OPR_SUB:;
  	  goto done;
        }
      case VT_REFL:
        ref = ip->parent->itemName((eREF)lhs->rtyp,lhs->value.rrf.index);
        goto done;
      case VT_VOID:
       use_value:
        ref = lhs->value.ref;
        goto done;
      }
    case OPR_UN_INST:
      *bx_idx = ModPool(mod->saved)->index(xpr->right);
      goto done;
    default:
      assert(0);
    }
  }
done:
  return ref;
}

poolRef VStmtSaved::name()
{
  assert(STMT_INST == stmt.Header.typ);

  plStmtInst *ps = (plStmtInst *)&stmt;

  return ps->name;
}

int VStmtSaved::stmtLine()
{
  if (!stmt.Header.typ) {
    plStmtAny *ps = ModPool(mod->saved)->getStmt(index);
    BCOPY(ps,&stmt,StmtSize[ps->Header.typ]);
  }

  return stmt.Header.line;
}

int VStmtSaved::stmtFileId()
{
  if (!stmt.Header.typ) {
    plStmtAny *ps = ModPool(mod->saved)->getStmt(index);
    BCOPY(ps,&stmt,StmtSize[ps->Header.typ]);
  }

  return stmt.Header.file_id;
}

int VStmtInMem::stmtLine()
{
  return stmt->line;
}

int VStmtInMem::stmtFileId()
{
  return stmt->file_id;
}

void VStmtSaved::reportPosn()
{
  poolRef nm = ModPool(mod->saved)->fileName(stmtFileId());

  ErrorMsg(STS_NORMAL," @ %s:%d",strDeref(nm),stmtLine());
}

void VStmtInMem::reportPosn()
{
  ErrorMsg(STS_NORMAL," @ %s:%d",
           strDeref(mod->file_map(stmt->file_id)->ref),stmt->line);
}

void VStmtSaved::evalDfPrm(Inst *ip)
{
  assert(STMT_DEFPARAM == stmt.Header.typ);

  plStmtDefparam *ps  = (plStmtDefparam *)&stmt;
  plExpr         *nxt = (plExpr *)ModPool(mod->saved)->deref(IR(ps->expr)),
                 *xpr,
                 *lhs;
  poolRef         ref = NullRef;
  XmrData         rft;
  Inst           *trgt;

  while (xpr = nxt) {
    nxt         = 0;
    int in_list = 0;
  list_item:
    switch (xpr->typ) {
    default:
      assert(0);
      break;
    case VT_OPERATOR:
      switch (xpr->value.op)
      {
      case OPR_LIST0:
        if (!in_list) {
          nxt = (plExpr *)ModPool(mod->saved)->deref(IR(xpr->right));
          xpr = (plExpr *)ModPool(mod->saved)->deref(IR(xpr->left));
          in_list = 1;
          goto list_item;
        }
        assert(0);
        break;
      case OPR_ASSIGN:
        lhs          = (plExpr *)ModPool(mod->saved)->deref(IR(xpr->left));
        xpr          = (plExpr *)ModPool(mod->saved)->deref(IR(xpr->right));
        rft.typ.need = RFF_PARM;
        rft.scp      = 0;
        if (trgt = ModPool(mod->saved)->locateRef(ip,lhs,&rft,XMR_NONE)) {
          plExpr    val;
          BaseExpr *bx  = (BaseExpr *)&val;
          switch(rft.typ.got) {
          default:
            bx->set(BX_SCALAR);
          }
          if (ModPool(mod->saved)->evalExpr(ip,xpr,&val)) {
            if (1 > trgt->setParm(&rft,(Expr *)&val,ip)) {
              assert("Need warning message" == 0);
            }
            goto ok;
          }
	}
        reportPosn();
      ok:
        break;
      default:
        assert(0);
      }
      break;
    }
  }
}

eSTMT VStmtInMem::stmt_typ() {return stmt->stmt_typ();}

bool VStmtInMem::set_mod(Inst *ip)
{
  assert(0);
  return false;
}

int VStmtInMem::parm_count()
{
  assert(0);
  return 0;
}

poolRef VStmtInMem::parm_name(int i)
{
  assert(0);
  return NullRef;
}

int VStmtInMem::inst_count()
{
  assert(0);
  return 0;
}

poolRef VStmtInMem::inst_name(int i,int *range,int *bx_idx,Inst *ip)
{
  assert(0);
  return NullRef;
}

poolRef VStmtInMem::name()
{
  assert(0);
  return NullRef;
}

int VStmtInMem::parm_val(Inst *ip,int i,Expr *ret,poolRef *pref)
{
  assert(0);

  return 0;
}

void VStmtInMem::evalDfPrm(Inst *ip)
{
  assert(0);
}

void *Module::forAllStmts(StmtCallBackFn cb,void *data,int recurse)
{
  if (saved) {
    return ModPool(saved)->forAllStmts(this,cb,data,recurse);
  } else {
    VStmtInMem  vs;
    int         i    = 0;
    Stmt       *stmt = stmts;
    void       *v;

    for (; stmt ; stmt = stmt->next) {
      vs.index  = i++;
      switch ((vs.stmt = stmt)->typ) {
# include "for_all_stmt.inc"
      }
    }
  }

  return 0;
}

int Module::evalIndices(Inst *ip,int c,RngVec *rvp)
{
  if (saved) {
    return ModPool(saved)->evalIndices(ip,c,rvp);
  } else {
    assert(NIY("Module::evalIndices"));
  }

  return -1;
}

eCG Module::codegen(Inst *ip,Stream *out,Stream *cls,eCodeMode cm)
{
  if (saved) {
    return ModPool(saved)->codegen(ip,this,out,cls,cm);
  } else {
    assert(NIY("Module::codegen"));
  }

  return CG_OK;
}

bool Module::bindPorts(Inst *parent,Inst *child)
{
  if (saved) {
    return ModPool(saved)->bindPorts(this,parent,child);
  } else {
    assert(NIY("Module::bindPorts"));
  }

  return 0;
}

int PrtdObj::indxd()
{
  int c = 0;

  if (saved) {
    c = ModPool(saved)->indxd_prts;
  } else {
    int   i = 0;
    Port *prt;

    for (; prt = port_map(i) ; i++) if (prt->pckd || prt->unpckd) c++;
  }

  return c;
}

int PrtdObj::unk_count()
{
  if (saved) return ModPool(saved)->unknowns;

  return            unknown()->count();
}

eREF PrtdObj::unkRefType(int i)
{
  if (saved) return ModPool(saved)->unkRefType(i);

  return            unknown_map(i)->rtyp;
}

int PrtdObj::parmIndex(poolRef pnm)
{
  poolRef ref;
  int     r = 0,
          n;

  if (saved) {
    MAX_PLATTR_OBJ *plAttr = ModPool(saved)->parm_list(&n);
    while (r < n) {
      if (!(plAttr->typ & ATTR_USR)) {
        ref.pool  = plAttr->pool;
        ref.index = plAttr->index;
      } else {
        ref       = plAttr->nm;
      }

      if (SAME_REF(pnm,ref)) return r;

        plAttr++; r++;
    }
  } else {
    Attr *pAttr = parm_map(0);
    while (pAttr) {
      if (!(pAttr->typ & ATTR_USR)) {
        ref.pool  = pAttr->pool;
        ref.index = pAttr->index;
      } else {
        AttrU *pu = (AttrU *)pAttr;
        ref       = pu->nm;
      }

      if (SAME_REF(pnm,ref)) return r;

        pAttr = pAttr->next; r++;
    }
  }

  return -1;
}


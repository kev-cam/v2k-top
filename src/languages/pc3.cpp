/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * pc3_cpp_rcsid() {return "$Id: pc3.cpp,v 1.37 2010/05/25 16:25:03 dkc Exp $";}

#include "system.h"
#include "args.h"
#include "env.h"
#include "error.h"
#include "language.h"
#include "poolmngr.h"
#include "strdb.h"
#include "v2k_mem.h"
#include "v2k.h"
#define  NEED_PUNCTUATION
#define  NEED_OPERATORS
#define  NEED_VERILOG
#define  NEED_QUOTES
#define  NEED_WHITESPACE
#define  NEED_INTEGER
#define  NEED_COMMENTS
#define  NEED_PP
#define  NEED_CPP
#define  NEED_PRC
#define  NEED_CPPOPS
#define  NEED_GNU
#define  NEED_LABELS
#define  NEED_TIMING
#define  NEED_BUILTIN
#define  NEED_SYSTEMC
#include "tokpool.h"
#define PC_POOL_DESC
#include "parser.h"
#include "pc.h"

#include "analyze.inc"

eAZR CppStmt::fixSystemC(CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  int sts = 0;
  return AZR(sts);
}

eAZR CppStmtDecl::fixSystemC(CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  int sts = 0;
  return AZR(sts);
}

void CppStmtExpr::fixScWait(CppExpr *wtx,CppStmt **strts)
{
  if (strts && !*strts) { 
    *strts = this;
  }

  if (CXPR_CALL == expr->opr) {
    CppExpr  *lhs= expr->x.hs.l;
    if (lhs->isRef(CppConst::cpp_wait)) {
      lhs->recycle();
      if (expr->x.hs.r) {
        assert(0);
      }
      expr->opr    = CXPR_SC_AT;
      expr->x.hs.l = wtx;
    }
  }
}

void CppScope::fixScWait(CppExpr *wtx,CppStmt **strts)
{
  CppStmt *scan = stmt0;

  for (; scan ; scan = scan->next) {
    scan->fixScWait(wtx,strts);
  }
}

void CppDecl::addBind(CppScope *scp,CppExpr *bind)
{
  CppExpr *val = Val(),
          *rhs;
 
  if (CXPR_CALL == bind->opr) {
    bind->opr = CXPR_ASSIGN;
  }

  if (rhs = val->x.hs.r) {
    val->x.hs.r = rhs->addAfter(bind);
  } else {
    val->x.hs.r = bind;
  }
}

eAZR CppStmtType::fixSystemC(CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  int      sts     = 0;
  CppStmt  *starts = 0;
  CppScope *bod;

  if (flgs & AZC_FN2MOD) {

    bod  = tv->cRef()->Scope();

    CppStmt  *stmt_nxt = bod->stmt0,
             *stmt;
  
    while (stmt = stmt_nxt) {
      stmt_nxt = stmt->next;
      switch (stmt->typ) {
      case CSTMT_return:
        stmt->typ = CSTMT_ScCmnt;
        break;
      case CSTMT_Decl: {
        CppStmtDecl *sd  = dynamic_cast<CppStmtDecl *>(stmt);
	CppTypeCls  *cls = sd->dcl->typ->cTypCls(); 
        if (cls && PCT_MODULE == cls->typ) {
          assert(!starts);
          CppExpr *val = sd->dcl->Val();
          val->opr     = CXPR_INST_SC;
          sd->dcl->dot = 1;
	} else {
          sts |= sd->Analyze(0,a_scp,where,level,flgs);
	}
      } break;
      case CSTMT_Expr:
        CppStmtExpr *sx  = dynamic_cast<CppStmtExpr *>(stmt);
	CppExpr     *xpr = sx->expr;
	switch (xpr->opr) {
	case CXPR_OBJ: CppExpr *lhs = xpr->x.hs.l;
	               poolRef  Ref;
		       if (lhs->getRef(&Ref)) {
                         CppDecl *dcl = 0;
			 int      sts = bod->findObj(Ref,0,&dcl,0);
                         if (dcl) {
                           CppTypeCls *cls = dcl->typ->cTypCls(); 
                           if (PCT_MODULE == cls->typ) {
                             xpr->opr = CXPR_CMNT;
                             dcl->addBind(bod,xpr->x.hs.r);
	                   }
			 }
		       }
	}
	if (!starts) {
	  starts = stmt;
	}
      }
    }
    goto done;
  }

  switch(tv->name.pool) {
  case SYSTEMC_POOL:
    switch (tv->name.index) {
    case SYSC_SC_CTOR:
      CppTypeRef *ctor     = tv->cRef();
      CppScope   *scp      = ctor->Scope();
      CppStmt    *stmt_nxt = scp->stmt0,
                 *stmt;
      
      while (stmt = stmt_nxt) {
        stmt_nxt = stmt->next;
        switch (stmt->typ) {
        case CSTMT_Expr: 
          CppStmtExpr *stmt_xpr = dynamic_cast<CppStmtExpr *>(stmt);
          CppExpr     *xpr      = stmt_xpr->expr,
                      *lhs;
          switch (xpr->opr) {
          case CXPR_CALL: lhs = xpr->x.hs.l;
	                  if (lhs->isRef(CppConst::sc_thread)) {
                            stmt_xpr->expr = new CppExpr(CXPR_CMNT,xpr);
			    CppExpr *rhs   = xpr->x.hs.r;
                            switch (rhs->opr) {
			    case CXPR_COMMA: lhs = rhs->x.hs.l;
			                     rhs = rhs->x.hs.r; break;
			    default:         lhs = rhs; 
                                             rhs = 0;           break;
			    }
                            poolRef thrd;
			    if (!lhs->getRef(&thrd)) { assert(0); }
                            CppStmt *scan = a_scp->stmt0;
                            for (; scan ; scan = scan->next) {
                              if (CSTMT_Type == scan->typ) {
                                CppStmtType *st  = dynamic_cast<CppStmtType *>(scan);
                                CppTypeRef  *ref = st->tv->cRef();  
                                if (SAME_REF(ref->name,thrd)) {
                                  scan->typ = CSTMT_ScThread;
                                  bod       = ref->Scope();
                                  if (rhs) {
                                    bod->fixScWait(rhs->sysc2pc(),&starts);
		     	          }
                                  break;
				}
			      }
			    }
			  }
			  goto done;
	  }
	}
      }
    }
  }

 done:
  if (starts) {
    if (flgs & AZC_FN2MOD) {
      CppStmt *proc = new CppStmtLabel(&Nowhere,CSTMT_ScProc,
                                       CppConst::main);
      starts->addStmt(proc,bod,-1);
    }
    CppStmt *strt = new CppStmtLabel(&Nowhere,CSTMT_Label,
                                     CppConst::start);
    starts->addStmt(strt,bod,-1);
  }

  return AZR(sts);
}

eAZR CppScope::fixSystemC(CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  int       sts      = 0;
  CppStmt  *stmt_nxt = stmt0,
           *stmt;
 
  while (stmt = stmt_nxt) {
    stmt_nxt = stmt->next;

    sts |= stmt->fixSystemC(this,where,level,flgs);
  }

  return AZR(sts);
}

int CppTypeScp::forkWait (int i) const
{
  int r = -1;

  for (; r < 0 && i >= 0 ; i--) {
    r = i < msz ? fmap[i] 
                : -1;
  } 

  return r >= 0 ? r 
                : 0;
}

void CppStmtFork::addLabel(int rp,int wt)
{
  int c  = count++;
  REALLOC(lbl,count,typeof(*lbl));
  lbl[c].rp = rp;
  lbl[c].wt = wt;
}

int CppTypeRef:: copyIndices(CppIndex **pRet) const
{
  CppIndex *pi;
 
  if (pi = index) {
    int i = 0;
    while (pi) {i++; pi = pi->next;}
    pi = *pRet = new CppIndex[i];
    CppIndex *p2 = index;
    while (p2) {
      pi->expr = p2->expr;
      if (p2 = p2->next) {
	pi->next = ++pi;
      } else{
	pi->next = 0;
	break;
      }
    }
    
    return i;
  }

  return typ->copyIndices(pRet);
}

CppTypeV *CppType::Signal;
CppTypeV *CppType::SigBool;
CppTypeV *CppType::Driver;
CppTypeV *CppType::Receiver;
CppTypeV *CppType::ReceiverP;
CppTypeV *CppType::PipeClient;
CppTypeV *CppType::PipeClientP;
CppTypeV *CppType::Module;
CppTypeV *CppType::ModuleP;
CppTypeV *CppType::CharP;
CppTypeV *CppType::ConstCharP;
CppTypeV *CppType::BitField;

void CppType::findBuiltins(const CppScope *scp)
{
  Signal          = scp->findType(BUILTIN_POOL,BLT_SIGNAL);
  assert(Signal);

  CppTypeRef *ref = new CppTypeRef(&Nowhere,0,Signal);
  CppTypeV   *bl  = scp->findType(CPP_POOL,CPP_BOOL);
  ref->set_inst(new CppDecl(bl,NullRef));
  SigBool         = ref;

  Driver          = scp->findType(BUILTIN_POOL,BLT_DRIVER);
  assert(Driver);
  Receiver        = scp->findType(CppConst::receiver);
  assert(Receiver);
  PipeClient      = scp->findType(CppConst::pipe_client);
  assert(PipeClient);

  CppTypeRef *rp  = new CppTypeRef(&Nowhere,0,Receiver);
  rp->x.f.indrct++;      
  ReceiverP       = rp;

  CppTypeRef *pp  = new CppTypeRef(&Nowhere,0,PipeClient);
  pp->x.f.indrct++;      
  PipeClientP     = pp;

  Module          = scp->findType(CppConst::Module);
  assert(Module);
  CppTypeRef *mp  = new CppTypeRef(&Nowhere,0,Module);
  mp->x.f.indrct++;      
  ModuleP         = mp;
}

#define RXD_TMPLT 1
typedef enum {
  RX_DRV     = 0,
  RX_RCV     = 1,
  RX_CLNT    = 2,
  RX_CLNT_RD = 4,
  RX_CLNT_WR = 8
} eRX;

poolRef addSzDecl(CppScope *scp,CppScope *a_scp, CppDecl *sig,
                  poolRef name,const char *item,CppTypeV *typ)
{
  String   nm("_sz_"); nm += item;
  poolRef  sx  = strSaveStr(nm);
  CppDecl *dcl = scp->findRef(sx);

  if (!dcl) {
    CppTypeRef       *ct;
    const CppTypeRef *ref  = sig->typ->cRef();
    int               cnst = 1,
                      idx  = 0;
    CppExpr          *val; 

    if (ref) {
      idx = ref->indices();
      if (ref->x.f.indrct) {
	cnst = 0; 
      }
    }

    if (cnst) {
      CppExpr *rhs = new CppExpr(CXPR_CALL,
		       new CppExpr(CXPR_REF,CppConst::sizeof_),
		       new CppExpr(CXPR_REF,name)),
              *lhs = new CppExpr(CXPR_REF,name);

      while (idx-- > 0) {
	lhs = new CppExpr(CXPR_DEREF,(CppExpr *)0,lhs);
      }

      val = new CppExpr(CXPR_DIVIDE,rhs,
		                    new CppExpr(CXPR_CALL,
			            new CppExpr(CXPR_REF,CppConst::sizeof_),lhs));

      ct = CppTypeRef::create(&Nowhere,0,CppType::Int);
      ct->x.f.cnst_src = 1;
      scp->addDecl(new CppDecl(ct,sx,val,PCX_NONE,INIT_PROC_C));
    } else {
      dcl = (scp = scp->up)->findRef(sx);
      if (!dcl) {
	val = new CppExpr(CXPR_CALL,
                new CppExpr(CXPR_PNTR,
	          new CppExpr(CXPR_REF,name),
		  new CppExpr(CXPR_REF,CppConst::size)));
                
        ct  = CppTypeRef::create(&Nowhere,0,CppType::Int);
	scp->addDecl(new CppDecl(ct,sx,val,PCX_NONE));
      }
    }

  }

  return sx;
}

CppTypeRef *SizeXmb(poolRef name,CppTypeV *base)
{
  CppTypeRef *r1    = new CppTypeRef(&Nowhere,0,base);
  String      sz("_sz_"); sz += strDeref(name);
  poolRef     sz_nm = strSaveStr(sz);

  r1->index = new CppIndex(0,new CppExpr(CXPR_MULTIPLY,
	                       new CppExpr(CXPR_REF,sz_nm),CppExpr::RefMB));
  return r1;  
}

CppDecl *addRxDecl(CppScope *proc,CppScope *a_scp,const Location *where,CppTypeV *node,
		   CppExpr *ridx,
		   CppTypeV *rtyp,int flags,
		   eRX mode,poolRef name,
                   CppExpr *sens = 0, const CppDecl **pArgT = 0)
{
  static const char *ms[] = {"_drv_","_rcv_","_clnt_"};

  CppDecl *dcl;
  CppTypeRef *rxt = 0;
  String rx_nm(ms[mode]);
  const char *nm = strDeref(name); rx_nm += nm;
  poolRef rx = strSaveStr(rx_nm);
  if (!(dcl = a_scp->findRef(rx))) {
    CppDecl          *sig  = a_scp->findRef(name);
    assert(sig);
    CppTypeV         *styp = sig->typ;
    const CppTypeV   *prb  = styp->cRef();
    const CppTypeRef *ref  = styp->cRef();
    const CppDecl    *targ = 0;
    poolRef           sgsz = addSzDecl(proc,a_scp,sig,name,nm,rtyp);    

    if (RXD_TMPLT & flags) {
      for (; ref = prb->cRef(); prb = ref->typ) {
        if (ref->isInst()) { targ = ref->args;
	                     break; }
      }
      assert(targ);
      rxt = new CppTypeRef(&Nowhere,0,rtyp);
      rxt->set_inst(const_cast<CppDecl *>(targ));        
      *pArgT = targ;
      rtyp   = rxt;
    }
    CppExpr  *size = 0;
    CppExpr  *val  = 0;
    CppExpr  *dstr = 0;
    eINIT     ini0 = INIT_RX_0,
              ini2 = INIT_RX_D;

    const CppTypeV *rtyp0 = rtyp;
    CppExpr *iarg = new CppExpr(CXPR_REF,name),
            *fn   = 0;
    poolRef  mthd = CppConst::prc_init;
    eCXPR    op   = CXPR_SCOPE;
    int      arr  = styp->indices();
    if (arr) {
      iarg = new CppExpr(CXPR_COMMA,
	       new CppExpr(CXPR_B_AND,0,
		 new CppExpr(CXPR_INDEX,iarg,new CppExpr(CXPR_U32,(U32)0))),
	       new CppExpr(CXPR_CALL,
	         new CppExpr(CXPR_REF,CppConst::sizeof_),new CppExpr(CXPR_REF,name)));
    } else if (mode & RX_CLNT) {
      iarg = new CppExpr(CXPR_COMMA,iarg,new CppExpr(CXPR_U32,(U32)0));
    }
    CppExpr *usg;
    switch (mode) {
    case RX_DRV:  fn   = new CppExpr(CXPR_REF,rx);
                  op   = CXPR_PNTR;
	          ini0 = INIT_DRV_A;
	          break;
    case RX_RCV:  if (arr) {
		    assert(0);
		  } else {
		    fn = new CppExpr(CXPR_REF,CppConst::receiver);
		  }
                  iarg = new CppExpr(CXPR_COMMA,
		           iarg,
                           new CppExpr(CXPR_COMMA,
                             new CppExpr(CXPR_CALL,
 		               new CppExpr((ref && ref->x.f.indrct) ? CXPR_PNTR
		     	    	                                    : CXPR_OBJ,
			       new CppExpr(CXPR_REF,name),
		               new CppExpr(CXPR_REF,CppConst::size))),
			       CppExpr::RefMB));
		  break;
    case RX_CLNT_RD: usg = new CppExpr(CXPR_REF,CppConst::CUSG_READ);  goto clnt;
    case RX_CLNT_WR: usg = new CppExpr(CXPR_REF,CppConst::CUSG_WRITE); goto clnt;
    case RX_CLNT:    usg = new CppExpr(CXPR_REF,CppConst::CUSG_NONE);  goto clnt;
            clnt: if (arr) {
		    fn = new CppExpr(styp,2);
		  } else {
		    fn = new CppExpr(CXPR_REF,CppConst::pipe_client);
		  }
  	          iarg = new CppExpr(CXPR_COMMA,
		       iarg,
                       new CppExpr(CXPR_COMMA,
			 new CppExpr(CXPR_U32,(U32)0),
			 new CppExpr(CXPR_COMMA,
                           new CppExpr(CXPR_U32,(U32)0),
                           new CppExpr(CXPR_COMMA,
  		             new CppExpr(CXPR_REF,CppConst::CUSG_NONE),
			     CppExpr::RefMB))));
	          break;
    }
    rxt = new CppTypeRef(&Nowhere,0,rtyp);
    rxt->x.f.indrct++;
    rtyp = rxt;

    if (fn) {
      val  = new CppExpr(CXPR_CALL,
                       new CppExpr(op,fn,new CppExpr(CXPR_REF,mthd)),iarg);
    } else {
      val  = new CppExpr(CXPR_CALL,new CppExpr(CXPR_REF,mthd),iarg);
    }

    dstr = new CppExpr(CXPR_INDEX,
		       new CppExpr(CXPR_REF,CppConst::delete_),
		       new CppExpr(CXPR_REF,rx));
    ini2 = INIT_RX_DA;

    CppExpr *rhs  = new CppExpr(CXPR_REF,name);
    if (size) {
      rhs = rhs->addAfter(size);
    }
    switch (mode) {
    case RX_CLNT_RD:
    case RX_CLNT_WR:
    case RX_CLNT:    rhs = rhs->addAfter(new CppExpr(CXPR_B_AND,0,
		 			  new CppExpr(CXPR_INDEX,
					    new CppExpr(CXPR_REF,CppConst::_wait),
				            new CppExpr(CXPR_REF,CppConst::_i))));
    }
    CppExpr *val2 = new CppExpr(CXPR_CALL,
				new CppExpr(CXPR_REF,CppConst::prc_init),
				rhs);
    CppDecl3 *dcl3 = new CppDecl3(rtyp,rx,val,val2,dstr);
    dcl3->init0 = ini0;                                 
    dcl3->init2 = ini2;
    dcl = proc->addDecl(dcl3,-1);
    if (sens) {
      if (ridx) {
        assert(CXPR_COMMA == sens->opr);
        sens->x.hs.r = new CppExpr(CXPR_COMMA,
                         new CppExpr(CXPR_B_AND,0,
                           new CppExpr(CXPR_INDEX,
			     new CppExpr(CXPR_SUB,
			       new CppExpr(CXPR_B_AND,0,
                                 new CppExpr(CXPR_INDEX,
			           new CppExpr(CXPR_REF,rx),
			             new CppExpr(CXPR_REF,CppConst::_W)))),
                           ridx)),
		       	 sens->x.hs.r);
        sens->x.hs.r = new CppExpr(CXPR_COMMA,
                         new CppExpr(CXPR_B_AND,0,
			   new CppExpr(CXPR_REF,rx)),
                         new CppExpr(CXPR_COMMA,
			   new CppExpr(CXPR_REF,CppConst::_W),
			   sens->x.hs.r));
        sens->x.hs.l = new CppExpr(CXPR_REF,CppConst::RCVR_ARRAY_);
      }
    }
  }

  return dcl;
}

void fixCall2(poolRef *pRef)
{
  poolRef &ref(*pRef);

  switch (ref.pool) {
  case TIMING_POOL: switch (ref.index) {
    case TIM_POSEDGE: *pRef = CppConst::RCVR_POSEDGE_; break;
    case TIM_NEGEDGE: *pRef = CppConst::RCVR_NEGEDGE_; break;
    }
  }
}

#define CONST_STR(n,s,p,i) poolRef CppConst::n;
#include "const_str.inc"

void CppConst::setStrings()
{
  if (NULL_REF(this_)) {
#define CONST_STR(n,s,p,i) if (p) {n.pool = p; n.index = i;} else n = strSaveStr(#s);
#include "const_str.inc"
  }
}

CppExpr *insertParc(CppExpr *ref_xpr) 
{
  return new CppExpr(CXPR_SCOPE,new CppExpr(CXPR_REF,CppConst::parc),
		                ref_xpr);
}

eAZR CppType::fixEvtRef(CppExpr **p_xpr,CppScope *a_scp,int level,eAZC flgs,
		       const Location *where,CppStmt *stmt)
{
  CppExpr       *xpr   = *p_xpr,
                *lhs,
                *rhs,
                *trg,
                *val,
                *tim;
  CppScope      *proc  = dynamic_cast<CppScope *>(this);
  char           buff[16];
  char          *pfn; 
  int            rp;
  poolRef        lbl;
  poolRef        rfn;
  poolRef        Ref;
  CppStmtIf     *susp;
  CppExpr       *sens  = 0;
  CppExpr       *ridx  = 0;
  poolRef        xba   = NullRef;
  CppDecl       *dcl   = 0;
  CppDecl       *dclx  = 0;
  const CppDecl *dclt  = 0;
  eCXPR          xop   = CXPR_PNTR;
  int            blkng = 0;
  int            fxd   = 0;        

#define HAS_RSM  rp  = proc->incrRsm();\
                 sprintf(buff,"L%d",rp);\
                 lbl = strSaveStr(buff);\
                 sprintf(buff,"resume%d",rp);\
                 rfn = strSaveStr(buff);
                     
  switch (xpr->opr) {
  default:
    if (dynamic_cast<CppStmtAt *>(stmt) ||
        dynamic_cast<CppStmtBefore *>(stmt)) {
       HAS_RSM
       proc->setWait();       
       lhs  = 0;
       rhs  = new CppExpr(CXPR_CALL,
			  new CppExpr(CXPR_REF,
                                      (flgs & AZC_BEFORE) ? CppConst::waitSigPre
				                          : CppConst::waitSig),
                          xpr);
       sens = xpr->addAtEnd(CppExpr::VoidP0);
       goto as_sense;
    }
    return AZR_NONE;
  case CXPR_BECOMES:
    xba = CppConst::nba;
    goto set_trg;
  case CXPR_ASSIGN:
    xba = CppConst::ba;
  set_trg:
    trg = xpr->x.hs.l;
    val = xpr->x.hs.r;
    rhs = tim = 0;
    assert(!NULL_REF(trg->getRef()));
    goto fix_assign;
  case CXPR_AT: 
  case CXPR_BEFORE: {
    xba = CppConst::nba;
    switch ((rhs = xpr->x.hs.r)->opr) {
    case CXPR_BECOMES: xba = CppConst::ba;
    case CXPR_ASSIGN:  
      trg = xpr->x.hs.l;
      tim = rhs->x.hs.l;
      val = rhs->x.hs.r;
      fix_assign:
      switch (trg->opr) {
      case CXPR_REF_DCL:
                     dcl = trg->x.dcl; goto have_dcl_1;
      case CXPR_REF: 
                     fxd = AZR_BUILTIN;
                     if (dcl = a_scp->findRef(trg->x.ref)) {
		      have_dcl_1:
	               if (dcl->typ->isA(BUILTIN_POOL,BLT_SIGNAL)) {
                         dclx = addRxDecl(proc,a_scp,where,dcl->typ,
                                          ridx,CppType::Driver,RXD_TMPLT,
                                          RX_DRV,dcl->name,0,&dclt);     
			 trg->setRef(dclx->name);
                         fxd       |= AZR_SIGNAL;
	               } else if (dcl->typ->isA(BUILTIN_POOL,BLT_DRIVER) ||
   	                          (blkng = dcl->typ->isA(BUILTIN_POOL,BLT_PIPE))) {
                         const CppTypeRef *drvt = dcl->typ->cRef();
                         if (drvt) dclt = drvt->args;
                         xop  = CXPR_OBJ;
                         fxd |= blkng ? AZR_PIPE
			              : AZR_DRIVER;
                       } else if (CXPR_ASSIGN == xpr->opr) {
                         goto leave;     
		       }
	             }
                     if (blkng && 0 == level) {
                       HAS_RSM
                       proc->setWait();   
                       proc->setChan();
		       val = val->addAtEnd(new CppExpr(CXPR_SUB,
					     new CppExpr(CXPR_B_AND,0,
					       new CppExpr(CXPR_INDEX,
			                         new CppExpr(CXPR_REF,CppConst::_wait),
					         new CppExpr(CXPR_REF,CppConst::_W)))));
                       val = val->addAtEnd(new CppExpr(CXPR_REF,rfn));
		     }
                     if (tim) {
                       rhs->opr    = CXPR_COMMA;
                       rhs->x.hs.l = val;
                       rhs->x.hs.r = tim;
                       val         = rhs;
                     }
                     if (dclt) {
                       String coerce("val_");
                       coerce     += strDeref(dclt->typ->Name());
                       poolRef cfn = strSaveStr(coerce);
                       if (a_scp->findType(cfn)) {
                         if (CXPR_COMMA == val->opr) {
                           val->x.hs.l = new CppExpr(CXPR_COERCE,
			  	  	     new CppExpr(dclt->typ),val->x.hs.l);
			 } else {
                           val = new CppExpr(CXPR_COERCE,
			  	  	     new CppExpr(dclt->typ),val);
			 }
		       }
		     }
		     xpr->opr    = xop;
                     xpr->x.hs.r = new CppExpr(CXPR_CALL,
				     new CppExpr(CXPR_REF,xba),val);
                     xpr->setRplcd();
                     if (blkng && 0 == level) {
                       CppScope *scp = new CppScope;
                       (*p_xpr = new CppExpr(COPR_BLOCK))->x.scp = scp;
		       xpr = new CppExpr(CXPR_LABEL,
			       new CppExpr(CXPR_REF,lbl),
                               new CppExpr(CXPR_ASSIGN,
                                 lhs = new CppExpr(CXPR_REF,CppConst::_csts),
		                 xpr));
		       scp->addStmt(new CppStmtExpr(where,CSTMT_Expr,xpr));
                       CppStmtIf *blkd = new CppStmtIf(where,
		                           new CppExpr(CXPR_L_EQ,
		  	                     new CppExpr(CXPR_REF,CppConst::CHNS_BLOCK_),
					     lhs));
                       blkd->tru.addStmt(new CppStmtExpr(where,CSTMT_return));
		       scp->addStmt(blkd);
		     }
                    leave:
	             break;
      default:       assert(0);      
      }
      break;
    default:
      HAS_RSM
      if (lhs = xpr->x.hs.l) {
        assert(0);
      } else {
        switch ((rhs = xpr->x.hs.r)->opr) {
        case CXPR_CAST2:
          assert(0);
          stmt->addStmt(new CppStmtExpr(where,CSTMT_Expr,rhs->x.hs.r),a_scp);
          rhs->x.hs.r = 0;
          rhs->opr    = CXPR_SUB;
        case CXPR_SUB:
          assert(0);
          proc->setWait();
          rhs->x.hs.r = rhs->x.hs.l;
          rhs->x.hs.l = new CppExpr(CXPR_REF,
                                         (flgs & AZC_BEFORE) ? CppConst::waitSigPre
					                     : CppConst::waitSig);
          rhs->opr    = CXPR_CALL;
          break;
        default:
          proc->setWait();
          rhs = new CppExpr(CXPR_CALL,
			    new CppExpr(CXPR_REF,
                                         (flgs & AZC_BEFORE) ? CppConst::waitSigPre
				             	             : CppConst::waitSig),
			    rhs);
	}
        sens        = rhs->x.hs.r->addAtEnd(CppExpr::VoidP0);
       as_sense:
        sens->setCmmn();
        proc->setWait();
        lhs            = new CppExpr(CXPR_SUB,
			   new CppExpr(CXPR_B_AND,0,
			     new CppExpr(CXPR_INDEX,
			       new CppExpr(CXPR_REF,CppConst::_wait),
			       new CppExpr(CXPR_REF,CppConst::_W))));
        lhs            = lhs->addAfter(new CppExpr(CXPR_REF,rfn));
        rhs->x.hs.r    = lhs->addAtEnd(sens);
        lhs            = new CppExpr((eCOPR)COPR_BLOCK);
        CppScope *wblk = new CppScope();
        lhs->x.scp     = wblk;
        wblk->addStmt(susp = new CppStmtIf(where,
			      new CppExpr(CXPR_L_EQ,
				new CppExpr(CXPR_REF,CppConst::SIG_BLCK_),
				rhs)));
        rhs         = new CppExpr(CXPR_SUB,lhs);
        susp->tru.addStmt(new CppStmtExpr(where,CSTMT_return));
        wblk->addStmt(new CppStmtLabel(where,CSTMT_Label,lbl));
        lhs         = new CppExpr(CXPR_B_AND,0,
			new CppExpr(CXPR_INDEX,
			  new CppExpr(CXPR_REF,CppConst::_wait),
			     new CppExpr(CXPR_REF,CppConst::_W)));
	lhs         = lhs->addAfter(sens);
        wblk->addStmt(new CppStmtExpr(where,CSTMT_Expr,
                        new CppExpr(CXPR_CALL,
			  new CppExpr(CXPR_REF,
			    CppConst::unWaitSig),
			    lhs)));
        (*p_xpr = rhs)->setRplcd();
        CppExpr *sens_next;
        for (; sens != CppExpr::VoidP0 ; sens = sens_next) {
	  CppExpr **lp;
          sens_next =  sens->x.hs.r;
          lhs       = *(lp = &sens->x.hs.l);
          switch (lhs->opr) {
          case CXPR_INDEX: ridx = lhs->x.hs.r;
	                   lhs  = *(lp = &lhs->x.hs.l);
          case CXPR_REF_DCL:
 	                   dcl  = lhs->x.dcl; goto have_dcl_2;
	  case CXPR_REF:   if (dcl = a_scp->findRef(lhs->x.ref)) {
			    have_dcl_2:
			     CppDecl *dclx;
	                     switch (dcl->typ->isBuiltin()) {
                             case BLT_SIGNAL:
                               dclx = addRxDecl(proc,a_scp,where,dcl->typ,
                                                ridx,SizeXmb(dcl->name,CppType::Receiver),0,
                                                RX_RCV,dcl->name,sens);
                               lhs->setRef(dclx->name);
                               *lp = new CppExpr(CXPR_SUB,
                                       new CppExpr(CXPR_B_AND,0,
			                 new CppExpr(CXPR_INDEX,
                                           lhs,
		     			   new CppExpr(CXPR_REF,CppConst::_W))));
			       break;
                             case BLT_PIPE:
                               dclx = addRxDecl(proc,a_scp,where,dcl->typ,
                                                ridx,SizeXmb(dcl->name,CppType::PipeClient),0,
                                                RX_CLNT,dcl->name,sens);
                               lhs->setRef(dclx->name);
			       *lp = new CppExpr(CXPR_SUB,
				       new CppExpr(CXPR_B_AND,0,
				         new CppExpr(CXPR_INDEX,
                                           lhs,
					   new CppExpr(CXPR_REF,CppConst::_W))));
			     }
	                   }
	                   break;
          case CXPR_CALL2: lhs->opr     = CXPR_COMMA;
	                   sens->x.hs.l = lhs->x.hs.l;
                           assert(CXPR_REF == lhs->x.hs.l->opr);
	                   fixCall2(&lhs->x.hs.l->x.ref);
			   lhs->x.hs.l  = lhs->x.hs.r;
                           lhs->x.hs.r  = sens_next;
                           sens_next    = lhs;
                           sens->x.hs.r = lhs;
	                   break;
          default:         assert(0);
	  }
	}
      }
    }
  } break; 
  case CXPR_CALL:
    if ((lhs = xpr->x.hs.l)->getRef(&Ref)) {
      int maybe = 0;
      switch (Ref.index) {
      case BLT_WAIT:   
      case BLT_SUSPEND:
      case BLT_MIGRATE:
        HAS_RSM
	fxd = AZR_BUILTIN;
        switch (Ref.index) {
        case BLT_MIGRATE: maybe = 1;
 	                  lhs->setRef(CppConst::migrate); goto suspend;
        case BLT_SUSPEND: lhs->setRef(CppConst::suspend);
        suspend:
          proc->setWait();
          rhs = new CppExpr(CXPR_SUB,
		  new CppExpr(CXPR_B_AND,0,
                    new CppExpr(CXPR_INDEX,
		      new CppExpr(CXPR_REF,CppConst::_wait),
	      	      new CppExpr(CXPR_REF,CppConst::_W))));
          if (xpr->x.hs.r) {
            rhs = new CppExpr(CXPR_COMMA,rhs,xpr->x.hs.r);
	  }
          xpr->x.hs.r = new CppExpr(CXPR_COMMA,
				    new CppExpr(CXPR_REF,rfn),rhs);
          if (stmt) {
            CppStmt *s2;
            if (maybe) {
              proc->setStts();
              *p_xpr  = new CppExpr(CXPR_ASSIGN,
                                    new CppExpr(CXPR_REF,CppConst::_ists),
		  	            xpr);
              susp = new CppStmtIf(where,new CppExpr(CXPR_REF,CppConst::_ists));
              stmt->addStmt(susp,a_scp);
              susp->tru.addStmt(new CppStmtExpr(where,CSTMT_return));
              s2 = susp;
	    } else {
              s2 = stmt->addStmt(new CppStmtExpr(where,CSTMT_return),a_scp);
	    }
            s2->addStmt(new CppStmtLabel(where,CSTMT_Label,lbl),a_scp);
          } else {
            assert(0);
	  }
	  break;
        case BLT_WAIT:  
          proc->setStts();
          proc->setWait();
          lhs->setRef(CppConst::cpp_wait);
          rhs = xpr->x.hs.r;
          xpr->x.hs.r = new CppExpr(CXPR_COMMA,
                                    rhs,
                            new CppExpr(CXPR_COMMA,
		              new CppExpr(CXPR_REF,rfn),
				lhs = new CppExpr(CXPR_SUB,
					new CppExpr(CXPR_B_AND,0,
					  new CppExpr(CXPR_INDEX,
			                    new CppExpr(CXPR_REF,CppConst::_wait),
			                    new CppExpr(CXPR_REF,CppConst::_W))))));
          *p_xpr      = new CppExpr(CXPR_ASSIGN,
                                   new CppExpr(CXPR_REF,CppConst::_ists),
		  	           xpr);
	  (*p_xpr)->setRplcd();
          susp = new CppStmtIf(where,new CppExpr(CXPR_REF,CppConst::_ists));
          stmt->addStmt(susp,a_scp);
          susp->tru.addStmt(new CppStmtExpr(where,CSTMT_return));
	  susp->fls = new CppScope();
	  susp->fls->addStmt(new CppStmtLabel(where,CSTMT_Label,lbl));
	  susp->fls->addStmt(new CppStmtExpr(where,CSTMT_Expr,
			       new CppExpr(CXPR_CALL,
			         new CppExpr(CXPR_REF,CppConst::unWait),
                                 lhs)));
          break;
	}
        goto done;
      }
    }
    break;
  case CXPR_PNTR: 
  case CXPR_OBJ: 
    lhs = xpr->x.hs.l;
  retry:
    switch (lhs->opr) {
    case CXPR_INDEX:
      if (!ridx) {
	ridx = lhs->x.hs.r;
      }
      lhs = lhs->x.hs.l;
      goto retry;
    case CXPR_REF_DCL:
      dcl = lhs->x.dcl; goto have_dcl_3;
    case CXPR_REF: 
      dcl = a_scp->findRef(lhs->x.ref);
      if (dcl) {
       have_dcl_3:     
        switch (dcl->typ->isBuiltin()) {
        case BLT_PIPE:
          fxd = AZR_PIPE|AZR_BUILTIN;
          proc->setChan();
          proc->setWait(); 
          HAS_RSM
	  dclx = addRxDecl(proc,a_scp,where,dcl->typ,
	      	           ridx,SizeXmb(dcl->name,CppType::PipeClient),0,
		           RX_CLNT,dcl->name,sens);
	  CppStmtExpr *sx;
	  CppExpr     *nw_x1 = new CppExpr(COPR_BLOCK),
	    	      *nw_x2 = new CppExpr(CXPR_SUB,nw_x1),
                      *nw_x3 = 0;
          (*p_xpr = nw_x2)->setRplcd();
          nw_x1->x.scp = new CppScope();
          assert(CXPR_CALL == xpr->x.hs.r->opr);  
          lhs          = xpr->x.hs.r->x.hs.l;
	  rhs          = xpr->x.hs.r->x.hs.r;
          poolRef bfn;
          lhs->getRef(&bfn);
          switch (bfn.index) {
          case BLT_WRITE:
            lhs->setRef(CppConst::write);
            nw_x3      =  new CppExpr(CXPR_COMMA,
			    new CppExpr(CXPR_B_AND,0,
			      new CppExpr(CXPR_INDEX,
			        new CppExpr(CXPR_REF,CppConst::_wait),
				new CppExpr(CXPR_REF,CppConst::_W))),
		            new CppExpr(CXPR_REF,rfn));
            break;
          case BLT_READ:
            lhs->setRef(CppConst::read);
            nw_x1->x.scp->addStmt(
                          new CppStmtExpr(where,CSTMT_Expr,
		 	    new CppExpr(CXPR_OBJ,
			      new CppExpr(CXPR_INDEX,
			        new CppExpr(CXPR_REF,dclx->name),
			          new CppExpr(CXPR_REF,CppConst::_W)),
			      new CppExpr(CXPR_CALL,
			        new CppExpr(CXPR_REF,CppConst::Clear)))));
            nw_x3      = new CppExpr(CXPR_COMMA,
			   new CppExpr(CXPR_REF,dclx->name),
		           new CppExpr(CXPR_COMMA,
 			       new CppExpr(CXPR_REF,CppConst::_wait),
			       new CppExpr(CXPR_COMMA,
			         new CppExpr(CXPR_REF,CppConst::_W),
				 new CppExpr(CXPR_REF,rfn))));
            break;
          }
          nw_x1->x.scp->addStmt(
			  new CppStmtLabel(where,CSTMT_Label,lbl));
          nw_x1->x.scp->addDecl(new CppDecl(CppType::Int,CppConst::_sz));
          nw_x2 = new CppExpr(CXPR_SUB,
                    new CppExpr(CXPR_ASSIGN,
                      new CppExpr(CXPR_REF,CppConst::_csts),xpr));
          CppStmtIf *prd = new CppStmtIf(where,
		             new CppExpr(CXPR_L_EQ,
		  	       new CppExpr(CXPR_REF,CppConst::CHNS_BLOCK_),
                                           nw_x2));
          nw_x2 = xpr->x.hs.r;
          while (CXPR_COMMA == nw_x2->x.hs.r->opr) {
            nw_x2 = nw_x2->x.hs.r;
          }
          nw_x2->x.hs.r = new CppExpr(CXPR_COMMA,
                                      nw_x2->x.hs.r,nw_x3);                            
          prd->tru.addStmt(new CppStmtExpr(where,CSTMT_return));
          nw_x1->x.scp->addStmt(prd);
          nw_x1->x.scp->addStmt(new CppStmtExpr(where,CSTMT_Expr,
                                   new CppExpr(CXPR_REF,CppConst::_csts)));
          goto done;
	}
      }
    }    
  }

 done:
  return AZR(fxd);
}

CppDecl *CppScope::addConst(char const *nm, int val)
{
  poolRef   Ref = strSaveStr(nm);
  CppType  *enm = new CppType(&Nowhere,PCT_ENUM);
  CppDecl  *var = new CppDecl(enm,Ref,new CppExpr(CXPR_I32,val));
  CppScope *scp = new CppScope;
  CppDecl  *dcl = addDecl(var,-1);

  enm->set_bod(this);
  addStmt(new CppStmtType(enm,&Nowhere),-1);

  return dcl;
}

void CppScope::moveStmts(CppScope *to)
{
  to->stmt0  = stmt0  ; stmt0  = 0;
  to->p_stmt = p_stmt ; p_stmt = &stmt0;
}

CppStmtBlock::CppStmtBlock(const CppStmtBlock *scp)
  : CppStmt(scp)
{
  up  = scp->up;
  typ = CSTMT_Block;
}

void CppScope::copyStmts(DupContext *cntxt,CppScope *to) const
{
  CppStmt *scan = stmt0;

  for (; scan ; scan = scan->next) {
    to->CppScope::addStmt(scan->duplicate(cntxt));
  }
}

CppStmt *CppStmtBlock::duplicate(DupContext *cntxt)
{
  CppStmtBlock *rblk = new CppStmtBlock(this);

  copyStmts(cntxt,rblk);

  return rblk;
}

CppScope::CppScope(const CppScope *scp)
 : next(0),
   sc_typ(scp->sc_typ),
   types(scp->types),
   decls(scp->decls),
   args(0), 
   stmt0(0), p_stmt(&stmt0),
   up(scp->up)
{
  assert(!nm_spcs);
  assert(!args);
}

CppScope *CppScope::duplicate(DupContext *cntxt,CppScope *rblk)
{
  assert(SCP_BLOCK == sc_typ);

  if (rblk) {
    rblk->up = up;
  } else {
    rblk = new CppScope(this);
  }

  copyStmts(cntxt,rblk);

  return rblk;
}

CppStmtExpr::CppStmtExpr(const CppStmtExpr *from)
 : CppStmt(from),
   expr(0)
{
}

CppStmt *CppStmtExpr::duplicate(DupContext *cntxt)
{
  CppStmtExpr *ret = new CppStmtExpr(this);

  ret->expr = expr->duplicate(cntxt);

  return ret;
}

CppStmt *CppStmtIf::duplicate(DupContext *cntxt)
{
  CppStmtIf *ret = new CppStmtIf(this);

  ret->cnd = cnd->duplicate(cntxt);

  tru.duplicate(cntxt,&ret->tru); 

  if (ret->fls) {
    ret->fls = fls->duplicate(cntxt); 
  }

  return ret;
}

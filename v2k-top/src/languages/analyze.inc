/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* $Id: analyze.inc,v 1.7 2010/05/25 16:25:03 dkc Exp $" */

eAZR CppContext::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  CppScope *scan = &root;
  int       sts  = 0;

  while (scan) {
    switch (scan->sc_typ) {
      case SCP_NAMESPACE: scan->clrCtrl(); break;
      default:            sts |= scan->Analyze(proc,a_scp,&source,level,flgs);
                          // if (sts) ???
    }
    scan = scan->next;    
    if (scan == &root) break;
  }

  return AZR(sts);
}

eAZR CppStmtBlock::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs) {
  return CppScope::Analyze(proc,this,&source,level,flgs);
}

eAZR CppStmtNmdBlk::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs) {
  nm_spc->ctrl_seg = this;
  return nm_spc->Analyze(proc,nm_spc,&source,level,flgs);
}

eAZR CppTypeScpR::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs) {
  assert(0);
}

eAZR CppTypeScp::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs) 
{
  if (!(flgs & (AZC_TYPEREF|AZC_DECL))) flgs = AZC(flgs|AZC_DO_SCOPE);

  return CppTypeCls::Analyze(proc,this,where,level,flgs);
}

eAZR CppType::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  int sts = 0;

  switch (typ) {
  case PCT_PROCESS:
    assert(0 == proc);
    proc = this;
    break;
  case PCT_ARCHITECTURE:
  case PCT_MODULE:
    if (flgs & AZC_DO_SCOPE) {
      CppTypeV *fn = ovrld;
      if (fn) do {
        CppScope   *fscp = fn->Scope();
        CppTypeRef *cnstrctr;
        if (SAME_REF(name,fn->name) && (cnstrctr = fn->cRef())) {
          CppDecl *arg    = new CppDecl(CppType::ConstCharP,
                                        CppConst::nm,CppExpr::Int0);
          arg->next       = cnstrctr->args;
          cnstrctr->args  = arg;
          arg             = new CppDecl(CppType::ModuleP,CppConst::up,CppExpr::Int0);
          arg->next       = cnstrctr->args;
          cnstrctr->args  = arg;
          cnstrctr->addInit(new CppDecl(CppConst::Module),
	                    new CppExpr(CXPR_COMMA,
		              new CppExpr(CXPR_REF,CppConst::up),
		              new CppExpr(CXPR_REF,CppConst::nm)));
	  CppExpr *rt = new CppExpr(CXPR_PNTR,
                          new CppExpr(CXPR_REF,CppConst::up),
                          new CppExpr(CXPR_CALL,
	 		  	      new CppExpr(CXPR_REF,CppConst::isRoot)));
	  CppStmtIf *top = new CppStmtIf(&Nowhere,rt);
          fscp->addStmt(top);
          CppExpr *bnd = new CppExpr(CXPR_CALL,
				     new CppExpr(CXPR_REF,CppConst::bind_children));
	  top->tru.addStmt(new CppStmtExpr(&Nowhere,CSTMT_Expr,bnd));
        }
        fn = fn->next;
      } while (fn != ovrld);
    }
    break;
# ifdef NEED_SYSTEMC
  case PCT_CLASS:
    if (flgs & AZC_SYSC) {
      CppTypeCls *cls = dynamic_cast<CppTypeCls *>(this);
      CppTypeV   *inh = cls->inherits;
      if (inh) {
        poolRef nm = inh->Name();
        switch (nm.pool) {
	case SYSTEMC_POOL:
          switch (nm.index) {
            case SYSC_SC_MODULE:
              typ           = PCT_MODULE;
              cls->inherits = 0;
              CppScope *scp = cls->Scope();
              scp->fixSystemC(a_scp,where,level,flgs);
	  }
	}
      }
    }
# endif
  }

  if (flgs & AZC_DO_SCOPE) {
    flgs = AZC(flgs & ~AZC_DO_SCOPE);
    sts  = a_scp->CppScope::Analyze(proc,0,where,level,flgs);
    sts &= ~(AZR_BUILTIN|AZR_INIT1);    
  } else {
    switch (name.pool) {
    case BUILTIN_POOL: sts |= AZR_INIT1;
    }
  }

  return AZR(sts);
}

eAZR CppScope::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs) {
  CppStmt *scan = const_cast<CppStmt *>(firstStmt());
  int      sts  = 0;

#if DBGLVL > 0 
  if (!(flgs & AZC_IN_FN)) {	
    assert(!visiting); visiting = 1;
  }
#endif

  CppStmt       *scan_next;
  const CppStmt *last = lastStmt();
  for (; scan != last ; scan = scan_next) {
    scan_next = scan->next;
    sts |= scan->Analyze(proc,this,where,level,flgs);
#   ifdef NEED_SYSTEMC 
    if (sts & AZR_FN2MOD) {
      sts       &= ~AZR_FN2MOD;
      scan->typ  = CSTMT_Fn2Mod;
      scan->fixSystemC(a_scp,where,level,AZC(flgs|AZC_FN2MOD));
    }
#   endif
  }

#if DBGLVL > 0 
  visiting = 0;
#endif

  return AZR(sts);
}

eAZR CppTypeRef::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  int sts = 0;

  if (typ) {

    if (AnalysisFn && !(flgs & AZC_IN_FN_T)) {
       return (*AnalysisFn)(this,proc,a_scp,where,level,AZC(flgs|AZC_IN_FN_T));
    }

    sts = const_cast<CppTypeV *>(typ)->Analyze(proc,a_scp,where,level,
                                                 AZC(flgs|AZC_TYPEREF));
    if (inits) {
      CppInit *init = inits->list;
      if (init) for (; init->super ; init++) {
        sts |= init->Analyze(proc,a_scp,where,level,flgs);
      }
    }

#   ifdef NEED_SYSTEMC
    if (flgs & AZC_SYSC) {
      switch (name.pool) {
      case SYSTEMC_POOL: switch (name.index) {
        case SYSC_SC_MAIN: sts |= AZR_FN2MOD;
        }
      }
    }
#   endif
  }

  return AZR(sts);
}

void mkSlcSttc (CppExpr *xpr)
{
  assert(CXPR_RANGE == xpr->opr);
  xpr->opr = CXPR_STTC_RNG;
}

eAZR CppDecl::Analyze(CppType *proc,CppScope *scp,const Location *where,int level,eAZC flgs)
{
  int sts = 0;
  int idx;

  if (Val()) {
    flgs = AZC(flgs|AZC_DECL);
    sts |= typ->Analyze(proc,scp,where,level,flgs);
    sts |= Val()->Analyze(proc,scp,where,level,flgs);
    if (sts & AZR_INIT1) {
      if (proc && 0 == level) {
        sts  &= ~AZR_INIT1;
        init0 = INIT_CNSTRCT_1;
        poolRef inm = CppConst::init;
        switch (idx = typ->isBuiltin()) {
        default: 
          assert(0);
        case BLT_DRIVER: 
          init0 = INIT_DRV_0;
          if (sts & AZR_RANGE) {
            sts &= ~AZR_RANGE;
            mkSlcSttc(val[0]);
	    init0  = INIT_DRV_2;
	    inm    = CppConst::init_drv;
	  }
          CppExpr *xpr = new CppExpr(CXPR_CALL,
  				       new CppExpr(CXPR_REF,inm),
				       val[0]);
          val[0] = xpr;
	}
      }
    } else if (sts & AZR_INST_M) {
      CppTypeRef *typ2 = typ->cRef();
      if (typ2) {
        typ2 = new CppTypeRef(*typ2);
      } else {
        typ2 = new CppTypeRef(&Nowhere,0,typ);
      }
      typ2->x.f.indrct++;
      typ   = typ2;
      if (sts & AZR_STTC_RNG) {
        assert(scp);
        scp->addSttcRngs();
      }
      sts  &= ~(AZR_INST_M|AZR_STTC_RNG);
      init0 = INIT_INST_0;
      init1 = INIT_INST_M;
      init2 = INIT_INST_B;
      init3 = INIT_INFNTCD;
    } else if (PCT_PROCESS == typ->getCT()) {
      setVal(1,new CppExpr(CXPR_CALL,
	         new CppExpr(CXPR_REF,CppConst::init)));
      init1 = INIT_PROC_1;
    }
    sts &= ~(AZR_SIGREF|AZR_PIPE);
  } else {
#   ifdef NEED_SYSTEMC
    if (flgs & AZC_SYSC) {
      CppTypeRef *ref = typ->cRef();
      if (ref) {
        if (SYSTEMC_POOL == ref->Name().pool) switch (ref->Name().index) {
        case SYSC_SC_IN:
        case SYSC_SC_OUT:
          ref->typ = CppType::Signal;
          ref->x.f.indrct++;
          break;
        case SYSC_SC_SIGNAL:
          ref->typ = CppType::Signal;
	}
      } else {
        CppTypeCls *cls = typ->cTypCls();
        if (cls && SYSTEMC_POOL == cls->Name().pool) switch (cls->Name().index) {
        case SYSC_SC_CLOCK:
          typ = CppType::SigBool;
	}
      }
    }
#   endif
  }

  return AZR(sts);
}

eAZR CppStmtDecl::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  return dcl->Analyze(proc,a_scp,where,level,flgs);
}

eAZR CppStmtIf::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  int sts = 0;

  if (cnd) { sts |= cnd->Analyze(proc,a_scp,&source,level,flgs); }
  sts |= tru.Analyze(proc,a_scp,&source,level,flgs);
  if (fls) { sts |= fls->Analyze(proc,a_scp,&source,level,flgs); }
 
  sts &= ~AZR_SIGREF;

  return AZR(sts);
}

eAZR CppStmtFork::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  static int     f_depth = 0;

  int            sts     = 0,
                 rp,
                 w0,
                 rp0;
  CppStmt       *scan    = const_cast<CppStmt *>(firstStmt());
  CppTypeScp    *scop    = dynamic_cast<CppTypeScp *>(proc);

  if (rplx) {
    assert(replicated < 0);
    f_depth++;
    CppExpr rslt(CXPR_U32,-1);
    if (rslt.eval(rplx) > 0) {
      replicated = rslt.x.i64;
      assert (replicated >= 0);
      if (replicated <= 0) return AZR_OK;
      if (replicated > 1) {
	DupContext    cntxt(this,&f_depth);
        CppStmtBlock *blk0 = new CppStmtBlock(this);
	char          buff[8];
        sprintf(buff,"__%d",f_depth);
	moveStmts(blk0);
        blk0->up = this;
        CppScope::addStmt(blk0);
	for (int r = 1; r < replicated ; r++) {
	  CppStmtBlock *blkr = dynamic_cast<CppStmtBlock *>(
                                     blk0->duplicate(&cntxt));
          CppScope::addStmt(blkr);
	  blkr->addConst(buff,r);
        }
	blk0->addConst(buff,0);	
	scan = const_cast<CppStmt *>(firstStmt());
      }
    } else {
      assert(!"Non-constant fork array!");
    }
  }

  const CppStmt *last = lastStmt();
  CppStmt       *scan_next;
  for (; scan != last ; scan = scan_next) { 
    rp0 = scop->forkRsm(&w0);
    addLabel(rp0,1+w0);
    scan_next = scan->next;
    sts |= scan->Analyze(proc,this,where,level,flgs);
#   ifdef NEED_SYSTEMC 
    if (sts & AZR_FN2MOD) {
      sts       &= ~AZR_FN2MOD;
      scan->typ  = CSTMT_Fn2Mod;
      scan->fixSystemC(a_scp,where,level,AZC(flgs|AZC_FN2MOD));
    }
#   endif
    rp = scop->rsmPoints();
    if (scop->msz < rp) {
      REALLOC(scop->fmap,rp,typeof(*scop->fmap));
      while (scop->msz < rp) scop->fmap[scop->msz++] = -1;
    }
    while (rp0 <= --rp) {
      if (scop->fmap[rp] < 0) scop->fmap[rp] = 1+w0; 
    }
  }

  if (rplx) {
    f_depth--;
  }

  return AZR_OK;
}

eAZR CppStmtWhile::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  int sts = 0;

  sts |= cnd->Analyze(proc,a_scp,&source,level,flgs);
  sts |= CppStmtBlock::Analyze(proc,a_scp,&source,level,flgs);

  return AZR(sts);
}

eAZR CppStmtDo::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  int sts = 0;

  sts |= CppStmtBlock::Analyze(proc,a_scp,&source,level,flgs);
  sts |= cnd->Analyze(proc,a_scp,&source,level,flgs);

  return AZR(sts);
}

eAZR CppStmtCatch::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  int sts = 0;

  sts |= CppStmtBlock::Analyze(proc,a_scp,&source,level,flgs);

  return AZR(sts);
}

eAZR CppStmt::Analyze(CppType *proc,CppScope *scp,const Location *where,int level,eAZC flgs)
{
  switch (typ) {
  default:            assert(0);
  case CSTMT_TmplRef:
  case CSTMT_break:   break;
  }

  return AZR_OK;
}

eAZR CppStmtSwitch::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  int sts = 0;

  sts |= cnd->Analyze(proc,a_scp,&source,level,flgs);
  if (sts & AZR_SIGNAL) {
    sts &= ~AZR_SIGNAL;
    sts |= cnd->fixSigValue(proc,a_scp,&source,level,flgs);
  }
  sts |= CppStmtBlock::Analyze(proc,a_scp,&source,level,flgs);

  return AZR(sts);
}

eAZR CppStmtAt::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  int sts = 0;

  setAnalyzed();

  cnd->setAnalyzed();

  int fxd = proc->fixEvtRef(&cnd,a_scp,level,flgs,&source,this);

  sts |= CppStmtBlock::Analyze(proc,a_scp,&source,level,flgs);

  return AZR(sts);
}

eAZR CppStmtBefore::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  int sts = 0;

  setAnalyzed();

  cnd->setAnalyzed();

  int fxd = proc->fixEvtRef(&cnd,a_scp,level,AZC(flgs|AZC_BEFORE),&source,this);

  sts |= CppStmtBlock::Analyze(proc,a_scp,&source,level,flgs);

  return AZR(sts);
}

eAZR CppStmtFor::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  int sts = 0;

  if (init) { sts |= init->Analyze(proc,a_scp,&source,level,flgs); }
  if (cnd)  { sts |= cnd->Analyze(proc,a_scp,&source,level,flgs);  }
  if (tail) { sts |= tail->Analyze(proc,a_scp,&source,level,flgs); }

  sts |= CppStmtBlock::Analyze(proc,a_scp,&source,level,flgs);

  return AZR(sts);
}

eAZR CppStmtType::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  return tv->Analyze(proc,a_scp,&source,level,flgs);
}

eAZR CppStmtLabel::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  if (SAME_REF_PI(name,LABELS_POOL,LBL_START)) {
    
  }

  return AZR_OK;
}

eAZR CppStmtExpr::Analyze(CppType *proc,CppScope *a_scp,const Location *where,int level,eAZC flgs)
{
  int sts = 0;

  if (expr) {
    int fxd;
    sts = expr->Analyze(proc,a_scp,&source,level,flgs);

    if (sts & AZR_BUILTIN) {
      switch (expr->opr) {
      case CXPR_CALL:
        poolRef Ref;
        if (this->expr->x.hs.l->getRef(&Ref)) {
          switch (Ref.index) {
	  case BLT_WAIT:
	  case BLT_SUSPEND:
	  case BLT_MIGRATE:
            fxd = proc->fixEvtRef(&expr,a_scp,level,flgs,&source,this);
            sts = sts & ~fxd;
	  }
	}
	break;
      case CXPR_AT:
      case CXPR_ASSIGN:
      case CXPR_BEFORE:
      case CXPR_BECOMES:
        fxd = proc->fixEvtRef(&expr,a_scp,level,flgs,&source,this);
        sts = sts & ~fxd;
        break; 
      case CXPR_OBJ:
      case CXPR_PNTR:
        fxd = proc->fixEvtRef(&expr,a_scp,level,flgs,&source,this);
	sts = sts & ~fxd;
      }
    }
  }

  return AZR(sts);
}


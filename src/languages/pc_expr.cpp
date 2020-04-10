/* Copyrhs (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * pc_expr_cpp_rcsid() {return "$Id: pc_expr.cpp,v 1.45 2010/05/29 20:30:25 dkc Exp $";}

#include "system.h"
#include "language.h"
#include "language.h"
#include "poolmngr.h"
#include "strdb.h"
#define  NEED_CPP
#define  NEED_GNU
#define  NEED_CHAR
#define  NEED_QUOTES
#define  NEED_CPPOPS
#define  NEED_PRCOPS
#define  NEED_PUNCTUATION
#define  NEED_WHITESPACE
#define  NEED_BUILTIN
#include "tokpool.h"
#include "parser.h"
#include "pc.h"
#include "cppops.h"

CppExpr::CppExpr(const Token *T,const Token *TL)
{
  init(CXPR_TOK);
  x.tk.l = T;
  x.tk.r = TL;
}

CppExpr::CppExpr(eCXPR op,CppExpr *l,CppExpr *r)
{
  init(op);
  if (x.hs.l = l) flgs = XPRF_SUB_L;
  if (x.hs.r = r) flgs = XPRF(flgs|XPRF_SUB_R);
}

CppExpr::CppExpr(eCXPR op,I64 i)
{
  init(op);
  x.i64 = i;
}

CppExpr::CppExpr(eCXPR op,U64 u)
{
  init(op);
  x.u64 = u;
}

CppExpr::CppExpr(eCXPR op,I32 i)
{
  init(op);
  x.i64 = i;
}

I32 CppExpr::x_i32() const
{
  return x.i64;
}

U32 CppExpr::x_u32() const
{
  return x.u64;
}

CppExpr::CppExpr(eCXPR op,U32 u)
{
  init(op);
  x.u64 = u;
}

CppExpr::CppExpr(eCXPR op,double d)
{
  init(op);
  x.d = d;
}

CppExpr::CppExpr(eCXPR op,poolRef Ref)
{
  init(op);
  x.ref = Ref;
}

CppExpr::CppExpr(eCOPR op)
{
  init((eCXPR)op);
  x.hs.l = x.hs.r = 0;
}

CppExpr::CppExpr(poolRef Ref)
{
  init(CXPR_REF);
  x.ref = Ref;
}

CppExpr::CppExpr(poolRef RefL,poolRef RefR)
{
  init(CXPR_LIST);
  x.hs.l = new CppExpr(RefL);
  x.hs.r = new CppExpr(RefR);
}

CppExpr::CppExpr(CppDecl *dcl)
{
  init(CXPR_REF_DCL);
  x.dcl = dcl;
}

CppExpr::CppExpr(CppTypeV *typ,int mode)
{
  switch (mode) {
  case  1: init(CXPR_TYPE_NAME);   break;
  case  2: init(CXPR_TYPE_NAME_D); break;
  default: init(CXPR_TYPE); 
  }
  x.typ = typ;
}

CppExpr *CppExpr::RecycleList;

void CppExpr::recycle()
{
  if (!(XPRF_LEAVE & flgs)) {
    flgs = XPRF_RCYCLD;
    x.hs.l      = RecycleList;
    RecycleList = this;
  }
}

CppExpr::~CppExpr()
{
  assert(!(XPRF_LEAVE & flgs));
  if (XPRF_SUB_L & flgs) RECYCLE(x.hs.l);  
  if (XPRF_SUB_R & flgs) RECYCLE(x.hs.r);
}

CppExpr *CppContext::prsDataItem(poolRef *pX,const Token **pT,int span,
                                 CppDecl **pDcl,int typ_ok)
{
  CppExpr     *x   = 0;
  const char  *str;
  CppTypeV    *typ[2];
  CppDecl     *dcl;
  const Token *T;
  int          l;
  ePcMod       pcx = PCX_NONE;
  
  switch (span) {
  case 0:  return 0;
  case 1:  str = strDeref(*pX);
           if (isdigit(*str)) goto number;
           switch (current->findObj(*pX,typ_ok ? &typ[0]
				               : (CppTypeV **)0,&dcl,0)) {
           case 1: return new CppExpr(typ[0],1);
           case 2: return new CppExpr(dcl);
	   }
           return new CppExpr(*pX);
  case 3:  if (SAME_REF_PI(pX[0],QUOTES_POOL,QUT_SINGLE) &&
               SAME_REF_PI(pX[2],QUOTES_POOL,QUT_SINGLE)) {
             return new CppExpr(CXPR_STRS,pX[1]);
	   }
  case 2:  switch (pX[0].pool) {
	   case GNU_POOL:
	     switch (pX[0].index) {
	     case GNU___ASM:
	     case GNU___ASM__: return new CppExpr(pX[0],pX[1]);
	     }
	     break;
	   case CHAR_POOL:
	     switch (pX[0].index) {
	     case CHR_DOLLAR:
	       return  new CppExpr(CXPR_INST_INDEX,x,prsDataItem(++pX,++pT,--span,0,0));
	     }
	   }
  default: while (span >= 3 &&
                  SAME_REF_PI(pX[0],QUOTES_POOL,QUT_DOUBLE) &&
                  SAME_REF_PI(pX[2],QUOTES_POOL,QUT_DOUBLE)) {
             CppExpr *y = new CppExpr(CXPR_STRD,pX[1]);
             if (x) {
               x = new CppExpr(CXPR_STRCAT,x,y);
	     } else {
               x = y;
	     }
	     span -= 3;
             if (span <= 0) return x;
             pX += 3;
             pT += 3;
	   }
           typ[1] = typ[0] = 0;
	   switch (typ_ok) {
	   case 2: pcx = PCX_CAST;
	   }
           T   = prsType(rescanning(*pT),pT[span],
                         CPRS_Expr,pcx,PCMD_UNKNOWN,0,typ,0);
           if (typ[1]) {
             if (typ[1]->isDecl()) {
               assert(pDcl);
               *pDcl = typ[1]->cRef()->args;
               assert(*pDcl);
               return 0;
	     }
	   }
           if (T >= pT[span]) {
	     if (typ[1]) return new CppExpr(typ[1]);
	     if (typ[0]) return new CppExpr(typ[0]);
	   }
           assert(0);
  } 

 number:
  U64         u64 = 0;
  int         s   = 1;
  const char *cp  = str;
  double      d   = 0;

  for (;;) switch (*cp++) {
  case 'e': 
  case 'E':
  case '.': s = sscanf(str,"%lg",&d);
            x = new CppExpr(CXPR_DBL,d);   goto done;
  case 'x':
  case 0:   s = sscanf(str,"%lli",&u64);
            x = new CppExpr(CXPR_U32,u64); goto done;
  }
 done:
  assert(1 == s);
  return x;
}

sOprInfo oprInfo[] = {
#define CPPOP(s,nm,u,p,a) {#s,u,p,a},
#include "prcops.inc"
#include "cppops.inc"
  {0}
};

CppExpr *CppContext::prsExpr(const Token *T, const Token *TL,
                             eCPRS parsing,ePcMod mods,ePCMD mode,
                             CppDecl **pDcl)
{
  int sz = TL - T;

  if (0 == sz++) return 0;

  svd_mods  = mods;
  svd_mode  = mode;
  svd_prsng = parsing;

  assert(sz > 0); 

  const Token  *Tx[sz+1],
              **pT    = Tx,
               *quote = 0;
  poolRef       x[sz+1],
               *pX    = x;
  
  while (T < TL) {

    switch (T->pi.pool) {
    case WHITESPACE_POOL: if (quote) goto keep;
                          T = nextTok(T,1); continue;
    case QUOTES_POOL:     if (quote) quote = 0;
                          else       quote = T;
    keep:
    default:             *pT++      = T; 
                          pX->pool  = T->pi.pool;
                          pX->index = T->pi.index;
                          pX++;
                          T = nextTok(T,1); continue;
    case TOK_FULL_REF:   *pT++      = T; 
                          pX->pool  = (++T)->as_int;
                          pX->index = (++T)->as_int;
			  pX++;
    }

    T = nextTok(T,1);
  }  

  *pX = NullRef;
  *pT = TL;

  CppExpr *rx = prsExprSub(x,Tx,pX - x,pDcl);

  rescanning(TL,0);

  return rx;
}

int CppContext::isTmplt(poolRef *pX,const Token **pT,int scan,
                        CppScope **pStart)
{
  int       br_cls_p = -1,
            br_opn_p = -1;
  int       scan0    =  scan,
            opn      =  0,
            jmp      = -1,
            comma    =  0,
            ret,
            cls;
  CppScope *start    = current;
  poolRef   nm;

  if (! pStart) pStart = &start;

  while (scan-- > 0) {
    int op = -1;
    switch (pX[scan].pool) {
      case CPPOPS_POOL: switch (pX[scan].index) {
#define CPPOPS_TDF
#define CPPOP(s,nm,u,p,a) case COP_##nm: op = COPR_##nm; break;
#include "cppops.inc"
                        }
	                nm = pX[(ret = scan)-1];
	                if (COPR_SCOPE == op) {
                          if (scan == scan0 - 1) {
                            if (0 == scan) {
                              *pStart = Root();
			      return 0;
			    } else {
                              if (SAME_REF_PI(nm,
                                            CPPOPS_POOL,COP_GT)) {
			        return isTmplt(pX,pT,scan-1,&start);
			      }
                              goto test_tmplt;
			    }
			  }
	                } else if (COPR_GT == op) {
                          int jmp = isTmplt(pX,pT,scan);
			  if (jmp > 0) {
                            scan = jmp;
                            continue;
			  }
			} else if (COPR_LT == op) {
                          if (!comma) {
                            return ret;
			  }
                          if (scan > 0) {
                            if (scan > 1) {
                              if (SAME_REF_PI(pX[scan-2],
                                              CPPOPS_POOL,COP_SCOPE)) {
				jmp = isTmplt(pX,pT,scan-1,&start);
			      }
			    }
			   test_tmplt:
                            CppTypeV *typ = 0;
                            CppDecl  *dcl = 0;
                            CppScope *nms = 0;
			    int       sts = start->findObj(nm,&typ,&dcl,&nms);
                            if (dcl && dcl->typ->isTmplt()) {
                              *pStart = dcl->typ->Scope();
                              return ret;
			    }
                            if (typ && typ->isTmplt()) {
                              *pStart = typ->Scope();
                              return ret;
			    }
                            if (nms) {
                              *pStart = nms;
                              return ret;
			    }
			  }
                          return -1;
			}
                        break;
      case PRCOPS_POOL: switch (pX[scan].index) {
#define CPPOP(s,nm,u,p,a) case PCO_##nm: op = COPR_##nm; break;
#include "prcops.inc"
                        } break;
      case PUNCTUATION_POOL: switch (cls = pX[scan].index) {
        case PUNC_CLS_BR:  opn = PUNC_OPN_BR; goto cls_brckt;
        case PUNC_CLS_SQ:  opn = PUNC_OPN_SQ; goto cls_brckt;
        case PUNC_CLS_CR:  opn = PUNC_OPN_CR; goto cls_brckt;
	case PUNC_COMMA:   comma = 1;
      }
      continue;
     cls_brckt: 
      int s0 = scan,
          br = 0;
      br_cls_p = scan--;
      for(; scan >= 0 ; scan--) {
        if (PUNCTUATION_POOL == pX[scan].pool) {
          if (cls == pX[scan].index) { 
            br++;
          } if (opn == pX[scan].index && 0 == br--) {
            br_opn_p = scan;
	  }
	}
      }
    }  
  }

  return -1;
}

CppExpr *CppContext::prsExprSub(poolRef *pX,const Token **pT,int span,
                                CppDecl **pDcl,int typ_ok)
{
#if DBGLVL > 1
  String xs;
  for (int i = 0; i < span ; i++) {xs += " ";
                                   xs += strDeref(pX[i]);}
#endif

  int      min_prec = CPREC_MAX;
  int      scan     = span;
  int      split    = -1;
  int      s_op;
  int      br_cls_p = -1,
           br_opn_p = -1;
  int      opn      = 0,
           last     = 1,
           cls;
  int      tl_ok    = typ_ok;
  CppExpr *ret;
  
  if (2 == typ_ok) {
    goto type_item;
  }

  for (; scan-- > 0 ; last = 0) {
    int op = -1;
    switch (pX[scan].pool) {
      case CPPOPS_POOL: switch (pX[scan].index) {
#define CPPOPS_TDF
#undef  CPPOP
#define CPPOP(s,nm,u,p,a) case COP_##nm: op = COPR_##nm; break;
#include "cppops.inc"
                        }
	                if (COPR_GT == op && scan == span-1) {
                          int jmp = isTmplt(pX,pT,scan);
			  if (jmp >= 0) {
                            scan = jmp; 
                            op   = COPR_T_INST;
                            goto test_op;
			  }
			}
                        break;
      case PRCOPS_POOL: switch (pX[scan].index) {
#undef  CPPOP
#define CPPOP(s,nm,u,p,a) case PCO_##nm: op = COPR_##nm; break;
#include "prcops.inc"
                        } break;
      case PUNCTUATION_POOL: switch (cls = pX[scan].index) {
        case PUNC_CLS_BR:  opn = PUNC_OPN_BR; goto cls_brckt;
        case PUNC_CLS_SQ:  opn = PUNC_OPN_SQ; goto cls_brckt;
        case PUNC_CLS_CR:  opn = PUNC_OPN_CR; goto cls_brckt;
        case PUNC_PERIOD:  op  = COPR_OBJ;    goto test_op;
        case PUNC_COMMA:   op  = COPR_COMMA;  goto test_op;
        case PUNC_COLON:   op  = COPR_COLON;  goto test_op;
      }
      case CPP_POOL: switch (cls = pX[scan].index) {
        case CPP_NEW:      op  = COPR_NEW;    goto test_op;
        case CPP_DELETE:   op  = COPR_DELETE; goto test_op;
        case CPP_SIZEOF:   op  = COPR_SIZEOF; goto test_op;
        case CPP_TYPENAME: op  = COPR_TYPNM;  goto test_op;
        default: continue;
      }
      assert(0);
     cls_brckt: 
      int s0 = scan,
          br = 0;
      br_cls_p = scan--;
      for(; scan >= 0 ; scan--) {
        if (PUNCTUATION_POOL == pX[scan].pool) {
          if (cls == pX[scan].index) { 
            br++;
          } if (opn == pX[scan].index && 0 == br--) {
            br_opn_p = scan;
            switch (opn) {
	    default:          op = COPR_SUB;   break;
	    case PUNC_OPN_SQ: if (!last) {
		                goto skip;
	                      }
                              op = COPR_INDEX; break;
	    case PUNC_OPN_CR: op = COPR_BLOCK; goto test_op;
	    }
            if (scan > 0 && scan + 1 == br_cls_p
                         && SAME_REF_PI(pX[scan-1],CPP_POOL,CPP_OPERATOR)) {
              op = COPR_OPR;
	    }
            goto test_op;
	  }
	}
      }
      if (s0 == span -1) {
        ret       = new CppExpr(CXPR_SUB,prsExprSub(pX+1,pT+1,span-2));
        ret->flgs = XPRF(ret->flgs|XPRF_SUB_L);
        goto done;
      }
      if (0 == scan && split < 0) {
        ret = new CppExpr(CXPR_CAST,prsExprSub(pX+1,pT+1,s0-1),
			            prsExprSub(pX+(s0+1),pT+(s0+1),span - (s0+1)));
        ret->flgs = XPRF(ret->flgs|XPRF_SUB_BOTH);
        goto done;
      } else {
        assert(scan > 0 || split >= 0); 
      }
    }
   test_op:
    if (op >= 0) {
      if (oprInfo[op].prec <= min_prec) {
        if (oprInfo[op].prec == min_prec) {
          if (oprInfo[op].assoc < 0) {
	    if (CXPR_MULTIPLY == op && split == (scan+1)) {
	      // * *
	    } else {
 	      continue;
	    }
	  }
        }
        min_prec = oprInfo[op].prec;
        split    = scan;
	s_op     = op;
      }
    }
   skip:;
  }

  if (split < 0) {
    switch (pX[0].pool) {
    case TIMING_POOL: ret   = new CppExpr(CXPR_CALL2,
			        new CppExpr(CXPR_REF,pX[0]));
		      split = 0;
		      goto get_rhs;
    }
   type_item:
    ret = prsDataItem(pX,pT,span,pDcl,typ_ok);
  } else {
    ret = new CppExpr((eCOPR)s_op);
    switch (s_op) {
    case COPR_BLOCK:
      pushDown(ret->x.scp = new CppScope());
      prsStmts(pT[1],pT[span],CPRS(svd_prsng|CPRS_Block|CPRS_Expr),svd_mods,svd_mode);
      popScope();
      break;
    case COPR_DELETE:
    case COPR_NEW:
      if (1 == split && SAME_REF_PI(*pX,CPP_POOL,CPP_OPERATOR)) {
        split    = 0;
        ret->opr = CXPR_CALL2;
      }
    case COPR_SIZEOF:
      if (0 != split) {
	ret->opr    = CXPR_CAST2;
	ret->x.hs.r = prsExprSub(&pX[1],&pT[1],split-2,(CppDecl **)0,-1);
	ret->x.hs.l = prsExprSub(&pX[split+1],&pT[split+1],span-(split+1));
      } else {
	ret->x.hs.l = prsExprSub(&pX[1],&pT[1],span-1);
      }
      break; 
    case COPR_OPR:
      assert(1 == split);
      ret->x.hs.l = new CppExpr(pX[1]);
      ret->x.hs.r = prsExprSub(&pX[4],&pT[4],span-5);
      break;
    case COPR_MULTIPLY:
      if (split + 1 == span) {
        ret = prsDataItem(pX,pT,span,(CppDecl **)0,typ_ok);
        goto done;
      }
      goto split;
    case COPR_OBJ:
      if (0 == split) {
	ret->opr = CXPR_POINT;
      }
      goto split;
    case COPR_T_INST:      
    case COPR_INDEX:
      assert(split != 0);
    case COPR_SUB:
      if (0 == split) {
        if (br_cls_p == span-1) {
          ret->flgs   = XPRF(ret->flgs|XPRF_SUB_L);
          ret->x.hs.l = prsExprSub(&pX[1],&pT[1],span-2);
          goto done;
        } else {
          split = br_cls_p;
          span--;
          if (typeList(pT[1],pT[br_cls_p],
                            CPRS_Expr,PCX_NONE,PCMD_UNKNOWN,0)) {
            ret->opr = CXPR_CAST2;
	    tl_ok    = 2;
          } else {
            ret->opr = CXPR_CALLI;
	  }
	}
        pX++; pT++; split--;
      } else {
        span--;
        if (SAME_REF_PI(pX[span],PUNCTUATION_POOL,PUNC_CLS_BR)) {
          ret->opr = CXPR_CALL;
        } else if (SAME_REF_PI(pX[span],PUNCTUATION_POOL,PUNC_CLS_SQ)) {
          ret->opr = CXPR_INDEX;
        } else {
 	  assert(SAME_REF_PI(pX[span],CPPOPS_POOL,COP_GT));
          ret->opr = CXPR_T_INST;
	}
        goto split;
      }
    default:
    split:
      if (split > 0) {
        CppDecl *dcl = 0;
        ret->flgs    = XPRF(ret->flgs|XPRF_SUB_BOTH);
        if (CXPR_CALL == ret->opr && SAME_REF_PI(pX[0],CPP_POOL,CPP_OPERATOR)) {	  
          ret->opr    = CXPR_CALL_OPR;
          ret->x.hs.l = prsExprSub(&pX[1],&pT[1],split-1,&dcl);
	} else {
          ret->x.hs.l = prsExprSub(pX,pT,split,&dcl,tl_ok);
	}
        if (!ret->x.hs.l) {
          assert(dcl);
          assert(CXPR_ASSIGN == ret->opr);
          RECYCLE(ret);
          ret = prsExprSub(&pX[split+1],&pT[split+1],span-(split+1));
          dcl->setVal(0,ret);
          return 0;           
	}
      } else {
        switch (s_op) {
#undef  CPPOP
#define CPPOP(s,nm,u,p,a) case PCO_##nm: assert(u); break;
#include "prcops.inc"
	}
        ret->flgs   = XPRF(ret->flgs|XPRF_SUB_R);
      }
    get_rhs:
      switch (ret->opr) {
      case CXPR_CALL: if (split+2 < span 
                          && SAME_REF_PI(pX[split+1],PUNCTUATION_POOL,PUNC_CLS_BR)) {
	                if (SAME_REF_PI(pX[split+2],PUNCTUATION_POOL,PUNC_OPN_BR)) {
                          split += 2;  
			}
	              }
	              break;
      }
      if (span-(split+1) > 0) {
        ret->x.hs.r = prsExprSub(&pX[split+1],&pT[split+1],span-(split+1),pDcl);
        if (!ret->x.hs.r) {
          assert(pDcl);
          RECYCLE(ret);
	}
      }
    }
  }

 done:
  return ret;
}

eAZR CppExpr::fixSigValue(CppType *proc,CppScope *scp,const Location *where,
                          int level,eAZC a_flgs) 
{
  int sts   = AZR_OK,
      sts_l = AZR_OK,
      sts_r = AZR_OK;

  if (flgs & XPRF_RPLCMNT) goto done;

  poolRef Ref;
  CppDecl *dcl;
  switch (opr) {
    case CXPR_REF_DCL: dcl = x.dcl; goto have_dcl;
    case CXPR_REF: {
      if (dcl = scp->findRef(x.ref)) {
       have_dcl:
        switch (dcl->typ->isBuiltin()) {
        case BLT_SIGNAL: 
	  opr    = CXPR_OBJ;
          flgs   = XPRF_SUB_BOTH;
          x.hs.l = new CppExpr(dcl->name);
          x.hs.r = new CppExpr(CXPR_CALL,
                               new CppExpr(CppConst::Value));
	  setRplcd();
        }
      }
    } break;
    default:
      if (x.hs.l && (flgs & XPRF_SUB_L)) {
        
        sts_l = x.hs.l->fixSigValue(proc,scp,where,level+1,a_flgs); 
      }
      if (x.hs.r && (flgs & XPRF_SUB_R)) {
        sts_r = x.hs.r->fixSigValue(proc,scp,where,level+1,a_flgs); 
      }
      sts |= sts_l|sts_r;
  }

 done:
  return AZR(sts);  
}

eAZR CppExpr::Analyze(CppType *proc,CppScope *scp,const Location *where,
                      int level,eAZC a_flgs) 
{

  int sts     = AZR_OK,
      sts_l   = AZR_OK,
      sts_r   = AZR_OK,
      fix_sig = 0;

  if (AnalysisFn) {
    if (!(a_flgs & AZC_IN_FN_E)) {
      return (*AnalysisFn)(this,proc,scp,where,level,AZC(a_flgs|AZC_IN_FN_E));
    }
  } else {
    if (flgs & XPRF_ANALYZED) {
      // ???
      goto done;
    }
    setAnalyzed();
  }
 
  if (proc || (a_flgs & AZC_BIND)) {

    CppDecl *dcl;
    poolRef  Ref;
    eCXPR    ol;
    switch (opr) {
    case CXPR_REF_DCL: dcl = x.dcl; goto have_dcl;
    case CXPR_REF: {
      if (dcl = scp->findRef(x.ref)) {
       have_dcl:
        switch (dcl->typ->isBuiltin()) {
        case BLT_SIGNAL: sts = AZR_SIGNAL; break;
        case BLT_DRIVER: sts = AZR_DRIVER; break;
        case BLT_PIPE:   sts = AZR_PIPE;   break;
        }
      }
    } break;
    case CXPR_I32:
    case CXPR_U32:
    case CXPR_I64:
    case CXPR_U64:
      break;
    case CXPR_CALL:
      if (ol = x.hs.l->getRef(&Ref)) {
	if (BUILTIN_POOL == Ref.pool) {
	  return AZR_BUILTIN;
	}
	switch (ol) {
	case CXPR_TYPE_NAME: if (x.hs.l->x.typ->isModMthd()) {
	                       opr = CXPR_CALL_MM;
			     }
	}
      }
      if (x.hs.r) {
	sts = x.hs.r->Analyze(proc,scp,where,level,a_flgs);
	if (sts & AZR_SIGNAL) {
	  sts &= ~AZR_SIGNAL;
	  sts |= x.hs.r->fixSigValue(proc,scp,where,level,a_flgs); 
	}
      }
      break;
    case CXPR_BLOCK:
      sts = x.scp->Analyze(proc,scp,where,level,a_flgs); 
      break;
    case CXPR_INDEX:
      switch (x.hs.r->opr) {
      case CXPR_RANGE: 
      case CXPR_COLON: 
        if (a_flgs & AZC_BIND) {
	  opr = CXPR_STTC_RNG;
          sts = AZR_STTC_RNG;
	} else {
          opr = CXPR_RANGE;
          sts = AZR_RANGE;
	}
        x.hs.r->opr = CXPR_COMMA;
      }
      goto as_dflt;
    case CXPR_L_NE:
    case CXPR_L_EQ:
      fix_sig = 1;
      goto as_dflt;
    case CXPR_BECOMES:
      fix_sig = 1;
    case CXPR_AT:
    case CXPR_BEFORE:
      sts |= AZR_BUILTIN;
    as_dflt:
    default:
      if (x.hs.l && (flgs & XPRF_SUB_L)) {
        sts_l = x.hs.l->Analyze(proc,scp,where,level+1,a_flgs); 
        if (sts_l & (AZR_SIGREF|AZR_PIPE)) {
          switch (opr) {
          case CXPR_ASSIGN: return AZR_BUILTIN;
          case CXPR_OBJ:    sts_l &= ~(AZR_SIGREF|AZR_PIPE);
          }
        }
      }
      if (x.hs.r && (flgs & XPRF_SUB_R)) {
        sts_r = x.hs.r->Analyze(proc,scp,where,level+1,a_flgs); 
        if (fix_sig && (sts_r & AZR_SIGNAL)) {
          sts_r &= ~AZR_SIGNAL;
          sts_r |= x.hs.r->fixSigValue(proc,scp,where,level+1,a_flgs); 
	}
      }
      sts |= sts_l|sts_r;
    }
    if ((sts & AZR_BUILTIN)) {
      switch (opr) {
      case CXPR_OBJ:
      case CXPR_PNTR:
      case CXPR_ASSIGN:
      case CXPR_BECOMES:
        int fxd = proc->fixEvtRef(&x.hs.r,scp,level,a_flgs,where);
        sts    &= ~fxd;
      }
    }
  } else {
    CppExpr *bnd;
    switch (opr) {
    case CXPR_BIND:    
      bnd    = x.hs.r;
      a_flgs = AZC(a_flgs | AZC_BIND);
      assert(bnd);
      switch (bnd->opr) {
	case CXPR_COMMA:   sts_l = bnd->x.hs.l->Analyze(proc,scp,where,level,a_flgs);
 	                   sts_r = bnd->x.hs.r->Analyze(proc,scp,where,level,a_flgs);
			   break;
	case CXPR_BND_SIG: sts_r = bnd->x.hs.r->Analyze(proc,scp,where,level,a_flgs);
	                   break;
        default:           assert(0);
      }	
      sts = AZR(sts | sts_r | sts_l);
      // drop thru
    case CXPR_BND_ARCH:  
      sts = AZR(sts|AZR_INST_M);
    }
  }

 done:
  return AZR(sts);
}

CppExpr *CppExpr::addAfter(CppExpr *xpr)
{
  assert(!(flgs & XPRF_COMMON));

  switch (opr) {
  case CXPR_COMMA: x.hs.r = new CppExpr(CXPR_COMMA,x.hs.r,xpr);
                   break;
  default:         return new CppExpr(CXPR_COMMA,this,xpr);
  }

  return this;
}

CppExpr *CppExpr::addAtEnd(CppExpr *xpr)
{
  assert(!(flgs & XPRF_COMMON));

  switch (opr) {
  case CXPR_COMMA: x.hs.r = x.hs.r->addAfter(xpr);
                   break;
  default:         return new CppExpr(CXPR_COMMA,this,xpr);
  }

  return this;
}

CppExpr::CppExpr(const CppExpr *from,DupContext *cntxt)
  : flgs(from->flgs),
    opr(from->opr) 
{
  switch (opr) {
  case CXPR_INST_INDEX: if (cntxt) {
                          int d = *cntxt->depth;
                        }
  case CXPR_REF_DCL:    // ???
  case CXPR_U32:        *this  = *from;
                        break;
  case CXPR_TYPE_NAME:  x.hs.l = from->x.hs.l;
                        assert(!from->x.hs.r);
                        break;
  case CXPR_B_AND:      x.hs.r = from->x.hs.r;
                        assert(!from->x.hs.l);
                        break;
  case CXPR_OBJ:
  case CXPR_PNTR:
  case CXPR_CALL:
  case CXPR_COMMA:
  case CXPR_INDEX:      x.hs.l = from->x.hs.l->duplicate(cntxt);
                        x.hs.r = from->x.hs.r->duplicate(cntxt);
		        break;
  default:              assert(0);
  }
}

int CppExpr::eval(const CppExpr *xpr)
{
  int sts = 0; // 1 if const, -1 not, 0 if unable to eval

  switch (xpr->opr) {
  case CXPR_U32: *this = *xpr;
                 sts   = 1;
  }

  return sts;
}

CppExpr *CppExpr::duplicate(DupContext *cntxt)
{
  int sts = 0; // 1 if const, -1 not, 0 if unable to eval

  return this ? new CppExpr(this,cntxt)
              : (CppExpr *)0;
}

int CppExpr::sameAs(CppExpr *othr)
{
  int same = 0;

  if (opr  == othr->opr && 
      flgs == othr->flgs) {
    if (flgs & XPRF_SUB_L) {
      same = x.hs.l->sameAs(othr->x.hs.l);
    } else {
      same = x.hs.l == othr->x.hs.l;
    }
    if (same) {
      if (flgs & XPRF_SUB_R) {
        same = x.hs.r->sameAs(othr->x.hs.r);
      } else {
        same = x.hs.r == othr->x.hs.r;
      }
    }
  }

  return same;
}

CppExpr *CppExpr::VoidP0;
CppExpr *CppExpr::Int0;
CppExpr *CppExpr::Int1;
CppExpr *CppExpr::RefMB;

void CppExpr::InitCommon()
{
  if (!VoidP0) {
    CppConst::setStrings();

    VoidP0 = new CppExpr(CXPR_CAST2,
	       new CppExpr(CXPR_MULTIPLY,
	         new CppExpr(CXPR_REF,CppConst::void_)),
	       new CppExpr(CXPR_I32,0));
    VoidP0->setPerm();

    Int0   = new CppExpr(CXPR_I32,0);
    Int0->setPerm();

    Int1   = new CppExpr(CXPR_I32,1);
    Int1->setPerm();

    RefMB  = new CppExpr(CXPR_REF,CppConst::_MB);
    RefMB->setPerm();
  }
}

CppExpr *CppExpr::ListAdd(CppExpr **pRet,CppExpr *xpr)
{
  CppExpr *cur = *pRet;
  
  if (cur) {
    CppExpr *lst = new CppExpr(CXPR_LIST,cur,xpr);
    *pRet = lst;
  } else {
    *pRet = xpr;
  }

  return xpr;
}


CppExpr *CppExpr::sysc2pc()
{
  CppExpr *tmp;
  poolRef  Ref;

  switch (opr) {
  case CXPR_OBJ: 
    switch((tmp = x.hs.r)->opr) {
    case CXPR_CALL: 
      if (tmp->x.hs.l->isRef(CppConst::pos)) {
        opr = CXPR_POSEDGE;
      }
    }
  }

  return this;
}
void CppExpr::MarkBind()
{
  switch (opr) {
  case CXPR_ASSIGN: opr = CXPR_BND_SIG; break;
  case CXPR_COMMA:  x.hs.l->MarkBind();
                    x.hs.r->MarkBind();
                    break;  
  default:          assert(0);
  }
}

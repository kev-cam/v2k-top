/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: $Id: param.inc,v 1.4 2010/04/21 01:43:12 dkc Exp $ */
  

        a_t = ATTR_VOID;
        xpr = 0;
        do {
	  T = getRef(T,TL,&Ref2);
	  if (VERILOG_POOL == Ref2.pool) switch (Ref2.index) {
          case VER_PARAMETER: goto new_param;
	  case VER_REAL:      a_t = ATTR_DOUBLE; T = getRef(T,TL,&Ref2); break;
	  case VER_TIME:      a_t = ATTR_U64;    T = getRef(T,TL,&Ref2); break;
	  case VER_INTEGER:   a_t = ATTR_INT;    T = getRef(T,TL,&Ref2); break;
	  } else if (PUNCTUATION_POOL == Ref2.pool) {
            switch (Ref2.index) {
	    case PUNC_OPN_SQ: a_t = ATTR_ULOGIC;
                              T   = getExpr(T,TL,SC_RANGE);
                              xpr = (cmplExpr() && evalExpr()) ? saveExpr()
                                                               : 0;
                              T   = skipToken(T,TL,TOK_PUNC_CLS_SQ);
	                      T   = getRef(T,TL,&Ref2);
                              break;
            }
	  } else if (ATTR_LOGIC == (a_t|ATTR_SIGNED) && xpr) {
            xpr = xpr->alias();
          }
          if (SAME_REF(Ref2,TOK_VER_SIGNED.pi)) {
            a_t = ATTR_TYPE(a_t|ATTR_SIGNED);
            T   = getRef(T,TL,&Ref2);
          }
	  checkName(Ref2);
	  T = skipToken(T,TL,TOK_OP_ASSIGN);
	  T = getExpr(T,TL,SC_PARAMETER);
#ifdef BPORTS
          if (!po) {
            StmtBlock *bp = (StmtBlock *)this;
            if ((cntxt & SC_BLOCK) &&
                ((VER_PARAMETER == Ref1.index &&
                  (STMT_BLOCK   == bp->typ2 || STMT_SPECIFY == bp->typ2)
                                              && !NULL_REF(bp->name)) ||
                 (VER_SPECPARAM == Ref1.index &&
                   STMT_SPECIFY == bp->typ2))) {
              if (VER_PARAMETER == Ref1.index && STMT_SPECIFY == bp->typ2) {
                reportError(S_WARNING(STS_SYNTAX),"%s disallowed here.",strDeref(Ref1));
              }
              po = ((StmtDecl *)this)->addLcls();
            } else {
              reportError(S_ERROR(STS_SYNTAX),"%s disallowed here.",strDeref(Ref1));
            }
          }
#endif
	  T = getRef(T,TL,&Ref1);
          atr = po->getParm(Ref2,a_t,Ref1,xpr);
          if (ATTR_LOGIC == (a_t|ATTR_SIGNED)) {
            /* no range */
          } else while(VERILOG_POOL == Ref1.pool &&
                  (VER_FROM    == Ref1.index || VER_EXCLUDE == Ref1.index))
          {
            Range r;
	    if (atr) rng = atr->addRange(new Range);
            else     rng = &r;
	    rng->exclude = VER_EXCLUDE == Ref1.index;
	    T            = getRef(TS = T,TL,&Ref1);
	    if (PUNCTUATION_POOL == Ref1.pool &&
                  (PUNC_OPN_BR == Ref1.index || PUNC_OPN_SQ == Ref1.index))
            {
	      const Token *TE = endExpr(TS = T,TL);
	      rng->inc_strt = PUNC_OPN_SQ == Ref1.index;
	      while (--TE > TS) {
		if (PUNCTUATION_POOL == TE->pi.pool &&
		    (PUNC_CLS_BR == TE->pi.index ||
		     PUNC_CLS_SQ == TE->pi.index)) break;
	      }
	      rng->inc_fnsh = PUNC_CLS_SQ == TE->pi.index;
	      getExpr(TS,TE++,STMT_CNTX(SC_COMPLETE|SC_RANGE|SC_PARAMETER));
	      T = getRef(TE,TL,&Ref1);
	    } else {
	      rng->inc_strt = rng->inc_fnsh = 1;
	      T = getExpr(TS,TL,STMT_CNTX(SC_RANGE|SC_PARAMETER));
	      T = getRef(T,TL,&Ref1);
	    }
	    if (cmplExpr() && evalExpr() && atr) {
	      rng->range = saveExpr();
	    }
	  }
	  T = po->addExprAttr(T,atr);
	  if (SAME_REF_PI(Ref1,VERILOG_POOL,VER_OPN_ATTR)) {
            goto get_attr;
	  } else if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_OPN_BR)) {
	    T = getRef(T,TL,&Ref1);
	    if (SAME_REF_PI(Ref1,OPERATORS_POOL,OP_MULTIPLY)) {
	     get_attr:
	      T = po->SaveAttr(T,TL,1);
	      po->addPendAttr(atr);
	      T = getRef(T,TL,&Ref1);
            }
          }
        } while (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_COMMA));

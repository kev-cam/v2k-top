/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: $Id: dump_expr.inc,v 1.21 2009/05/22 21:24:16 dkc Exp $ */
  

#ifndef prtty_prnt
#define prtty_prnt(s)
#endif

  int          i,
               u   = 0;
  const char  *pad = "",
              *opn;
  poolRef      nm;
  ContextObj  *po;
  Scope       *scp;

retry:
  if (expr) switch (expr->vt()) {
  case VT_ALIAS:   expr = expr->unalias();
                   goto retry;
  case VT_U64:
  case VT_I64:
  case VT_INT:
  case VT_UINT:
  case VT_LUINT:
  case VT_LOGIC:
  case VT_DOUBLE:
  case VT_STRING:   dump(expr->vf(), STRM_ARG
                         expr->vt(),expr->lhs(),expr->rhs()
                         XTRA_ARG);
                    break;
  case VT_OPERATOR: opn = strDump(expr->opr()->name);
                    switch(expr->opr()->self) {
                    case OPR_AT:       Strm()->printf("@ ");
                                       opn = " ";
                                       pad = " ";         break;
                    case OPR_ANYEDGE:  opn = " /*chng*/";
                                       pad = " ";         break;
                    case OPR_REPEAT:   Strm()->printf("repeat (");
                                       opn = ")";
                                       pad = " ";         break;
                    case OPR_DELAY:    Strm()->printf("#(");
                                       opn = ")";
                                       pad = " ";         break;
                    case OPR_BIND:     opn = "(";
                                       Strm()->printf("."); break;
                    case OPR_PLI_CALL: Strm()->printf("$"); break;
                    default:           pad = " ";         break;
                    case OPR_FLOW:     opn = "flow(";      goto center;
                    case OPR_POTENTIAL:opn = "potential("; goto center;
                    case OPR_LIST1:    prtty_prnt("\t");
                    case OPR_SYS_CALL:
                    case OPR_ACCESS:
                    case OPR_INDEX:
                    case OPR_UN_INST:
                    case OPR_INST:
                    case OPR_CALL:
                    case OPR_DT_CALL:  break;
                    }
                    if (expr->lhs()) DUMP(expr->lhs(),strm);
                  center:
                    Strm()->printf("%s%s%s",pad,opn,pad);
                    if (expr->rhs()) DUMP(expr->rhs(),strm);
                    switch(expr->opr()->self) {
                    case OPR_SUB:
                    case OPR_BIND:
                    case OPR_UN_INST:
                    case OPR_INST:
                    case OPR_CALL:
                    case OPR_FLOW:
                    case OPR_ACCESS:
                    case OPR_POTENTIAL:
                    case OPR_DT_CALL:
                    case OPR_SYS_CALL:
                    case OPR_PLI_CALL: Strm()->printf(")"); break;
                    case OPR_ARRAY:
                    case OPR_INDEX:    Strm()->printf("]"); break;
                    case OPR_NCAT:
                    case OPR_CAT:      Strm()->printf("}"); break;
                    default:;
                    }
                    break;
  case VT_REFG:     i = expr->rrf().index;
                    if (i < 0) {
                      REF_FROM_INT(nm,-i);
                      switch (nm.pool) {
                      case TASKS_POOL:
                      case SYSFUNC_POOL: goto print_dllr_nm;
                      }
                      goto print_nm;
                    }
                    switch (expr->rtyp) {
                    case REF_MODULE:
                      nm       = MAPS module_map(i)->name;
                      goto print_nm;
                    case REF_ACCESS:
                      nm.pool  = VERILOG_POOL;
                      nm.index = VER_ACCESS;
                      nm       = MAPS nature_map(i)->attrVal(nm);
                      goto print_nm;
                    default:;
                    }
                    goto def;
  case VT_REFL:     switch (expr->rtyp) {
                    case REF_SDF_PORT:
                       ROOT->dumpPortRef(Strm(),expr->rrf().scp,
                                                expr->rrf().index);
                       goto done;
                    default:
                      if ((i = expr->rrf().scp) >= 0)
                      {
                        Scope *ps   = ROOT->scope_map(i);
                        Stmt  *stmt = ps->stmt;
                        po = 0;
                        if (stmt) switch (stmt->stmt_typ()) {
                        case STMT_FUNC_A:
                        case STMT_FUNC:    po = funcObj(stmt);
                                           break;
                        case STMT_TASK:    po = taskObj(stmt);
                                           break;
                        case STMT_SPECIFY:
                        case STMT_BLOCK:
                        case STMT_FORK:    po = ((StmtBlock *)stmt)->lcls; break;
                        case STMT_GEN:     if (REF_COUNTER == expr->rtyp) {
                                             nm = ((StmtGen *)stmt)->cntr;
#ifdef FIND_GEN
                                             genVal *fnd = FIND_GEN(nm);
                                             if (fnd) {
                                               Strm()->printf("%d",
                                                        fnd->genvar.pval()->i);
                                               goto done;
                                             }
#endif
                                             goto print_nm;
		                           }
                        default:;
                        }
                        break;
                      }
                      /* drop thru */
                    case REF_UNKNOWN:
                    case REF_SCOPE:
                      po = ROOT;
                    }
                    assert(po);
                    switch (expr->rtyp) {
                    case REF_UNKNOWN:
                      nm = po->unknown_map(expr->rrf().index)->ref;
                      goto print_nm;
                    case REF_PORT:
                      nm = po->port_map(expr->rrf().index)->name;
                      goto print_nm;
                    case REF_BRANCH:
                      nm = po->branch_map(expr->rrf().index)->name[0];
                      goto print_nm;
                    case REF_PARM:
                      nm = po->parm_map(expr->rrf().index)->name();
                      goto print_nm;
                    case REF_TASK:
                      po = taskObj(
                               (scp = po->scope_map(expr->rrf().index))->stmt);
                      goto po_name;
                    case REF_FUNC:
                      po = funcObj(
                               (scp = po->scope_map(expr->rrf().index))->stmt);
                      goto po_name;
                    case REF_SCOPE:
                      nm = (scp = po->scope_map(expr->rrf().index))->name;
                      goto print_nm;
                    po_name:
                    case REF_RETURN:
                      nm = po->name;
                      goto print_nm;
                    default:;
                    }
                    goto def;
  case VT_VOID:     if (VTX_REF & expr->xtra) {
                      Strm()->printf("%s",strDump(expr->val()->ref));
                      break;
                    }
                    if (VTX_HIER & expr->xtra) {
                      const char *punc = "";
                      u = (intptr_t)expr->rhs();
                      for (i = 0; i < u; i++,punc = ".") {
                        Strm()->printf("%s%s",punc,strDump(expr->val()->hrf[i]));
                      }
                      break;
                    }
  def:
  default:          assert("Possible bad value type" == 0);
  } else {
    Strm()->printf("/* Null */");
  }
done:
  return;
print_dllr_nm:
  Strm()->printf("$");
print_nm:
  Strm()->printf("%s",strDump(nm));


#ifdef KEEP_DEFS
# undef KEEP_DEFS
#else
# undef ROOT
# undef MAPS
# undef DUMP
# undef VFN_EXPR
# undef STRM_ARG
# undef Strm
#endif

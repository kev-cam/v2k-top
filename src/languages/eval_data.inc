/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: $Id: eval_data.inc,v 1.10 2012/10/16 22:38:45 cvs Exp $ */
  

extern int fixSysRef(InfoRef *);

Expr *EVAL_CLASS::evalData(InfoRef *pRef,int len)
{
  Expr *ret = 0;

  if (len < 1) {
    reportError(S_ERROR(STS_SYNTAX),"Data item missing");
    return 0;
  }

  switch (pRef[0].pool) {
  case QUOTES_POOL:
    switch (pRef[0].index) {
    case QUT_SINGLE:
      if (2 == len) {
        ret = evalNumber(pRef,len,1);
        goto done;
      }
      break;
    case QUT_DOUBLE:
      if (len == 3 && SAME_REF(pRef[0],pRef[2])) {
        ret = new Expr(pRef[1]);
        goto done;
      }
      break;
    }
    { String str; char spc[2] = {' ',0};
      reportError(S_ERROR(STS_SYNTAX),"Confused @ %s",strDerefArr(pRef,len,&str,spc));
    }
    break;
  case CHAR_POOL:
    if (CHR_DOLLAR == pRef[0].index && len == 2) {
      int usr = 0,
          tsk = 1;
      fixSysRef(&pRef[1]);
      if (TASKS_POOL == pRef[1].pool ||
          SAME_REF_PI(pRef[1],VERILOG_POOL,VER_TIME) ||
          (tsk = 0, SYSFUNC_POOL ==  pRef[1].pool) ||
          (usr = checkName(pRef[1]))) {
        ret       = new Expr(pRef[1],(XData)(XD_FLAG|XD_DLLR));
        ret->rtyp = usr ? REF_PLI_FUNC
                        : tsk ? REF_SYS_FUNC
                              : REF_SYS_TASK;
        goto done;
      }
    }
  default:
    ret = evalNumber(pRef,len,0);
  }
done:
  return ret;
}

#undef EVAL_CLASS

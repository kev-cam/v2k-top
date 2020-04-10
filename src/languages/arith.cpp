/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * arith_cpp_rcsid() {return "$Id: arith.cpp,v 1.24 2007/07/10 23:31:40 dkc Exp $";}
 


#define VERILOG3

#include "system.h"
#include "error.h"
#include "fenum.h"
#include "assertions.h"
#include "strdb.h"
#define NEED_MATH
#define NEED_CHAR
#define NEED_QUOTES
#define NEED_VERILOG
#include "tokpool.h"
#include "verilog.h"

bool Expr::CheckInUse;
int  BaseExpr::zero;

CllType  BaseExpr::rtns[] = {
#define  ROUTINE(nm,argc,args)
#include "routines.inc"
  CT_NONE
};

ePrcsn BaseExpr::normalize(BaseExpr *a,BaseExpr *b)
{
  ePrcsn max = Precision[typ],
         tst;

  if ((tst = Precision[a->typ]) > max) max = tst;
  if ((tst = Precision[b->typ]) > max) max = tst;

  return max;
}

ePrcsn BaseExpr::normalize(BaseExpr *a)
{
  ePrcsn max = Precision[typ],
         tst;

  if ((tst = Precision[a->typ]) > max) max = tst;

  return max;
}

void fixTypDesc(const typDesc *l,const typDesc *r,typDesc *ret)
{
  if (VT_DOUBLE == l->vt || VT_DOUBLE == r->vt) {
    ret->vt   = VT_DOUBLE;
    ret->size = 64;
  } else {
    if (r->vt   > l->vt)   ret->vt   = r->vt;
    else                   ret->vt   = l->vt;
    if (r->size > l->size) ret->size = r->size;
    else                   ret->size = l->size;
  }
}

#define POINTER(p)      p

#include "arith.icc"

#define EVAL_CLASS VerCntxtObj
#define PRTDOBJ_ONLY
#include "eval_data-v.inc"
#undef  PRTDOBJ_ONLY

#define EVAL_CLASS Stmt
#define STMT_ONLY
#include "eval_data-s.inc"
#undef  STMT_ONLY

#define EVAL_CLASS Nature
#define NATURE_ONLY
#include "eval_data-n.inc"
#undef  NATURE_ONLY

#define EVAL_CLASS Disc
#define DISC_ONLY
#include "eval_data-d.inc"
#undef  NATURE_ONLY

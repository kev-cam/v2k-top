/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: $Id: expr.inc,v 1.12 2007/02/01 06:49:08 dkc Exp $ */
  


void Expr::setXd(int xd)
{
  POINTER(left) = (Expr *)(xd | XD_FLAG);
}

XData Expr::Xd() const
{
  return XDATA(POINTER(left));
}

#ifdef EXPR_ALIAS
void Expr::addDeAlias(deAliasFn fn)
{
  deAliaser *dap = MALLOC2(deAliaser);

  dap->fn   = fn;
  dap->next = deAliasers;

  deAliasers = dap;
}

bool Expr::deAlias()
{
  deAliaser *d_alias = deAliasers;
  bool       ret     = 0;

  for (; d_alias ; d_alias = d_alias->next) {
    if ((*d_alias->fn)(this)) ret = 1;
  }

  return ret;
}
#endif

void Expr::clear()
{
  if (xtra&VTX_RM) {
    assert(!(xtra&VTX_PERM));
    if (NON_EXPR(POINTER(left))) {
      switch(XDATA(POINTER(left))) {
      default:      FREE(POINTER(right));
      case XD_HREF:;
      }
    } else {
#ifdef EXPR_DATA_ONLY
      assert(!POINTER(left) && !POINTER(right));
#else
      POINTER(left)->destroy();
      POINTER(right)->destroy();
#endif
    }
  } else {
    if (xtra & (VTX_INUSE|VTX_ALIASSED)) assert(!isInUse());
    if (NON_EXPR(POINTER(left))) {
      switch(XDATA(POINTER(left))) {
      default:      FREE(POINTER(right));
      case XD_HREF:;
      }
    } else {
#ifdef EXPR_DATA_ONLY
      assert(!POINTER(left) && !POINTER(right));
#else
      POINTER(left)->dispose();
      POINTER(right)->dispose();
#endif
    }
  }
#ifdef DEBUG
  xtra  = VL_TYP_XTRA(-1);
#ifndef EXPR_DATA_ONLY
  left  = DEAD_BEEF(Expr);
  right = DEAD_BEEF(Expr);
#endif
#endif
}

Expr::Expr() {
  bx()->set(BX_VOID);
  value.u64      = 0;
  POINTER(left)  = 0;
  POINTER(right) = 0;
}

Expr::Expr(eBX b)
{
  bx()->set(b);
  value.u64      = 0;
  POINTER(left)  = 0;
  POINTER(right) = 0;
};

Expr::Expr(eBX b,int i)
{
  bx()->set(b);
  value.i = i;
  value.u64      = 0;
  POINTER(left)  = 0;
  POINTER(right) = 0;
};

Expr::Expr(VlTypXtra x)
{
  bx()->set(BX_VOID);
  bx()->set_vtx(x);
  POINTER(left)   = 0;
  POINTER(right)  = 0;
};

Expr::Expr(poolRef ref) {
  bx()->set(BX_REFERENCE);
  this->value.ref = ref;
  POINTER(left)   = 0;
  POINTER(right)  = 0;
}

Expr::Expr(poolRef ref,XData xd) {
  bx()->set(BX_REFERENCE);
  this->value.ref = ref;
  POINTER(left)   = (Expr *)xd;
  POINTER(right)  = 0;
}

void Expr::copy(Expr *xpr) {
  *this = *xpr;
#ifndef EXPR_DATA_ONLY
  if (!NON_EXPR(left)) {
    if (POINTER(left))  left->xtra  = VL_TYP_XTRA(left->xtra|VTX_INUSE);
    if (POINTER(right)) right->xtra = VL_TYP_XTRA(right->xtra|VTX_INUSE);
  }
#endif
}

void Expr::copyUnique(Expr *xpr) {
  *this = *xpr;
#ifndef EXPR_DATA_ONLY
  if (!NON_EXPR(left)) {
    assert(("NIY",0));
  }
#endif
}

Expr::Expr(OPR op, Expr *lhs, Expr *rhs) {
  bx()->set(BX_OPERATOR);
  value.op       = op;
  POINTER(left)  = lhs;
  POINTER(right) = rhs;
}

Expr::Expr(BaseExpr *xpr, Expr *lhs, Expr *rhs) {
  *bx()          = *xpr;
  POINTER(left)  = lhs;
  POINTER(right) = rhs;
}

Expr::Expr(Expr *xpr, Expr *lhs, Expr *rhs, VlTypXtra x) {
  *this          = *xpr;
  POINTER(left)  = lhs;
  POINTER(right) = rhs;
  xtra           = VL_TYP_XTRA(xtra|x);
}

Expr::Expr(int i32) {
  bx()->set(BX_INTEGER);
  value.i        = i32;
  xtra           = VL_TYP_XTRA(xtra|VTX_CONST);
  POINTER(left)  = 0;
  POINTER(right) = 0;
}

Expr::Expr(unsigned int i32) {
  bx()->set(BX_UNSGNDINT);
  value.i        = i32;
  xtra           = VL_TYP_XTRA(xtra|VTX_CONST);
  POINTER(left)  = 0;
  POINTER(right) = 0;
}

Expr::Expr(szdInt i32) {
  bx()->set(BX_LOGIC_INT);
  value.lgc      = i32;
  xtra           = VL_TYP_XTRA(xtra|VTX_CONST);
  POINTER(left)  = 0;
  POINTER(right) = 0;
}

Expr::Expr(U64 ll) {
  bx()->set(BX_UNSGND64);
  value.u64      = ll;
  xtra           = VL_TYP_XTRA(xtra|VTX_CONST);
  POINTER(left)  = 0;
  POINTER(right) = 0;
}

Expr::Expr(U64 ll,XData xd,void *xtnd) {
  bx()->set(BX_LOGIC);
  value.u64      = ll;
  xtra           = VL_TYP_XTRA(xtra|VTX_CONST);
  POINTER(left)  = (Expr *)xd;
  POINTER(right) = (Expr *)xtnd;
}

Expr::Expr(poolRef *pRef,XData xd,int n) {
  bx()->set(BX_HIER_REF);
  value.hrf      = pRef;
  POINTER(left)  = (Expr *)xd;
  POINTER(right) = (Expr *)n;
}

Expr::Expr(double d) {
  bx()->set(BX_DOUBLE);
  value.d        = d;
  xtra           = VL_TYP_XTRA(xtra|VTX_CONST);
  POINTER(left)  = 0;
  POINTER(right) = 0;
}


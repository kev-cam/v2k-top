/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: $Id: arith_2.inc,v 1.6 2007/02/01 06:47:29 dkc Exp $ */
  


INLINE_ARITH bool Eval_LT(_Var<__V1__,__B1__,__W1__> l,_Var<__V2__,__B2__,__W2__> r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;
  
  return l.value[0] < r.value[0];
}

INLINE_ARITH bool Eval_LT(_Var<__V1__,__B1__,__W1__> l,__V2__ r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;
  
  return l.value[0] < r;
}

INLINE_ARITH bool Eval_LT(__V1__ l,_Var<__V2__,__B2__,__W2__> r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;
  
  return l < r.value[0];
}

INLINE_ARITH bool Eval_LE(_Var<__V1__,__B1__,__W1__> l,_Var<__V2__,__B2__,__W2__> r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;
  
  return l.value[0] <= r.value[0];
}

INLINE_ARITH bool Eval_LE(_Var<__V1__,__B1__,__W1__> l,__V2__ r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;
  
  return l.value[0] <= r;
}

INLINE_ARITH bool Eval_GT(_Var<__V1__,__B1__,__W1__> l,_Var<__V2__,__B2__,__W2__> r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;
  
  return l.value[0] > r.value[0];
}

INLINE_ARITH bool Eval_GE(_Var<__V1__,__B1__,__W1__> l,_Var<__V2__,__B2__,__W2__> r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;
  
  return l.value[0] >= r.value[0];
}

INLINE_ARITH _Var<__VX__,__BX__,__WX__> Eval_PLUS(_Var<__V1__,__B1__,__W1__> l,_Var<__V2__,__B2__,__W2__> r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;
  _Var<__VX__,__BX__,__WX__> res;

  res.value[0] = l.value[0] + r.value[0];

  return res;
}

INLINE_ARITH _Var<__VX__,__BX__,__WX__> Eval_PLUS(_Var<__V1__,__B1__,__W1__> l,__V2__ r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;
  _Var<__VX__,__BX__,__WX__> res;

  res.value[0] = l.value[0] + r;

  return res;
}

INLINE_ARITH _Var<__VX__,__BX__,__WX__> Eval_MINUS(_Var<__V1__,__B1__,__W1__> l,_Var<__V2__,__B2__,__W2__> r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;
  _Var<__VX__,__BX__,__WX__> res;

  res.value[0] = l.value[0] - r.value[0];

  return res;
}

INLINE_ARITH _Var<__VX__,__BX__,__WX__> Eval_MULTIPLY(_Var<__V1__,__B1__,__W1__> l,_Var<__V2__,__B2__,__W2__> r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;

  _Var<__VX__,__BX__,__WX__> res;

  res.value[0] = l.value[0] * r.value[0];

  return res;
}

INLINE_ARITH _Var<__VX__,__BX__,__WX__> Eval_DIVIDE(_Var<__V1__,__B1__,__W1__> l,_Var<__V2__,__B2__,__W2__> r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;

  _Var<__VX__,__BX__,__WX__> res;

  res.value[0] = l.value[0] / r.value[0];

  return res;
}

INLINE_ARITH _Var<__VX__,__BX__,__WX__> Eval_B_OR(_Var<__V1__,__B1__,__W1__> l,_Var<__V2__,__B2__,__W2__> r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;

  _Var<__VX__,__BX__,__WX__> res;

  res.value[0] = l.value[0] | r.value[0];

  return res;
}

INLINE_ARITH _Var<__VX__,__BX__,__WX__> Eval_B_AND(_Var<__V1__,__B1__,__W1__> l,_Var<__V2__,__B2__,__W2__> r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;

  _Var<__VX__,__BX__,__WX__> res;

  res.value[0] = l.value[0] & r.value[0];

  return res;
}

INLINE_ARITH _Var<__VX__,__BX__,__WX__> Eval_B_EQ1(_Var<__V1__,__B1__,__W1__> l,_Var<__V2__,__B2__,__W2__> r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;

  _Var<__VX__,__BX__,__WX__> res;

  res.value[0] = l.value[0] == r.value[0];

  return res;
}

ALWAYS_INLINE __VX__ Eval_B_EQ1(__V1__ l,__V2__ r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;

  __VX__ res;

  res = l == r;

  return res;
}

INLINE_ARITH void Eval_CAT(_Var<__V1__,__B1__,__W1__> *l,int *p,int w2,_Var<__V2__,__B2__,__W2__> r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;

  assert(0);
}

INLINE_ARITH void Eval_CAT(_Var<__V1__,__B1__,__W1__> *l,int *p,int w2,__V2__ r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;

  assert(0);
}

INLINE_ARITH bool Eval_CS_EQ(_Var<__V1__,__B1__,__W1__> l,_Var<__V2__,__B2__,__W2__> r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;

  _Var<__VX__,__BX__,__WX__> res;

  assert(0);

  return res;
}

ALWAYS_INLINE bool Eval_CS_EQ(__V1__ l,__V2__ r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;

  __VX__ res;

  res = l == r;

  return res;
}

INLINE_ARITH bool Eval_L_NE(_Var<__V1__,__B1__,__W1__> l,_Var<__V2__,__B2__,__W2__> r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;

  if (l.value[0] != r.value[0]) return true;

  return false;
}

INLINE_ARITH bool Eval_L_NE(__V1__ l,_Var<__V2__,__B2__,__W2__> r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;

  if (l != r.value[0]) return true;

  return false;
}

INLINE_ARITH bool Eval_L_NE(_Var<__V1__,__B1__,__W1__> l,__V2__ r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;

  if (l.value[0] != r) return true;

  return false;
}

ALWAYS_INLINE bool Eval_L_NE(__V1__ l,__V2__ r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;

  if (l != r) return true;

  return false;
}

INLINE_ARITH bool Eval_CS_NE(_Var<__V1__,__B1__,__W1__> l,_Var<__V2__,__B2__,__W2__> r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;

  if (l.value[0] != r.value[0]) return true;

  return false;
}

INLINE_ARITH bool Eval_CS_NE(_Var<__V1__,__B1__,__W1__> l,__V2__ r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;

  if (l.value[0] != r) return true;

  return false;
}

ALWAYS_INLINE bool Eval_CS_NE(__V1__ l,__V2__ r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;

  return l != r;
}

INLINE_ARITH _Var<__VX__,__BX__,__WX__> Eval_B_EQ2(_Var<__V1__,__B1__,__W1__> l,_Var<__V2__,__B2__,__W2__> r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;

  _Var<__VX__,__BX__,__WX__> res;

  res.value[0] = l.value[0] == r.value[0];

  return res;
}

ALWAYS_INLINE __VX__ Eval_B_EQ2(__V1__ l,__V2__ r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;

  __VX__ res;

  res = l == r;

  return res;
}

INLINE_ARITH _Var<__VX__,__BX__,__WX__> Eval_LFT_SH(_Var<__V1__,__B1__,__W1__> l,_Var<__V2__,__B2__,__W2__> r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;
  _Var<__VX__,__BX__,__WX__> res;

  res.value[0] = l.value[0] << r.value[0];

  return res;
}


INLINE_ARITH _Var<__VX__,__BX__,__WX__> Eval_LFT_SH(_Var<__V1__,__B1__,__W1__> l,__V2__ r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;
  _Var<__VX__,__BX__,__WX__> res;

  res.value[0] = l.value[0] << r;

  return res;
}


INLINE_ARITH __VX__ Eval_LFT_SH(__V1__ l,__V2__ r)
{
  int sgnd1 = __S1__,
      sgnd2 = __S2__;
  __VX__ res;

  res = l << r;

  return res;
}


/* RESET */
#undef __S1__
#undef __V1__
#undef __B1__
#undef __W1__
#undef __D1__
#undef __S2__
#undef __V2__
#undef __B2__
#undef __W2__
#undef __D2__
#undef __SX__
#undef __VX__
#undef __BX__
#undef __WX__
#undef __DX__

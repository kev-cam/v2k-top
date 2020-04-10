/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: $Id: arith_1.inc,v 1.8 2009/07/08 08:35:18 dkc Exp $ */
  
template <>
INLINE_ARITH void _Port<__V__,__B__,__W__,__D__>::assign(const _Var<__V__,__B__,__W__> &i)
{
  _Var<__V__,__B__,__W__> *val = VP();

  if (i.value[0] != val->value[0]) {
    edge  = val->value[0] < i.value[0] ?  1
                                         : -1;
    val->value[0] = i.value[0];
    v2kSimKrnl.Eval(index,0,(void *)VP());
    edge      = 0;
  }
}

template <>
INLINE_ARITH void _Port<__V__,__B__,__W__,__D__>::assign(__V__ i)
{
  _Var<__V__,__B__,__W__> *val = VP();

  if (i != val->value[0]) {
    edge      = val->value[0] < i ?  1
                                    : -1;
    val->value[0] = i;
    v2kSimKrnl.Eval(index,0,(void *)VP());
    edge      = 0;
  }
}

template <>
INLINE_ARITH _SimTmp<__V__,__B__,__W__> *_SimTmp<__V__,__B__,__W__>::allocate(unsigned int size)
{
  return (_SimTmp<__V__,__B__,__W__> *)v2kSimKrnl.Allocate(size);
}

INLINE_ARITH _Var<__V__,__B__,__W__> Eval_UMINUS(_Var<__V__,__B__,__W__> oprnd)
{
  _Var<__V__,__B__,__W__> res = oprnd;

  res.value[0] = -oprnd.value[0];
  return res;
}

INLINE_ARITH _Var<__V__,__B__,__W__> Eval_B_NEG(_Var<__V__,__B__,__W__> oprnd)
{
  _Var<__V__,__B__,__W__> res = oprnd;

  res.value[0] = -oprnd.value[0];
  return res;
}

ALWAYS_INLINE int _Bool(_Var<__V__,__B__,__W__> &oprnd)
{
  return oprnd.value[0];
}

ALWAYS_INLINE int _Bool(_Port<__V__,__B__,__W__,1> &oprnd)
{
  return oprnd.value[0];
}

/* RESET */
#undef __V__
#undef __B__
#undef __W__
#undef __D__

/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  expr_il_h_rcsid
#define expr_il_h_rcsid() {return "$Id: expr_il.h,v 1.9 2007/03/28 07:23:26 dkc Exp $";} /* RCS ID */
 
inline double EXPR_IL_CLASS dbl()  const {
  switch(typ)
  { default:          return 1/zero;
    case VT_OPERATOR: return eval_dbl();
    case VT_U64:      return value.u64;
    case VT_I64:      return value.i64;
    case VT_LUINT:    return value.lgc.i;
    case VT_SIZE:     return value.td.size;
    case VT_BOOL:
    case VT_UINT:     return value.u;
    case VT_INT:      return value.i;
    case VT_DOUBLE:   return value.d;
    case VT_ALIAS:    return unalias()->dbl();}
};

inline I64 EXPR_IL_CLASS i64()  const {
  switch(typ)
  { default:          return 1/zero;
    case VT_OPERATOR: return eval_I64();
    case VT_U64:      return value.u64;
    case VT_I64:      return value.i64;
    case VT_LUINT:    return value.lgc.i;
    case VT_SIZE:     return value.td.size;
    case VT_BOOL: 
    case VT_UINT:     return value.u;
    case VT_INT:      return value.i;
    case VT_DOUBLE:   return dbl2I64();
    case VT_ALIAS:    return unalias()->i64();}
};

#undef EXPR_IL_CLASS


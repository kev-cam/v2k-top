/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: $Id: op_eval.inc,v 1.10 2007/02/01 06:49:08 dkc Exp $ */
  


#ifndef EVAL_RET
#define EVAL_RET tx
#endif
#ifndef OPR_EVAL_RET
#define OPR_EVAL_RET OPR_SET_DATA
#endif
#ifndef EVAL_LHS
#define EVAL_LHS l
#endif
#ifndef OPR_EVAL_LHS
#define OPR_EVAL_LHS OPR_EVAL_DATA
#endif
#ifndef EVAL_RHS
#define EVAL_RHS r
#endif
#ifndef OPR_EVAL_RHS
#define OPR_EVAL_RHS OPR_EVAL_DATA
#endif

// OPR_EVAL(c,        s,       ret,data,            op0, lhs,data1,            op, rhs,data2,         op_type,cls)

#ifdef SLICE_OPS

OPR_EVAL(OPR_INDEX,   Index,   EVAL_RET,.rngSelect,   OPN_BRKT, EVAL_LHS, .pl(), COMMA,\
                                                                EVAL_RHS, .pl(), OT_RANGE, CLS_BRKT)
#undef SLICE_OPS
#endif // SLICE_OPS

#ifdef BI_OPS

OPR_EVAL(OPR_PLUS,    Plus,    EVAL_RET,OPR_EVAL_RET, =, EVAL_LHS,OPR_EVAL_LHS, +, EVAL_RHS,OPR_EVAL_RHS,OT_SIMPLE,/**/)
OPR_EVAL(OPR_MINUS,   Minus,   EVAL_RET,OPR_EVAL_RET, =, EVAL_LHS,OPR_EVAL_LHS, -, EVAL_RHS,OPR_EVAL_RHS,OT_SIMPLE,/**/)
OPR_EVAL(OPR_DIVIDE,  Divide,  EVAL_RET,OPR_EVAL_RET, =, EVAL_LHS,OPR_EVAL_LHS, /, EVAL_LHS,OPR_EVAL_RHS,OT_SIMPLE,/**/)
OPR_EVAL(OPR_MULTIPLY,Multiply,EVAL_RET,OPR_EVAL_RET, =, EVAL_LHS,OPR_EVAL_LHS, *, EVAL_LHS,OPR_EVAL_RHS,OT_SIMPLE,/**/)

OPR_EVAL(OPR_GT,      GT,      EVAL_RET,OPR_EVAL_RET, =, EVAL_LHS,OPR_EVAL_LHS, >, EVAL_LHS,OPR_EVAL_RHS,OT_SIMPLE,/**/)
OPR_EVAL(OPR_LT,      LT,      EVAL_RET,OPR_EVAL_RET, =, EVAL_LHS,OPR_EVAL_LHS, <, EVAL_LHS,OPR_EVAL_RHS,OT_SIMPLE,/**/)
OPR_EVAL(OPR_GE,      GE,      EVAL_RET,OPR_EVAL_RET, =, EVAL_LHS,OPR_EVAL_LHS, >=,EVAL_LHS,OPR_EVAL_RHS,OT_SIMPLE,/**/)
OPR_EVAL(OPR_LE,      LE,      EVAL_RET,OPR_EVAL_RET, =, EVAL_LHS,OPR_EVAL_LHS, <=,EVAL_RHS,OPR_EVAL_RHS,OT_SIMPLE,/**/)
OPR_EVAL(OPR_L_EQ,    L_EQ,    EVAL_RET,OPR_EVAL_RET, =, EVAL_LHS,OPR_EVAL_LHS, ==,EVAL_RHS,OPR_EVAL_RHS,OT_SIMPLE,/**/)

#ifdef OPR_INDEX_OK
OPR_EVAL(OPR_INDEX,   Index,   EVAL_RET,OPR_EVAL_RET, =, EVAL_LHS,OPR_EVAL_LHS, [, EVAL_RHS,OPR_EVAL_RHS,OT_INDEX, ])
#else
OPR_EVAL(OPR_INDEX,   Index,   EVAL_RET,.bitSelect,   OPN_BRKT, EVAL_LHS, .pl(), COMMA,\
                                                                EVAL_RHS, .pl(), OT_INDEX, CLS_BRKT)
#endif

OPR_EVAL(OPR_NCAT,    NCat,    EVAL_RET,.nCat,        OPN_BRKT, EVAL_LHS, .pl(), COMMA,\
                                                                EVAL_RHS, .pl(), OT_UNSET, CLS_BRKT)

#ifdef OPR_BIT_OK
OPR_EVAL(OPR_LFT_SH,  LSH,     EVAL_RET,OPR_EVAL_RET, =, EVAL_LHS,OPR_EVAL_LHS, <<,EVAL_RHS,OPR_EVAL_RHS,OT_LEFT,  /**/)
OPR_EVAL(OPR_RHT_SH,  RSH,     EVAL_RET,OPR_EVAL_RET, =, EVAL_LHS,OPR_EVAL_LHS, >>,EVAL_RHS,OPR_EVAL_RHS,OT_LEFT,  /**/)
OPR_EVAL(OPR_B_AND,   BAnd,    EVAL_RET,OPR_EVAL_RET, =, EVAL_LHS,OPR_EVAL_LHS, &, EVAL_RHS,OPR_EVAL_RHS,OT_SIMPLE,/**/)
OPR_EVAL(OPR_B_OR,    BOr,     EVAL_RET,OPR_EVAL_RET, =, EVAL_LHS,OPR_EVAL_LHS, |, EVAL_RHS,OPR_EVAL_RHS,OT_SIMPLE,/**/)
#endif

#undef BI_OPS
#endif // BI_OPS

#ifdef UNI_OPS

OPR_EVAL(OPR_UMINUS,Negate, EVAL_RET,OPR_EVAL_RET, =, EVAL_LHS,OPR_EVAL_LHS, -, EVAL_RHS,OPR_EVAL_RHS,OT_RIGHT,/**/)
OPR_EVAL(OPR_L_NEG, Not,    EVAL_RET,OPR_EVAL_RET, =, EVAL_LHS,OPR_EVAL_LHS, !, EVAL_RHS,OPR_EVAL_RHS,OT_RIGHT,/**/)

#ifdef OPR_BIT_OK
OPR_EVAL(OPR_B_NEG, BitNeg, EVAL_RET,OPR_EVAL_RET, =, EVAL_LHS,OPR_EVAL_LHS, ~, EVAL_RHS,OPR_EVAL_RHS,OT_RIGHT,/**/)
#endif

#undef UNI_OPS
#endif // UNI_OPS

#undef OPR_EVAL
#undef OPR_BIT_OK
#undef OPR_EVAL_LHS
#undef OPR_EVAL_DATA
#undef OPR_SET_DATA
#undef EVAL_RET
#undef OPR_EVAL_RET
#undef EVAL_LHS
#undef OPR_EVAL_LHS
#undef EVAL_RHS
#undef OPR_EVAL_RHS


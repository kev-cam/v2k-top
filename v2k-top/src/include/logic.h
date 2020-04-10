/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  logic_h_rcsid
#define logic_h_rcsid() {return "$Id: logic.h,v 1.9 2007/03/28 07:23:25 dkc Exp $";} /* RCS ID */

#define OPR_EVAL(c,s,ret,data,op0,lhs,data1,op,rhs,data2,ot,cls)\
        int logic##s(PARM_EXPR *);
#define OPR_BIT_OK
#include "../languages/uni_op_eval.inc"

#define OPR_EVAL(c,s,ret,data,op0,lhs,data1,op,rhs,data2,ot,cls)\
        int logic##s(PARM_EXPR *,PARM_EXPR *);
#define OPR_BIT_OK
#include "../languages/bi_op_eval.inc"

#define OPR_EVAL(c,s,ret,data,op0,lhs,data1,op,rhs,data2,ot,cls)\
        int slice##s(PARM_EXPR *,PARM_EXPR *);
#define OPR_BIT_OK
#include "../languages/slice_op_eval.inc"

#define OPN_BRKT (
#define CLS_BRKT )
#define COMMA    ,

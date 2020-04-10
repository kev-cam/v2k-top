/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */

#ifndef PC_OP_H
#define PC_OP_H

typedef enum {
  CPREC_NONE  = -1,
  CPREC_COMMA =  0,
  CPREC_ELLIPSIS,
  CPREC_ASSIGN,
  CPREC_QC1,
  CPREC_QC2,
  CPREC_OR,
  CPREC_AND,
  CPREC_BOR,
  CPREC_BXOR,
  CPREC_BAND,
  CPREC_DIFF,
  CPREC_CMP,
  CPREC_RANGE,
  CPREC_SHFT,
  CPREC_ADD,
  CPREC_MULT,
  CPREC_MBR,
  CPREC_MISC,
  CPREC_ACCESS,
  CPREC_SCOPE,
  CPREC_OPR,
  CPREC_CALL,
  CPREC_SUB,
  CPREC_T_INST,
  CPREC_I_INDEX,
  CPREC_MAX
} eCPPopPrec;

typedef enum {
  CASSOC_LR   = -1,
  CASSOC_NONE =  0,
  CASSOC_RL   =  1
} eCPPopAssoc;

typedef struct {
  const char *str;
  int         unary;
  eCPPopPrec  prec;
  eCPPopAssoc assoc;
} sOprInfo;

#endif

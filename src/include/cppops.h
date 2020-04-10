/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  cppops_h_rcsid
#define cppops_h_rcsid() {return "$Id: cppops.h,v 1.10 2010/05/14 22:21:48 dkc Exp $";} /* RCS ID */

#ifndef CPPOPS_H
#define CPPOPS_H

#include "pc_op.h"

typedef enum {
#define CPPOP(op,nm,...) CXPR_##nm,
#include "prcops.inc"
#include "cppops.inc"
  CXPR_TOK,
  CXPR_LIST,
  CXPR_I32,
  CXPR_U32,
  CXPR_I64,
  CXPR_U64,
  CXPR_DBL,
  CXPR_REF,
  CXPR_REF_TYP,
  CXPR_REF_DCL,
  CXPR_STRD,
  CXPR_STRS,
  CXPR_STRB,
  CXPR_STRCAT,
  CXPR_CMNT,
  CXPR_POINT,
  CXPR_POSEDGE,
  CXPR_NEGEDGE,
  CXPR_INST_SC,
  CXPR_CALL_MM,
  CXPR_Last
} eCXPR;
#define CXPR(e) ((eCXPR)(e)) 

typedef enum {
#define CPPOP(op,nm,...) COPR_##nm,
#include "prcops.inc"
#include "cppops.inc"
  COPR_Last
} eCOPR;
#define CXPR(e) ((eCXPR)(e)) 

extern sOprInfo oprInfo[];

#endif

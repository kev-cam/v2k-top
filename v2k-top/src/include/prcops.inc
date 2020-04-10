// Copyright (c) 1998-2007 Kevin Cameron
// Distributed under the GNU Lesser General Public License
// RCS ID: $Id: prcops.inc,v 1.6 2010/05/13 18:39:00 dkc Exp $
//    op   name         unary    prec              assoc
CPPOP(@,   AT,            1,     CPREC_NONE,       CASSOC_NONE)
CPPOP($,   INST_INDEX,    1,     CPREC_I_INDEX,    CASSOC_NONE)
CPPOP(@?,  BEFORE,        1,     CPREC_NONE,       CASSOC_NONE)
CPPOP(:=,  BECOMES,       0,     CPREC_ASSIGN,     CASSOC_RL)
#ifndef CPPOPS_TDF
CPPOP(:,   RANGE,         0,     CPREC_RANGE,      CASSOC_NONE)
CPPOP($,   STTC_RNG,      0,     CPREC_RANGE,      CASSOC_NONE)
CPPOP(@,   SENSE,         0,     CPREC_NONE,       CASSOC_NONE)
CPPOP(@,   SC_AT,         0,     CPREC_NONE,       CASSOC_NONE)
#endif

/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  cg_h_rcsid
#define cg_h_rcsid() {return "$Id: cg_enums.h,v 1.2 2009/07/08 08:34:54 dkc Exp $";} /* RCS ID */

#ifndef CG_ENUMS_H
#define CG_ENUMS_H

typedef enum {
  CM_GNU  = 0,
  CM_ANSI = 1,
  CM_PARC = 2
} eCodeMode;

typedef enum {
  CG_FAILED   = -1,
  CG_OK       =  0,
  CG_TASK     =  2,
  CG_FUNC,
  CG_SYS_TASK,
  CG_SYS_FUNC,
  CG_PLI_TASK,
  CG_PLI_FUNC
} eCG;

typedef enum {
  CGS_NONE = 0,
  CGS_DECL,
  CGS_PROC = 1,
  CGS_TASK = 2,
  CGS_FUNC = 4,
  CGS_TF   = CGS_TASK|CGS_FUNC,
  CGS_EDGE = 8

} eCGS;

#define CGS(c) ((eCGS)(c))

typedef enum {
  CGT_CLONE,
  CGT_CC,
  CGT_CC_CUT,
  CGT_O,
  CGT_SO
} eCGT;

#endif


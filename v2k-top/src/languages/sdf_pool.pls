# Copyright (c) 1998,1999,2001,2002,2003 Kevin Cameron
# Distributed under the GNU Lesser General Public License
# RCS ID: $Id: sdf_pool.pls,v 1.11 2007/01/13 03:44:15 dkc Exp $
#


#


#


STRUCT plRtriple
  char typ[4];
  int  val[3];

STRUCT plRvalue
  eRVT typ;
  int  val;

STRUCT plRval2
  plRvalue v[2];

STRUCT plRval3
  plRvalue v[3];

STRUCT plCellInst
  I32              elems;
  IREF(poolRef)    VAR_ARRAY_PTR(elem);

STRUCT plPortPath
  ePSP             typ:8;
  char             edge;
  char             ranges;
  int              range[2];
  I32              inst;
  I32              name;
  IREF(plExpr)     cond;

STRUCT plCell
  poolRef          cll_typ;
  poolRef          corr_str;
  int              corr_fact;
  int              insts;
  int              chks;
  IREF(int)        VAR_ARRAY_PTR(inst);
  IREF(plCheck)    chk;
  IREF(plCell)     next;

STRUCT plCheck
  eCHK             typ:8;
  eCHK             inner:8;
  eDMD             mode:8;
  int              ports;
  plRvalue         val;
  IREF(int)        VAR_ARRAY_PTR(port);
  IREF(plCheck)    next;

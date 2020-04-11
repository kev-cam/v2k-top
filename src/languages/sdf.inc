/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: $Id: sdf.inc,v 1.8 2007/02/01 06:49:08 dkc Exp $ */
  
SDF_TBL(plPortPath,pp)
SDF_TBL(double,    dbl)
SDF_TBL(plRtriple, trpl)
SDF_TBL(plRval2,   val2)
SDF_TBL(plRval3,   val3)
SDF_TBL(poolRef,   port)
SDF_TBL(poolRef,   name)

#ifndef COMP_ONLY
SDF_TBL(CellInst,  path)
SDF_TBL(int,       inst)
#endif

#undef SDF_TBL
#undef COMP_ONLY

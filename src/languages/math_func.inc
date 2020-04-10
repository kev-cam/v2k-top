/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: $Id: math_func.inc,v 1.7 2007/02/01 06:49:06 dkc Exp $ */
  


MATH_FUNC(MTH_POW,  BX_DOUBLE, 2,d,pow(args[0].dbl(),args[1].dbl()),"pow")
MATH_FUNC(MTH_ISNAN,BX_INTEGER,1,i,isnan(args[0].dbl()),            "isnan")

#undef MATH_FUNC

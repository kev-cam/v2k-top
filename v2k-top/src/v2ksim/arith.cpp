/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * arith_cpp_rcsid() {return "$Id: arith.cpp,v 1.7 2012/10/16 23:16:34 cvs Exp $";}
 
#include "system.h"
#include "error.h"

#include "sim_extra.h"
#include "simulation.h"

#include "fenum.h"
#include "strfunc.h"
#include "file.h"
#include "dyn.h"
#include "sim_kern.h"

inline void v2kSim::Eval(int si,int drv_n,void *drv_v)
{
  v2kSimEval(ip,si,drv_n,drv_v);
}

#define INLINE_ARITH
#include "sim_arith.inc"

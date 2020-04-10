/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * expr_cpp_rcsid() {return "$Id: expr.cpp,v 1.9 2007/02/01 06:49:07 dkc Exp $";}
 


#define EXPR_CPP
#define VERILOG3

#include "assertions.h"
#include "tokpool.h"
#include "verilog.h"

#define POINTER(p)      p

#include "expr.inc"

#ifdef LD_BUG_01
# define inline
# define EXPR_IL_CLASS Expr::
# include "expr_il.h"
# undef  inline
#endif

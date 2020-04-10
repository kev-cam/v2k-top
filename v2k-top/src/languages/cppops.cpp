/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * cppops_cpp_rcsid() {return "$Id: cppops.cpp,v 1.2 2007/07/10 23:31:40 dkc Exp $";}

// Just to generate the .tdf

#define CPPOPS_TDF
#define CPPOP(a,b,...) a b
#include "cppops.inc"

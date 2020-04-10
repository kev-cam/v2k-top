/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * lists_cpp_rcsid() {return "$Id: lists.cpp,v 1.11 2007/02/01 06:46:49 dkc Exp $";}

#include "v2k_mem.h"
#include "list.h"
#include "assert.h"

template class sortedList<void,true>;
template class sortedList<void,false>;

/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * test_cpp_rcsid() {return "$Id: test.cpp,v 1.10 2007/02/01 06:52:42 dkc Exp $";}
 


#include "system.h"
#include "tref.h"
#include "template.h"
#include "str_pool.h"

void d2(OnDiskPool *ip)
{
  ip->describe(DescribePool);
  printf("\nClass = %s,Mode = %d (%s)\n\n",
         ip->strClass(),ip->getMode(),ip->strMode());
}

main()
{
  OnDiskPool *test = new OnDiskPool;

  d2(test);

  test->changeMode(PM_InMem);

  d2(test);
}

/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * basepool_cpp_rcsid() {return "$Id: basepool.cpp,v 1.19 2012/10/16 22:30:51 cvs Exp $";}

#include "v2k_mem.h"
#include "error.h"
#include "pool.h"
#include "env.h"
#include "file.h"

const char *strPoolMode[16] = {"InMem",
                               "Contiguous",
                               "OnDisk",
                               "Virtual",
                               "Mapped",
                               "Shared",
                               "Locked",
                               "Nailed"};

BasePool:: BasePool()                                       {}
BasePool:: BasePool(int)                                    {}
BasePool:: BasePool(const File *f,const char *mode)         {}
BasePool:: BasePool(const File *f,const char *mode,int,int) {}
BasePool::~BasePool()                                       {}

int BasePool:: close_pool()                {this->~BasePool(); return 0;}
int BasePool:: close_pool(int id,void *pd) {this->~BasePool(); return 0;}

void ref_cast(double d,void *ret)
{
  voidRef *pref = (voidRef *)ret;

  pref->as_double = d;
}

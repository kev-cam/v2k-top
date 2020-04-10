/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * sim_kern_cpp_rcsid() {return "$Id: sim_kern.cpp,v 1.5 2012/10/16 22:38:45 cvs Exp $";}
 

#include "system.h"
#include "error.h"
#include "simulation.h"
#include "env.h"
#include "sim_kern.h"
#include "strfunc.h"
#include "dyn.h"

#ifdef STATICLIB
extern v2kSim v2kSimKrnl;
#endif

Simulator *Simulator::Kernel
#ifdef STATICLIB
                             = &v2kSimKrnl
#endif
;

extern "C" void V2KsetSimKrnl(Simulator *kern)
{
  Simulator::Kernel = kern;
}

static DynObj *SimLib;

eSTS loadSimKrnl(const char *lib)
{
  eSTS sts = STS_NORMAL;

#ifndef STATICLIB  
  String s(lib);
  envExpand(&s);

  SimLib = new DynObj(s,"v2kInit",1);

  sts = SimLib->Status();
#endif

  return sts;
}

extern "C" eSTS v2kStartSim()
{
  eSTS sts = initSim();

  if (STS_NORMAL == sts) {
    sts = Simulator::Kernel ? Simulator::Kernel->Start(ErrControl.mode)
                            : STS_NREADY;
  }

  return sts;
}

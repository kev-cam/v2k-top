/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * gates_cpp_rcsid() {return "$Id: gates.cpp,v 1.13 2012/10/16 22:39:16 cvs Exp $";}

#include "system.h"
#include "error.h"
#define  VERILOG4
#include "veri_enum.h"
#include "v2k_mem.h"
#include "models.h"
#include "resolve.h"
#define  DEFINE_MODELS
#include "gates.h"
#include "strfunc.h"
#include "error.h"
#include "dyn.h"

model *Gates[GATE_LAST];

model *getGate(int id)
{
  if (!Gates[id]) switch (id) {
#define PRIM(g,l) case GATE_##g: Gates[GATE_##g] = new gate_##l; break;
#include "vprim.inc"
  }

  return Gates[id];
}

static GateLib v2kGateLib[] = {{ getGate,
                                 Gates }};

#if (defined(PERL_LIB) || !defined(DYNLOADING))
GateLib *V2KgateLib = v2kGateLib;
#else

extern "C" {

extern void V2KsetGateLib(GateLib *);

int v2kInit(DynObj *lib_handle)
{
  V2KsetGateLib(v2kGateLib);
#if DBGLVL > 0
  printf("Loaded V2K gate library\n");
#endif
  return 0;
}

int v2kUnload(void *p)
{
  V2KsetGateLib(0);
#if DBGLVL > 0
  printf("Unloaded V2K gate library\n");
#endif
  return 0;
}

}

#endif

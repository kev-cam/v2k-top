/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * recon_cpp_rcsid() {return "$Id: recon.cpp,v 1.18 2009/11/23 02:28:35 dkc Exp $";}
 


#include "system.h"
#include "tokpool.h"
#include "verilog.h"
#include "error.h"
#include "args.h"
#include "poolmngr.h"
#include "strdb.h"

FILE *vIn = stdin;

static argHndlr tstHndlrs[] = {
  {"in",argSetInput,&vIn,AT_ARG},
  {0,0,0}
};

int main(int argc,const char **argv)
{
  int sts = processArgs(&argc,&argv,tstHndlrs);

  if (!sts) 
  { 
    InitLang("cr",0);

    tokReconstruct(vIn,stdout," ");
  }

  return ERRNO(sts);
}

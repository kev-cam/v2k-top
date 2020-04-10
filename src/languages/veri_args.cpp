/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * veri_args_cpp_rcsid() {return "$Id: veri_args.cpp,v 1.19 2007/09/24 17:39:05 dkc Exp $";}

#define VERILOG3

#include "system.h"
#include "args.h"
#include "env.h"
#include "error.h"
#include "language.h"
#include "poolmngr.h"
#include "strdb.h"
#include "v2k_mem.h"
#include "v2k.h"
#define  NEED_FUNC
#define  NEED_TASKS
#define  NEED_MATH
#define  NEED_SIGNAL
#include "tokpool.h"
#include "verilog.h"
#include "dump.h"

extern "C" eSTS verSetDF(int *argc,const char ***argv,void *var)
{
  DmpFlgs     flgs = DMP_NONE;
  const char *eq   = argEquals(argc,argv);
  eSTS        sts  = STS_NORMAL;
  dumper     *dmp  = var ? (dumper *)var
                         : (dumper *)DumpList;

  if (!dmp) return STS_BAD_ARG;
  if (!eq)  return STS_MSSNG_ARG;

  if (isdigit(*eq))
  {
    if (1 != sscanf(eq,"%d",&flgs)) return STS_BAD_ARG;

  } else {
    for (;*eq;eq++) switch (*eq) {
    default:  goto strm;
      /* ON */
    case '*': flgs = DMP_ALL;                          break;
    case '~': flgs = DMP_FLGS(flgs|DMP_NO_DIGITAL);    break;
    case '=': flgs = DMP_FLGS(flgs|DMP_NO_ANALOG);     break;
    case 'o': flgs = DMP_FLGS(flgs|DMP_OBFISCATE);     break;
    case 'v': flgs = DMP_FLGS(flgs|DMP_VERBOSE);       break;
    case 'm': flgs = DMP_FLGS(flgs|DMP_MODULE);        break;
    case 'n': flgs = DMP_FLGS(flgs|DMP_NATURE);        break;
    case 'd': flgs = DMP_FLGS(flgs|DMP_DISCIPLINE);    break;
    case 'u': flgs = DMP_FLGS(flgs|DMP_USAGE);         break;
    case 'p': flgs = DMP_FLGS(flgs|DMP_PRIMITIVE);     break;
    case 's': flgs = DMP_FLGS(flgs|DMP_SDF);           break;
    case 'l': flgs = DMP_FLGS(flgs|DMP_LN_DRCTV);      break;
    case 'g': flgs = DMP_FLGS(flgs|DMP_AG_UNROLL);     break;
    case '{': flgs = DMP_FLGS(flgs|DMP_PRETTY);        break;
    case '!': flgs = DMP_FLGS(flgs|DMP_DEBUG);         break;
      /* OFF */
    case '.': flgs = DMP_NONE;                         break;
    case '_': flgs = DMP_FLGS(flgs&~DMP_NO_DIGITAL);   break;
    case '#': flgs = DMP_FLGS(flgs&~DMP_NO_ANALOG);    break;
    case 'O': flgs = DMP_FLGS(flgs&~DMP_OBFISCATE);    break;
    case 'V': flgs = DMP_FLGS(flgs&~DMP_VERBOSE);      break;
    case 'M': flgs = DMP_FLGS(flgs&~DMP_MODULE);       break;
    case 'N': flgs = DMP_FLGS(flgs&~DMP_NATURE);       break;
    case 'D': flgs = DMP_FLGS(flgs&~DMP_DISCIPLINE);   break;
    case 'U': flgs = DMP_FLGS(flgs&~DMP_USAGE);        break;
    case 'P': flgs = DMP_FLGS(flgs&~DMP_PRIMITIVE);    break;
    case 'S': flgs = DMP_FLGS(flgs&~DMP_SDF);          break;
    case 'L': flgs = DMP_FLGS(flgs&~DMP_LN_DRCTV);     break;
    case 'G': flgs = DMP_FLGS(flgs&~DMP_AG_UNROLL);    break;
    case '}': flgs = DMP_FLGS(flgs&~DMP_PRETTY);       break;
    }
    goto set;
  }

strm:
  switch (*eq++) {
    case '+': if (!dmp->Open(eq,"a")) sts = dmp->Status();
              break;
    case ',': if (!dmp->Open(eq,"w")) sts = dmp->Status();
              break;
    case ';': if (!var) for (; dmp ; dmp = dmp->Next()) {
                if (0 == strcmp(eq,dmp->Strm()->Name())) goto set;
              }
    default:  return STS_BAD_ARG;
  }

set:

  dmp->setFlags(flgs);

  return sts;
}

extern "C" eSTS verDumpV(int *argc,const char ***argv,void *var)
{
  return verSetDF(argc,argv,(void *)(new vdump(&DumpList)));
}

void VerilogObj::setSaveFlags(DmpFlgs flgs)
{
  vd()->save = flgs;
}

extern "C" eSTS verSetSF(int *argc,const char ***argv,void *var)
{
  VerilogObj *vo   = (VerilogObj *)var;
  DmpFlgs     flgs = DMP_NONE;
  const char *eq   = argEquals(argc,argv);

  if (!eq) return STS_MSSNG_ARG;

  if (isdigit(*eq)) {
    flgs = DMP_FLGS(atoi(eq));
  } else for (;*eq;eq++) switch (*eq) {
    /* ON */
  case 'a': flgs = DMP_ALL;                        break;
  case 'o': flgs = DMP_FLGS(flgs|DMP_OBFISCATE);   break;
  case 'v': flgs = DMP_FLGS(flgs|DMP_VERBOSE);     break;
  case 'm': flgs = DMP_FLGS(flgs|DMP_MODULE);      break;
  case 'n': flgs = DMP_FLGS(flgs|DMP_NATURE);      break;
  case 'd': flgs = DMP_FLGS(flgs|DMP_DISCIPLINE);  break;
  case 'u':
  case 'p': flgs = DMP_FLGS(flgs|DMP_PRIMITIVE);   break;
    /* OFF */
  case 'A': flgs = DMP_NONE;                       break;
  case 'O': flgs = DMP_FLGS(flgs&~DMP_OBFISCATE);  break;
  case 'V': flgs = DMP_FLGS(flgs&~DMP_VERBOSE);    break;
  case 'M': flgs = DMP_FLGS(flgs&~DMP_MODULE);     break;
  case 'N': flgs = DMP_FLGS(flgs&~DMP_NATURE);     break;
  case 'D': flgs = DMP_FLGS(flgs&~DMP_DISCIPLINE); break;
  case 'U':
  case 'P': flgs = DMP_FLGS(flgs&~DMP_PRIMITIVE);  break;
  }

  vo->setSaveFlags(flgs);

  return STS_NORMAL;
}

extern "C" void verInit()
{
  VerilogObj *vp = 0;

  vp->vd()->def_disc_id = -1;
  vp->vd()->cur_scp     = -1;

  vp->setSaveFlags(DMP_FLGS(DMP_MODULE|DMP_PRIMITIVE));
}

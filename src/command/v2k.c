/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ const char * v2k_c_rcsid() {return "$Id: v2k.c,v 1.74 2012/10/16 22:27:04 cvs Exp $";}

#include "system.h"
#include "args.h"
#include "env.h"
#include "verilog.h"
#include "pc.h"
#include "cdump.h"
#include "sdf.h"
#define V2K_DECL_MODE static
#include "v2k.h"
#include "shell.h"
#include "version.h"
#include "cg_enums.h"
#include "error.h"
#include "v2k_misc.h"

#if DBGLVL > 1
# include "veri_enum.h"
#endif

int Elaborated;
int CodeLoaded;

#define CL_ARG(s,fn,fa,typ,hlp) {s,(hndlrFn)fn,fa,typ,hlp}

argHndlr v2kHndlrs[] = {
  CL_ARG("*.v",       prsVerilog,  0,                 AT_FILE,
                "Verilog[-AMS]"),
  CL_ARG("*.V",       prsVerilog,  0,                 AT_FILE,
                "Verilog[-AMS]"),
  CL_ARG("*.h",       prsVerilog,  0,                 AT_FILE,
                "Verilog[-AMS]"),
  CL_ARG("*.va",      prsVerilogA, 0,                 AT_FILE,
                "Verilog-A"),
  CL_ARG("*.f",       argFile2Arg, 0,                 AT_FILE,
                "File of arguments"),
  CL_ARG("*.sdf",     prsSDF,      0,                 AT_FILE,
                "Standard Delay Format"),
  CL_ARG("*.pc",      prsPC,       (void *)PCMD_PC,   AT_FILE,
                "ParC"),
  CL_ARG("*.pi",      prsPC,       (void *)PCMD_PI,   AT_FILE,
                "Preprocessed ParC"),
  CL_ARG("*.i",       prsPC,       (void *)PCMD_I,    AT_FILE,
                "Preprocessed C"),
  CL_ARG("*.cpp",     prsPCextra,  0,                 AT_FILE,
                "Add C++ file to compile command"),
  CL_ARG("*.a",       prsPCextra,  0,                 AT_FILE,
                "Add library o link"),
  CL_ARG("sbsz",      sdfSetSz,    0,                 ARG_TYPE(AT_INT|AT_ARG),
                "SDF Buffer Size"),
  CL_ARG("sdfp",      sdfPiped,    0,                 ARG_TYPE(AT_BOOL|AT_ARG),
                "SDF Piped (bool)"),
  CL_ARG("E",         pcEfile,     (void *)1,         AT_ARG,
                "Output parser input"),
  CL_ARG("f",         argFile2Arg, (void *)1,         AT_ARG,
                "File of arguments"),
  CL_ARG("v",         prsVerilog,  (void *)1,         AT_ARG,
                "Verilog[-AMS]"),
  CL_ARG("sc2pc",     prsSysC,     (void *)PCMD_SYSC, AT_ARG,
                "Convert SystemC to ParallelC"),
  CL_ARG("incdir+*",  prsInclude,  "incdir+$",        AT_ARG,
                "Include directory (Verilog)"),
  CL_ARG("I*",        prsIncludeC, "I$",              AT_ARG,
                "Include directory (C)"),
  CL_ARG("plidir",    addPliPath,  0,                 AT_ARG,
                "PLI directory"),  
  CL_ARG("V*",        verAddLib,   "V$",              AT_ARG,
                "Library directory"),
  CL_ARG("X*",        argXtra,     "X$",              AT_ARG,
                "Miscellaneous arguments"),
  CL_ARG("define+*",  prsDefine,   "define+$",        AT_ARG,
                "Define pre-processor value (Verilog)"),
  CL_ARG("D*",        prsDefineC,  "D$",              AT_ARG,
                "Define pre-processor value (C)"),
  CL_ARG("undef+*",   prsUndef,    "undef+$",         AT_ARG,
                "Undefine pre-processor value"),
  CL_ARG("library",   verSetLib,   0,                 AT_ARG,
                "Set Library"),
  CL_ARG("autoload",  autoLoad,    &V2kAuto,          ARG_TYPE(AT_INT|AT_ARG),
                "Auto-load modules"),
  CL_ARG("minimize",  argSetBool,  &V2kMinimize,      ARG_TYPE(AT_BOOL|AT_ARG),
                "Reload created pools"),
  CL_ARG("unmatched", unmatchedOK, &V2kUnmatchedOK,   ARG_TYPE(AT_BOOL|AT_ARG),
                "Unmatched instances OK [bool]"),
  CL_ARG("portmode",  portMode,    &V2kPortMode,      ARG_TYPE(AT_INT|AT_ARG),
                "Port Mode [xdwu]"),
  CL_ARG("wiremode",  portMode,    &V2kWireMode,      ARG_TYPE(AT_INT|AT_ARG),
                "Wire Mode [k]"),
  CL_ARG("top",       setTop,      0,                 ARG_TYPE(AT_ARG|AT_STR),
                "Set top module"),
  CL_ARG("elaborate", doElab,      0,                 AT_ARG,
                "Elaborate Design [ath]"),
  CL_ARG("shell",     doShell,     0,                 AT_FLAG,
                "Go interactive"),
  CL_ARG("command",   doCommand,   0,                 AT_FLAG,
                "Run shell command"),
  CL_ARG("source",    doSource,    0,                 ARG_TYPE(AT_ARG|AT_STR),
                "Run Script"),
  CL_ARG("dump",      verSetDF,    0,                 ARG_TYPE(AT_INT|AT_ARG),
                "ndumov...[,<file>]"),
  CL_ARG("vdmp",      verDumpV,    0,                 AT_ARG,
                "ndumov...[,<file>]"),
  CL_ARG("cdmp",      cDumpC,      0,                 AT_ARG,
                "Dump C++ [<file>]"),
  CL_ARG("cp",        argSetStr,   PostProcessC,      AT_ARG,
                "C processor DLL"),
  CL_ARG("compile",   compilePC,   0,                 AT_ARG,
                "Compile generated C++ [srcoxvg0124]"),
  CL_ARG("output",    outputPC,    0,                 AT_ARG,
                "Path for compilation output"),
  CL_ARG("codegen",   codeGen,     0,                 AT_ARG,
                "Generate code for simulation [ap]"),
  CL_ARG("simulate",  startSim,    0,                 AT_ARG,
                "Simulate"),
  CL_ARG("save",      verSetSF,    0,                 ARG_TYPE(AT_INT|AT_ARG),
                "[m]"),
  CL_ARG("pt",        argSetPM,    0,                 AT_ARG,
                "Pool Type [icd...]"),
  CL_ARG("make",      argMake,     &V2kMake,          ARG_TYPE(AT_INT|AT_ARG),
                "Make mode [fdei...]"),
  CL_ARG("jobs",      jobLimit,    0,                 AT_ARG,
                "Job limit"),
  CL_ARG("timeout",   timeOut,     0,                 ARG_TYPE(AT_INT|AT_ARG),
                "Runtime limit (seconds)"),
#ifdef DEBUG
  CL_ARG("showtok",   showTok,     &Arg.show_tok,     AT_ARG,
                "Reconstruct tokenized file(s)"),
  CL_ARG("showmode",  showMode,    &Arg.show_mode,    AT_ARG,
                "Reconstruct tokenized file(s)"),
#endif
  {0,0,0}
};

int setTop(int *argc,const char ***argv,void *var)
{
  const char *eq  = argEquals(argc,argv);

  if (!eq) return STS_MSSNG_ARG;

  return setTopMod(eq);
}

eSTS doElab(int *argc,const char ***argv,void *var)
{
  int         flgs = ELAB_ALL;
  const char *eq   = argEquals(argc,argv);
  eSTS        sts  = STS_NORMAL;

  if (eq) switch (*eq) {
    case 'a': break;
    default:  return STS_BAD_ARG;
    case 't': flgs = ELAB_TOP;  break;
    case 'h': flgs = ELAB_HIER; break;
    case 'd': flgs = ELAB_DIMN; break;
    case 'b': flgs = ELAB_BIND; break;
    case 'm': flgs = ELAB_MKSI; break;
    case 'g': flgs = ELAB_GTSI; break;
  }

  return elaborate(flgs,&Elaborated);
}

int startSim(int *argc,const char ***argv,void *var)
{
  const char *flag = *(*argv)++;

  (*argc)--;

  if (!Elaborated) {
    int sts = elaborate(-1,&Elaborated);
    if (!Elaborated) return sts;
  }

  return v2kStartSim();
}

int codeGen(int *argc,const char ***argv,void *var)
{
  const char *flag = *(*argv);
  const char *eq   = argEquals(argc,argv);

  if (!Elaborated) {
    int sts = elaborate(-1,&Elaborated);
    if (!Elaborated) return sts;
  }

  eCodeMode cm = CM_GNU;

  if (eq) switch (*eq) {
    default:  return STS_BAD_ARG;
    case 'a': cm = CM_ANSI ; break;
    case 'p': cm = CM_PARC ; break;
  }

  return v2kCodegen(cm);
}

int doShell(int *argc,const char ***argv,void *var)
{
  const char *flag = *(*argv)++;
  int         sts  = STS_NORMAL;

  if (*argc) (*argc)--;

  Arg.interactive++;

  sts = dumbShell(0,*argc,*argv);

  Arg.interactive--;

  return sts;
}

int doSource(int *argc,const char ***argv,void *var)
{
  eSTS        sts = STS_NORMAL;
  const char *eq  = argEquals(argc,argv);

  if (!eq) return STS_MSSNG_ARG;

  Arg.interactive++;

  sts = dumbShell(eq,*argc,*argv);

  Arg.interactive--;

  return sts;
}

int doCommand(int *argc,const char ***argv,void *var)
{
  eSTS        sts = STS_NORMAL;
  const char *eq  = argEquals(argc,argv);

  if (!eq) return STS_MSSNG_ARG;

  Arg.interactive++;

  sts = shellCommand(eq);

  Arg.interactive--;

  return sts;
}

int argMake(int *argc,const char ***argv,void *var)
{
   int        *ip  = (int *)var;
   eSTS        sts = STS_NORMAL;
   const char *eq  = argEquals(argc,argv);

   if (!eq) return STS_MSSNG_ARG;

   switch (*eq) {
   case 'f': *ip |=  MAKE_FILE; break;
   case 'F': *ip &= ~MAKE_FILE; break;
   case 'd': *ip |=  MAKE_DEFS; break;
   case 'D': *ip &= ~MAKE_DEFS; break;
   case 'e': *ip |=  MAKE_ENV;  break;
   case 'E': *ip &= ~MAKE_ENV;  break;
   case 'a': *ip |=  MAKE_ALL;  break;
   case 'A': *ip &= ~MAKE_ALL;  break;

   default:  sts = STS_BAD_ARG;
   }

   return sts;
}

int autoLoad(int *argc,const char ***argv,void *var)
{
   int        *ip  = (int *)var;
   eSTS        sts = STS_NORMAL;
   const char *eq  = argEquals(argc,argv);

   if (!eq) return STS_MSSNG_ARG;

   switch (*eq) {
   case 'm': *ip |=  AUTO_MODULE;  break;
   case 'M': *ip &= ~AUTO_MODULE;  break;
   case 'a': *ip |=  AUTO_ALL;     break;
   case 'A': *ip &= ~AUTO_ALL;     break;

   default:  sts = STS_BAD_ARG;
   }

   return sts;
}

int portMode(int *argc,const char ***argv,void *var)
{
   int        *ip  = (int *)var;
   eSTS        sts = STS_NORMAL;
   const char *eq  = argEquals(argc,argv);

   if (!eq) return STS_MSSNG_ARG;

   switch (*eq) {
   case 'x': *ip |=  PORT_NOEXCESS; break;
   case 'X': *ip &= ~PORT_NOEXCESS; break;
   case 'u': *ip |=  PORT_NOUNCONN; break;
   case 'U': *ip &= ~PORT_NOUNCONN; break;
   case 'd': *ip |=  PORT_NODANGLE; break;
   case 'D': *ip &= ~PORT_NODANGLE; break;
   case 'w': *ip |=  PORT_WRNUNCNN; break;
   case 'W': *ip &= ~PORT_WRNUNCNN; break;
   case 'k': *ip |=  PORT_WRNUNKWN; break;
   case 'K': *ip &= ~PORT_WRNUNKWN; break;

   default:  sts = STS_BAD_ARG;
   }

   return sts;
}

int jobLimit(int *argc,const char ***argv,void *var)
{
   int        *ip  = (int *)var,
               tmp;
   eSTS        sts = STS_NORMAL;
   const char *eq  = argEquals(argc,argv);

   if (!eq) return STS_MSSNG_ARG;

   if (!ip) ip = &tmp;

   sts = argInt(eq,ip);

   if (STS_NORMAL == sts) SetJobLimit(*ip);

   return sts; 
}

int timeOut(int *argc,const char ***argv,void *var)
{
   int        *ip  = (int *)var,
               tmp;
   eSTS        sts = STS_NORMAL;
   const char *eq  = argEquals(argc,argv);

   if (!eq) return STS_MSSNG_ARG;

   if (!ip) ip = &tmp;

   sts = argInt(eq,ip);

   if (STS_NORMAL == sts) {
     alarm(tmp);
   }

   return sts;   
}

static int setBool(int *argc,const char ***argv,void *var)
{
   int        *ip  = (int *)var;
   eSTS        sts = STS_NORMAL;
   const char *eq  = argEquals(argc,argv);

   if (!eq) return STS_MSSNG_ARG;

   return argBool(eq,var);
}

int unmatchedOK(int *argc,const char ***argv,void *var)
{
  return setBool(argc,argv,var);
}

int showTok(int *argc,const char ***argv,void *var)
{
  return setBool(argc,argv,var);
}

int showMode(int *argc,const char ***argv,void *var)
{
  return setBool(argc,argv,var);
}

static eSTS v2kPrcssArgs(int *argc,const char ***argv)
{
  return processArgs(argc,argv,v2kHndlrs);
}

static void runtime_checks()
{
#if DBGLVL > 1
  assert((ATTR_SIGNED|ATTR_UINT)   == ATTR_INT);
  assert((ATTR_SIGNED|ATTR_U64)    == ATTR_I64);
  assert((ATTR_SIGNED|ATTR_LUINT)  == ATTR_LINT);
  assert((ATTR_SIGNED|ATTR_ULOGIC) == ATTR_LOGIC);
#endif
}

int main(int argc,const char **argv)
{
  eSTS sts;

  sts = initShell(argc,argv);

  runtime_checks();

#ifndef main
#ifdef PTHREADS
  SET_MAIN_THREAD_ID
  ASSERT(MAIN_THREAD_ID == pthread_self());
#endif

  if (!sts) {sts = doV2kRCs();}
#endif

  if (STS_NORMAL == sts) {

    verInit();

    argDefPrcssr = v2kPrcssArgs;
    sts          = argPrcssArgs(&argc,&argv);

    prsVMdone();

    ErrControl.verbosity |= VRB_EXIT;
  }

  Exit(sts);
}

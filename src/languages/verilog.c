/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ const char * verilog_c_rcsid() {return "$Id: verilog.c,v 1.17 2012/10/16 22:38:45 cvs Exp $";}
 


#include "system.h"
#include "args.h"
#include "verilog.h"
#include "language.h"
#include "poolmngr.h"
#include "strdb.h"

int prsVerSkipArg;

static const char **Argv;
static int          Argc;

int checkNextArg(const char *find,const char *found)
{
  int n = 0;

  while (n < Argc) {
    const char *pa = Argv[n++];
    if (0 == strcmp(find,pa)) return 1;
    break;
  }

  return 0;
}

eSTS verSetLib(int *argc,const char ***argv,void *var)
{
  const char *eq = argEquals(argc,argv);

  if (!eq) return STS_MSSNG_ARG;

  setLibrary(eq);

  return STS_NORMAL;
}

eSTS verAddLib(int *argc,const char ***argv,void *var)
{
  char       *fmt = (char *)var,
             *ap  = strchr(fmt,'$');
  const char *def = *(*argv)++;
  int         sts = STS_NORMAL,
              len = strlen(def),
              l1  = ap - fmt;

  TMPARR_POD(char,buf,len);

  --(*argc);

  while (strncmp(def,fmt,l1)) def++;

  def += l1;
  strcpy(buf,def);
  l1       = strlen(&fmt[l1 +1]);
  len      = strlen(buf) - l1;
  buf[len] = '\0';

  addLibrary(buf);

  return sts;
}

eSTS argXtra(int *argc,const char ***argv,void *var,int pls,int mns)
{
  char       *fmt = (char *)var,
             *ap  = strchr(fmt,'$');
  const char *def = *(*argv)++;
  int         sts = STS_NORMAL,
              len = strlen(def),
              l1  = ap - fmt;

  TMPARR_POD(char,buf,len);

  --(*argc);

  while (strncmp(def,fmt,l1)) def++;

  def += l1;
  strcpy(buf,def);
  l1       = strlen(&fmt[l1 +1]);
  len      = strlen(buf) - l1;
  buf[len] = '\0';

  //  addXtra(buf);

  return sts;
}

int prsVerilog(int *argc,const char ***argv,void *var,int pls,int mns)
{
  const char *file;
  int         sts  = STS_NORMAL;
  eVMD        vmd  = VMD_IEEE;

  if (var) {
    file = argEquals(argc,argv);
    vmd  = VMD(VMD_IEEE|VMD_DASHV);
  } else {
    file = *(*argv)++;
    Argv = *argv;
    Argc = --(*argc);
  }

  if (prsVerSkipArg) {
    prsVerSkipArg--;
  } else {
    InitLang(defPoolMode,0);

    sts = prsVM(file,vmd);

    switch (sts) {
    case STS_ALREADY: sts = STS_NORMAL; break;
    }
  }

  return sts;
}

int prsVerilogA(int *argc,const char ***argv,void *var,int pls,int mns)
{
  const char *file = *(*argv)++;
  int         sts  = STS_NORMAL;

  Argv = *argv;
  Argc = --(*argc);

  if (prsVerSkipArg) {
    prsVerSkipArg--;
  } else {
    InitLang(defPoolMode,0);

    sts = prsVM(file,VMD_IEEE|VMD_VA);

    switch (sts) {
    case STS_ALREADY: sts = STS_NORMAL; break;
    }
  }

  return sts;
}

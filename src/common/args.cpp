/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * args_cpp_rcsid() {return "$Id: args.cpp,v 1.62 2010/05/01 09:54:02 dkc Exp $";}

#include "system.h"
#include "error.h"
#include "args.h"
#include "strdb.h"
#include "modes-a.h"
#include "libs.h"

int V2kMake;

extern "C" {

Args Arg = {INIT_ARGS}; //!< Current argument list (may get swapped)

int ArgCount()
{
  return Arg.argc0;
}

const char *Arg0()
{
  return Arg.exe ? Arg.exe
                 : "v2k";
}

const char *ArgN(int n)
{
  if (0 == n) return Arg0();

  if (n < Arg.argc0) return Arg.argv0[n];

  return "";
}

}

//!< look for value for argument 
const char *argEquals(int *argc,const char ***argv)
{
  const char *arg = **argv,
             *eq  = strchr(arg,'=');

  (*argc)--;
  (*argv)++;

  if (eq) { // -<arg>=<val>
    eq++;
  } else {  // -<arg> <val>
    if (*argc <= 0) return 0;
    if ('-' != ***argv && '+' != ***argv) { // skip if not value
      eq = **argv;
      (*argc)--;
      (*argv)++;
    }
  }

  return eq; // pointer to <val> or null
}

//!< Call-back to set a number
eSTS argSetVar(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
   int        *ip = (int *)var;
   const char *eq = argEquals(argc,argv);

   if (!eq) return STS_MSSNG_ARG;

   *ip = atoi(eq);

   return STS_NORMAL;
}

//!< Call-back to set a boolean
eSTS argSetBool(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
   int        *ip = (int *)var;
   const char *eq = argEquals(argc,argv);

   if (!eq) return STS_MSSNG_ARG;

   return argBool(eq,ip);
}

//!< Call-back to change verbosity
eSTS argSetVerb(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  int        *ip  = (int *)var;
  const char *eq  = argEquals(argc,argv);
  eSTS        sts = STS_NORMAL;

  if (!eq) return STS_MSSNG_ARG;

  if (isdigit(*eq)) {
    *ip = atoi(eq);
  } else {
    while (*eq) switch (*eq++) {
    case 'a':  *ip |=  VRB_ALL;  break; // add
    case 'A':  *ip &= ~VRB_ALL;  break; // subtract
    case 'c':  *ip |=  VRB_CODE; break;
    case 'C':  *ip &= ~VRB_CODE; break;
    case 'd':  *ip |=  VRB_DUMP; break;
    case 'D':  *ip &= ~VRB_DUMP; break;
    case 'g':  *ip |=  VRB_GEN;  break;
    case 'G':  *ip &= ~VRB_GEN;  break;
    case 'e':  *ip |=  VRB_ELAB; break;
    case 'E':  *ip &= ~VRB_ELAB; break;
    case 'i':  *ip |=  VRB_INFO; break;
    case 'I':  *ip &= ~VRB_INFO; break;
    case 'm':  *ip |=  VRB_MAKE; break;
    case 'M':  *ip &= ~VRB_MAKE; break;
    case 'p':  *ip |=  VRB_PARC; break;
    case 'P':  *ip &= ~VRB_PARC; break;
    case 's':  *ip |=  VRB_SUMM; break;
    case 'S':  *ip &= ~VRB_SUMM; break;
    case 'v':  *ip |=  VRB_ECHO; break;
    case 'V':  *ip &= ~VRB_ECHO; break;
    case 'w':  *ip |=  VRB_WARN; break;
    case 'W':  *ip &= ~VRB_WARN; break;
    case 'x':  *ip |=  VRB_EXIT; break;
    case 'X':  *ip &= ~VRB_EXIT; break;
    case '*':  *ip |=  VRB_ARG;  break;
    case '|':  *ip &= ~VRB_ARG;  break;
    case '+':  *ip |=  VRB_MISC; break;
    case '-':  *ip &= ~VRB_MISC; break;
    case '`':  *ip |=  VRB_DEFN; break;
    case '\'': *ip &= ~VRB_DEFN; break;

    default:  sts = STS_BAD_ARG;
    }
  }

  return sts;
}

//!< Call-back to set a string
eSTS argSetStr(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
   char       *str = (char *)var;
   const char *eq  = argEquals(argc,argv);

   if (!eq) return STS_MSSNG_ARG;

   strcpy(str,eq);

   return STS_NORMAL;
}

//!< Call-back to set an IO stream
eSTS argSetIO(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  intptr_t    fd  = (intptr_t)var, // usually 0,1 or 2
              nw;
  const char *eq  = argEquals(argc,argv);

  if (!eq) return STS_MSSNG_ARG;

  nw = open(eq,fd ? O_WRONLY|O_CREAT
                  : O_RDONLY,        0640);

  if (nw < 0) return ERROR(errno);

  dup2(nw,fd); close(fd);

  return STS_NORMAL;
}

//! Explains a return code
eSTS argExplain(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  const char  *eq  = argEquals(argc,argv);
  int          sts;

  if (!eq) return STS_MSSNG_ARG;

  if (1 != sscanf(eq,"%d",&sts)) return STS_BAD_ARG;

  ErrorMsg(ERROR(sts & 0xFF),"");

  return STS_NORMAL;
}

//! Changes working directory
eSTS argCD(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  const char  *eq  = argEquals(argc,argv);

  if (!eq) return STS_MSSNG_ARG;

  if (chdir(eq)) return ERROR(errno);

  return STS_NORMAL;
}

//! Pause program (for attaching debugger)
eSTS argPause(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  int pid = getpid();

  (*argc)--;
  (*argv)++;

  kill(pid,SIGSTOP);

  return STS_NORMAL;
}

//! Call back to open an input
eSTS argSetInput(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  FILE       **fp = (FILE **)var;
  const char  *eq = argEquals(argc,argv);

  if (!eq) return STS_MSSNG_ARG;

  if (!(*fp = fopen(eq,"r"))) {
    return ERROR(errno);
  }

  return STS_NORMAL;
}

//! Call back to open an output
eSTS argSetOutput(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  FILE       **fp = (FILE **)var;
  const char  *eq = argEquals(argc,argv);

  if (!eq) return STS_MSSNG_ARG;

  if (!(*fp = fopen(eq,"w"))) {
    return ERROR(errno);
  }

  return STS_NORMAL;
}

//! Convert "Y|y[es]" "N|n[o]" 1 0 to boolean value
eSTS argBool(const char *val,int *ret)
{
  int t;

  if (1 <= strlen(val)) switch (*val++) {
  case '1':
  case 'Y':
  case 'y': t = 1 ; goto ok;
  case '0':
  case 'n':
  case 'N': t = 0 ; goto ok;
  }

  return STS_BAD_ARG;

ok:
  *ret = t;
  return STS_NORMAL;
}

//! Parse and return an integer
eSTS argInt(const char *val,int *ret)
{
  int t = 0,
      s = 1;

  if ('-' == *val) { val++ ; s = -1; }

  for (; isdigit(*val) ; val++) {
    t = (t * 10) + (*val - '0');
  }

  if (*val) return STS_BAD_ARG;

ok:
  *ret = s * t;
  return STS_NORMAL;
}

eSTS argDump(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1);

//! Convert file of arguments to a command line
eSTS argFile2Arg(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  struct stat  buf;
  FILE        *fp;
  int          nl       = -1,
               n;
  char       **new_argv = 0,
              *file,
              *fname,
              *scan,
              *cmnt0;

  if (var) {
    fname = (char *)argEquals(argc,argv);

    if (!fname) return STS_MSSNG_ARG;
  } else {
    fname = (char *)*(*argv)++;
    (*argc)--;
  }

  if (!(fp = fopen(fname,"r"))) {
    return ERROR(errno);
  }

  fstat(fileno(fp),&buf);

  file = MALLOC2_N(buf.st_size +1,char);

  fread(file, buf.st_size, 1, fp);

  file[buf.st_size] = '\0';

  while (!new_argv) {

    if (nl >= 0 && !(new_argv = MALLOC2_N(nl + *argc,char *))) break;

    int quote = 0,
        ws    = 1,
        cmnt  = 0,
        ln    = 1,
        ql,
        ch;

    for (nl = 0,scan = file; ch = *scan; scan++) switch (ch) {
    case '/':  if ('/' != scan[1] &&
                   '*' != scan[1]) goto regular;
               ch = scan[1];
    case '#':  if (!(quote || cmnt)) {
                 if (new_argv) *scan = '\0';
                 cmnt  = ch;
                 cmnt0 = scan;
               }
               break;
    case '*':  if ('*' != cmnt || '/' != scan[1]) goto regular;
               memset(cmnt0,' ',scan + 2 - cmnt0);
               cmnt = 0;
               break;
    case '\'':
    case '"':  if      (!quote)      {quote = ch; ql = ln;}
               else if (ch == quote) {quote = 0;}
    regular:
    default:   if (!(quote || cmnt)) {
                 if (isspace(ch)) {
                   if (!ws++ && new_argv) *scan = '\0';
                 } else if (ws) {
                   ws= 0;
                   if (new_argv) new_argv[nl] = scan;
		   nl++;
                 }
               }
               if ('\n' == ch) {
                 ln++;
                 if (cmnt) {
                   quote = 0;
                   if ('*' != cmnt) cmnt = 0;
                 }
               }
    }

    if (quote) {
      fprintf(stderr,"Unterminated string (%c) at line %d of: %s\n",
                     quote,ql,fname);
      return STS_BAD_ARG;
    }
  }

  while ((*argc)-- > 0) {
    new_argv[nl++] = (char *)*(*argv)++;
  }

  *argc = nl;
  *argv = (const char **)new_argv;

  return STS_NORMAL;
}

//! Show loaded DLL versions
eSTS argVersion(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  LibInfo *scan = V2KloadedLibs;

  (*argc)--;
  (*argv)++;

  fprintf(stderr,"\n");
  for (; scan ; scan = scan->next) {
    fprintf(stderr,"%-20s%s\n",scan->name,scan->version);
  }

  return STS_NORMAL;
}

//! Standard argument handlers
static argHndlr stdHndlrs[] = {
  {"verbose",      argSetVerb,&Arg.verbosity,        ARG_TYPE(AT_ARG|AT_INT),
                   "Verbosity level"},
  {"err.verbose",  argSetVerb,&ErrControl.verbosity, ARG_TYPE(AT_ARG|AT_INT),
                   "Error verbosity level"},
  {"err.dont_stop",argSetVar, &ErrControl.dont_stop, ARG_TYPE(AT_ARG|AT_INT),
                   "Ignore fatal errors [bool]"},
  {"err.give_up",  argSetVar, &ErrControl.give_up,   ARG_TYPE(AT_ARG|AT_INT),
                   "Give up after too many errors [n]"},
  {"err.do_stop",  argSetVar, &ErrControl.do_stop,   ARG_TYPE(AT_ARG|AT_INT),
                   "Stop at first error"},
  {"debug",        argSetVar, &Arg.debug,            ARG_TYPE(AT_ARG|AT_INT),
                   "Debug level"},
  {"stdin",        argSetIO,  (void *)0,             ARG_TYPE(AT_ARG|AT_IO),
                   "Standard IO stream spec."},
  {"stdout",       argSetIO,  (void *)1,             ARG_TYPE(AT_ARG|AT_IO),
                   "Standard IO stream spec." },
  {"stderr",       argSetIO,  (void *)2,             ARG_TYPE(AT_ARG|AT_IO),
                   "Standard IO stream spec."},
  {"pm",           argSetStr, defPoolMode,           ARG_TYPE(AT_ARG|AT_INT),
                   "Pool creation/access mode [crw...]"},
  {"explain",      argExplain, (void *)0,            ARG_TYPE(AT_ARG|AT_INT),
                   "Explain a return code [n]"},
  {"chdir",        argCD, (void *)0,                 ARG_TYPE(AT_ARG|AT_STR),
                   "Change working directory"},
  {"pause",        argPause,  0,                     AT_ARG,
                   "Suspend program"},
  {"help",         argDump,   0,                     AT_ARG,
                   "Print help"},
  {"version",      argVersion,   0,                  AT_ARG,
                   "Show component versions"},
  {0,0,0,AT_NONE}
};

#define MAX_HANDLR 4
argHndlr *ArgHandlrs[MAX_HANDLR] = {stdHndlrs}; //!< Argument handler stack

//! Print out argument handler info
eSTS argDump(int *argc,const char ***argv,void *var,int pls,int mns)
{
  int n = MAX_HANDLR;
  while (n-- > 0) {
    argHndlr *pa = ArgHandlrs[n];
    for (;pa && pa->name;pa++) {
      fprintf(stderr,"%c%-12s\t%s\n",
	             pa->atyp & AT_FLAG ? '-' : ' ',
                     pa->name,pa->descr ? pa->descr : "* ??? *");
    }
  }

  return ERROR(Exit(STS_NORMAL));
}

static const char ***Argv; //<! pointer to current argument list value pointer
static int          *Argc; //<! pointer to current argument list count

//! get next argument
const char *argNext(int *argc,const char ***argv)
{
  const char *ret = *(*argv)++;

  --(*argc);

  return ret;
}

//! Process arguments until match with 'ret'
eSTS prcssArgs(int *argc,const char ***argv,argHndlr *hndlrs,int h,eAT ret)
{
  eSTS          sts        = STS_MSSNG_ARG;
  int          *argc_svd   = Argc;
  const char ***argv_svd   = Argv;
  argHndlr     *hndlrs_svd = ArgHandlrs[h];

  if (argv) {
    ArgHandlrs[h] = hndlrs;
    Argv          = argv;
    Argc          = argc;

    if (AT_NONE == ret) {
      Arg.argv0 = *Argv;
      Arg.argc0 = *Argc;
      (*Argc)--;
      Arg.exe   = *(*Argv)++;
    }
  }

  while (*Argc > 0) {
    const char *ap  = **Argv;
    int         mns = 0,
                pls = 0;

    while ('-' == *ap) {ap++;mns++;}
    if ('+' == *ap)    {ap++;pls++;}

    if ('\\' == *ap) ap++; // escape a leading + or -

    int       a,
              mtch  = 0;
    argHndlr *hndlr = 0;

    for (a = MAX_HANDLR; a-- > 0;) {
      if (hndlrs = ArgHandlrs[a]) {
        const char *np;
        int         nl = 0;
        for (;np = hndlrs->name;hndlrs++) {

          if (((pls || mns) && !(hndlrs->atyp & AT_ARG)) ||
              ((hndlrs->atyp & AT_ARG) && !(pls || mns))) continue;

          int         m  = 0;
          const char *sp = ap;
          while (*np && *sp) {
            if (*np == *sp || ('?' == *np && '\\' != nl)) {
              nl = *np;
              m++; np++; sp++;
            } else if (*np == '*' && '\\' != nl) {
              np++;
              int         l  = strlen(sp);
              const char *ep = sp + l,
                         *ew = np;
              if (*ew) {
                while (*ew && '?' != *ew && '*' != *ew) ew++;
                l = ew - np;
                while (ep-- >= sp) {
                  if (0 == strncmp(np,ep,l)) goto match;
                }
	      } else {
                sp = ep;
              }
              break;
            match:
              sp  = ep;
              np += l;
              sp += l;
              m  += l;
            } else {
              break;
            }
          }
          if (!*sp || ('=' == *sp && (hndlrs->atyp & AT_VALUE))) {
             if (!*np) {
               hndlr = hndlrs;
               goto exact;
             }
             if (m >= mtch) {
               if (m > mtch)       hndlr = hndlrs;
               else if (m == mtch) hndlr = 0;
               mtch = m;
             }
          }
        }
      }
    }
    if (!hndlr) {

      if (mtch && !hndlr) {
        if (VRB_ARG & Arg.verbosity) {
          fprintf(stderr,"Ambiguous argument: %s\n",**Argv);
        }
        return STS_AMBG_ARGS;
      }
      strncpy(Arg.last,**Argv,sizeof(Arg.last)-1);
      if (VRB_ARG & Arg.verbosity) {
        fprintf(stderr,"Can't process argument: %s\n",**Argv);
      }
      return STS_BAD_ARG;
    }
  exact:
    if (VRB_ARG & Arg.verbosity) {
      fprintf(stderr,"Processing argument: %s (%s)\n",
	                 **Argv,hndlr->name);
    }
    if (hndlr->atyp == ret) {
      return STS_TODO;
    } else {
      strncpy(Arg.last,**Argv,sizeof(Arg.last)-1);
      if (sts = (*hndlr->fn)(Argc,Argv,hndlr->data,pls,mns)) {
        break;
      }
    }
  }

  if (argv) {
    Argv          = argv_svd;
    Argc          = argc_svd;
    ArgHandlrs[h] = hndlrs_svd;
  }

  return sts;
}

//! Process arguments until a file, return the file
extern "C" const char *argNextFile (const char *alt,int none_ok)
{
  eSTS sts = prcssArgs(0,0,0,0,AT_FILE);

  if (STS_TODO == sts) return (**Argv);

  if (!none_ok && STS_NORMAL != sts) Exit(sts);

  return alt;
}

//! Skip to next argument
extern "C" void argOK ()
{
  if (*Argc) {
    (*Argc)--;
    (*Argv)++;
  }
}

extern "C" {
eSTS processArgs(int *argc,const char ***argv,argHndlr *hndlrs)
{
  return prcssArgs(argc,argv,hndlrs,1,AT_NONE);
}

static eSTS defPrcssArgs(int *argc,const char ***argv)
{
  return prcssArgs(argc,argv,0,1,AT_NONE);
}

argPrcssFn argDefPrcssr = defPrcssArgs;

eSTS argPrcssArgs(int *argc,const char ***argv)
{
  return (*argDefPrcssr)(argc,argv);
}
}

ePM defPoolType = PM_Contig;
//! Set Default Pool Mode
extern "C" eSTS argSetPM(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  const char *eq = argEquals(argc,argv);

  if (!eq) return STS_MSSNG_ARG;

  switch (*eq) {
  case 'i': defPoolType = PM_InMem;  break;
  case 'c': defPoolType = PM_Contig; break;
  case 'd': defPoolType = PM_OnDisk; break;
  default:  return STS_BAD_ARG;
  }

  return STS_NORMAL;
}


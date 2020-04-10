/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * pc_cpp_rcsid() {return "$Id: pc.cpp,v 1.22 2010/05/01 09:48:43 dkc Exp $";}

#include "system.h"
#include "args.h"
#include "env.h"
#include "error.h"
#include "list.h"
#include "language.h"
#include "poolmngr.h"
#include "strdb.h"
#include "v2k_mem.h"
#include "v2k.h"
#define  NEED_PUNCTUATION
#define  NEED_QUOTES
#define  NEED_WHITESPACE
#define  NEED_INTEGER
#define  NEED_COMMENTS
#define  NEED_PP
#define  NEED_CPP
#define  NEED_CPPOPS
#include "tokpool.h"
#define PC_POOL_DESC
#include "parser.h"
#include "dyn.h"
#include "job.h"
#include "pc.h"

CompilerOpts CppDriver;

static CppContext *currCntxt;

static StringList PCfilesDone;
static StringList PCfilesExtra;

static String     PCoutput("pc-sim");

CompilerOpts::CompilerOpts()
{
  // includes.add(new CompilerOpt("${" V2K_PC_INC "}"));
}

int prsPCloadTok(const char *src,File &tok,File *deps,ePCMD mode)
{
  CppContext top;

  currCntxt = &top;

  top.setCache(tok.str());

  CppConst::setStrings();

  int sts = top.prsPCtok(src,tok,deps,mode);

  if (0 == sts) {

    eAZC azc = AZC_NONE;
    eFT  ft  = FT_Inter;

    eWFLG flgs = WFLG_NONE;
    if (PCMD_SYSC & mode) {
      azc  = AZC_SYSC;
      flgs = WFLG_SYSC2PC;
      ft   = FT_Pc;
    }

    if (PCMD_PAR & mode) {
      CppType::findBuiltins(top.Root());
      sts = top.Analyze(0,0,0,0,azc);
    }

    DynObj *dll = 0;
    if (PostProcessC.len()) {
      sts = (dll = new DynObj(PostProcessC))->Status();
      if (0 == sts) {
	CppFunc fp = (CppFunc)dll->DlSym("v2kCppFunc");
        if (fp) {
          sts = (*fp)(&top);
	} else {
	  sts = STS_DLNOSYM;
	}
      }
    }

    if (0 == sts) {
      int m = Arg.show_mode & 1;

      File f("${" V2K_CC_TMP "}",tok,ft,FILE_NO_SEP|FILE_BASENAME);

      sts = top.reWrite(f,flgs);

      if (m) Arg.show_mode |= 1;

      if (0 == sts) PCfilesDone.add(new StringListItem(f));
    }

    if (dll) delete dll;
  }  

  return sts;
}

int prsPCld(const char *file,ePCMD mode,int load, File *rtok,File *rdeps)
{
  Filename  src(file);
  char     *sp   = src;
  int       make = V2kMake;
  int       sts  = 0;

  envExpand(src,sizeof(src));

  if (!src.Exists()) {
    return ErrorMsg(S_ERROR(STS_BAD_ARG),"File missing - %s",file);
  }

  Stream comp;

  if (PCMD_I & mode) {
    comp.Open(src,"r");
  } else {
    File f(file);
    File cmd("${" V2K_CC_PC "}");

    const CompilerOpt *ip = CppDriver.include0();

    for (; ip ; ip = ip->Next()) {
      cmd += " -I";
      cmd += ip->str();
    }

    for (ip = CppDriver.define0(); ip ; ip = ip->next) {
      cmd += " -D";
      cmd += ip->str();
    }

    cmd += " -E ";
    cmd += f;

    if (Arg.verbosity & VRB_PARC) {
      envExpand(cmd);
      fprintf(stderr,"Preprocessing: %s\n",cmd.str());
    }

    comp.Open(&cmd,FM(FM_PIPE|FM_READ));
  }

  poolRef src_nm = strSaveStr(src);

  U64 mtime = (make & MAKE_FILE) ? src.Date()
                                 : 0;
  PrsrCntxt::encode(src);

  File  tok("${V2K_REPOSITORY}/cache/",src,FT_Token);
  if (!mtime || (tok.Date() <= mtime)) {
    Stream  out;
    File   *fp;

    if (Arg.verbosity & VRB_MAKE) {
      Stream::Stdio(STDERR_FILENO)->printf("Tokenizing: %s\n",sp);
    }

    if (out.Open(fp = &tok,"pw")) {
      tokPC(&comp,&out,mode);
      out.Close();
    } else {
      ExitMsg(S_ERROR(Error()),"can't open %s",fp->str());
    }

    make = MAKE_NONE;
  }

  comp.Close();

  if (rtok) *rtok = tok.str();

  if (load) {
    sts = prsPCloadTok(file,tok,0,mode);
  }

  return sts;
}

extern "C" int outputPC(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  const char *file = argEquals(argc,argv);

  PCoutput = file;  

  return 0;
}

extern "C" int prsPCextra(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  const char *file = *(*argv)++; (*argc)--;

  PCfilesExtra.add(new CompilerOpt(file));

  return 0;
}

extern "C" int compilePC(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  const char *eq    = argEquals(argc,argv);
  const char *copts = *(*argv)++; (*argc)--;

  int so  = (eq) ? 0
                 : 1;
  int rp  =  0;
  int obj =  0;
  int src =  0;
  int exe =  0;
  int opt = -1;
  int dbg =  0;
  int vrb =  Arg.verbosity & VRB_PARC;

  const char *cxx = "${" V2K_CXX "} ${" V2K_CXX_FLAGS "}",
             *lng = " ${" V2K_CXX_LANG "} ",
             *lbs = " ${" V2K_CXX_LIBS "} ";

  if (eq) for (; *eq ; eq++) {
    switch (*eq) {
    case 's': so  = 1; break; // create .so
    case 'r': rp  = 1; break; // - with -R
    case 'k': src = 1; break; // kernel given as source
    case 'o': obj = 1; break; // create .o
    case 'x': exe = 1; break; // make an executable
    case 'v': vrb = 1; break; // verbose
    case 'g': dbg = 1; break;
    case '0': opt = 0; break;
    case '1': opt = 1; break;
    case '2': opt = 2; break;
    case '3': opt = 3; break;
    case 'c': lbs = 0;
              lng = " ";
              cxx = "${" V2K_CC "} ${" V2K_CC_FLAGS "}";
    }
  }

  File cmd(cxx);

  if (dbg) cmd += " -g ";

  if (opt >= 0) {
    char ostr[8]; sprintf(ostr," -O%d ",opt);
    cmd += ostr;
  }

  cmd += " -o ";
  cmd += PCoutput;

  const CompilerOpt *pc = PCfilesDone.first();

  for (; pc ; pc = pc->Next()) {
    cmd += lng;
    cmd += pc->str();
  }

  pc = PCfilesExtra.first();

  if (pc) {
    const CompilerOpt *ip = CppDriver.include0();

    for (; ip ; ip = ip->Next()) {
      cmd += " -I";
      cmd += ip->str();
    }
  }

  for (; pc ; pc = pc->Next()) {
    cmd += " ";
    cmd += pc->str();
  }

  if (obj) {
    cmd += " -c";
  } else {
    if (so) {
      cmd += " ${" V2K_SIM_FLAGS "}";
    }
    if (rp) {
      cmd += " ${" V2K_LD_RPATH "} ${" V2K_SO_LIB "}";
    }
    if (!src) {
      cmd += " ${" V2K_PC_LD_FLAGS "}";
    }
    if (lbs) {
      cmd += lbs;
    }
  }

  envExpand(cmd);

  if (vrb) {
    fprintf(stderr,"%s\n",cmd.str());
  }

  Job do_it(cmd);

  return do_it.Status();  
}

static const char **Argv;
static int          Argc;

extern "C" {
  Filename PostProcessC;
}

extern "C" int prsPC(int *argc,const char ***argv,void *var,int pls,int mns)
{
  const char *file = *(*argv)++;
  ePCMD       mode = (ePCMD)(intptr_t)var;

  (*argc)--;

  InitLang(defPoolMode,0);

  return prsPCld(file,mode,1,0,0);
}

extern "C" int prsSysC(int *argc,const char ***argv,void *var,int pls,int mns)
{
  const char *file;
  int         sts  = STS_NORMAL;
  ePCMD       mode = (ePCMD)(intptr_t)var;

  file = argEquals(argc,argv);

  InitLang(defPoolMode,0);

  return prsPCld(file,mode,1,0,0);

  return sts;
}

void CompilerOpts::addIncDir(const char *dir)
{
  includes.add(new StringListItem(dir));
}

extern "C" int prsIncludeC(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  char       *fmt = (char *)var,
             *ap  = strchr(fmt,'$');
  const char *def = *(*argv)++;
                   --(*argc);
  int         sts = STS_NORMAL,
              len = strlen(def),
              l1  = ap - fmt;

  TMPARR(char,buf,len);

  while (len-- > 0 && strncmp(def,fmt,l1)) def++;

  if (len <= 0) return STS_BAD_ARG;

  def += l1;
  strcpy(buf,def);
  l1       = strlen(&fmt[l1 +1]);
  len      = strlen(buf) - l1;
  buf[len] = '\0';

  CppDriver.addIncDir(buf);

  return sts;
}

void CompilerOpts::addDef(const char *def)
{
  defines.add(new StringListItem(def));
}

extern "C" int prsDefineC(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  char       *fmt = (char *)var,
             *ap  = strchr(fmt,'$');
  const char *def = *(*argv)++;
                   --(*argc);
  int         sts = STS_NORMAL,
              len = strlen(def),
              l1  = ap - fmt;

  TMPARR(char,buf,len);

  while (strncmp(def,fmt,l1)) def++;

  def += l1;
  strcpy(buf,def);
  l1       = strlen(&fmt[l1 +1]);
  len      = strlen(buf) - l1;
  buf[len] = '\0';

  CppDriver.addDef(buf);

  return sts;
}

const Token *CppContext::NextTok(const Token *tok,int needed)
{
  return tok;
}

int loadFromCDeps(const char *src_cache,const char *deps)
{
  return 0;
}

int cDepsOK(Stream *deps,int make,int depth)
{
  return STS_OUTOFDATE;
}

int tokPC (Stream *in,Stream *out,ePCMD mode)
{
  return tokPC(in->Fp(),out->FwriteFn(),out->Fp(),mode);
}

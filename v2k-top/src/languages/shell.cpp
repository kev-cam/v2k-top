/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * shell_cpp_rcsid() {return "$Id: shell.cpp,v 1.64 2009/05/22 21:27:59 dkc Exp $";}

#define VERILOG3

#include "system.h"
#include "args.h"
#include "env.h"
#include "dyn.h"
#include "file.h"
#include "strdb.h"
#include "v2k_mem.h"
#include "v2k.h"
#define  NEED_VERILOG
#define  NEED_COMMANDS
#define  NEED_PUNCTUATION
#include "tokpool.h"
#include "language.h"
#include "veri_enum.h"
#include "verilog.h"
#include "shell.h"

static const char NullStr[]   = "",
                  Equals[]    = "=",
                  Equiv[]     = "==",
                  EquivF[]    = "=~",
                  TestNot[]   = "!",
                  EquivF_[]   = "!~",
                  TestSL[]    = "<<",
                  TestSR[]    = ">>",
                  TestLE[]    = "<=",
                  TestGE[]    = ">=",
                  TestNE[]    = ">=",
                  OpenBr[]    = "(",
                  CloseBr[]   = ")",
                  Colon[]     = ":",
                  Ampersand[] = "&",
                  LgcAnd[]    = "&&",
                  DirectOut[] = ">",
                  DirectIn[]  = "<",
                  BackQuote[] = "`";

CBlock CShell::ElsBlk    = {CSH_ELSE},
       CShell::IfBlk     = {CSH_IF},
       CShell::IfBarBlk  = {CSH_IF_},
       CShell::WhlBarBlk = {CSH_WHILE_};

CShell     *Shell::CSH;
ExecShell  *Shell::ESH;
ReadlineFn  Shell::Readline,
            Shell::AddHistory;
bool        Shell::InitDone;
DynObj     *Shell::RdLnLib;
FILE      **Shell::rl_instream,
	  **Shell::rl_outstream;

typedef struct {
  const char *lib_name,
             *lib_dir;
  Stream     *in,
             *out,
             *err;
} CBdata;

void *printFile(const char *name,void *vp)
{
  CBdata *dp = (CBdata *)vp;

  dp->out->printf("%s\n",name);

  return 0;
}

extern "C" int loadObject(int,const char *,void *);

void *loadFile(const char *name,void *vp)
{
  CBdata   *dp = (CBdata *)vp;
  Filename  fnm(dp->lib_dir);

  fnm += OS_DIR_SEP;
  fnm += name;

  if (!loadObject(0,0,&fnm)) {
    dp->err->printf("Failed to load: %s\n",name);
  }

  return 0;
}

static void *printObj(void *vp,...)
{
  CBdata *dp = (CBdata *)vp;
  va_list pvar;

  va_start(pvar, vp);
  char *cp;
  while (cp = va_arg(pvar, char *)) {
    dp->out->printf("%s",cp);
  }
  dp->out->printf("\n");
  va_end(pvar);

  return 0;
}

extern "C" int prntAllObj(int,anyCB,void *);

static int forLoaded(const char **argv,Stream *out,Stream *err,anyCB call)
{
  CBdata data;

  data.out = out;
  data.err = err;

  return prntAllObj(RFF_MODULE|RFF_PRIM,call,&data);
}

static int forLibs(const char **argv,Stream *out,Stream *err,fileCB call)
{
  const char *arg;
  CBdata      data;
  int         any = 0;

  if (arg = *argv) {
    argv++;
  } else {
    arg = ".:";
  }

  data.out = out;

  for (; arg ; arg = *argv++) {
    const char *cln = strchr(arg,':');
    int         l   = 0;
    if (cln) {
      l = cln - arg;
      cln++;
    } else {
      cln = arg;
    }
    if (!*cln) cln = 0;
    TMPARR(char,lib,l +1);
    strncpy(lib,arg,l);
    lib[l] = '\0';
    String llb(lib);
    if (!*lib || 0 == strcmp(".",lib)) {
      llb = "${" V2K_LIB_REPOS "}";
      envExpand(&llb,err,SHF_TILDE);
    } else if (!envPathFind(V2K_LIB_PATH,lib,F_OK,&llb)) {
      err->printf("%s: Not Found\n",lib);
      continue;
    }

    data.lib_name = lib;
    data.lib_dir  = llb.str();
    int m = llb.FileName()->dirMatch(cln,call,&data);
    if (m > 0) any = 1;
  }

  return any;
}

void Shell::Init()
{
  InitDone = true;

#ifndef BAD_READLINE
  File rdln_so("${V2K_LIBREADLINE}");

  rdln_so.Expand();

  if (*rdln_so.str() && (RdLnLib = new DynObj(rdln_so))) {
    rl_instream  = (FILE **)RdLnLib->DlSym("rl_instream");
    rl_outstream = (FILE **)RdLnLib->DlSym("rl_outstream");
    if (rl_instream && rl_outstream) {
      Readline   = (ReadlineFn)RdLnLib->DlSym("readline");
      AddHistory = (ReadlineFn)RdLnLib->DlSym("add_history");
    }
  }
#endif
  if (!CSH) CSH = new CShell(&RootShell);
  if (!ESH) ESH = new ExecShell(&RootShell);
}

char *Shell::readline(const char *prompt,Stream *in,Stream *out)
{
  *rl_instream  = in->Fp(STRM_IN);
  *rl_outstream = out->Fp(STRM_OUT);

  char *lp = (*Readline)(prompt);

  if (lp && *lp && AddHistory) (*AddHistory)(lp);

  return lp;
}

int Shell::help(Stream *strm,const char *pream,const char *format)
{
  int l = 0,
      i = 0;

  if (help_data) {

    if (pream) { strm->printf(pream,name); l++; }

    for (; help_data[i].command ; i++) {
      l++;
      strm->printf(format,help_data[i].command,help_data[i].comment);
    }
  }

  return l;
}

Shell *Shell::RootShell;
eWST   Shell::Waiting;

void CShell::interrupt()
{
  Shell *sp =  this;

  retry = 2;

  if (prompt && terminal) {
    terminal->printf("\n");
  }

  if (goto_lbll || use_line) {
    goto_lbll = 0;
    use_line  = 0;
    return;
  }

  switch (Waiting) {
  case WST_EXEC:  while (sp = sp->next) if (sp->proc) {
                    sp->proc->Kill(killsig);
                    goto next_kill;
                  }
                  return;
  case WST_BCKQ:
  case WST_SUBS:  KillSubs(killsig);
                  goto next_kill;
  }

next_kill:
  switch (killsig) {
  case SIGINT: killsig = SIGHUP;  break;
  case SIGHUP: killsig = SIGKILL; break;
  }
}

void CShell::sigcont()
{
  retry = 2;
}

void CShell::sigabrt()
{
  if (jump_buff) {
    SigTrap::Reset(SIGABRT);
    longjmp(jump_buff,ENOTCONN);
    // throw JumpBuff(jump_buff,ENOTCONN);
  }

  throw Exception(ENOTCONN);
}

void CShell::sigstop()
{
  Shell *scan;

  switch (Waiting) {
  case WST_FG:    if (proc) {
                    proc->Kill(SIGSTOP);
                    proc = 0;
                  }
                  break;
  case WST_EXEC:  for (scan = this; scan ; scan = scan->next) {
                    if (scan->proc) {
                      LogJob(scan->proc);
                      scan->proc->Kill(SIGSTOP);
                      scan->proc = 0;
                    }
                  }
                  break;
  default:        KillSubs(SIGSTOP); break;
  case WST_NONE:  raise(SIGSTOP);    break;
  }
}

void CShell::sigchild()
{
  int  n = 0;
  Job *pj;

  retry = 1;

  if (Waiting) {

    for (; n < sub_max ; n++) if (pj = subs[n]) {
      if (!pj->Busy() && pj->Zombie()) {
        subs[n] = 0;
        pj->Dead();
      }
    }

    if (notify) {
      for (; n < job_max ; n++) if (pj = jobs[n].proc) {
        if (!pj->Busy() && pj->Zombie()) {
          jobs[n].proc = 0;
          stdio[STDERR_FILENO]->printf("[%d]   %-30s %s\n",
                                       n+1,pj->StatStr(),pj->Command()->str());
          pj->Dead();
          retry = 2;
        }
      }
    }
  }
}

static SigDo csh_int(SigMap map,void *cp)
{
  ((CShell *)cp)->interrupt();

  return SGD_CONT;
}

static SigDo csh_cld(SigMap map,void *cp)
{
  ((CShell *)cp)->sigchild();

  return SGD_CONT;
}

static SigDo csh_stop(SigMap map,void *cp)
{
  ((CShell *)cp)->sigstop();

  return SGD_CONT;
}

static SigDo csh_cont(SigMap map,void *cp)
{
  ((CShell *)cp)->sigcont();

  return SGD_CONT;
}

static SigDo csh_abrt(SigMap map,void *cp)
{
  ((CShell *)cp)->sigabrt();

  return SGD_ABORT;
}

CShell::CShell(Shell **rp) : Shell()
{
  static ShellHelp csh_help[] = {
    {"cd",    "change directory"},
    {"echo",  "print arguments"},
    {"eval",  "re-evalute statement"},
    {"ldd",   "list loaded libraries"},
    {"ll",    "list libraries"},
    {"lpths", "show paths"},
    {"load",  "load library components"},
    {"jobs",  "list jobs"},
    {"kill",  "kill jobs"},
    {"set",   "set a variable"},
    {"setenv","set an environment variable"},
    {"unset", "unset a variable"},
    {"v2k",   "execute command line arguments"},
    {0,0}
  };

  priority  = SHP_CSH;
  cntxt     = SHC_COMMAND;
  flags     = SHF_NONE;
  exit_sts  = (eSTS)-1;
  name      = "csh";
  prompt    = 0;
  help_data = csh_help;
  src_name  = 0;
  src_line  = 0;
  terminal  = 0;
  slave     = 0;
  alss      = 0;
  relay     = 0;
  jobs      = 0;
  job_max   = 0;
  notify    = 0;
  glbd      = 0;
  glbd_sz   = 0;
  quote     = 0;
  arg_max   = 0;
  subs      = 0;
  sub_max   = 0;
  depth     = 0;
  level     = 0;
  lvls      = 0;
  lvl_max   = 0;
  use_line  = 0;
  goto_lbll = 0;
  goto_lbls = 0;
  history   = 0;
  h_root    = 0;
  goto_end  = -1;
  h_max     = -1;
  jump_buff = 0;
  status    = envAddFlg("status","0",ENV_LOCAL);

  addParser(rp);

  new SigTrap(SM_INT,csh_int, SA_NONE,this,this);
  new SigTrap(SM_CLD,csh_cld, SA_NONE,this,this);
  new SigTrap(SM_HUP,csh_stop,SA_NONE,this,this);
  new SigTrap(SM_CNT,csh_cont,SA_NONE,this,this);
  new SigTrap(SM_ABT,csh_abrt,SA_NONE,this,this);
}

CShell::~CShell()
{
  CHist    *hnxt = h_root,
           *hp;
  CshAlias *anxt = alss,
           *ap;

  unlink();

  while (hp = hnxt) {
    hnxt = hp->next;
    Free(hp);
  }
  while (ap = anxt) {
    anxt = ap->next;
    Free(ap);
  }
  FREE(goto_lbls);
  FREE(level);
  FREE(glbd);
  FREE(quote);
  FREE(jobs);
  FREE(subs);
  SigTrap::Traps->UnTrap(this);
}

ExecShell::ExecShell(Shell **rp) : Shell()
{
  priority = SHP_LAST;
  cntxt    = SHC_COMMAND;
  name     = "exec";
  glob     = true;

  addParser(rp);
}

#define IN     rdr[STDIN_FILENO]
#define OUT    rdr[STDOUT_FILENO]
#define SH_ERR std[STDERR_FILENO]

#ifdef DEBUG
# define DR_EXTRA(a) ,a
#else
# define DR_EXTRA(a)
#endif

static int DoRedirect(Stream **std,Stream **rdr,int do_dup
                      DR_EXTRA(const char **argv))
{
  int s,fd;

  if ((s = STDIN_FILENO)  != (fd = rdr[STDIN_FILENO]->Fd(STRM_IN))) {
    if (do_dup && dup2(fd,STDIN_FILENO) < 0) goto ioerr;
    rdr[STDIN_FILENO]->Close(STRM_OUT);
  }
  if ((s = STDOUT_FILENO) != (fd = rdr[STDOUT_FILENO]->Fd(STRM_OUT))) {
    if (do_dup && dup2(fd,STDOUT_FILENO) < 0) goto ioerr;
    rdr[STDOUT_FILENO]->Close(STRM_IN);
  }
  if ((s = STDERR_FILENO) != (fd = rdr[STDERR_FILENO]->Fd(STRM_OUT))) {
    if (do_dup && dup2(fd,STDERR_FILENO) < 0) goto ioerr;
    rdr[STDERR_FILENO]->Close(STRM_IN);
  }

  return 0;

ioerr:
#ifdef DEBUG
  fprintf(stderr,"DoRedirect (%s,%d,%d): %s\n",*argv,s,fd,strerror(errno));
#endif
  return errno;
}

static void FixRedirect(Stream **std,Stream **rdr) {
  if (rdr[STDIN_FILENO] &&
      STDIN_FILENO  != rdr[STDIN_FILENO]->Fd(STRM_IN)) {
    rdr[STDIN_FILENO]->Close(STRM_IN);
  }
  if (rdr[STDOUT_FILENO] &&
      STDOUT_FILENO != rdr[STDOUT_FILENO]->Fd(STRM_OUT)) {
    rdr[STDOUT_FILENO]->Close(STRM_OUT);
  }
  if (rdr[STDERR_FILENO] &&
      STDERR_FILENO != rdr[STDERR_FILENO]->Fd(STRM_OUT)) {
    rdr[STDERR_FILENO]->Close(STRM_OUT);
  }
}

int CShell::LogJob(Job *prc)
{
  int n = -1,
      j;

  while (++n < job_max) {
    if (!jobs[n].proc) {
      jobs[j = n].proc = prc;
      time(&jobs[n].bg_fg);
      goto done;
    }
  }

  job_max += 8;

  REALLOC2(jobs,job_max,CshJob);

  jobs[j = n].proc = prc;
  time(&jobs[n].bg_fg);

  while (++n < job_max) jobs[n].proc = 0;

 done:
  return last_job = j;
}

eSTS CShell::WaitJobs(int j)
{
  int   n   = 0;
  eSTS  sts = STS_NORMAL;

  for (; n < job_max && j > 0 ; n++) if (jobs[n].proc) {
    Waiting   = WST_JOBS;
    eSTS sts2 = jobs[n].proc->Status(0);
    Waiting   = WST_NONE;
    j--;
    if (STS_EINTR == sts2) break;
    delete(jobs[n].proc);
    jobs[n].proc = 0;
    if (sts2) sts = sts2;
  }

  return sts;
}

int CShell::FixTags(int *cl)
{
  int    n  = 0,
         c  = -1,
         l  = -1,
         j  = 0;
  time_t tc = 0,
         tl = 0;

  for (; n < job_max ; n++) if (jobs[n].proc) {
    j++;
    jobs[n].tag = ' ';
    if (jobs[n].bg_fg >= tc) {
      tl = tc;
      tc = jobs[n].bg_fg;
      l  = c;
      c  = n;
    } else if (jobs[n].bg_fg >= tl) {
      tl = jobs[n].bg_fg;
      l  = n;
    }
  }

  if (c >= 0) jobs[c].tag = '+';
  if (l >= 0) jobs[l].tag = '-';

  if (cl) {
    cl[0] = c;
    cl[1] = l;
  }

  return j;
}

int Shell::LogJob(Job *prc)
{
  /* No-op */
}

void Shell::LogSub(Job *prc)
{
  /* No-op */
}

void CShell::LogSub(Job *proc)
{
  int n = -1;

  while (++n < sub_max) {
    if (!subs[n]) { subs[n] = proc; return; }
  }

  sub_max += 8;

  REALLOC2(subs,sub_max,Job *);

  subs[n] = proc;

  while (++n < sub_max) subs[n] = 0;
}

void CShell::LogSub(int pid,int argc,const char **sargv)
{
  String exe;
  int    n = -1;

  for (exe = *sargv; --argc > 0 ;) {
    exe += " ";
    exe += *sargv;
  }

  LogSub(new Job(pid,exe.str()));
}

eSTS CShell::WaitSubs(eSTS sts0)
{
  int   n   = 0;
  eSTS  sts = sts0;

  for (; n < sub_max ; n++) if (subs[n]) {
    Waiting   = WST_SUBS;
    eSTS sts2 = subs[n]->Status();
    Waiting   = WST_NONE;
    delete(subs[n]);
    subs[n] = 0;
    if (sts2) sts = sts2;
  }

  return sts;
}

void CShell::KillSubs(int sig)
{
  int n = 0;

  for (; n < sub_max ; n++) if (subs[n]) {
    subs[n]->Kill(sig);
  }
}

void CShell::sortArgs(int cnt,const char **ap,char *sp,char *qp)
{
  int swaps = 1;

  while (swaps-- > 0) {
    int c = cnt-1;
    while (--c > 0) {
      const char *cp;
      if (strcmp(cp = ap[c],ap[c-1]) > 0) {
        ap[c]  = ap[c-1]; ap[c-1] = cp;
        int ch = sp[c];
        sp[c]  = sp[c-1]; sp[c-1] = ch;
        ch     = qp[c];
        qp[c]  = qp[c-1]; qp[c-1] = ch;
        swaps  = 1;
      }
    }
  }
}

int CShell::glob(StringArray &args,char *qutd,char *spc)
{
  int max  = 0,
      argc = args;

resize:
  int cnt  = argc,
      gc   = 0,
      l    = 0;

  for (; l < cnt ; l++) {
    int g  = qutd[l] ? 0
                     : fileGlob(args[l],0,&max,0);
    switch (g) {
    default: gc  += g;
    case 0:  break;
    case -1: return -1;
    }
  }

  if (gc) {

    int tot = 0,
        gsz = max*gc;

    if (gsz > glbd_sz && !(glbd = REALLOC(glbd,glbd_sz = gsz,char))) {
      return -2;
    }

    int   allow = glbd_sz/max;
    char *gp    = glbd;

    for (cnt = l = 0; l < argc ; l++) {
      int g  = qutd[l] ? 0
                       : fileGlob(args[l],gp,&max,&allow),
          c0 = cnt;

      if (cnt + g >= arg_max) extendArgs(g+1);

      switch (g) {
      case -2: goto resize;
      case -1: return -1;
      default: for (; g-- > 0 ; cnt++) {
                 quote[cnt]  = 0;
                 space[cnt]  = 1;
  	         argv[cnt]   = gp;
                 gp         += max;
               }
               tot += g;
               sortArgs(cnt-c0,&argv[c0],&space[c0],&quote[c0]);
               break;
      case 0:  tot++;
               space[cnt]  = spc[l];
               quote[cnt]  = qutd[l];
               argv[cnt++] = args[l];
      }
    }

    argv[cnt] = 0;
    cargs.set(argv,cnt);
  }

  return cnt;
}

typedef struct {
  const char *op;
  int         len;
  CSPR        prec;
  CSOP        opr;
  int         ra,
              blank;
} CshOp;

static CshOp CshOps[] = {{0,   1,CSPR_MAX, CSOP_NONE,  0,0},
                         {"-r",2,CSPR_FILE,CSOP_FL_R,  0,1},
                         {"-w",2,CSPR_FILE,CSOP_FL_W,  0,1},
                         {"-x",2,CSPR_FILE,CSOP_FL_X,  0,1},
                         {"-e",2,CSPR_FILE,CSOP_FL_E,  0,1},
                         {"-o",2,CSPR_FILE,CSOP_FL_O,  0,1},
                         {"-z",2,CSPR_FILE,CSOP_FL_Z,  0,1},
                         {"-f",2,CSPR_FILE,CSOP_FL_F,  0,1},
                         {"-d",2,CSPR_FILE,CSOP_FL_D,  0,1},
                         {"||",2,CSPR_LOR, CSOP_LOR,   0,0},
                         {"&&",2,CSPR_LAND,CSOP_LAND,  0,0},
                         {"==",2,CSPR_CMP, CSOP_EQU,   0,0},
                         {"!=",2,CSPR_CMP, CSOP_NEQU,  0,0},
                         {"=~",2,CSPR_CMP, CSOP_FEQU,  0,0},
                         {"!~",2,CSPR_CMP, CSOP_NFEQU, 0,0},
                         {"<<",2,CSPR_SHFT,CSOP_LSH,   0,0},
                         {"^", 1,CSPR_BXOR,CSOP_BXOR,  0,0},
                         {"&", 1,CSPR_BAND,CSOP_BAND,  0,0},
                         {"<", 1,CSPR_LGC, CSOP_LT,    0,0},
                         {"+", 1,CSPR_ADD, CSOP_ADD,   1,0},
                         {"-", 1,CSPR_ADD, CSOP_SUB,   1,0},
                         {"*", 1,CSPR_MULT,CSOP_MULT,  1,0},
                         {"/", 1,CSPR_MULT,CSOP_DIV,   1,0},
                         {"%", 1,CSPR_MULT,CSOP_MOD,   1,0},
                         {"!", 1,CSPR_NOT, CSOP_NOT,   0,0},
                         {"~", 1,CSPR_1CMP,CSOP_1CMP,  0,0},
                         {0,   1,CSPR_NONE,CSOP_NONE,  0,0}};

static void fixStr(char *buf)
{
  char *cp1 = buf,
       *cp2;
  int   ch;

  if ('"' == *cp1) {
    for (cp2 = buf; ch = *cp2 = *++cp1 ; cp2++) switch (ch) {
    case '\\': *cp2 = *++cp1; break;
    case '"':  *cp2 = 0;      return;
    }
  } else {
    for (cp2 = 0; ch = *cp1 ; cp1++) if (isspace(ch)) cp2 = cp1;
                                     else             cp2 = 0;
    if (cp2) *cp2 = 0;
  }
}

static int cshBool(char *expr,int *pval)
{
  int    n    = -1,
         i    = 0,
         br   = 0,
         esc  = 0,
         qu   = 0,
         l,
         r;
  char  *xp,
        *posn = 0,
        *bo   = 0,
        *bc,
        *lhs,
        *rhs;
  CshOp *pm   = CshOps;

  while(isspace(*expr)) expr++;

  for (xp = expr ; *xp ; xp++) {
    switch (*xp) {
    case '"':  if (!esc) qu = !qu;            break;
    case '\\': if (!esc) {esc = 1; continue;} break;
    case '(':  if (!esc && !qu) {
                 if (!bo) bo = xp;
                 br++;
               }
               break;
    case ')':  if (!esc && !qu) {
                 bc = xp;
                 br--;
               }
               break;
    default:   if (!esc && !br && !qu) {
                 CshOp *po = &CshOps[1];
                 for (i = 0 ; po->op ; po++) {
	           if (0 == strncmp(xp,po->op,po->len)) {
	             if (po->prec < pm->prec ||
                         (pm->ra && po->prec == pm->prec)) {
	               pm = po; posn = xp;
	             }
	           }
                 }
                 xp  += po->len -1;
               }
    }
    esc  = 0;
  }

  if (posn) {
    lhs   = expr;
    rhs   = posn + pm->len;
    *posn = 0;
    while (isspace(*lhs)) lhs++;
    while (isspace(*rhs)) rhs++;
  }

  switch (pm->opr) {
  default:        assert(("CSH OP NIY",0));

  case CSOP_NOT:  if (cshBool(rhs,&r) >= 0) {
                    n = 0 != (*pval = !r);
                  }
                  break;
  case CSOP_ADD:  if (cshBool(lhs,&l) >= 0 && cshBool(rhs,&r) >= 0) {
                    n = 0 != (*pval = l + r);
                  }
                  break;
  case CSOP_SUB:  if (cshBool(lhs,&l) >= 0 && cshBool(rhs,&r) >= 0) {
                    n = 0 != (*pval = l - r);
                  }
                  break;
  case CSOP_LOR:  if (cshBool(lhs,&l) >= 0 && cshBool(rhs,&r) >= 0) {
                    n = *pval = (l || r);
                  }
                  break;
  case CSOP_LAND: if (cshBool(lhs,&l) >= 0 && cshBool(rhs,&r) >= 0) {
                    n = *pval = (l && r);
                  }
                  break;
  case CSOP_EQU:  fixStr(lhs);
                  fixStr(rhs);
                  *pval = n = (0 == strcmp(lhs,rhs));
                  break;
  case CSOP_FL_R: if (!*lhs) {
                    fixStr(rhs);
                    *pval = n = (0 == access(rhs,R_OK));
                  }
                  break;
  case CSOP_FL_W: if (!*lhs) {
                    fixStr(rhs);
                    *pval = n = (0 == access(rhs,W_OK));
                  }
                  break;
  case CSOP_FL_X: if (!*lhs) {
                    fixStr(rhs);
                    *pval = n = (0 == access(rhs,X_OK));
                  }
                  break;
  case CSOP_FL_E: if (!*lhs) {
                    fixStr(rhs);
                    *pval = n = (0 == access(rhs,F_OK));
                  }
                  break;
  case CSOP_FL_O: if (!*lhs) {
                    struct stat buf;
                    fixStr(rhs);
                    n = 0;
                    if (0 == stat(rhs, &buf)) n = (getuid() == buf.st_uid);
                    *pval = n;
                  }
                  break;
  case CSOP_FL_Z: if (!*lhs) {
                    struct stat buf;
                    fixStr(rhs);
                    n = 0;
                    if (0 == stat(rhs, &buf)) n = (0 == buf.st_size);
                    *pval = n;
                  }
                  break;
  case CSOP_FL_F: if (!*lhs) {
                    struct stat buf;
                    fixStr(rhs);
                    n = 0;
                    if (0 == stat(rhs, &buf)) {
                      if (buf.st_mode & S_IFREG) n = 1;
                    }
                    *pval = n;
                  }
                  break;
  case CSOP_FL_D: if (!*lhs) {
                    struct stat buf;
                    fixStr(rhs);
                    n = 0;
                    if (0 == stat(rhs, &buf)) {
                      if (buf.st_mode & S_IFDIR) n = 1;
                    }
                    *pval = n;
                  }
                  break;
  case CSOP_NONE: if (bo == expr) {
                    for (xp = bc; *++xp;) if (!isspace(*xp)) goto done;
                    *bc = 0;
                    n = cshBool(bo +1,pval);
                  } else {
                    xp = expr;
                    while(isspace(*xp)) xp++;
                    if (isdigit(*(xp = expr))) {
                      do { i = (i * 10) + *xp++ - '0'; } while (isdigit(*xp));
                      while(isspace(*xp)) xp++;
                      if (!*xp) n = 0 != i;
                    }
                    *pval = i;
                  }
                  break;
  }

done:
  return n;
}

static int cshBool(int argc,const char **argv,const char *qutd)
{
  const char *cp;
  int         n    = -1,
              i    =  0,
              opw  =  1,
              l    =  1,
              esc  =  0;

  for (; i < argc ; i++) {cp = argv[i];
                          if (qutd[i]) {
                            l += 2;
                            while (*cp) switch (*cp++) {
                            case '"':  if (!esc) l++;
                            default:   esc = 0; break;
                            case '\\': if (!esc) esc = 1;
                            }
                            cp = argv[i];
                          }
                          l += 1 + strlen(cp);}
  if (argc) {
    TMPARR(char,xa,l);
    char  *xp  = xa;
    int    pad = 0;
    CshOp *pm  = CshOps;

    for (i = 0 ; i < argc ; i++,pad = ' ') {
	if (pad) *xp++ = pad;
      cp = argv[i];
      if (qutd[i]) {
        *xp++ = '"';
        while (*cp) {
          switch (*cp) {
          case '"':  if (!esc) *xp++ = '\\';
          default:   esc = 0; break;
          case '\\': esc = 1;
          }
          *xp++ = *cp++;
        }
        *xp++ = '"';
      } else {
	  strcpy(xp,argv[i]);
        xp += strlen(argv[i]);
      }
    }

    n = cshBool(xa,&i);
  }

  return n;
}

eSTS Shell::subsh_fork(int do_dup,int *pp,Stream **std,Stream **rdr,
                       int argc,const char **argv,eSHX shx)
{
  int s,
      pid = *pp = Fork(1);

  if (pid < 0) {
    return Error();
  } else if (pid > 0) {
    FixRedirect(std,rdr);
    proc = new Job(pid,argc,argv);
    if (shx & SHX_DETACH) LogJob(proc);
    else                  LogSub(proc);
  } else {
    if (s = DoRedirect(std,rdr,do_dup DR_EXTRA(argv))) _exit(s);
  }

  return STS_NORMAL;
}

int CShell::substJobs(int argc,const char **sargv)
{
  int bad = 0,
      l   = 0,
      cl[2];

  FixTags(cl);

  for (; l < argc ; l++) {
    const char *pp = sargv[l];

    if ('%' == *pp++) {
      const char   *p0 = pp;
      unsigned int  jid;
      while (isdigit(*pp)) pp++;
      if (!*pp) {
        if (1 == sscanf(p0,"%d",&jid)) {
	  if (--jid >= job_max || !jobs[jid].proc) {
            bad++;
	  } else {
	    sargv[l]  = jobs[jid].proc->StrPid();
	  }
        } else if (!*p0 && cl[0] >=0) {
          sargv[l] = jobs[cl[0]].proc->StrPid();
        } else {
          bad++;
        }
      } else if ('?' == *pp) {
      } else if ('-' == *pp && !pp[1] && cl[1] >=0) {
          sargv[l] = jobs[cl[1]].proc->StrPid();
      } else {
        bad++;
      }
    }
  }

  return !bad;
}


void CShell::extendArgs(int by)
{
  if (by < 40) {
     if (by <= 0) return;
     by = 40;
  }

  arg_max += by;

  if (arg_max < 100) arg_max = 100;

  int r    = arg_max%8;
  arg_max += 8-r;

  int  sz = arg_max * (sizeof(*space) + sizeof(*quote) + sizeof(*argv));

  REALLOC2(quote,sz + sizeof(void *),char);

  space = &quote[arg_max];
  argv  = (const char **)&space[arg_max];

  cargs.set(argv,cargs);

#if DBGLVL > 0
  void **pg = (void **)&argv[arg_max];
  *pg       = (void *)0xDEADBEEF;
#endif
}

const char *CShell::DefaultPrompt = "> ";

const char *CShell::setPrompt(const char *str) {
  if (str && prompt_buff != str) prompt_buff = str;
  return prompt = str ? prompt_buff.str()
                      : str;
}

const char *CShell::Prompt()
{
  Env env;

  if (prompt) return prompt;

  return DefaultPrompt;
}

void CShell::checkLvls() {
  if (lvls >= lvl_max) {
    int l = lvl_max;
    lvl_max += 10;
    REALLOC2(level,lvl_max,CBlock *);
    while (l < lvl_max) level[l++] = 0;
  }
}

void CShell::clearLvls() {
  while (lvls >= 0) {
    if (level[--lvls]->allc) Free(level[lvls]);
    level[lvls] = 0;
  }
}

#define ERR_LINE() {if (src_name) {SH_ERR->printf("%s:%d - ",src_name,src_line);}}

#define LOG_SUB    {if (pid) { LogSub(proc = new Job(pid,argc,argv));\
                               pid = 0;}}

#define GLOB(s,j)  {if (!glbd) switch (cnt = glob(cargs,quote,space)) {\
                    case -1: SH_ERR->printf("%s: No match.\n",s);\
                             sts  = ERROR(1); goto j;\
		    case -2: SH_ERR->printf("%s: Globbing failed.\n",s);\
                             sts  = Error(); goto j;\
                    default: argc = cnt; glbd = 1;}}

eSTS CShell::parse(int cnt,char const **ptk,char *spc,char *qutd,
                   Stream **std,Stream **rdr0,eSHX shx)
{
  TMPARR(String,args,cnt);
  TMPARR(Break,arge,cnt+1);
  TMPARR(const char *,argv0,cnt+1);
  StrStrArray  sargs(args,cnt);

  return parse(cnt,ptk,spc,qutd,std,rdr0,shx,args,arge,argv0,sargs);
}


eSTS CShell::parse(int cnt,char const **ptk,char *spc,char *qutd,
                   Stream **std,Stream **rdr0,eSHX shx,String *args,
                   Break *arge,const char **argv0,StrStrArray &sargs)
{
  const char *ccp,
             *argx  = 0,
             *arg,
             *cp,
             *pad;
  char       *wcp;
  int         argc0 = cnt,
              argc  = -1,
              l     = 0,
              echo  = flags & SHF_ECHO,
              verb  = flags & SHF_VERBOSE,
              ch,
              si,
              pid   = 0,
              glbd  = 0,
              xarg  = 0,
              xl    = 0;
  eSTS        sts   = STS_NORMAL;
  Shell      *scan;
  Stream      io[3],
             *rdr[3];
  CBlock     *blk;
  poolRef     nm;

  if (cnt >= arg_max) extendArgs(1 + cnt - arg_max);

  cargs.set(argv,cnt);

  BCOPY(rdr0,rdr,sizeof(rdr));

  for (pad = ""; l < cnt ; pad = " ",l++) {
    ccp = ptk[l];
    if (verb) SH_ERR->printf("%s%s ",pad,ccp);
    args[l]  = ccp;
    space[l] = spc[l];
    quote[l] = qutd[l];
    if (!ccp[1]) switch (*ccp) {
    case '&': if (l > 0 && 0 == strcmp(">",ptk[l-1])) {
                shx = SHX(shx|SHX_FOLDERR);
                rdr[STDERR_FILENO] = rdr[STDOUT_FILENO];
              } else if (shx & SHX_SUBSHL) {
                SH_ERR->printf("Inappropriate &.");
                goto syntax2;
              } else {
                shx = SHX(shx|SHX_SUBSHL|SHX_DETACH);
                if (argc < 0) argc = l;
              }
              break;
    case '>': if (argc < 0) argc = l;
              cp = "w";
              if (ptk[l+1] && 0 == strcmp(">",ptk[l+1])) {
                cp = "a";
                l++;
              }
              if (ptk[l+1] && !(shx & SHX_RDRCT_O)) {
                if (io[STDOUT_FILENO].Open(ptk[++l],cp) < 0) {
                  SH_ERR->printf("%s: %s\n",ptk[l],strerror(errno));
                  return Error();
                }
                shx = SHX(shx|SHX_RDRCT_O);
                rdr[STDOUT_FILENO] = &io[STDOUT_FILENO];
              } else {
                SH_ERR->printf("Ambiguous output redirect.");
                goto syntax2;
              }
              break;
    case '<': if (argc < 0) argc = l;
              if (ptk[l+1] && !(shx & SHX_RDRCT_I)) {
                if (io[STDIN_FILENO].Open(ptk[++l],"r") < 0) {
                  SH_ERR->printf("%s: %s\n",ptk[l],strerror(errno));
                  return Error();
                }
                shx = SHX(shx|SHX_RDRCT_I);
                rdr[STDOUT_FILENO] = &io[STDIN_FILENO];
              } else {
                SH_ERR->printf("Ambiguous input redirect.");
                goto syntax2;
              }
              break;
    }
    arge[l].expa = 0;
    arge[l].expi = 0;
    envExpand(&args[l],SH_ERR,SHF(SHF_TILDE|(flags & ~SHF_BSDECHO)),
              qutd[l] ? 0
                      : &arge[l].expa,
              &arge[l].expi);
    argv0[l] = argv[l] = args[l].str();
    if (arge[l].expa) {
      xarg += arge[l].expa -1;
      xl   += 1 + strlen(argv[l]);
    }
  }

  argv0[argc0] = 0;

  if (xarg) {

    extendArgs(1 + cnt + xarg - arg_max);

    char *px = MALLOC2_N(xl,char);
    argx     = px;

    for (argc = l = 0; l < cnt ; l++) {
      Break      *bp = &arge[l];
      int         y;
      const char *ap = args[l].str();
      if (y = bp->expa) {
        int il = 0;
        do {
          int         idx = bp->expi[il];
          const char *ap2 = &ap[idx];
          int         al  = (bp->expi[++il] - idx) -1;

          while (isspace(ap2[al-1])) al--;
          strncpy(px,ap2,al);
          quote[argc]   = 0;
          space[argc]   = il ? ' '
                             : spc[l];
          argv[argc++]  = px;
          px        += al;
          *px++      = 0;
        } while (il < y);
        Free(bp->expi);
      } else {
        quote[argc]  = qutd[l];
        space[argc]  = spc[l];
        argv[argc++] = ap;
      }
    }

    qutd  = quote;

  } else if (argc < 0) {

    argc = argc0;

  }

  cargs.set(argv,argc);
  argv[argc] = 0;

  if (verb) SH_ERR->printf("\n");

  if (alss) {
    CshAlias *as = alss;
    for (ccp = argv[0]; as ; as = as->next) if (0 == strcmp(as->name,ccp)) {
      String deref(as->value);
      StringStream eval(&deref);
      eval.printf("\n");
      SUBSH_FORK(0);
      if (!pid) {
	String   svd_prompt = prompt;
	Stream **svd_stdio  = stdio;

	prompt           = 0;
	eSTS sts         = start(&eval,IN,OUT,SH_ERR,0);
	setPrompt(svd_prompt);
	stdio = svd_stdio;
      }
      SUBSH_JOIN(STS_NORMAL);
      LOG_SUB;
      goto done;
    }
  }

  if (echo) {
    for (l = 0,pad = ""; l < argc ; pad = " ",l++) {
      SH_ERR->printf("%s%s ",pad,argv[l]);
    }
    SH_ERR->printf("\n");
  }

  InitLang(defPoolMode,0);

rescan:
  nm = strFind(argv[0]);

  switch (nm.pool) {
  case PUNCTUATION_POOL:
                      switch (nm.index) {
                      case PUNC_OPN_BR:
                        if (argc != 3 ||
                            !SAME_REF_PI(strFind(argv[2]),
                                         PUNCTUATION_POOL,PUNC_CLS_BR)) {
                          ERR_LINE();
                          SH_ERR->printf("Badly placed ()'s.\n");
                          sts = ERROR(1);
                          goto done;
                        } else {
                          String       estr(argv[1]);
                          StringStream eval(&estr);
                          eval.printf("\n");
                          shx = SHX(shx|SHX_SUBSHL);
                          SUBSH_FORK(0);
                          if (!pid) {
                            String   svd_prompt(prompt);
                            Stream **svd_stdio = stdio;

                            prompt   = 0;
                            eSTS sts = start(&eval,IN,OUT,SH_ERR,0);
                            setPrompt(svd_prompt);
                            stdio = svd_stdio;
                          }
                          SUBSH_JOIN(STS_NORMAL);
                          LOG_SUB;
                          goto done;
                        }
                      }
                      break;
  case COMMANDS_POOL: switch (nm.index) {
                      case SHL_FOREACH:
                        if (!use_line || !use_line->blk) {
			  if (argc < 4) {
			    ERR_LINE();
                            SH_ERR->printf("foreach: Words not parenthesized.\n");
			    goto syntax2;
			  } else if (0 != strcmp(argv[2],"(") ||
				     0 != strcmp(argv[argc-1],")")) {
			    ERR_LINE();
                            SH_ERR->printf("foreach: Words not parenthesized.\n");
			    goto syntax2;
			  } else {
			    GLOB("foreach",for_join);
			    EnvItem *pe = pe->add(argv[1],argv[3]);
			    int      i   = 4,
			             bsz = sizeof(CBlock) + sizeof(void *);
			    for (; i < argc - 1 ; i++) {
			      bsz += 1 + sizeof(void *) + strlen(argv[i]);
			    }

  			    blk            = (CBlock *)Malloc2(bsz);
			    blk->arg       = 0;
			    blk->line      = use_line ? use_line
                                                      : history;
                            blk->end       = 0;
                            blk->depth     = depth;
                            blk->line->blk = blk;
			    blk->var       = pe;
			    blk->stmt      = stmt;
			    blk->bt        = CSH_FOR;

 			    checkLvls();
			    level[lvls++] = blk;
			    wcp = (char *)&blk->argv[argc-4];
			    for (i = 4 ; i < argc - 1 ; i++) {
			      blk->argv[i-4] = wcp;
			      strcpy(wcp,argv[i]);
			      wcp += 1 + strlen(wcp);
			    }
			    cp             = wcp;
			    blk->argv[i-4] = 0;
			  }
			}
                      for_join:
                        goto done;
                      case SHL_HELP:
                        for(scan = this; scan ; scan = scan->next) {
                          scan->help(OUT,"%s::\n","  %-12s %s\n");
                        }
                        goto done;
                      case SHL_HISTORY:
                          SUBSH_FORK(0);
                          if (!pid) {
                            char   num[8];
                            int    n   = h_max,
                                   r   = 0,
                                   a   = 0,
                                   arg = 0,
                                   idx = 1;
                            while (++arg < argc) {
                              switch (ch = *(ccp = argv[arg])) {
                              case '-': while (ch = *++ccp) switch (ch) {
                                        case 'a': a   = 1; break;
                                        case 'r': r   = 1; break;
                                        case 'h': idx = 0; break;
			                default:  goto h_usage;
			                }
                                        break;
                              default:  if (isdigit(ch)) {
                                          n = 0;
                                          do {
                                            n = (n * 10) + ch - '0';
                                          } while (isdigit(ch = *++ccp));
                                          if (!ch) break;
                                        }
                              h_usage:  SH_ERR->printf("Usage: history [-rh]"
                                                       " [# number of events]\n");
                              }
                            }
                            int sl = history->line - n;
			    if (r) {
                              CHist *hp = history;
                              for (pad = ""; hp ; hp = hp->prev) {
                                if (a || hp->line > sl) {
				  int s = 0;
                                  if (idx) sprintf(num,"%6d  ",hp->line);
		 		  for (pad=num; s < hp->stmts ; s++,pad=";") {
				    OUT->printf("%s%s",pad,hp->stmt[s]);
				  }
				  OUT->printf("\n");
  			        }
			      }
                            } else {
                              CHist *hp = h_root;
                              for (; hp ; hp = hp->next) {
                                if (a || hp->line > sl) {
				  int s = 0;
                                  if (idx) {
				    sprintf(num,"%6d  ",hp->line);
				    pad = num;
				  }
                                  else pad = "";
		 		  for (; s < hp->stmts ; s++,pad=";") {
				    OUT->printf("%s%s",pad,hp->stmt[s]);
				  }
				  OUT->printf("\n");
  			        }
			      }
			    }
                          }
                          SUBSH_JOIN(STS_NORMAL);
                          LOG_SUB;
                        goto done;
                      case SHL_CD:
                        GLOB("cd",done);
                        ccp = argv[1];
                        switch(argc) {
                        case 1:  { Env home("HOME"); ccp = home; }
                        case 2:  if (!chdir(ccp)) break;
                                 sts = Error();
                                 SH_ERR->printf("%s: %s\n",ccp,strerror(errno));
                                 break;
                        default: SH_ERR->printf("cd: Too many arguments.\n");
                                 sts = ERROR(1);
                        }
                        goto done;
                      case SHL_KILL:
                        if (substJobs(argc-1,argv+1)) goto do_sub;
                        goto no_job;
                      case SHL_BG:
                        if (argc < 2) {
                          int cl[2];
                          if (!FixTags(cl)) goto no_curr;
                          pid = jobs[cl[0]].proc->Pid();
                          goto do_bg;
                        } else if (substJobs(argc-1,argv+1)) {
                          for (l = 1 ; l < argc ; l++) {
                            pid   = atoi(argv[l]);
                          do_bg:
                            int j = job_max;
                            while (j-- > 0) if (jobs[j].proc->Pid() == pid) {
                              break;
                            }
                            if (j < 0) goto no_job;
                            time(&jobs[j].bg_fg);
                            jobs[j].proc->Kill(SIGCONT);
                          }
                          goto done;
                        }
                        goto no_job;
                      case SHL_FG:
                        if (argc < 2) {
                          int cl[2];
                          if (!FixTags(cl)) goto no_curr;
                          pid = jobs[cl[0]].proc->Pid();
                          goto do_fg;
                        } else if (substJobs(argc-1,argv+1)) {
                          for (l = 1 ; l < argc ; l++) {
                            pid   = atoi(argv[l]);
                          do_fg:
                            int j = job_max;
                            while (j-- > 0) if (jobs[j].proc->Pid() == pid) {
                              break;
                            }
                            if (j < 0) goto no_job;
                            time(&jobs[j].bg_fg);
                            proc = jobs[j].proc;
                            proc->Kill(SIGCONT);
                            do {
                              Waiting = WST_FG;
                              sts     = proc->Status(0);
                              Waiting = WST_NONE;
                            } while (proc && !proc->Zombie());
                          }
                          goto done;
                        }
                      no_job:
                        ERR_LINE();
                        SH_ERR->printf("%s: No such job.\n",*argv);
                        sts = ERROR(1);
                        goto done;
                      no_curr:
                        ERR_LINE();
                        SH_ERR->printf("%s: No current job.\n",*argv);
                        sts = ERROR(1);
                        goto done;
                      case SHL_JOBS: {
                          SUBSH_FORK(0);
                          if (!pid) {
                            int n = 0;
                            FixTags(0);
                            for (; n < job_max ; n++) {
                              Job *job = jobs[n].proc;
                              if (job) {
                                if (job->Zombie()) {
                                  job->Status();
                                } else {
                                  OUT->printf("[%d] %c %-30s %s\n",
                                              n+1,jobs[n].tag,job->StatStr(),
                                              job->Command()->str());
                                }
                              }
                            }
                          }
                          SUBSH_JOIN(STS_NORMAL);
                          LOG_SUB;
                        }
                        goto done;
                      case SHL_ECHO:
                        SUBSH_FORK(0);
                        if (!pid) {
                          int nl  = 1,
                              any = 0;
                          if (flags & SHF_BSDECHO) {
			    for (l = 0; l < argc ; l++) {
			      ccp = ptk[l];
			      args[l] = ccp;
			      envExpand(&args[l],SH_ERR,
                                        SHF(SHF_BSDECHO|SHF_TILDE));
			    }
			  }
			  GLOB("echo",echo_join);
			  for (l = 1;
                               l < argc && (arg = argv[l]) && '-' == *arg ;
                               l++) {
                            while (ch = *++arg) switch (ch) {
                            case 'n': nl = 0; break;
                            }
			  }
			  for (pad = ""; l < argc ; pad = " ") {
			    if (!space[l]) pad = "";
                            any += quote[l];
			    any += OUT->printf("%s%s",pad,argv[l++]);
			  }
			  if (any && nl) OUT->printf("\n");
			echo_join:;}
                        SUBSH_JOIN(STS_NORMAL);
                        LOG_SUB;
                        goto done;
                      case SHL_DLD:
                        SUBSH_FORK(0);
                        if (!pid) {
			  GLOB("dld",dld_join);
                          for (l = 1; arg = argv[l++] ;) {
                            DynObj *dp = new DynObj(arg);
                            if (!*dp) { sts = Error(); }
                          }
			dld_join:;}
                        SUBSH_JOIN(STS_NORMAL);
                        LOG_SUB;
                        goto done;
                      case SHL_DULD:
                        SUBSH_FORK(0);
                        if (!pid) {
			  GLOB("dld",duld_join);
                          for (l = 1; arg = argv[l++] ;) {
                            DynObj *dp = 0;
                            if (dp->unload(arg)) { sts = Error(); }
                          }
			duld_join:;}
                        SUBSH_JOIN(STS_NORMAL);
                        LOG_SUB;
                        goto done;
                      case SHL_EVAL:
                        { String       estr;
                          StringStream eval(&estr);
                          GLOB("eval",eval_done);
                          for (si = 1,l = 1,pad = "";
                               arg = argv[l] ;
                               pad = " ") {
                            if (!space[l++]) pad = "";
                            eval.printf("%s%s",pad,arg);
                          }
                          eval.printf("\n");
                          SUBSH_FORK(0);
                          if (!pid) {
                            String   svd_prompt = prompt;
                            Stream **svd_stdio  = stdio;

                            prompt   = 0;
                            eSTS sts = start(&eval,IN,OUT,SH_ERR,0);
                            setPrompt(svd_prompt);
                            stdio = svd_stdio;
                          }
                          SUBSH_JOIN(STS_NORMAL);
                          LOG_SUB;
                        eval_done:
                          goto done;
                        }
                      case SHL_V2K: {
                          int          tmp;
                          const char **pargv;
                          SUBSH_FORK(0);
                          GLOB("v2k",v2k_join);
                          {
			    int        sjr;
                            SysJmpBuf *old_jb = jump_buff;
                            jmp_buf    jb;
                            ErrorControl err   = ErrControl;
                            BZEROS(ErrControl);
                            ErrControl.give_up = GIVE_UP;
                            ErrControl.mode    = EMD_SCRIPT;
                            tmp                = cnt;
                            pargv              = argv;
                            if (sjr = SetJmp(jump_buff = (SysJmpBuf *)&jb)) {
			      sts = ERROR(sjr);
			    } else {
			      try {
                                sts            = argPrcssArgs(&tmp,&pargv);
			      }
 			      catch (Exception x) {
			        sts = x.sts;
			      } 
			    }
                            jump_buff = old_jb;
                            ErrControl         = err;
                            SH_ERR->printf("%s\n",StrError(sts));
                          }
		        v2k_join:
                          SUBSH_JOIN(STS_NORMAL);
                          LOG_SUB;
                          goto done;
                        }
                      case SHL_SOURCE:
                        SUBSH_FORK(0);
                        GLOB("source",src_join);
                        if (!pid) {
                          if (argc > 1) {
                            Stream src;

                            if (src.Open(argv[1],"r")) {
                              const char **svd_argv     = argv,
				          *svd_src_name = src_name;
                              String       svd_prompt   = prompt;
                              char        *svd_space    = space,
                                          *svd_quote    = quote;
                              CHist       *svd_use_line = use_line,
                                          *svd_h_root   = h_root,
                                          *svd_history  = history,
                                          *hp,
                                          *hn;
                              int          svd_src_line = src_line,
                                           svd_arg_max  = arg_max,
                                           svd_stmt     = stmt;
                              Stream     **svd_stdio    = stdio;

                              src_line = 0;
                              src_name = svd_argv[1];
                              h_root   = 0;
                              use_line = 0;
                              history  = 0;
                              stmt     = 0;
                              argv     = 0;
                              arg_max  = 0;
                              prompt   = 0;
                              quote    = 0;
                              space    = 0;
                              depth++;
                              sts      = start(&src,rdr[STDIN_FILENO],
                                                    rdr[STDOUT_FILENO],
                                                    rdr[STDERR_FILENO],-1);
                              FREE(quote);
                              for (hp = h_root; hp ; hp = hn) {
                                hn = hp->next;
                                Free(hp);
                              }
                              src.Close();
                              src_name = svd_src_name;
                              src_line = svd_src_line;
                              argv     = svd_argv;
                              quote    = svd_quote;
                              space    = svd_space;
                              arg_max  = svd_arg_max;
                              use_line = svd_use_line;
                              h_root   = svd_h_root;
                              history  = svd_history;
                              stdio    = svd_stdio;
                              stmt     = svd_stmt;
                              setPrompt(svd_prompt);
                            } else {
                              ERR_LINE();
                              SH_ERR->printf("%s: %s\n",argv[1],strerror(errno));
                              sts = Error();
                            }
                          } else {
                            ERR_LINE();
                            SH_ERR->printf("source: Too few arguments.\n");
                            sts = ERROR(1);
                          }
                        }
                      src_join:
                        SUBSH_JOIN(STS_NORMAL);
                        LOG_SUB;
                        goto done;
                      case SHL_ENV:
                        SUBSH_FORK(0);
                        if (!pid) {
                          const EnvItem *pe = pe->frst();
                          for (; pe ; pe = pe->nxt()) {
                            if (pe->flgs() & ENV_EXPORT) {
                              OUT->printf("%s\n",pe->nm_vl());
                            }
                          }
                        }
                        SUBSH_JOIN(STS_NORMAL);
                        LOG_SUB;
                        goto done;
                      case SHL_GOTO:
                        if (argc > 2) {
                          ERR_LINE();
                          SH_ERR->printf("goto: Too many arguments.\n");
                          sts = ERROR(1); goto done;
                        }
                        if (2 > argc) {
                          ERR_LINE();
                          SH_ERR->printf("goto: Too few arguments.\n");
                          sts = ERROR(1); goto done;
                        }
                        SUBSH_FORK(0);
                        if (!pid) {
                          CHist *hp = h_root;
                          int    ll = strlen(argv[1]);

                          for (; hp ; hp = hp->next) {
                            if (hp->lbll == ll &&
                                0 == strncmp(hp->stmt[hp->lbls],argv[1],ll)) {
                              use_line = hp;
                              use_stmt = hp->lbls;
                              goto g_join;
			    }
			  }

                          if (shx & SHX_SUBSHL) {
                            ERR_LINE();
                            SH_ERR->printf("%s: label not found.\n",argv[1]);
                            sts = ERROR(1); goto done;
                          } else {
                            FREE(goto_lbls);
                            strcpy(goto_lbls = MALLOC2_N(ll +1,char),argv[1]);
                            goto_lbll = ll;
                          }
                        }
                      g_join:
                        SUBSH_JOIN(STS_NORMAL);
                        LOG_SUB;
                        goto done;
                      case SHL_NICE:
                        if (argc > 1) goto do_sub;
#ifndef NO_NICE
                        nice(10);
#endif
                        goto done;
                      case SHL_SETENV:
                        switch (argc) {
                          EnvItem *env;
		        case 3:
                          env->add(argv[1],argv[2],ENV_EXPORT);
                          goto done;
		        case 2:
                          env->add(argv[1],"",ENV_EXPORT);
                          goto done;
                        case 1:
                          argv[0] = "env";
                          goto do_sub;
                        default:
                          ERR_LINE();
                          SH_ERR->printf("setenv: Too many arguments.\n");
                        }
                        goto syntax2;
                      case SHL_SET: {
  			  if (1 == argc) {
                            SUBSH_FORK(0);
                            if (!pid) {
                              const EnvItem *pe = pe->frst();
                              for (; pe ; pe = pe->nxt()) {
                                if (!(pe->flgs() & ENV_EXPORT)) {
                                  OUT->Write(pe->nm_vl(),pe->nm_ln());
                                  const char *vp = pe->Value();
                                  if (pe->array()) OUT->printf("\t(%s)\n",vp);
                                  else             OUT->printf("\t%s\n",vp);
                                }
                              }
                            }
                            SUBSH_JOIN(STS_NORMAL);
                            LOG_SUB;
                            goto done;
                          } else for (l = 1; l < argc ; l++) {
                            EnvItem     *env = 0;
                            const char **val = 0,
                                        *nam = argv[l++];
                            nm               = strFind(nam);
                            int          ea  = 0;
                            if (argc > l && 0 == strcmp("=",argv[l])) {
                              val = &argv[++l];
                              if (++l < argc && 0 == strcmp("(",*val)) {
                                val++;
                                while (l < argc && strcmp(")",argv[l])) {
                                  ea++; l++;
                                }
                              } else {
                                ea = -1;
                              }
                            }
                            switch (nm.pool) {
                            case COMMANDS_POOL:
                              switch (nm.index) {
                              case SHL_VERBOSE:
                                flags  = SHF(flags|SHF_VERBOSE);
                                goto add_var;
                              case SHL_ECHO:
                                flags  = SHF(flags|SHF_ECHO);
                                goto add_var;
                              case SHL_ECHO_STYLE:
                                if (ea && 0 == strcmp(*val,"bsd")) {
                                  flags  = SHF(flags|SHF_BSDECHO);
                                } else {
                                  flags  = SHF(flags&~SHF_BSDECHO);
                                }
                                goto add_var;
                              case SHL_PROMPT:
                                setPrompt(env->add(nam,ea,val,ENV_LOCAL)->Value());
                                break;
                              case SHL_PATH:
                                env = env->add(nam,ea,val,ENV_LOCAL);
                                env->expEnv("PATH");
                                break;
                              case SHL_NOTIFY:
                                notify = 1;
                                goto add_var;
                              case SHL_HISTORY:
                                if (1 != sscanf(*val,"%d",&h_max)) {
                                  h_max = -1;
                                }
                                break;
                              default:
                                goto add_var;
                              }
                              break;
                            default:
                            add_var:
                              env->add(nam,ea,val,ENV_LOCAL);
                            }
                          }
                          goto done;
                        }
                        goto syntax;
                      case SHL_ALIAS:
                        if (argc < 2) {
                          CshAlias *ap = alss;
                          for (; ap ; ap = ap->next) {
                            OUT->printf("%s\t%s\n",ap->name,ap->value);
                          }
                        } else if (2 == argc) {
                          CshAlias *ap = alss;
                          for (; ap ; ap = ap->next) {
                            if (0 == strcmp(ap->name,argv[1])) {
                              OUT->printf("%s\t%s\n",ap->name,ap->value);
                              break;
                            }
                          }
                        } else {
                          CshAlias **app = &alss,
                                    *ap;
                          for (; ap = *app ; app = &ap->next) {
                            int s = strcmp(ap->name,argv[1]);
                            if (s >= 0) {
                              if (0 == s) {
                                *app = ap->next;
                                Free(ap);
                              }
                              break;
                            }
                          }
                          int c = 1,
                              l = 0;
                          while (c < argc) l += 1 + strlen(argv[c++]);
                          ap = (CshAlias *)MALLOC2_N(l + sizeof(*ap),char);
                          ap->next = *app;
                          *app     = ap;
                          char *vp = ap->value;
                          for (c = 2 ;;) {
                            strcpy(vp,argv[c++]);
                            vp += strlen(vp);
                            if (c >= argc) break;
                              *vp++ = ' ';
                          }
                          *vp++ = 0;
                          strcpy(ap->name = vp,argv[1]);
                        }
                        goto done;
                      case SHL_UNALIAS:
                        if (argc < 2) {
                          ERR_LINE();
                          SH_ERR->printf("unalias: Too few arguments.\n");
                        } else {
                          int c = 0;
                          while (++c < argc) {
                            CshAlias **app = &alss,
                                      *ap;
                            for (; ap = *app ; app = &ap->next) {
                              if (0 == strcmp(ap->name,argv[c])) {
                                *app = ap->next;
                                Free(ap);
                                break;
                              }
                            }
                          }
                        }
                        goto done;
                      case SHL_SHIFT:
                        if (argc < 2) {
                          ccp = "argv"; goto shift;
                        } else if (argc == 2) {
                          ccp = argv[1];
                        shift:
                          Env e(ccp);
                          if (true == e) {
                            e.Shift();
                            nm = strFind(ccp);
                            switch (nm.pool) {
                            case COMMANDS_POOL:
                              switch (nm.index) {
                              case SHL_PATH: e.Item()->expEnv("PATH");
                              }
                            }
                          } else {
                            ERR_LINE();
                            SH_ERR->printf("%s: Undefined variable.\n",ccp);
                            sts = ERROR(1);
                          }
                        } else {
                          ERR_LINE();
                          SH_ERR->printf("shift: Too many arguments.\n");
                          sts = ERROR(1);
                        }
                        goto done;
                      case SHL_UNLIMIT:
                        for (l = 0 ; Limits[l].name ; l++) {
			  if (Limits[l].lim >= 0 &&
			      (argc < 2 ||
			       0 == strcmp(Limits[l].name,argv[1]))) {
			    I64 val = -1;
			    setRlimit(Limits[l].lim,val);
			    if (argc > 1) goto done;
			  }
                        }
                        goto no_limit;
                      case SHL_LIMIT:
                        if (argc > 2) {
                          for (l = 0 ; Limits[l].name ; l++) {
                            if (Limits[l].lim >= 0 &&
                                0 == strcmp(Limits[l].name,argv[1])) {
                              int         scl = Limits[l].byt,
                                          v   = -1;
                              const char *sp  = argv[2];
                              if (!scl) scl = 1;
                              if (0 == strcmp("unlimited",sp)) {
                                sp = "";
                              } else {
                                if (1 != sscanf(sp,"%d",&v)) goto bad_limit;
                              }
                              while (isdigit(*sp)) sp++;
                              if (*sp || (sp = argv[3])) {
                                if (0 == strcasecmp("k",sp)) {
                                  scl = 0x400;
                                } else if (0 == strcasecmp("m",sp)) {
                                  scl = 0x100000;
                                } else {
                                bad_limit:
                                  ERR_LINE();
                                  SH_ERR->printf("limit: Improper or unknown scale factor.\n");
                                  goto done;
                                }
                              }
                              I64 val = scl * v;
                              setRlimit(Limits[l].lim,val);
                              goto done;
                            }
                          }
                          goto no_limit;
                        } else {
                          for (l = 0 ; Limits[l].name ; l++) {
                            if (Limits[l].lim >= 0 &&
                                (argc < 2 ||
                                    0 == strcmp(Limits[l].name,argv[1])))
                            {
                              OUT-printf("%-16s",Limits[l].name);
                              I64   val;
                              char *u   = "";
                              int   lmt = getRlimit(Limits[l].lim,&val);
                              if (lmt) {
                                if (Limits[l].byt) {
                                  if (val/0x400 > 100)     {val >>= 10;
                                                            u     = " kbytes";}
                                }
                                lmt = val;
                                OUT-printf("%d%s\n",lmt,u);
			      } else {
                                OUT-printf("unlimited\n");
                              }
                              if (argc == 2) goto done;
			    }
                          }
                          if (!Limits[l].name && argc != 1) goto no_limit;
                        }
                        goto done;
                      no_limit:
                        ERR_LINE();
                        SH_ERR->printf("limit: No such limit.\n");
                        goto done;
                      case SHL_UNSETENV:
                      case SHL_UNSET:
                        for (l = 1; l < argc ; l++) {
                          Env e(argv[l]);
                          e.destroy();
                          nm = strFind(argv[l]);
                          switch (nm.pool) {
                          case COMMANDS_POOL:
                            switch (nm.index) {
                            case SHL_VERBOSE:
                              flags  = SHF(flags&~SHF_VERBOSE); break;
                            case SHL_ECHO:
                              flags  = SHF(flags&~SHF_ECHO);    break;
                            case SHL_ECHO_STYLE:
                              flags  = SHF(flags&~SHF_BSDECHO); break;
                            case SHL_HISTORY:
                              h_max  = -1;                      break;
                            case SHL_NOTIFY:
                              notify = -1;                      break;
                            case SHL_PROMPT:
                              setPrompt(0);                     break;
                            }
                          }
                        }
                        goto done;
                      case SHL_LL:
                        SUBSH_FORK(0);
                        if (!pid) {
                          if (!forLibs(&argv[1],OUT,SH_ERR,printFile)) {
                            ERR_LINE();
                            SH_ERR->printf("ll:  No match.\n");
                          }
                        }
                        SUBSH_JOIN(STS_NORMAL);
                        LOG_SUB;
                        goto done;
                      case SHL_LPTHS:
                        SUBSH_FORK(0);
                        if (!pid) {
                          int      l = 0;
                          Library *plib;
			  OUT->printf("Libraries\n");
                          for (; plib = plib->libLst_map(l) ; l++) {
                             OUT->printf(" %s\t%s\n",plib->Name(),
                                                     plib->Path());
                          }
                        }
                        SUBSH_JOIN(STS_NORMAL);
                        LOG_SUB;
                        goto done;
                      case SHL_LOAD:
                        if (!pid) {
                          if (!forLibs(&argv[1],OUT,SH_ERR,loadFile)) {
                            ERR_LINE();
                            SH_ERR->printf("load:  No match.\n");
                          }
                        }
                        goto done;
                      case SHL_LDD:
                        SUBSH_FORK(0);
                        if (!pid) forLoaded(&argv[1],OUT,SH_ERR,printObj);
                        SUBSH_JOIN(STS_NORMAL);
                        LOG_SUB;
                        goto done;
                      case SHL_EXEC:
                        shx = SHX(shx|SHX_EXEC);
                        sts = ESH->parse(argc0-1,&argv0[1],\
					 space,qutd, std,rdr,shx);
                        goto done;
                      case SHL__EXIT:
                        goto done;
                      case SHL_EXIT:
                        l = 0;
                        switch (argc) {
                        case 1:  l = 0;
                                 break;
                        case 2:  if (isdigit(*ccp)) {
                                   for (ccp = argv[1]; isdigit(ch = *ccp++);) {
                                      l = l*10 + ch - '0';
			           }
                                   if (!ch) break;
			         }
                        default: ERR_LINE();
                                 SH_ERR->printf("exit: Expression Syntax.\n");
                                 goto syntax2;
                        }
                        sts = ERROR(l);
			if (SHL_EXIT == nm.index) exit_sts = sts;
                        goto done;
                      case SHL_ENDIF:
			if (lvls) {
			  blk = level[lvls-1];
                          switch (blk->bt) {
                          default:       goto no_if;
                          case CSH_ELSE:
                          case CSH_IF_:
                          case CSH_IF:   break;
                          }
                          level[--lvls] = 0;
                          if (blk->line) blk->line->blk = 0;
                        } else {
                        no_if:
			  ERR_LINE();
                          SH_ERR->printf("endif: Not in if.\n");
			}
			goto done;
                      case SHL_BREAKSW:
			if (lvls) {
                          int lvl = lvls -1;
			  for (; blk = level[lvl] ; lvl--) {
                            switch (blk->bt) {
                            default:         if (!lvl) goto no_sw2;
                                             continue;
                            case CSH_CASE_:
                            case CSH_SWITCH: goto_end  = lvl;
                                             strt_stmt = CSH_SKIP2ESW;
                                             goto breaksw_ok;
                            }
                          }
                        } else {
                        no_sw2:
			  ERR_LINE();
                          SH_ERR->printf("breaksw: Not in switch.\n");
                          sts = ERROR(1);
			}
                      breaksw_ok:
			goto done;
                      case SHL_ENDSW:
			if (lvls) {
			  blk = level[lvls-1];
                          switch (blk->bt) {
                          default:         goto no_sw;
                          case CSH_SWITCH:
                          case CSH_CASE_:  break;
                          }
                          level[--lvls] = 0;
                          if (blk->line) blk->line->blk = 0;
                        } else {
                        no_sw:
			  ERR_LINE();
                          SH_ERR->printf("endsw: Not in switch.\n");
                          sts = ERROR(1);
			}
			goto done;
                      case SHL_SWITCH:
                        GLOB("switch",sw_join);
                        if (argc < 5 && 0 == strcmp("(",argv[1])
                                     && 0 == strcmp(")",argv[3])) {
                          if (argc == 1) {
                            SH_ERR->printf("switch: Too few arguments.\n");
                            sts = ERROR(1);
                            goto sw_join;
                          }

  			  blk            = (CBlock *)Malloc2(sizeof(*blk) + 1 +
                                                             sizeof(char *) +
                                                             strlen(argv[2]));
                          blk->allc      = 1;
			  blk->argv[0]   = (char *)&blk->argv[1];
                          strcpy(blk->argv[0],argv[2]);
		          blk->line      = use_line ? use_line
                                                    : history;
                          blk->line->blk = blk;
		          blk->stmt      = stmt;
		          blk->bt        = CSH_SWITCH;
                          goto_end       = lvls;
                          strt_stmt      = CSH_SKIP2CS;

                          checkLvls();
                          level[lvls++] = blk;

                        } else {
                        sw_syntax:
                          ERR_LINE();
                          SH_ERR->printf("Syntax Error.\n");
                          sts = ERROR(1);
                        }
                      sw_join:
                        goto done;
                      }
                      break;
  case VERILOG_POOL:{ int elsif = 0;
                      switch (nm.index) {
                      case VER_ELSE:
			if (lvls) {
                          if (argc > 1) {
                            nm    = strFind(argv[1]);
                            elsif = SAME_REF_PI(nm,VERILOG_POOL,VER_IF);
                          }
			  blk = level[lvls-1];
                          switch (blk->bt) {
                          default:       goto no_if2;
                          case CSH_IF:   goto_end  = lvls-1;
                                         strt_stmt = CSH_SKIP2EIF;
                                         break;
                          case CSH_IF_:  if (elsif) goto as_if;
                                         goto_end  = -1;
                                         strt_stmt =  0;
                          }
                          level[lvls-1] = &ElsBlk;
		        } else {
                        no_if2:
			  SH_ERR->printf("else: Not in if.\n");
		        }
                        goto done;
                      case VER_CASE:
			if (lvls) {
                          int l = 0;
                          TMPARR(char,cs,argc > 1 ? (l = strlen(argv[1])) +1
                                                  : 4);
			  blk = level[lvls-1];
                          if (l) {
                            strcpy(cs,argv[1]);
                            if (':' == cs[--l]) cs[l] = 0;
                          }
                          switch (blk->bt) {
                          default:         goto no_case;
                          case CSH_SWITCH: if (argc > 1 &&
                                               wildCmp(cs,blk->argv[0])) {
                                             goto_end  = -1;
                                             strt_stmt = 0;
                                             blk->bt   = CSH_CASE_;
			                   } else {
                                             goto_end  = lvls-1;
                                             strt_stmt = CSH_SKIP2CS;
			                   }
                          case CSH_CASE_:  break;
                          }
		        } else {
                        no_case:
			  ERR_LINE();
                          SH_ERR->printf("else: Not in switch.\n");
		        }
                        goto done;
                      as_if:
                      case VER_IF:
                        GLOB("if",if_join);
                        if (argc > 2 && 0 == strcmp("then",argv[argc-1])) {
 			  if (!elsif) checkLvls();
                          switch (cshBool(argc-(2+elsif),
                                          &argv[1 + elsif],&quote[1])) {
                  	  case 1:  if (elsif) {
                                     goto_end  = -1;
                                     strt_stmt =  0;
			           } else {
                                     level[lvls++] = &IfBlk;
                                   }
                                   break;
                          case 0:  if (elsif) {
                                     goto_end      = lvls-1;
			           } else {
                                     goto_end      = lvls;
                                     level[lvls++] = &IfBarBlk;
                                   }
                                   strt_stmt = CSH_SKIP2EE;
                                   break;
                          case -1: ERR_LINE();
                                   SH_ERR->printf("if: Badly formed number\n");
                                   sts = ERROR(1);
                                   goto done;
                          }
                        } else {
                          if (0 == strcmp(argv[1],"(")) {
                            int cls = 2,
                                br  = 1;
                            for (; br > 0 ; cls++) {
                              if (argc <= cls) break;
                              if (0 == strcmp(argv[cls],")")) {
                                if (2 == cls) break;
                                switch (cshBool(cls - 2,
                                                &argv[2],&quote[2])) {
                                case -1: break;
                                case  0: goto inl_ok;
                                case  1: int c = 0;
                                         for (; ++cls <= argc ; c++) {
                                           argv[c] = argv[cls];
                                           qutd[c] = qutd[cls];
                                           spc[c]  = spc[cls];
				         }
                                         argv[argc = c - 1] = 0;
                                         goto rescan;
                                }
                              }
                            }
                            ERR_LINE();
                            SH_ERR->printf("if: Expression Syntax - NIY.\n");
                            sts = ERROR(1);
                            inl_ok:;
                          } else {
                          if_syntax:
                            ERR_LINE();
                            SH_ERR->printf("if: Expression Syntax.\n");
                            sts = ERROR(1);
                          }
                        }
                      if_join:
                        goto done;
                      case VER_BREAK:
			if (lvls) {
                          int lvl = lvls -1;
			  for (; blk = level[lvl] ; lvl--) {
                            switch (blk->bt) {
                            default:         if (!lvl) goto no_loop2;
                                             continue;
                            case CSH_WHILE:
                            case CSH_FOR:    goto_end  = lvl;
                                             strt_stmt = CSH_SKIP2ELP;
                                             goto break_ok;
                            }
                          }
                        } else {
                        no_loop2:
			  ERR_LINE();
                          SH_ERR->printf("endif: Not in switch.\n");
			}
                      break_ok:
			goto done;
                      case VER_END:
			if (lvls) {
			  blk = level[lvls-1];
                          switch (blk->bt) {
                          case CSH_WHILE:  blk->end = use_line;
                                           use_line = blk->line;
                      		           use_stmt = blk->stmt;
                                           break;
			  case CSH_FOR:    if (cp = blk->argv[blk->arg++]) {
		                             blk->var->change(cp);
                                             blk->end = use_line;
		                             use_line = blk->line;
                      		             use_stmt = blk->stmt;
                                             break;
			                   }
                          case CSH_WHILE_: level[--lvls]  = 0;
                                           if (blk->line) blk->line->blk = 0;
			                   if (blk->allc) Free(blk);
    			                   break;
			  default:         goto no_loop;
                          }
			} else {
                        no_loop:
			  ERR_LINE();
                          SH_ERR->printf("end: Not in while/foreach.\n");
			}
			goto done;
                      case VER_WAIT:
                        sts = WaitJobs();
                        goto done;
                      case VER_WHILE:
                        GLOB("while",whl_join);
                        if (use_line && (blk = use_line->blk)) {
                          switch (cshBool(blk->args,argv  + blk->arg,
                                                    quote + blk->arg)) {
                          default: ERR_LINE();
                                   SH_ERR->printf("while: Badly formed number\n");
                                   sts = ERROR(1);
                          case 0:  blk->bt        = CSH_WHILE_;
                                   goto_end       = lvls -1;
                                   if (blk->end) {
                                     use_line     = blk->end;
                                     use_stmt     = 0;
                                   }
                                   strt_stmt      = CSH_SKIP2ELP;
                                   use_line->blk  = 0;
                          case 1:  break;
                          }
                        } else {
                          int n  = argc-1,
                              ai = 1;

			  if (argc < 2) {
			    ERR_LINE();
                            SH_ERR->printf("while: Too few arguments.\n");
			    goto syntax2;
			  }

                          if (0 == strcmp(argv[1],"(")) {
			    if (0 != strcmp(argv[argc-1],")")) {
                            whl_syntax:
			      ERR_LINE();
                              SH_ERR->printf("while: Expression Syntax.\n");
			      goto syntax2;
                            }
                            n -= 2;
                            ai++;
			  }
 			  checkLvls();
                          switch (cshBool(argc -1,argv + 1,quote + 1)) {
                          case 0:  level[lvls++]  = &WhlBarBlk;
                                   strt_stmt      = CSH_SKIP2ELP;
                                   break;
                          case 1:  blk            = MALLOC2(CBlock);
                                   blk->allc      = 1;
		                   blk->line      = use_line ? use_line
                                                             : history;
                                   blk->end       = 0;
                                   blk->depth     = depth;
                                   blk->line->blk = blk;
		                   blk->stmt      = stmt;
		                   blk->bt        = CSH_WHILE;
                                   blk->arg       = ai;
                                   blk->args      = n;
			           history->blk   = blk;
		                   level[lvls++]  = blk;
                                   break;
                          case -1: ERR_LINE();
                                   SH_ERR->printf("while: Badly formed number\n");
                                   sts = ERROR(1);
                                   goto done;
                          }
			}
                      whl_join:
                        goto done;
                      }
                    }
  }

do_sub:
  for (sts = STS_NOTCOMM, scan = next ;
       scan && STS_NOTCOMM == sts ;
       scan = scan->next)
  {
    if (scan->glob) {
      GLOB(scan->name,done);
      sts = scan->parse(argc, argv, space,quote,std,rdr,shx);
    } else {
      sts = scan->parse(argc0,argv0,space,qutd, std,rdr,shx);
    }

    if (STS_NOTCOMM != sts) {
      if (scan->Proc()) {
        if (shx & SHX_DETACH) last_job = LogJob(scan->Proc());
        else                  LogSub(scan->Proc());
      }
      goto done;
    }
  }

  ERR_LINE();
  SH_ERR->printf("%s: Command not found.\n",argv[0]);
  goto done;

syntax:
  ERR_LINE();
  SH_ERR->printf("Command not understood.\n");
syntax2:
  sts = STS_SYNTAX;
done:
  DELETE(argx);

  return sts;
}

eSTS ExecShell::parse(int cnt,const char **ptk,char *space,char *qutd,
                      Stream **std,Stream **rdr,eSHX extra)
{
  String exe(*ptk);
  eSTS   sts = STS_NOTCOMM;

  proc = 0;

  if ((*OS_ROOT == **ptk && 0 == access(*ptk,X_OK)) ||
      envPathFind(OS_EXE_PATH,*ptk,X_OK,&exe)) {

    int swap = 0,
        pid;

    if (extra & SHX_EXEC) {
      pid = 0;
    } else {
      pid = Fork(1);
    }

    if (pid < 0) {
      sts = Error();
#ifdef DEBUG
      fprintf(stderr,"fork: %s\n",strerror(errno));
#endif
    } else if (!pid || swap) {
      const char *xc  = "exec";
      int         sts = DoRedirect(std,rdr,1 DR_EXTRA(&xc));
      if (!sts) {
        execv(exe.str(),(char * const *)ptk);
        sts = errno;
      }
#ifdef DEBUG
      fprintf(stderr,"exec: %s\n",strerror(errno));
#endif
      if (!(extra & SHX_EXEC)) _exit(sts);
    } else {
      FixRedirect(std,rdr);
      LOG_PROC(pid,cnt,ptk);
      if (extra & (SHX_SUBSHL|SHX_NOWAIT)) {
        sts = STS_NORMAL;
      } else {
        do {
          Waiting = WST_EXEC;
          sts = proc->Status(0);
          Waiting = WST_NONE;
        } while (proc && !proc->Zombie());
        DELETE(proc);
      }
    }
  }

  return sts;
}

void Shell::unlink()
{
  if (next) next->prev = prev;
  if (prev) prev->next = next;

  if (this == RootShell) RootShell= next;
}

void Shell::addParser(Shell **next_p)
{
  Shell *last_p = 0;

  while (*next_p && (*next_p)->priority >= priority) {
    next_p = &(last_p = *next_p)->next;
  }

  if (next = *next_p) {
    next->prev = this;
  }

  prev    = last_p;
  *next_p = this;
}

typedef struct {
  CShell *csh;
  Stream *relay,
         *rdr_in,
         *out,
         *err;
} relay_s;

static void *SlaveCShell(void *vp)
{
  relay_s *rp = (relay_s *)vp;

  rp->csh->exit_sts = rp->csh->start(rp->relay,rp->rdr_in,rp->out,rp->err,
                                     0,SMT_SLAVE);

  return 0;
}

extern "C" eSTS shellCommand(const char *cmd)
{
  String        s(cmd);
  StringStream  ss(&s);

  return Shell::CSH->start(&ss,
                           Stream::Stdio(STDIN_FILENO),
                           Stream::Stdio(STDOUT_FILENO),
                           Stream::Stdio(STDERR_FILENO),
                           0,SMT_NONE,0);
}

eSTS CShell::start(Stream *in,Stream *rdr_in,Stream *out,Stream *err,
                   int log, eSMT mt,eSTS *jb_sts)
{
  int     ll,
          nl,
          sc,
          br,
          tkns,
          qutch,
          stmts,
          built_in,
          bq,
          rscn,
          sub     = 0,
          shw_tok = 0;
  char    buff[1024],
         *bp      = buff,
         *buf2    = 0,
         *cp,
          ch;
  int     sz      = sizeof(buff),
          sts     = STS_NORMAL,
          use_rdl = prompt && Readline
                           && !((mt & SMT_SLAVE) || jb_sts);
  eSHF    sflgs   = SHF_NONE;
  Stream *std[3],
         *rdr[3];
  relay_s rs;

  std[STDIN_FILENO]  = rdr_in;
  std[STDOUT_FILENO] = out;
  std[STDERR_FILENO] = err;

  stdio = std;

  if ((SMT_FORK & mt) && ! relay) {
    rs.csh    = this;
    rs.relay  = relay
              = new Stream;
    rs.rdr_in = rdr_in;
    rs.out    = out;
    rs.err    = err;
    master    = pthread_self();
    rs.relay->Pipe();  
    pthread_cond_init(&mt_sts,0);
    pthread_mutex_init(&sync,0);
    pthread_create(&slave,0,SlaveCShell,&rs);
  }

  while (exit_sts < 0) {

    if (use_line) {
      while (use_line && use_stmt >= use_line->stmts) {
        use_line = use_line->next;
        use_stmt = 0;
      }
      if (use_line) {
        strcpy(bp,use_line->stmt[use_stmt++]);
	rscn = 2;
	goto rescan;
      }
    }

    for (;;) {
      retry   = 2;
      killsig = SIGINT;
      do {
        int  n;
        Job *pj;

	if (retry > 1) {

          for (n = 0; n < job_max ; n++) if (pj = jobs[n].proc) {
            if (!pj->Busy() && pj->Zombie()) {
              jobs[n].proc = 0;
              SH_ERR->printf("[%d]   %-30s %s\n",
                             n+1,pj->StatStr(),pj->Command()->str());
              pj->Dead();
            }
          }

	  if (prompt && (terminal = out)->Ready(STRM_IN) <= 0
                     && !jb_sts) {
	    if (goto_lbll) {
	      out->Write(goto_lbls,goto_lbll);
	      out->Write("?",1);
	    } else if (lvls) switch (level[lvls-1]->bt) {
	    case CSH_IF_:
  	    case CSH_IF:     out->Write("if?",3);     break;
  	    case CSH_ELSE:   out->Write("else?",5);   break;
	    case CSH_FOR:    out->Write("for?",4);    break;
	    case CSH_WHILE_:
            case CSH_WHILE:  out->Write("while?",6);  break;
	    case CSH_CASE_:
            case CSH_SWITCH: out->Write("switch?",7); break;
	    }
	    if (!use_rdl) {
	      const char *p = Prompt(); 
              out->Write(p,strlen(p));
	    }
	  } else {
	    terminal = 0;
	  }
	}

        retry   = 0;
        Waiting = WST_IO;
        if (use_rdl) {
          char *rp;
          if (rp = readline(Prompt(),in,out)) {
            ll = strlen(rp);
            if (ll > sz) {
              sz = ll +1;
              if (buf2) REALLOC2(buf2,sz,char);
              else      strcpy(buf2 = MALLOC2_N(sz,char),buff);
              bp  = buf2;
            }
            strcpy(bp,rp);
	    free(rp);
            retry = 1;
	  } else {
            ll = 0;
          }
        } else {
          ll    = in->gets(bp,sz);
        }
        Waiting = WST_NONE;
      } while (ll <= 0 && retry);

      if (ll > 0) break;

      if (prompt && EnvItem::List->have("ignoreeof")) {
        SH_ERR->printf("Use \"exit\" to leave csh.\n");
      } else {
        if (terminal) terminal->Write("\r",1);
        goto quit;
      }
    }

    sts = STS_NORMAL;

  test_nl:
    if (nl = ('\n' == bp[ll -1])) {
      bp[--ll] = '\0';
    }

    if (!use_rdl && (!nl && ll -1 == sz)) {
      sz += 1024;
      if (buf2) REALLOC2(buf2,sz,char);
      else      strcpy(buf2 = MALLOC2_N(sz,char),buff);
      bp  = buf2;
      ll += in->gets(&bp[ll],sz - ll);
      goto test_nl;
    }
    src_line++;

    if (mt && !(SMT_SLAVE & mt)) {
      relay->printf("%s\n",bp);
      pthread_cond_wait(&mt_sts,&sync);
      continue;
    } else if (SMT_SLAVE & mt) {
      pthread_cond_broadcast(&mt_sts);
    }

    rscn = 0;
  rescan:
    for (cp = bp; isspace(*cp) ; cp++);
    if (goto_lbll) {
      if (0   == strncmp(cp,goto_lbls,goto_lbll) &&
          ':' == cp[goto_lbll])
      {
        FREE(goto_lbls);
        goto_lbll = 0;
      }
    }
    char *cp0  = cp;
    int   wrds = 0;
    if ('#' == *cp && !terminal) goto nl;
    for (br = bq = stmts = qutch = built_in = 0,sc = tkns = 2; *cp ; cp++) {
      int   hd,
            hn,
            skp;
      switch (ch = *cp) {
      case '!':  hn  = hd = 1;
                 cp0 = cp;
                 skp = 0;
                 switch (ch = *++cp) {
                 case '=':
                 case '~': continue;
                 case '*': skp =  1;
                 case '!': hd  = -1; ch = *++cp; goto h_sub;
                 case '-': hd  = -1; ch = *++cp;
                 default:  if (isdigit(ch)) {
                             hn = 0;
                             do {
                               hn = (hn*10) + ch - '0';
                             } while (ch = *++cp,isdigit(ch));
   		           h_sub:
                             CHist *ph = history;
                             if (hd > 0) {
                               if (((ph = h_root) && hn >= h_root->line) &&
				   hn <= src_line) {
                                 for (; ph && ph->line != hn ; ph = ph->next);
                               } else {
			       enf:
                                 SH_ERR->printf("%d: Event not found.\n",hn);
                                 goto nl;
                               }
                             } else {
                               for (; ph && !++hn ; ph = ph->prev);
                               if (!ph) goto enf;
                             }
                             int l2 = ll - (cp - cp0),
                                 s  = 0,
                                 lx = 0;
                             for (; s < ph->stmts ; s++) {
                                if (s) lx++;
                                lx += strlen(ph->stmt[s]);
                             }
                             if ((l2 + lx + 1) >= sz) {
			       sz  = l2 + lx + 1024;
			       if (buf2) REALLOC2(buf2,sz,char);
			       else      strcpy(buf2=MALLOC2_N(sz,char),buff);
                               cp0 = &buf2[cp0 - bp];
                               cp  = &buf2[cp  - bp];
			       bp  =  buf2;
                             }
                             ll  = l2 + lx;
                             l2 -= cp0 - bp;
                             bcopy(cp,cp0 + lx,l2+1);
                             for (s = 0; s < ph->stmts ; s++) {
                                if (s) *cp0++ = ';';
                                l2   = strlen(ph->stmt[s]);
                                bcopy(ph->stmt[s],cp0,l2);
                                if (skp) {
                                  skp--;
                                  while (!isspace(*cp0) && l2 > 0) {
                                    *cp0++ = ' ';
                                    l2--;
                                  }
                                }
                                cp0 += l2;
                             }
                             goto rescan;
		           }
                 }
      case '=':  if (built_in) goto nw_tk;
      default:   if (isspace(ch)) {
                   if (!qutch) {
                     tkns++;
                     if (0 == wrds) switch (cp - cp0) {
		     case 1: if (0 == strncmp(cp0,"@",1))   built_in = 1;
                             break;
		     case 3: if (0 == strncmp(cp0,"set",3)) built_in = 2;
                             break;
                     }
                   }
                   while (isspace(cp[1])) cp++;
                   cp0 = cp;
                   wrds++;
                 }
                 break;
      case '&':
      case '>':
      case '<':
      case ':':
      case '[':
      case ']':
      nw_tk:     if (!qutch) tkns++;
                 break;
      case '|':
      case ';':  if (!qutch) {stmts++; sc = ++tkns; wrds = 0;}
                 break;
      case '(':  if (sc == tkns) {
                   if (!qutch) qutch = ')';
                 } else {
                   br++;
                 }
                 if (cp0 != cp) tkns++;
                 tkns++;
                 break;
      case '`':  if (!qutch || qutch == ch) { tkns++; bq++; }
                 goto opn_cls;
      case ')':  if (!qutch && !br) {
                   SH_ERR->printf("Too many )'s.\n"); goto nl;
                 } else if (!br) {
                   goto opn_cls;
                 }
                 if (cp0 != cp) tkns++;
                 br--;
                 tkns++;
                 break;
      opn_cls:
      case '"':
      case '\'': if (qutch == ch) {
                   qutch = 0;
                 } else if (!qutch) {
                   qutch = ch;
                 }
                 break;
      case '\\': if ((cp - bp) == ll -1) {
                   ll--;
                   if ((nl = in->gets(&bp[ll],sz - ll)) > 0) {
                     ll += nl;
                     goto test_nl;
                   }
                 } else {
                   cp++;
                 }
                 break;
      }
    }

    if (qutch) {
      switch (qutch) {
      case ')': SH_ERR->printf("Too many ('s.\n",qutch); break;
      default:  SH_ERR->printf("Unmatched %c.\n",qutch); break;
      }
      sts = STS_UNTERM;
    } else if (tkns) {
      TMPARR(char,sp,tkns);
      TMPARR(char,qut,tkns);
      TMPARR(char *,pt,tkns);
      const char  *tok;
      char       **pp,
                  *psp = sp,
                  *pqu = qut;
      TMPARR(int,si, stmts+2);
      TMPARR(int,ppd,stmts+2);
      int          n   = 0,
                   s   = 0,
                   bar = 0,
                   pps = 0,
                   cln = 0,
                   sub = 0;

      pt[n]  =  0;
      si[0]  =  0;
      sp[0]  =  0;
      qut[0] =  0;
      ppd[0] = -1;

      for (cp = bp,br = sc = qutch = 0 ;; cp++) {
        assert (n < tkns || !*cp);
        switch (ch = *cp) {
        plain_tk:
        default:   if (isspace(ch))   goto blank;
                   goto keep;
        case '\\': if (!(ch = *++cp)) goto eos;
        keep:      if (!pt[n]) {
                     pt[n] = cp;
                     si[s]++;
	           }
                   break;
        case ':':  if (1 == si[s] || Colon == pt[n-1]) {
                     cln++;
                     tok = Colon;
                     goto new_tok;
	           }
                   break;
        case '&':  if (!qutch) {
 	             if ('&' == cp[1]) {tok = LgcAnd; goto new_tok2;}
                     tok = Ampersand; goto new_tok;
                   }
                   break;
        case '=':  if (!built_in) goto plain_tk;
                   if (!qutch) {tok = Equals;    goto test_op;} break;
        case '<':  if (!qutch) {tok = DirectIn;  goto test_op;} break;
        case '>':  if (!qutch) {tok = DirectOut; goto test_op;} break;
        case '!':  if (!qutch) {tok = TestNot;   goto test_op;} break;
	test_op:   if (('=' == cp[1] || '~' == cp[1] ||
                        (br && ('>' == cp[1] || '<' == cp[1])))) {
	             switch (ch) {
                     case '=': switch (cp[1]) {
                               case '=': tok = Equiv;   goto new_tok2;
                               case '~': tok = EquivF;  goto new_tok2;
		               }
                               break;
                     case '<': switch (cp[1]) {
                               case '=': tok = TestLE;  goto new_tok2;
                               case '<': tok = TestSL;  goto new_tok2;
		               }
                               break;
                     case '>': switch (cp[1]) {
                               case '=': tok = TestGE;  goto new_tok2;
                               case '>': tok = TestSR;  goto new_tok2;
		               }
                               break;
                     case '!': switch (cp[1]) {
                               case '=': tok = TestNE;  goto new_tok2;
                               case '~': tok = EquivF_; goto new_tok2;
		               }
                               continue;
                     }
                   }
                   goto new_tok;
        case ' ':
        blank:     if (!qutch) {
                     *cp = 0;
                     if (pt[n]) {
                       pt[++n]  = 0;
                       qut[n]   = 0;
                     }
                   }
  	           if (isspace(ch)) sp[n] = 1;
                   if (!ch) goto done;
                   break;
        case '|':  bar = 1;
        eos:
        case '\0':
        case ';':  if (!qutch) {
                     if (si[s]) { if (bar) ppd[s] = pps++;
		                  si[++s] = 0;
                                  ppd[s]  = -1;}
                     bar = 0;
                     sc  = s;
                     goto blank;
	           }
                   bar = 0;
                   break;
        case '(':  if (si[s]) {
                     br++; tok = OpenBr; goto new_tok;
                   }
 	           goto quoted;
        case ')':  if (br) {
                     br--; tok = CloseBr; goto new_tok;
	           }
        case '`':
        case '"':
        case '\'': 
        quoted:    if (qutch == ch) {
                     qut[n] = ch;
                     qutch  = 0;
                   } else if (!qutch) {
                     qutch = ch;
                   } else {
                     goto keep;
                   }
                   switch (ch) {
                   case '(': qutch = ')';
                             tok   = OpenBr;    goto new_tok;
                   case ')': tok   = CloseBr;   goto new_tok;
                   case '`': tok   = BackQuote; goto new_tok;
                   }
                   goto keep;
	new_tok2:  *cp++ = 0;
        new_tok:   *cp   = 0;
                   if (!pt[n]) {
                     pt[n]   = (char *)tok;
                     pt[++n] = 0;           sp[n] = qut[n] = 0;
        	   } else {
                     pt[++n] = (char *)tok; sp[n] = qut[n] = 0;
                     pt[++n] = 0;           sp[n] = qut[n] = 0;
	           }
                   si[s]++;
                   break;
        }
      }
    done:

      if (log && !rscn) {
        int    lsz = sizeof(CHist) + (s * sizeof(void *)) + ll + 4,
               mx  = h_max,
               lbl = cln%2;
        CHist *ph  = (CHist *)Calloc2(lsz,1),
              *pr;
        char **pp2 = &ph->stmt[0],
              *cp2 = (char *)&ph->stmt[s];
        ph->stmts  = s;
        ph->line   = src_line;
        if (mx < 0) mx = -mx;
        while ((pr = h_root) && (ph->line - pr->line >= mx)
                             && !pr->lbll) {
          if (pr->blk) break;
          if (pr->next) {
            pr->next->prev = 0;
            h_root         = pr->next;
          } else {
            h_root = history = 0;
          }
          Free(pr);
        }
        for (s = 0,psp = sp,pp = pt,pqu = qut;
             n = si[s];
             s++,pp += n, psp += n,pqu += n)
        {
           int i;
           if (lbl && Colon == pp[1] && Colon != pp[2]) {
             ph->lbls = s;
             ph->lbll = strlen(*pp);
             lbl      = 0;
           }
           *pp2++ = cp2;
           for (i = 0; i < n ; i++) {
             if (psp[i]) *cp2++ = ' ';
             sprintf(cp2,"%s",pp[i]);
             cp2 += strlen(cp2);
           }
           cp2++;
        }
        assert(cp2 - lsz < (char *)ph);
        if (ph->prev = history) history->next = ph;
        else                    h_root        = ph;
        history = ph;
      }

      strt_stmt = 0;

      if (goto_end >= 0 || goto_lbll) {
        int    *psi = si,
                chk = 1,
                els = -1,
                m;
        CBlock *blk;

        for (s = *psi,n = 0; cp = pt[n]; n++) {
          if (chk) switch (*cp) {
          case 'd':
            if (0 == strncmp(cp,"default",7)) {
              cp += 7;
              if (!*cp || ':' == *cp) {
                if (lvls -1 == goto_end && goto_end   == lvls -1
                                        && CSH_SWITCH == level[lvls -1]->bt) {
                  level[lvls -1]->bt = CSH_CASE_;
                  goto_end = -1;
                  goto got_lvl;
                }
              }
            }
            break;
          case 'c':
            if (0 == strcmp(cp,"case")) {
              if (lvls -1 == goto_end && CSH_SWITCH == level[lvls -1]->bt) {
                goto_end = -1;
                goto got_lvl;
              }
            }
            break;
	  case 'e':
            if (0 == strcmp(cp,"else")) {
              els = n;
              if (goto_end == lvls -1 && CSH_IF_  == level[lvls -1]->bt) {
                goto_end = -1;
                goto got_lvl;
              }
            }
            if ((m = (0 == strcmp(cp,"endif"))) || 0 == strcmp(cp,"end")
                                                || 0 == strcmp(cp,"endsw")) {
              if (m) els = -1;
              if (lvls -1 == goto_end) {
                goto_end = -1;
                goto got_lvl;
              }
              if (blk = level[--lvls]) {
                if (blk->allc) Free(blk);
                level[lvls] = 0;
              }
            }
            break;
          case 'i':
          case 'f':
          case 's':
          case 'w':
            if ((0 == strcmp(cp,"if") && (els != n -1 || els < 0)) ||
                0 == strcmp(cp,"foreach") ||
                0 == strcmp(cp,"while")   || 0 == strcmp(cp,"switch")) {
              checkLvls();
              lvls++;
	    }
          }
          if (s == n) {
            s  += *++psi;
            chk = 1;
            strt_stmt++;
          }
        }
        goto nl;
      got_lvl:;
      }

      if (goto_lbll) goto nl;

#ifdef DEBUG
      if (shw_tok) for (n=0; pt[n]; n++) fprintf(stderr,"%s\n",pt[n]);
#endif

      if (bq && rscn <= 1) {
        String  tmp1;
        int     sn;

        for (sn = si[s = 0],n = 0; cp = pt[n]; n++) {
          if (sp[n])    tmp1 += " ";
          if (sn == n) {tmp1 += ";";
	                sn   += si[++sn];}
	  if (cp != BackQuote || sn < strt_stmt) {
            tmp1 += cp;
          } else {
            cp = pt[++n]; n++;
            Stream *pip = new Stream;
            pip->Pipe(FM_NONE);
            int pid = Fork(1);
            if (pid < 0) {
              SH_ERR->printf("fork: %s\n",strerror(errno)); goto nl;
            }
            int swap = 0;
            if (pid && !swap) {
              String  tmp2;
              char   *pad;
              LogSub(proc = new Job(pid,cp));
              pip->Close(STRM_OUT);
              Waiting  = WST_BCKQ;
              for (pad = ""; (ll = pip->gets(&tmp2)) >= 0 ; pad = " ") {
                if (ll-- > 0 && '\n' == tmp2.str()[ll]) {
                  tmp1 += pad;
                  tmp2.cut(ll);
                }
                tmp1 += tmp2;
              }
              Waiting  = WST_NONE;
              last_sts = WaitSubs(STS_NORMAL);
            } else {
              pip->Close(STRM_IN);
              out->Close();
              in      = new StringStream(new String(cp));
              out     = std[STDOUT_FILENO] = pip;
              sub     = 1;
              prompt  = 0;
              use_rdl = 0;
              goto nl;
            }
            pip->Close();
            DELETE(pip);
          }
        }

        if (tmp1.len() >= sz) {
          sz = tmp1.len() + 1024;
          if (buf2) REALLOC2(buf2,sz,char);
          else      strcpy(buf2 = MALLOC2_N(sz,char),buff);
          bp  = buf2;
        }

        strcpy(bp,tmp1.str());
        rscn = 1;
        goto rescan;
      }

      rdr[STDERR_FILENO] = std[STDERR_FILENO];

      if (s > 0 && ppd[s-1] >= 0 && ! si[s -1]) {
        SH_ERR->printf("Invalid null command.\n");
        status->change(ERRNO(1));
        goto nl;
      }

      TMPARR(Stream,pipes,pps);
      TMPARR_INIT(pipes,pps,Init);

      for (n = pps; n-- > 0; ) {
        int sts = pipes[n].Pipe(FM_NONE);
	if (pipes[n].Pipe(FM_NONE) <0) {
          SH_ERR->printf("Pipe failed: %s.\n",strerror(sts));
          status->change(ERRNO(sts));
          goto cls_pps;
        }
      }

      for (s = 0,psp = sp,pp = pt,pqu = qut;
           n = si[s];
           s++,pp += n,psp += n,pqu += n) {

        if (s < strt_stmt) {
          if (((strt_stmt & CSH_SKIP2EIF) && 0 == strcmp(*pp,"endif")) ||
              ((strt_stmt & CSH_SKIP2ELP) && 0 == strcmp(*pp,"end"))   ||
              ((strt_stmt & CSH_SKIP2ELS) && 0 == strcmp(*pp,"else"))  ||
              ((strt_stmt & CSH_SKIP2CS)  && 0 == strcmp(*pp,"case"))  ||
              ((strt_stmt & CSH_SKIP2ESW) && 0 == strcmp(*pp,"endsw")))
          {
            strt_stmt = 0;
          } else {
            continue;
          }
        }

        char buff[4];
        eSHX shx = SHX_NONE;

        stmt = s;

        if (s > strt_stmt && ppd[s-1] >= 0) {
          shx = SHX(shx|SHX_RDRCT_I|SHX_NOWAIT);
          rdr[STDIN_FILENO] = &pipes[ppd[s-1]];
        } else {
          rdr[STDIN_FILENO] = std[STDIN_FILENO];
        }

        if (ppd[s] >= 0) {
          shx = SHX(shx|SHX_RDRCT_O|SHX_SUBSHL|SHX_NOWAIT);
          rdr[STDOUT_FILENO] = &pipes[ppd[s]];
        } else {
          rdr[STDOUT_FILENO] = std[STDOUT_FILENO];
        }

	last_job = -1;
        if (n > 1 && Colon == pp[1] && !(n > 2 && Colon == pp[2])) {
          if (n > 2) {
            last_sts = parse(n-2,(const char **)&pp[2],psp,pqu,std,rdr,
                             shx);
          } else {
            last_sts = STS_NORMAL;
          }
        } else {
          last_sts = parse(n,(const char **)pp,psp,pqu,std,rdr,
                           shx);
        }
        if (last_job >= 0 && jb_sts) {
          jobs[last_job].proc->LogSts(jb_sts);
        }
        if (STS_NOTCOMM != last_sts && (shx & SHX_NOWAIT)
                                    && !(shx & SHX_SUBSHL)) {
          last_sts = WaitSubs(last_sts);
        }

        status->change(ERRNO(last_sts));

        if (STS_NORMAL != last_sts) break;
      }

      WaitSubs(STS_NORMAL);
    cls_pps:
      TMPARR_DESTROY(pipes,pps,~Stream);
    }
  nl:;
  }

quit:
  if (sub) {
    _exit(last_sts);
  }

  stdio = 0;
  FREE(buf2);

  if (SMT_FORK & mt) {
    if (SMT_JOIN & mt) {
      relay->Close(STRM_OUT);
      pthread_join(slave,0);
      DELETE(relay);
    } else if (SMT_WAIT) {
      relay->printf("_exit $?\n");
      pthread_cond_wait(&mt_sts,&sync);
      exit_sts = last_sts;
    }  
  }

  return exit_sts < 0 ? STS_NORMAL
                      : exit_sts;
}

extern "C" eSTS dumbShell(const char *src,int argc0,const char **argv0)
{
  Stream     *in  = Stream::Stdio(STDIN_FILENO),
             *out = Stream::Stdio(STDOUT_FILENO),
             *err = Stream::Stdio(STDERR_FILENO),
              alt;
  const char *pp  = 0;
  eSTS        sts;

  if (src) {
    if (!(in = &alt)->Open(src,"r")) {
      return Error();
    }
  } else {
    Env prmpt("V2K_PROMPT");
    Shell::CSH->setPrompt(prmpt ? prmpt.Value()
                                : CShell::DefaultPrompt);
  }

  EnvItem *ea = argv0 ? EnvItem::List->add("argv",argc0,argv0,0)
                      : EnvItem::List->have("argv");

  envPath2var("PATH","path");

  if (argc0 != ea->Items()) {
    // Fix args ???
  }

  sts = Shell::CSH->start(in,in,out,err,1);

  alt.Close();
  return sts;
}


extern "C" eSTS initShell(int argc,const char **argv)
{
  if (!Arg.exe || !*Arg.exe) Arg.exe = *argv;

  EnvItem::List->add("argv",argc-1,argv+1,0);

  Shell::RootShell->Init();

  return STS_NORMAL;
}

extern "C" eSTS doV2kRC(const char *path,const char *file)
{
  String rc(path);

  rc += "/";
  rc += file;

  File rcf(rc);

  if (rcf.Exists()) {
    return dumbShell(rcf,0,0);
  }

  return STS_NORMAL;
}

extern "C" eSTS doV2kRCs()
{
  return envPathDo(doV2kRC,V2K_RC_PATH,".v2krc");
}

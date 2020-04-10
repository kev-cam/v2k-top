/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  shell_h_rcsid
#define shell_h_rcsid() {return "$Id: shell.h,v 1.37 2012/10/16 22:30:00 cvs Exp $";} /* RCS ID */

 
#ifndef JOB_H
# include "job.h"
#endif

#ifndef SIGTRAP_H
# include "sigtrap.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

eSTS shellCommand(const char *);
eSTS dumbShell(const char *,int,const char **);
eSTS initShell(int,const char **);
eSTS doV2kRC(const char *,const char *);
eSTS doV2kRCs();

#ifdef __cplusplus
}

class Stream;

typedef enum {
  SHC_COMMAND = 0
} SH_CNTXT;

typedef enum {
  SHP_FIRST = 127,
  SHP_CSH   = 100,
  SHP_HIER  = 80,
  SHP_ARB   = 64,
  SHP_LAST  = 0
} SH_PRIOR;

typedef enum {
  SHX_NONE    =  0,
  SHX_SUBSHL  =  1,
  SHX_FOLDERR =  2,
  SHX_RDRCT_I =  4,
  SHX_RDRCT_O =  8,
  SHX_NOWAIT  = 16,
  SHX_DETACH  = 32,
  SHX_EXEC    = 64
} eSHX;

#define SHX(x) ((eSHX)(x))

typedef struct {
  const char *command,
             *comment;
} ShellHelp;

typedef enum {
  WST_NONE = 0,
  WST_IO,
  WST_SUBS,
  WST_JOBS,
  WST_EXEC,
  WST_BCKQ,
  WST_FG
} eWST;

typedef char  *(*ReadlineFn)(const char *);

class CShell;
class ExecShell;
class Shell {
public:
  static Shell       *RootShell;
  static ExecShell   *ESH;
  static CShell      *CSH;
  static eWST         Waiting;
  static DynObj      *RdLnLib;
  static ReadlineFn   Readline;
  static ReadlineFn   AddHistory;
  static FILE       **rl_instream,
                    **rl_outstream;
  static bool         InitDone;

  SH_PRIOR    priority;
  SH_CNTXT    cntxt;
  Shell      *next,
             *prev;
  const char *name;
  ShellHelp  *help_data;
  Job        *proc;
  bool        glob;

  void unlink();
  void Init();

  inline ~Shell() {unlink();};
  inline Shell()  {glob      = 0;
                   next      = 0;
                   prev      = 0;
                   name      = 0;
                   help_data = 0;
                   proc      = 0;
                   if (!InitDone) Init();}

  inline Job *Proc()             {return proc;};
  inline int  set_proc(Job *prc) {if ((proc = prc)) return proc->Pid();
                                  else              return -1;};

  char *readline(const char *,Stream *,Stream *);

  eSTS subsh_fork(int,int *,Stream **,Stream **,int,const char **,eSHX);

  virtual eSTS parse (int,const char **,char *,char *,
                      Stream **,Stream **,eSHX) = 0;
  virtual void LogSub(Job *);
  virtual int  LogJob(Job *);

  int help(Stream *,const char *,const char *);

  void addParser(Shell **);
};

typedef enum {
  CSH_IF,
  CSH_IF_,
  CSH_ELSE,
  CSH_SWITCH,
  CSH_CASE_,
  CSH_FOR,
  CSH_WHILE,
  CSH_WHILE_
} CBT;

typedef enum {
  CSPR_NONE,
  CSPR_FILE,
  CSPR_LOR,
  CSPR_LAND,
  CSPR_BXOR,
  CSPR_BAND,
  CSPR_CMP,
  CSPR_LGC,
  CSPR_SHFT,
  CSPR_ADD,
  CSPR_MULT,
  CSPR_NOT,
  CSPR_1CMP,
  CSPR_BR,
  CSPR_MAX
} CSPR;

typedef enum {
  CSOP_NONE,
  CSOP_FL_R,
  CSOP_FL_W,
  CSOP_FL_X,
  CSOP_FL_E,
  CSOP_FL_O,
  CSOP_FL_Z,
  CSOP_FL_F,
  CSOP_FL_D,
  CSOP_LOR,
  CSOP_LAND,
  CSOP_BXOR,
  CSOP_BAND,
  CSOP_EQU,
  CSOP_NEQU,
  CSOP_FEQU,
  CSOP_NFEQU,
  CSOP_LT,
  CSOP_LSH,
  CSOP_ADD,
  CSOP_SUB,
  CSOP_MULT,
  CSOP_DIV,
  CSOP_MOD,
  CSOP_NOT,
  CSOP_1CMP
} CSOP;

typedef struct CBlock_s {
  BITF(CBT,bt,16,short);
  int              depth:16;
  struct CHist_s  *line,
                  *end;
  int              stmt:16,
                   allc:16,
                   arg,
                   args;
  EnvItem         *var;
  char            *argv[UNSIZED(0)];
/*char             a0[]*/
} CBlock;

typedef struct CHist_s {
  struct CHist_s *next,
                 *prev;
  CBlock         *blk;
  int             line,
                  lbls:16,
                  lbll:16,
                  stmts;
  char           *stmt[UNSIZED(stmts)];
/*char            s0[]*/
} CHist;

typedef struct {
  Job    *proc;
  time_t  bg_fg;
  int     tag;
} CshJob;

typedef struct CshAlias_s {
  struct CshAlias_s *next;
  char              *name,
                     value[UNSIZED(0)];
} CshAlias;

typedef enum {
  SMT_NONE  = 0,
  SMT_FORK  = 1,
  SMT_SLAVE = 2,
  SMT_JOIN  = 4,
  SMT_WAIT  = 8,
  SMT_FRKJN = SMT_FORK|SMT_JOIN,
  SMT_FRKWT = SMT_FORK|SMT_WAIT
} eSMT;

#define SMT(s) ((eSMT)(s))

typedef struct {
  int  expa,
      *expi;
} Break;

class Exception {
 public:
  eSTS sts;
  Exception(int i) {sts = ERROR(i);};
};

class CShell : public Shell {
  String            prompt_buff;
public:
  const char       *prompt;
  eSHF              flags;
  eSTS              exit_sts,
                    last_sts;
  EnvItem          *status;
  char              retry,
                    notify;
  int               killsig;
  CshAlias         *alss;
  CshJob           *jobs;
  int               job_max,
                    last_job;
  Job             **subs;
  pthread_t         slave,
                    master;
  pthread_mutex_t   sync;
  pthread_cond_t    mt_sts;
  int               sub_max;
  Stream           *terminal,
                   *relay,
                  **stdio;
  char             *glbd;
  int               glbd_sz;
  char             *quote,
                   *space;
  const char      **argv;
  unsigned int      arg_max;
  ChStrArray        cargs;
  CBlock          **level;
  int               lvls,
                    lvl_max,
                    h_max,
                    depth;
  const char       *src_name;
  CHist            *history,
                   *h_root,
                   *use_line;
  int               use_stmt,
                    src_line,
                    stmt;
  unsigned int      strt_stmt;
#define             CSH_SKIP2EIF 0x01000000
#define             CSH_SKIP2ELS 0x02000000
#define             CSH_SKIP2EE  0x03000000
#define             CSH_SKIP2ELP 0x04000000
#define             CSH_SKIP2CS  0x08000000
#define             CSH_SKIP2ESW 0x10000000
  char             *goto_lbls;
  int               goto_lbll:16,
                    goto_end:16;
  SysJmpBuf        *jump_buff;

  static CBlock     ElsBlk,
                    IfBlk,
                    IfBarBlk,
                    WhlBarBlk;

  static const char *DefaultPrompt;

  virtual eSTS parse(int,const char **,char *,char *,Stream **,Stream **,eSHX);
  eSTS         parse(int,const char **,char *,char *,Stream **,Stream **,eSHX,
                     String *,Break *,const char **,StrStrArray &sargs);
  virtual void LogSub(Job *);
  virtual int  LogJob(Job *);

  CShell(Shell **);
  ~CShell();

  void        LogSub(int,int,const char **);
  eSTS        WaitSubs(eSTS);
  void        KillSubs(int);
  eSTS        WaitJobs(int jobs = -1);
  int         FixTags(int *);
  void        interrupt();
  void        sigchild();
  void        sigstop();
  void        sigcont();
  void        sigabrt();
  int         glob(StringArray &,char *,char *);
  void        extendArgs(int);
  void        checkLvls();
  void        clearLvls();
  void        sortArgs(int,const char **,char *,char *);
  int         substJobs(int,const char **);
  const char *Prompt();
  const char *setPrompt(const char *);

  eSTS start(Stream *,Stream *,Stream *,Stream *,int,
             eSMT mt = SMT_NONE,eSTS *js = 0);

};

class ExecShell : public Shell {
public:

  virtual eSTS parse(int,const char **,char *,char *,Stream **,Stream **,eSHX);

  ExecShell(Shell **);
};

#define SUBSH_FORK(r)     {if (shx & SHX_SUBSHL) {\
                           if ((sts = subsh_fork(r,&pid,std,rdr,argc,argv,shx)))\
                             goto done;}}
#define SUBSH_JOIN(s)     {if (shx & SHX_SUBSHL) {if (!pid) _exit(s);\
                                                  pid = 0;}}
#define LOG_PROC(p,c,v)   {if (p) proc = new Job(p,c,v);}

#endif

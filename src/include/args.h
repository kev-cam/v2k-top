/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  args_h_rcsid
#define args_h_rcsid() {return "$Id: args.h,v 1.49 2010/04/23 21:05:55 dkc Exp $";} /* RCS ID */

 
#ifndef ARGS_H
#define ARGS_H

#ifndef ERROR_H
# include "error.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

//! Argument list processor data
typedef struct {
  const char  *exe;         //!< pointer to original argv[0]
  char         show_tok;
  char         show_mode;
  int          verbosity;
  int          debug;
  int          interactive;
  int          argc0;       //!< starting count
  const char **argv0;       //!< starting arg
  const char **argv;        //!< current arg
  char         last[32];    //!< last processed arg (for debug)
} Args;

#define INIT_ARGS "",0,0,(VRB_EXIT|VRB_WARN)

extern Args Arg;

extern int V2kMake;

typedef eSTS (*hndlrFn)(int *argc,const char ***argv,void *,int,int);

//! Command line argument type
typedef enum {
  /* Types */
  AT_NONE   =  0,               /* Invalid */
  AT_FILE   =  1,
  AT_DIR    =  2,
  AT_PATH   =  AT_FILE|AT_DIR,
  AT_STREAM,
  AT_BOOL,
  AT_INT,
  AT_STR,
  AT_IO,
  /* Other Bit Flags */
  AT_VALUE  =  64,              /* has a value */
  AT_FLAG   =  128,             /* is an -/+ argument */
  AT_ARG    =  AT_FLAG|AT_VALUE
} eAT;

#define ARG_TYPE(a) ((eAT)(a))

//! File dependency flags for "make" mode
typedef enum {
  MAKE_ALL  = -1,
  MAKE_NONE = 0,
  MAKE_FILE = 1,
  MAKE_DEFS = 2,
  MAKE_ENV  = 4,
  MAKE_INC  = 8,
  MAKE_DEPS = (MAKE_DEFS|MAKE_ENV|MAKE_INC),
  MAKE_USER = 16
} eMAKE;

//! Auto-loading control
typedef enum {
  AUTO_ALL    = -1,
  AUTO_NONE   = 0,
  AUTO_MODULE = 1    //< Auto-load modules
} eAUTO;

//! Message verbosity control flags
typedef enum {
  VRB_ALL    = -1,
  VRB_NONE   =  0,
  VRB_MAKE   =  1,
  VRB_DUMP   =  1 <<  1,
  VRB_GEN    =  1 <<  2,
  VRB_EXIT_W =  1 <<  3,
  VRB_EXIT_E =  1 <<  4,
  VRB_EXIT   =  VRB_EXIT_W|VRB_EXIT_E,
  VRB_INFO   =  1 <<  5,
  VRB_ELAB   =  1 <<  6,
  VRB_SUMM   =  1 <<  7,
  VRB_ARG    =  1 <<  8,
  VRB_WARN   =  1 <<  9,
  VRB_ECHO   =  1 << 10,
  VRB_DEFN   =  1 << 11,
  VRB_CODE   =  1 << 12,
  VRB_PARC   =  1 << 13,
  VRB_MISC   =  1 << 31
} eVERB;

//! Command line argument call-back handler info.
typedef struct {
  const char *name;   //!< "foo" if using "-foo"
  hndlrFn     fn;     //!< call back function 
  void       *data;   //!< context for call back
  eAT         atyp;   //!< arg type flags
  const char *descr;  //!< description for user
} argHndlr;

eSTS        argSetPM    (int *argc,const char ***argv,void *var,int,int);
eSTS        argSetVerb  (int *argc,const char ***argv,void *var,int,int);
eSTS        argSetVar   (int *argc,const char ***argv,void *var,int,int);
eSTS        argSetBool  (int *argc,const char ***argv,void *var,int,int);
eSTS        argSetStr   (int *argc,const char ***argv,void *var,int,int);
eSTS        argSetInput (int *argc,const char ***argv,void *var,int,int);
eSTS        argSetOutput(int *argc,const char ***argv,void *var,int,int);
eSTS        argFile2Arg (int *argc,const char ***argv,void *var,int,int);
eSTS        argExplain  (int *argc,const char ***argv,void *var,int,int);
eSTS        argCD       (int *argc,const char ***argv,void *var,int,int);
eSTS        argPause    (int *argc,const char ***argv,void *var,int,int);
const char *argEquals   (int *argc,const char ***argv);
const char *argNext     (int *argc,const char ***argv);
eSTS        processArgs (int *argc,const char ***argv,argHndlr *);
eSTS        argPrcssArgs(int *argc,const char ***argv);
const char *argNextFile (const char *,int);
void        argOK       ();
const char *Arg0        ();
const char *ArgN        (int);
int         ArgCount    ();
eSTS        argBool     (const char *,int *);
eSTS        argInt      (const char *,int *);
typedef eSTS (* argPrcssFn)(int *,const char ***);

extern argPrcssFn argDefPrcssr;

#ifdef __cplusplus
}
#endif

#endif

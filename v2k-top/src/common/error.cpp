/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * error_cpp_rcsid() {return "$Id: error.cpp,v 1.35 2009/05/22 21:21:11 dkc Exp $";}

#include "error.h"
#include "args.h"
#include "system.h"
#include "v2k_mem.h"
#include "poolmngr.h"

ErrorControl ErrControl = {1000,VRB_EXIT_E};

const char *ErrorString[] = {
  "",
#undef  SYSTEM_ERROR
#define SYSTEM_ERROR(n,e,s) s,
#include "syserrs-a.h"
#include "usrerrs-a.h"
  0
};

typedef struct sEH {
  struct sEH *next;
  ExitRtn     rtn;
  void       *data;

} ExitHandler;

ExitHandler *ExitHndlrs;

extern "C" void LwpCount(int incr)
{

}

extern "C" int addExitRtn(ExitRtn fn,void *dt)
{
  ExitHandler *nw = (ExitHandler *)Malloc(sizeof(ExitHandler));

  if (nw) {
    nw->next   = ExitHndlrs;
    ExitHndlrs = nw;
    nw->rtn    = fn;
    nw->data   = dt;
    return 1;
  }

  return 0;
}

extern "C" void rmExitRtn(ExitRtn fn,void *dt)
{
  ExitHandler **old = &ExitHndlrs,
               *nxt;

  for (; nxt = *old; old = &nxt->next) {
    if (nxt->rtn == fn && nxt->data == dt) {
      *old = nxt->next;
      Free(nxt);
      return;
    }
  }
}

extern "C" eSTS Error()
{
  return ERROR(errno);
}

DECL_MAIN_THREAD_ID

extern "C" void  FinalExit(int n)
{
#ifdef PTHREADS

  static eSTS thrd_sts;

  pthread_t id = pthread_self();

  if (id != MAIN_THREAD_ID) {
    if (!thrd_sts) thrd_sts = S_FATAL(ERROR(n));

    pthread_exit(&thrd_sts);
  }

#endif

  if (!ErrControl.dont_stop) {
    if (!ErrControl.am_chld && PoolMngr) PoolMngr->~PoolManager();

    if (ErrControl.verbosity & VRB_SUMM ||
        ((ErrControl.verbosity & VRB_EXIT) && (ErrControl.fatal ||
                                               ErrControl.errors))) {
      fprintf(stderr,"%d Error%s, and %d Warning%s%s\n",
                     ErrControl.fatal + ErrControl.errors,
            	     ErrControl.fatal + ErrControl.errors == 1 ? "" : "s",
                     ErrControl.warnings,
                     ErrControl.warnings == 1 ? "" : "s",
                     ErrControl.suppressed ? " (suppressed)" : "");
    }

    if (ErrControl.f_errno) exit(ErrControl.f_errno);
    if (ErrControl.e_errno) exit(ErrControl.e_errno);

    if (ErrControl.am_chld) _exit(n);

    exit(n);
  }
}

extern "C" const char *StrError(eSTS status)
{
  static char unknown[32];

  unsigned int n = ERRNO(status);

  if (n < sizeof(ErrorString)/sizeof(ErrorString[0])) {
    return ErrorString[n];
  }

  sprintf(unknown,"Error Code (%d)",n);
  return unknown;
}

extern "C" eSTS ErrorMsg(eSTS status,const char *format,...)
{
  static int lockout = 0;

  int n        = ERRNO(status),
      preamble = 0;

  if (status & STS_MASK) {
    preamble = 1;
         if (status & STS_FATAL)   {ErrControl.fatal++;
                                    ErrControl.f_errno = n;
                                    fprintf(stderr,"[Fatal Error]");}
    else if (status & STS_ERROR)   {ErrControl.errors++;
                                    ErrControl.e_errno = n;
                                    fprintf(stderr,"[Error]");}
    else if (status & STS_WARNING) {ErrControl.warnings++;
                                    ErrControl.w_errno = n;
                                    if (!(Arg.verbosity & VRB_WARN)) {
                                      ErrControl.suppressed++;
                                      return STS_NORMAL;
                                    }
                                    fprintf(stderr,"[Warning]");}
    else if (status & STS_INFO)    {ErrControl.info++;
                                    ErrControl.i_errno = n;
                                    if (!(Arg.verbosity & VRB_INFO)) return STS_NORMAL;
                                    fprintf(stderr,"[Info]");}
  }
  if (status) {
    if (preamble++) fprintf(stderr," ");
    fprintf(stderr,"%s",StrError(status));
  }
  if (format && *format) {
    va_list pvar;
    va_start(pvar, format);
    if (preamble) fprintf(stderr,": ");
    vfprintf(stderr, format, pvar);
    va_end(pvar);
  }
  fprintf(stderr,"\n");

  if (status & STS_FATAL) FinalExit(n);

  if (ErrControl.give_up && ErrControl.errors > ErrControl.give_up)
  {
    if (!lockout++) {ErrorMsg(S_FATAL(STS_TOO_MANY),0);
                     lockout--;}
  }

  return status;
}

extern "C" int ExitMsg(eSTS status,const char *format,...)
{
  int n = ERRNO(status),
      m = 0;

       if (status & STS_FATAL)   {ErrControl.f_errno = n; m = VRB_EXIT_E;}
  else if (status & STS_ERROR)   {ErrControl.e_errno = n; m = VRB_EXIT_E;}
  else if (status & STS_WARNING) {ErrControl.w_errno = n; m = VRB_EXIT_W;}
  else if (status & STS_INFO)    {ErrControl.i_errno = n;}

  if (ErrControl.verbosity & m) {
    va_list pvar;
    va_start(pvar, format);
    String msg(format,pvar);
    va_end(pvar);
    ErrorMsg(status,*msg.str() ? "%s" : "",msg.str());
  }

  FinalExit(n);

  return n;
}

int statusOK(eSTS status)
{
  if (!status || ErrControl.dont_stop) return 1;

  return 0;
}

extern "C" int Exit(eSTS status)
{
  const char *frmt = "",
             *xtra = 0;
  int         n;

  switch (status) {
  case STS_BAD_ARG: frmt = "%s (try -help)"; xtra=Arg.last;
  default:          break;
  case STS_NORMAL:  if (n = ErrControl.f_errno) {status = S_FATAL(n); break;}
                    if (n = ErrControl.e_errno) {status = S_ERROR(n); break;}
  }

  return ExitMsg(status,frmt,xtra);
}


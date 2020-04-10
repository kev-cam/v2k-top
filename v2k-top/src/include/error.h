/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  error_h_rcsid
#define error_h_rcsid() {return "$Id: error.h,v 1.25 2007/03/28 07:23:25 dkc Exp $";} /* RCS ID */
 
#ifndef ERROR_H
#define ERROR_H

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
  STS_ALLBAD     = -1,
  STS_NORMAL     =  0,
  STS_INFO       =  1,
  STS_WARNING    =  2,
  STS_ERROR      =  4,
  STS_FATAL      =  8,
  STS_MASK       = 15,
#define SYSTEM_ERROR(n,e,s) n = e*16,
#include "syserrs-a.h"
#include "usrerrs-a.h"
  STS_LAST
} eSTS;

#define S_INFO(e)    ((eSTS)(STS_INFO   |((e) & ~STS_MASK)))
#define S_FATAL(e)   ((eSTS)(STS_FATAL  |((e) & ~STS_MASK)))
#define S_ERROR(e)   ((eSTS)(STS_ERROR  |((e) & ~STS_MASK)))
#define S_WARNING(e) ((eSTS)(STS_WARNING|((e) & ~STS_MASK)))

#define ERRNO(e) ((e)>>4)
#define ERROR(e) ((eSTS)((e)<<4))

typedef enum {
  EMD_DEFAULT,
  EMD_INTERACTIVE,
  EMD_SCRIPT
} eEMD;

typedef struct {
# define GIVE_UP 1000
  int  give_up,
       verbosity;
  char am_chld;
  eEMD mode:8;
  int  dont_stop,
       do_stop,
       fatal,    f_errno,
       errors,   e_errno,
       warnings, w_errno, suppressed,
       info,     i_errno;
} ErrorControl;

extern ErrorControl ErrControl;

extern const char *ErrorString[];

typedef int (*ExitRtn)(void *);

extern int         Exit      (eSTS);
extern int         ExitMsg   (eSTS,const char *,...);
extern eSTS        ErrorMsg  (eSTS,const char *,...);
extern int         addExitRtn(ExitRtn,void *);
extern void        rmExitRtn (ExitRtn,void *);
extern void        LwpCount  (int);
extern eSTS        Error     ();
extern int         statusOK  (eSTS);
extern const char *StrError  (eSTS);

#ifdef __cplusplus
}
#endif

#endif

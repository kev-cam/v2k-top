/* Copyright (c) 1998,1999,2001,2002,2003 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  system_h_rcsid
#define system_h_rcsid() {return "$id";} /* RCS ID */

 
#ifndef SYSTEM_H
#define SYSTEM_H

#define MAC_MJR_VRS 8
#define MAC_MNR_VRS 10

#include "v2k_linux.h"

# define OS_TYPE   "mcd-PPC"
# define OS_SO_EXT ".dylib"
# define PTR_ITYPE unsigned long
# define BSD_MEM
# define POSIX_PW

# ifdef DEBUG
#  ifndef NO_FAST_HEADERS
//#   define FAST_HEADERS
#  endif
# endif

# ifndef CPP_ONLY
#  ifndef FAST_HEADERS
/*FAST*/
#ifdef __cplusplus
#   include <typeinfo>
#endif
#   include <sys/types.h>
#   include <sys/stat.h>
#   include <sys/param.h>
#   include <sys/mman.h>
#   include <sys/resource.h>
#   include <sys/select.h>
#   include <assert.h>
#   include <unistd.h>
#   include <inttypes.h>
#   include <dirent.h>
#   include <stdio.h>
#   include <stdlib.h>
#   include <stddef.h>
#   include <stdarg.h>
#   include <string.h>
#   include <time.h>
#   include <ctype.h>
#   include <pwd.h>
#   include <strings.h>
#   include <errno.h>
#   include <fcntl.h>
#   include <math.h>
#   include <signal.h>
#   include <sys/wait.h>
#   include <sys/poll.h>
#   include <dlfcn.h>
#   include <setjmp.h>
#   include <pthread.h>
#   ifndef HUGE
#    define HUGE HUGE_VAL
#   endif
/*ENDFAST*/
#  else
#   include "sys-inc.h"
#  endif
# endif

#ifdef _LP64
#define BAD_STDARG
#endif

# define MAIN_THREAD_ID      main_thread_id
# define SET_MAIN_THREAD_ID  {main_thread_id = pthread_self();}
# define DECL_MAIN_THREAD_ID uintptr_t main_thread_id;
extern DECL_MAIN_THREAD_ID

# define SIGCLD SIGCHLD

# define FD_NFDBITS               FD_SETSIZE
# define FD_WORDS(fd_lim,fd_bits) ((fd_lim + FD_NFDBITS)/FD_NFDBITS)

# define ADDR void *

# ifndef MAP_FAILED
# define MAP_FAILED    ((void *)(-1))
# endif

# include "unix_defs.h"

# ifdef __GNUC__
#  include "gnu_defs.h"
# else
#  include "cc_defs.h"
# endif

# include "misc_defs.h"

/*
#define JMP_BUFF_TYPE jmp_buf
*/

#define STS_ENOENT STS_E2
#define STS_EINTR STS_E4

#endif /* SYSTEM_H */


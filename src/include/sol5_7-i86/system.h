/* Copyright (c) 1998,1999,2001,2002,2003 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  system_h_rcsid
#define system_h_rcsid() {return "$id";} /* RCS ID */

 
#ifndef SYSTEM_H
#define SYSTEM_H

# define OS_TYPE   "sol5_7-i86"
# define PTR_ITYPE unsigned long
# define BSD_MEM
# if defined(__GNUC__) &&  __GNUC__ >= 3
#  define POSIX_PW
# else
#  define HAVE_GETPWNAM_R
# endif
# ifndef NOTHREADS
#  define PTHREADS
# endif

#if defined(__SUNPRO_C) || defined(__SUNPRO_CC)
# include "sunpro.h"
/* Workarounds */
# define _SYS_MACHTYPES_H
# define _SYS_INT_TYPES_H
# define EDG_0x EDG_0X
typedef long long          int64_t;
typedef unsigned long long uint64_t;
typedef unsigned char      uint8_t;
typedef unsigned int       uint32_t;
#endif

# ifdef DEBUG
#  ifndef NO_FAST_HEADERS
/*
  #   define FAST_HEADERS
*/
#  endif
# endif

# ifndef CPP_ONLY
#  ifndef FAST_HEADERS
/*FAST*/
#   include <sys/types.h>
#   include <sys/stat.h>
#   include <sys/param.h>
#   include <sys/mman.h>
#   include <sys/select.h>
#   include <sys/resource.h>
#   include <sys/siginfo.h>
#   include <assert.h>
#   include <unistd.h>
#   include <dirent.h>
#   include <stdio.h>
#   include <stdlib.h>
#   include <stdarg.h>
#   include <string.h>
#   include <ctype.h>
#   include <pwd.h>
#   include <strings.h>
#   include <errno.h>
#   include <fcntl.h>
#   include <ulimit.h>
#   include <math.h>
#   include <signal.h>
#   include <wait.h>
#   include <poll.h>
#   include <dlfcn.h>
#   include <setjmp.h>
#   ifdef PTHREADS
#    include <pthread.h>
#   endif
/*ENDFAST*/
#  else
#   include "sys-inc.h"
#  endif
# endif

# ifdef FD_NFDBITS
#  define FD_WORDS(fd_lim,fd_bits) __howmany(fd_lim,fd_bits)
# else
#  define FD_NFDBITS NFDBITS
#  define FD_WORDS(fd_lim,fd_bits) howmany(fd_lim,fd_bits)
# endif

# define ADDR caddr_t

# include "unix_defs.h"

# ifdef __GNUC__
#  include "gnu_defs.h"
# else
#  include "cc_defs.h"
# endif

# include "misc_defs.h"

#endif /* SYSTEM_H */

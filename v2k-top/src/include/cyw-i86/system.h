/* Copyright (c) 1998,1999,2001,2002,2003 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  system_h_rcsid
#define system_h_rcsid() {return "$id";} /* RCS ID */

 
#ifndef SYSTEM_H
#define SYSTEM_H

# define OS_TYPE    "cyw-i86"
# define PTR_ITYPE  unsigned long
# define BSD_MEM
# define NO_NICE
# define BZ_PTR_TYP char *

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
#   include <sys/wait.h>
#   include <assert.h>
#   include <unistd.h>
#   include <dirent.h>
#   include <stdio.h>
#   include <stdlib.h>
#   include <stdarg.h>
#   include <pwd.h>
#   include <string.h>
#   include <ctype.h>
#   include <strings.h>
#   include <errno.h>
#   include <fcntl.h>
#   include <limits.h>
#   include <math.h>
#   include <signal.h>
#   include <dlfcn.h>
#   include <pthread.h>
#   include <setjmp.h>
/*ENDFAST*/
#  else
#   include "sys-inc.h"
#  endif
# endif

#define FD_NFDBITS               NFDBITS
#define FD_WORDS(fd_lim,fd_bits) howmany(fd_lim,fd_bits)

# define ADDR caddr_t

# define OS_DIR_SEP2 "\\"
# include "unix_defs.h"

# ifdef __GNUC__
#  include "gnu_defs.h"
# else
#  include "cc_defs.h"
# endif

#ifndef MAX_DESC
# define MAX_DESC   63
#endif

#ifndef MAP_NORESERVE
# define MAP_NORESERVE 0
#endif

#define RTLD_LOCAL 0
#define HUGE       HUGE_VAL
#define O_RAW      O_BINARY

# include "misc_defs.h"

#endif /* SYSTEM_H */

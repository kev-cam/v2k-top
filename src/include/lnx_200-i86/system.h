/* Copyright (c) 1998,1999,2001,2002,2003 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  system_h_rcsid
#define system_h_rcsid() {return "$id";} /* RCS ID */

 
#ifndef SYSTEM_H
#define SYSTEM_H

# define OS_TYPE   "lnx200-i86"
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
#   include <sys/types.h>
#   include <sys/stat.h>
#   include <sys/param.h>
#   include <sys/mman.h>
#   include <sys/resource.h>
#   include <sys/select.h>
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
#   include <math.h>
#   include <signal.h>
#   include <wait.h>
#   include <waitflags.h>
#   include <dlfcn.h>
/*ENDFAST*/
#  else
#   include "sys-inc.h"
#  endif
# endif

#define FD_NFDBITS               __NFDBITS
#define FD_WORDS(fd_lim,fd_bits) ((fd_lim + FD_NFDBITS)/FD_NFDBITS)

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

#endif /* SYSTEM_H */



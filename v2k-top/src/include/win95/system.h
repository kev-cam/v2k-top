/* Copyright (c) 1998,1999,2001,2002,2003 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  system_h_rcsid
#define system_h_rcsid() {return "$id";} /* RCS ID */

 
#ifndef SYSTEM_H
#define SYSTEM_H

#define OS_TYPE "win95"
#define WIN95

# ifndef CPP_ONLY
#ifdef __cplusplus
#  include <typeinfo>
#endif
#  include <sys/types.h>
#  include <sys/stat.h>
#  include <assert.h>
#  include <stdio.h>
#  include <stdlib.h>
#  include <stdarg.h>
#  include <string.h>
#  include <ctype.h>
#  include <errno.h>
#  include <fcntl.h>
#  include <io.h>
#  include <math.h>
#  include <dir.h>
#  include <malloc.h>

#  ifndef MAXPATHLEN
#    define MAXPATHLEN MAXPATH
#  endif

# endif

# include "windows_defs.h"

# ifdef __GNUC__
#  include "gnu_defs.h"
# else
#  include "cc_defs.h"
# endif

#define STDIN_FILENO  0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2

#define MAX_DESC      255
#define F_OK          00

#define mkdir(p,m)    mkdir(p)

#define NO_MMAP
#define MAP_FAILED    0
#define PROT_READ     001
#define PROT_WRITE    002
#define PROT_EXEC     004
#define MAP_SHARED    010
#define MAP_PRIVATE   020

#endif /* SYSTEM_H */






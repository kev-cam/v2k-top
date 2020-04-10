/* Copyright (c) 1998,1999,2001,2002,2003 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  system_h_rcsid
#define system_h_rcsid() {return "$id";} /* RCS ID */

 
#ifndef SYSTEM_H
#define SYSTEM_H

#define OS_TYPE "hpb-PAR"

#define BSD_MEM

# ifndef CPP_ONLY
#  include <sys/types.h>
#  include <sys/stat.h>
#  include <sys/param.h>
#  include <sys/mman.h>
#  include <sys/resource.h>
#  include <assert.h>
#  include <unistd.h>
#  include <stdio.h>
#  include <stdlib.h>
#  include <stdarg.h>
#  include <string.h>
#  include <ctype.h>
#  include <strings.h>
#  include <errno.h>
#  include <fcntl.h>
#  include <ulimit.h>
#  include <math.h>
# endif

# include "unix_defs.h"

# ifdef __GNUC__
#  include "gnu_defs.h"
# else
#  include "cc_defs.h"
# endif

#endif /* SYSTEM_H */






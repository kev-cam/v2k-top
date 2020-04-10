/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  gnu_defs_h_rcsid
#define gnu_defs_h_rcsid() {return "$Id: gnu_defs.h,v 1.16 2007/03/28 07:23:26 dkc Exp $";} /* RCS ID */
 
#if OPTLVL > 0
# define INLINE      inline
#else
# define INLINE
#endif
#define InLine       inline
#define UNSIZED(def) 0
#define SIZED(t,n)   (sizeof(t) * (n))
#define CS_INIT(v)
#define S_INIT(v)    = v
#define SIZEOF_VTAB  (sizeof(void *))

# define CMPLR       "GNU"
# define CMPLR_MAJOR __GNUC__
# define CMPLR_MINOR __GNUC_MINOR__

#define HAVE_LL
#include "v2k_ll.h"

#if defined (__GNUC__) && __GNUC__ > 2
# define NO_BITFIELD_ADDR
#endif

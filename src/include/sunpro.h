/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  sunpro_h_rcsid
#define sunpro_h_rcsid() {return "$Id: sunpro.h,v 1.7 2007/03/28 07:23:26 dkc Exp $";} /* RCS ID */

 
# define HAVE_LL
# define SIZEOF_VTAB (sizeof(void *))
# define USE_ALLOCA
# define NO_BITFIELD_ADDR
# define CMPLR       "SUNPRO"
# define CMPLR_MAJOR (__SUNPRO_C & 0xFF)
# define CMPLR_MINOR (__SUNPRO_C >> 8)

# include <sys/int_types.h>

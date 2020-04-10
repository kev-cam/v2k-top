/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  v2k_linux_h_rcsid
#define v2k_linux_h_rcsid() {return "$Id: v2k_linux.h,v 1.7 2007/03/28 07:23:27 dkc Exp $";} /* RCS ID */

#ifndef V2K_LINUX_H
#define V2K_LINUX_H
 
/* linux common stuff */

#if LNX_MJR_VRS < 2
# define NOTHREADS
#else
# if LNX_MNR_VRS < 3
#  define NOTHREADS
# endif
#endif

#ifndef _SVID_SOURCE
# define _SVID_SOURCE
#endif

#endif /* V2K_LINUX_H */

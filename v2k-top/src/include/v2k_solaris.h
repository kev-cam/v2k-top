/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  v2k_solaris_h_rcsid
#define v2k_solaris_h_rcsid() {return "$Id: v2k_solaris.h,v 1.5 2007/03/28 07:23:27 dkc Exp $";} /* RCS ID */

 
/* linux common stuff */

#if SOL_MJR_VRS < 5
# define NOTHREADS
#else
# if SOL_MNR_VRS < 7
#  define NOTHREADS
# endif
#endif


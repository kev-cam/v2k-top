/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  defpmode_h_rcsid
#define defpmode_h_rcsid() {return "$Id: defpmode.h,v 1.13 2007/03/28 07:23:27 dkc Exp $";} /* RCS ID */

 
#ifndef DEFMODE_H

#undef  POOL_MODE
#define POOL_MODE  PM_Template
#undef  PM_PRFX
#define PM_PRFX    Template
#undef  PM_CLASS
#define PM_CLASS   "Template"

#undef  POOL_MODE_INM
#undef  POOL_MODE_CTG
#undef  POOL_MODE_DSK
#undef  POOL_MODE_MPD
#undef  POOL_MODE_NLD
#undef  POOL_MODE_VRT

#ifdef POOL_MODE_INM
#endif
#ifdef POOL_MODE_CTG
#endif
#ifdef POOL_MODE_DSK
#endif
#ifdef POOL_MODE_MPD
#endif
#ifdef POOL_MODE_NLD
#endif
#ifdef POOL_MODE_VRT
#endif

#endif



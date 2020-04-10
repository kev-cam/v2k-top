/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  assertions_h_rcsid
#define assertions_h_rcsid() {return "$Id: assertions.h,v 1.14 2007/03/28 07:23:25 dkc Exp $";} /* RCS ID */

 
#ifdef NO_ASSERTIONS
#undef  assert
#define assert(e) 0
#else
#ifndef SYSTEM_H
#include "system.h"
#endif
#ifdef DEBUG
#define ASSERT(e) assert(e)
#else
#define ASSERT(e) 0
#endif
#endif

#define NIY(s) ("Not Implemented:" s == 0)

#ifndef INLINE
/* controlled by DBGLVL & OPTLVL in for each compiler system.h */
# define INLINE inline
#endif

#ifndef DEAD_BEEF
#define DEAD_BEEF(t) ((t*)0xDEADBEEF)
#endif

/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  tref_h_rcsid
#define tref_h_rcsid() {return "$Id: tref.h,v 1.12 2007/03/28 07:23:25 dkc Exp $";} /* RCS ID */

 
#include "ref.h"

#ifdef __cplusplus
#include "ref-pp.h"
#else
#undef  IREF
#define IREF(x) poolRef
#endif

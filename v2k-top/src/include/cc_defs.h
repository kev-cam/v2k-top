/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  cc_defs_h_rcsid
#define cc_defs_h_rcsid() {return "$Id: cc_defs.h,v 1.10 2007/03/28 07:23:26 dkc Exp $";} /* RCS ID */

 
#define InLine
#define UNSIZED(def) 1
#define SIZED(t,n) (sizeof(t) * ((n)-1))
#define CS_INIT(v)
#define S_INIT(v) = v

#include "v2k_ll.h"


/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  str_pool_h_rcsid
#define str_pool_h_rcsid() {return "$Id: str_pool.h,v 1.14 2007/03/28 07:23:26 dkc Exp $";} /* RCS ID */

 
#include "template.h"
#include "ref-pp.h"
#include "str_pool-pp.h"

#include "poolmngr.h"

#define TOK_POOL_START 0x100

BasePool *CoerceStringPool(PoolIdentifier);

/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  mod_pool_h_rcsid
#define mod_pool_h_rcsid() {return "$Id: mod_pool.h,v 1.12 2007/03/28 07:23:26 dkc Exp $";} /* RCS ID */
 
#include "po_pool.h"
#include "mod_pool-pp.h"

BasePool        *CoerceModulePool(PoolIdentifier);
InMemModulePool *ModPool(int);


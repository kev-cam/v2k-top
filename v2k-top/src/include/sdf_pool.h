/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  sdf_pool_h_rcsid
#define sdf_pool_h_rcsid() {return "$Id: sdf_pool.h,v 1.7 2007/03/28 07:23:25 dkc Exp $";} /* RCS ID */
 
class InMemSdfPool;

#include "template.h"
#include "ref-pp.h"
#include "xp_pool.h"
#include "sdf_pool-pp.h"

#include "poolmngr.h"

void          SDF_Pool(PoolIdentifier);
BasePool     *CoerceSdfPool(PoolIdentifier);
InMemSdfPool *SdfPool(int);

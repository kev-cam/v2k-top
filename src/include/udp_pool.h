/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  udp_pool_h_rcsid
#define udp_pool_h_rcsid() {return "$Id: udp_pool.h,v 1.11 2007/03/28 07:23:27 dkc Exp $";} /* RCS ID */

 
#include "po_pool.h"
#include "udp_pool-pp.h"

BasePool     *CoerceUdpPool(PoolIdentifier);
InMemUdpPool *UdpPool(int);


/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  po_pool_h_rcsid
#define po_pool_h_rcsid() {return "$Id: po_pool.h,v 1.7 2007/03/28 07:23:27 dkc Exp $";} /* RCS ID */

 
class Inst;
class InMemPoPool;
class InMemUdpPool;
class InMemModulePool;

#define A_STMT(p) ((p) ? 1 : 0)

#include "xp_pool.h"
#include "po_pool-pp.h"

InMemPoPool *PoPool(int);

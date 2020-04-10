/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  arith_h_rcsid
#define arith_h_rcsid() {return "$Id: arith.h,v 1.10 2007/03/28 07:23:26 dkc Exp $";} /* RCS ID */

 
#ifndef REF_H
#include "ref.h"
#endif

class InMemExpr;  DECL_XREF(InMemExpr);
class ContigExpr; DECL_XREF(ContigExpr);
class OnDiskExpr; DECL_XREF(OnDiskExpr);
class MappedExpr; DECL_XREF(MappedExpr);

#include "ref-pp.h"
#include "arith-pp.h"

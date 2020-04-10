/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: $Id: edges.inc,v 1.6 2007/02/01 06:47:29 dkc Exp $ */
  


EDGE(POSEDGE,	"posedge")
EDGE(NEGEDGE,	"negedge")
#ifndef KEY_EDGES
EDGE(X1,	"x1")
EDGE(X0,	"x0")
EDGE(XZ,	"xz")
EDGE(01,	"01")
EDGE(0Z,	"0z")
EDGE(0X,	"0x")
EDGE(10,	"10")
EDGE(1Z,	"1z")
EDGE(1X,	"1x")
EDGE(Z1,	"z1")
EDGE(Z0,	"z0")
EDGE(ZX,	"zx")
#endif
#undef KEY_EDGES

/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: $Id: poolmode.inc,v 1.6 2007/02/01 06:47:29 dkc Exp $ */
  


#ifndef MODES_A_H
#include "modes-a.h"
#endif

#ifdef USING_MPD
PM_ACTION(Mapped)
#endif
#ifdef USING_CTG
PM_ACTION(Contig)
#endif
#ifdef USING_INM
PM_ACTION(InMem)
#endif
#ifdef USING_DSK
PM_ACTION(OnDisk)
#endif
#ifdef USING_VRT
PM_ACTION(Virtual)
#endif

#undef PM_ACTION


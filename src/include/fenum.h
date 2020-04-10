/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  fenum_h_rcsid
#define fenum_h_rcsid() {return "$Id: fenum.h,v 1.2 2007/10/01 22:46:31 dkc Exp $";} /* RCS ID */

#ifndef FENUM_H
#define FENUM_H

typedef enum {
  FM_NONE       = 0,
  FM_READ       = 0x00001,
  FM_WRITE      = 0x00002,
  FM_RW         = FM_READ|FM_WRITE,
  FM_APPEND     = 0x00004,
  FM_RAW        = 0x00008,
  FM_CREATE     = 0x00010,
  FM_TRUNC      = 0x00020,
  FM_ZCLOSE     = 0x00040,
  FM_DCLOSE     = 0x00080,
  FM_EXE        = 0x00100,
  FM_MKDIR      = 0x00200,
  FM_SHARED     = 0x00400,
  FM_ZPIPED     = 0x00800,
  FM_CMP        = 0x01000,
  FM_BIN        = 0x02000,
  FM_NO_CLOSE   = 0x04000,
  FM_NO_SUSPEND = 0x08000,
  FM_LOCAL      = 0x10000,
  FM_PIPE       = 0x20000,

  FM_ANY_WRITE = FM_WRITE|FM_APPEND|FM_CREATE

} eFM;

#endif

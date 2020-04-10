/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  v2k_misc_h_rcsid
#define v2k_misc_h_rcsid() {return "$Id: v2k_misc.h,v 1.5 2020/04/07 06:20:57 dkc Exp $";} /* RCS ID */

 
#ifndef MISC_H
#define MISC_H

#include "v2k_ll.h"

#ifdef __cplusplus
extern "C" {
#endif

  void CopyVirtual(const void *,void *);
  int  getRlimit(int,I64 *);
  int  setRlimit(int,I64);
#ifndef HAVE_CODE_MODE
#ifndef SYSTEM_ERROR
  typedef int eSTS;
#endif
  eSTS v2kCodegen(int);
#endif

#ifdef __cplusplus
}
#endif

typedef struct {
  int   lim,
        byt;
  char *name;
} Limit;

extern const Limit Limits[];

#ifndef UI_TYPES
typedef unsigned int   U32;
typedef int            I32;
typedef unsigned short U16;
typedef short          I16;
#endif

#endif

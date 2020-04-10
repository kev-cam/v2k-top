/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  desc_h_rcsid
#define desc_h_rcsid() {return "$Id: desc.h,v 1.15 2007/03/28 07:23:26 dkc Exp $";} /* RCS ID */

 
#ifndef DESC_H
#define DESC_H

typedef struct sFieldDesc {
  const char        *type,
                    *name;
  int                size,
                     offset;
  struct sFieldDesc *desc;
} FieldDesc;

extern FieldDesc *const voidDesc,
                 *const intDesc,
                 *const poolRefDesc,
                 *const charDesc;

typedef int (*DescFn)(FieldDesc *);

#ifdef __cplusplus
extern "C"
#endif
int DescribePool(FieldDesc *);

#endif

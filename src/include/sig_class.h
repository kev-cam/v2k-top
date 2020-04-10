/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  sig_class_h_rcsid
#define sig_class_h_rcsid() {return "$Id: sig_class.h,v 1.9 2007/03/28 07:23:26 dkc Exp $";} /* RCS ID */

 
#define DATA_WIDTH :1
#define DATA_TYPE  int
#define VEC_CLASS  CmpctBit
#define VEC_PARENT SignalBase
#include "vector.inc"

#define DATA_TYPE  char
#define VEC_CLASS  Bit
#define VEC_ARRAY  Bit_A
#define VEC_PARENT SignalBase
#include "vector.inc"

#define VEC_PARENT VectorBase
#define DATA_TYPE  int8_t
#define VEC_CLASS  VecByte
#define VEC_ARRAY  VecByte_A
#include "vector.inc"

#define VEC_PARENT VectorBase
#define DATA_TYPE  uint8_t
#define VEC_CLASS  VecByteU
#define VEC_ARRAY  VecByteU_A
#include "vector.inc"

#define VEC_PARENT VectorBase
#define DATA_TYPE  int32_t
#define VEC_CLASS  Vec32
#define VEC_ARRAY  Vec32_A
#include "vector.inc"

#define VEC_PARENT VectorBase
#define DATA_TYPE  uint32_t
#define VEC_CLASS  Vec32U
#define VEC_ARRAY  Vec32U_A
#include "vector.inc"

#define VEC_PARENT VectorBase
#define DATA_TYPE  int64_t
#define VEC_CLASS  Vec64
#define VEC_ARRAY  Vec64_A
#include "vector.inc"

#define VEC_PARENT VectorBase
#define DATA_TYPE  uint64_t
#define VEC_CLASS  Vec64U
#define VEC_ARRAY  Vec64U_A
#include "vector.inc"


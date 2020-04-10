/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  ref_h_rcsid
#define ref_h_rcsid() {return "$Id: ref.h,v 1.37 2010/05/13 18:39:34 dkc Exp $";} /* RCS ID */
 
#ifndef REF_H
#define REF_H

#define POOL_REF_FLDS int pool,index;

typedef struct {
  POOL_REF_FLDS
} poolRef;

extern const poolRef BadRef,
                     NlRef,
                     CrRef,
                     NullRef;

#define SAME_REF(a,b)       ((a).pool == (b).pool && (a).index == (b).index)
#define NULL_REF(a)         ((a).pool == 0 && (a).index == 0)
#define BAD_REF(a)          ((a).pool < 0)
#define SAME_REF_PI(a,p,i)  ((a).pool == (p) && (a).index == (i))
#define REF_AS_INT(ref)     ((ref.pool << 16) | ref.index)
#define REF_FROM_INT(ref,i) (ref.pool = (i) >> 16,ref.index = (i) & 0xFFFF,ref)

#define EOL_REF(a)          (SAME_REF(a,NlRef) || SAME_REF(a,CrRef))

#define DECL_REF(t) \
typedef union  {\
  double  as_double;\
  I64     i64;\
  t      *ptr;\
  char   *cp;\
  poolRef pi;\
} t##Ref;
#define REF(t)  t##Ref

#define DECL_IREF(t) \
typedef union  {\
  double  as_double;\
  I64     i64;\
  t      *ptr;\
  char   *cp;\
  poolRef pi;\
} t##Iref;
#define IREF(t) t##Iref

#define DECL_XREF(t) \
typedef union  {\
  double  as_double;\
  I64     i64;\
  t      *ptr;\
  poolRef pi;\
} t##Xref;
#define XREF(t) t##Xref

#define IR(a) ((voidIref *)&(a))
#define XR(a) ((voidXref *)&(a))

#define PTR2DATA(p) ((int)(p))
#define DATA2PTR(p) ((void *)(p))

#define VAR_ARRAY(t)     t[1]
#define VAR_ARRAY_PTR(t) t

/* Pool Reference Operators */

void ref_cast(double,void *);

#ifndef REF_CAST
# define REF_CAST(t,ir,r,tmp) return *(t##ir *)&r
#endif

#define DECL_IDEREF(t)\
  inline t       *P(t##Iref &X)  {return (t *)i_deref((voidIref *)&X,0);};\
  inline t       *Px(t##Iref &X) {return (t *)i_deref((voidIref *)&X,1);};\
  inline t##Iref  I(t##Xref &X)  {REF_CAST(t,Iref,X,i);};

#define DECL_XDEREF(t)\
  inline t       *P(t##Xref &X) {return (t *)deref((voidXref *)&X);};\
  inline t##Xref  X(t##Iref &X) {REF_CAST(t,Xref,X,x);};

#define INCR_REF(p) incr_ref((charIref *)&(p),sizeof(*p.ptr))

#define DECL_IXR(t) DECL_REF(t)\
                    DECL_IREF(t)\
                    DECL_XREF(t)

#include "ixderef.inc"

#undef  DECL_IXR
#define DECL_IXR(t) DECL_IDEREF(t)\
                    DECL_XDEREF(t)

#define P_TYPE(t,r) ((t)((void *)P(r)))
#define P_EXPR(r)   P_TYPE(plExpr *,r)

#endif

/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  v2k_ll_h_rcsid
#define v2k_ll_h_rcsid() {return "$Id: v2k_ll.h,v 1.4 2007/03/28 07:23:27 dkc Exp $";} /* RCS ID */

#ifndef V2K_LL_H 
#define V2K_LL_H 

#ifdef HAVE_LL

typedef int64_t  I64;
typedef uint64_t U64;

# define SET_LL(ll,vl) (ll = (vl))
# define LL2INT(ll)    (ll)

#else

/* Long-long type for non-supporting compilers */

# ifdef __cplusplus

class U64 {
 unsigned int hi,
              lo;
};

class I64 {
 int          hi;
 unsigned int lo;
};

# define SET_LL(ll,vl) (ll.hi = (vl) >> 32,ll.lo = (vl) & 0xFFFFFFFF)
# define LL2INT(ll)    (ll.lo)

# endif
#endif

#endif /* V2K_LL_H */

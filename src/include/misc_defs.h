/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  misc_defs_h_rcsid
#define misc_defs_h_rcsid() {return "$Id: misc_defs.h,v 1.14 2012/10/16 22:30:00 cvs Exp $";} /* RCS ID */
 
#if DBGLVL > 0
#define DEBUG
#endif

#if __GNUC_MINOR__ > 8
# define NO_BITFIELD_ADDR
#endif

#ifdef NO_BITFIELD_ADDR
# define BITF(t,n,s,a) a n
#else
# define BITF(t,n,s,a) t n:s
#endif

#ifdef USE_ALLOCA
#include <alloca.h>
#define TMPARR_POD(t,n,s)     t *n = (t *)alloca((s) * sizeof(t))
#define TMPARR(t,n,s)         t *n = (t *)alloca((s) * sizeof(t))
#define TMPARR_INIT(n,s,i)    {int c = s; while (c-- > 0) n[c].i();}
#define TMPARR_DESTROY(n,s,d) {int c = s; while (c-- > 0) n[c].d();}
#else
#define TMPARR_POD(t,n,s)     t n[s]
#ifdef __clang__
/* gcc incompatibiity */
#define TMPARR(t,n,s)         t *n = new t[s]
#define TMPARR_DESTROY(n,s,d) delete [] n
#else
#define TMPARR(t,n,s)         t n[s]
#define TMPARR_DESTROY(n,s,d) 
#endif
#define TMPARR_INIT(n,s,i)
#endif

#define UNSET_PTR_VAL(t) ((t *)-1)
#define UNSET_PTR(p)     (UNSET_PTR_VAL(void) == ((void *)(p)))

#ifndef WNOWAIT
# define WNOWAIT 0
#endif

#ifndef WCONTINUED
# define WCONTINUED 0
#endif

#ifndef SA_SIGINFO
typedef int siginfo_t;
#endif

typedef size_t (*fwrite_fn)(const void *,size_t,size_t,FILE *);

#define OffsetOf(s,f) offsetof(s,f)

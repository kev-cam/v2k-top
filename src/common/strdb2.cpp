/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * strdb2_cpp_rcsid() {return "$Id: strdb2.cpp,v 1.29 2009/05/22 21:21:39 dkc Exp $";}
 
#include "assertions.h"
#include "system.h"
#include "poolmngr.h"
#include "str_pool.h"
#include "strdb.h"

poolRef NullStrRef;

extern "C" const char *strDeref(const poolRef ref)
{
  if (!ref.pool) {
    if (!ref.index)  return "(null)";
    return "??EOF??";
  }
  if (SAME_REF(ref,NullStrRef)) return "";

  PoolStr *s = (PoolStr *)(PoolMngr->pool(ref.pool)->deref(ref.index));

  if (!*s->buff) {
    strReload(ref.pool);
    s = (PoolStr *)(PoolMngr->pool(ref.pool)->deref(ref.index));
    assert("String-pool out of sync" && *s->buff);
  }

  return s->buff;
}

extern "C" char *strCheck(const poolRef ref)
{
  PoolStr *s = (PoolStr *)(PoolMngr->pool(ref.pool)->deref(ref.index));

  return s->buff;
}

extern "C" const char *strDerefPI(int p,int i)
{
   poolRef ref;

   ref.pool  = p;
   ref.index = i;

   return strDeref(ref);
}

extern "C" const char *strDerefArr(poolRef *pRef,int len,String *str,const char *pad)
{
#ifdef DEBUG
  static String *buff;

  if (!str) {
    if (!buff) buff = new String;
    *(str = buff)   = "";
  }
#endif

  while (len-- > 0) {*str += strDeref(*pRef++);
                     *str += pad;}

  return str->str();
}

extern "C" int strRefCmpV(const poolRefV *a,const poolRefV *b)
{
  const poolRef &A(*a),
                &B(*b);

  int diff = B.pool - A.pool;

  if (!diff) {
    diff = B.index - A.index;
    if (!diff) return 0;
  }

  return diff > 0 ? 1
                  : -1;
}

extern "C" int strRefCmpVdbg(const poolRefV *a,const poolRefV *b)
{
  const poolRef &A(*a),
                &B(*b);

  fprintf(stderr,"%s=?=%s",strDeref(A),strDeref(B));

  int diff = strRefCmpV(a,b);

  fprintf(stderr," = %d\n",diff);

  return diff;
}

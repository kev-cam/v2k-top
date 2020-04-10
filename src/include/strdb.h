/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  strdb_h_rcsid
#define strdb_h_rcsid() {return "$Id: strdb.h,v 1.28 2012/10/16 22:30:00 cvs Exp $";} /* RCS ID */

 
#ifndef STRDB_H
#define STRDB_H

#ifndef POOL_H
#include "pool.h"
#endif

#ifdef __cplusplus

struct poolRefV : public poolRef {
  virtual       poolRef *ref()       {return this;}
  virtual const poolRef *ref() const {return this;}
};

extern "C" {
#endif

typedef void (*poolStrFn)(poolRef);

void            strRegister(const PoolStr *,const poolRef,int mngr_idx);
poolRef         strFind(const char *);
poolRef         strFindSeg(const char *,int);
const char     *strDeref(const poolRef);
char           *strCheck(const poolRef);
void            strForAll(poolStrFn);
poolRef         strSaveStr(const char *);
void            InitStrDB(const char *);
const char     *strDerefArr(poolRef *,int,String *,const char *);
void            strReload(int);
void            strPoolShared(int);
int             strRefCmp(const poolRef *,const poolRef *);
int             strRefCmpDbg(const poolRef *,const poolRef *);
#ifdef __cplusplus
int             strRefCmpV(const poolRefV *,const poolRefV *);
int             strRefCmpVdbg(const poolRefV *,const poolRefV *);
#endif
extern char     defPoolMode[];
extern poolRef  NullStrRef;

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus


#endif

#endif /* STRDB_H */


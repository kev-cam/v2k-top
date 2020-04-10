/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  dyn_h_rcsid
#define dyn_h_rcsid() {return "$Id: dyn.h,v 1.9 2007/03/28 07:23:26 dkc Exp $";} /* RCS ID */
 
#ifndef DLD_H
#define DLD_H

typedef int (*dyn_fn)(void *);

class DynObj {

  static DynObj *List;

  DynObj *next;
  void   *handle;
  void   *data;
  String  path;
  String  error;
 public:
  DynObj(const char *,const char *init = "v2kInit",int shr_sym = 0);
  ~DynObj();

  int   Call(const char *);
  int   unload();
  int   unload(const char *);
  void *DlSym(const char *);

  inline operator bool () {return 0 != handle;};

  inline eSTS Status() {return handle ? STS_NORMAL
                                      : STS_DLERROR;};
};

#endif

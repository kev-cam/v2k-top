/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * dyn_cpp_rcsid() {return "$Id: dyn.cpp,v 1.13 2009/07/14 18:04:22 dkc Exp $";}

#include "system.h"
#include "strfunc.h"
#include "error.h"
#include "env.h"
#include "dyn.h"
#include "args.h"

DynObj *DynObj::List;

DynObj::DynObj(const char *lib_spec,const char *init,int shr_sym)
{
  int  done = 0,
       sep  = strstr(lib_spec,OS_DIR_SEP) ||
#ifdef OS_DIR_SEP2
              strstr(lib_spec,OS_DIR_SEP2) ||
#endif
              strstr(lib_spec,OS_TYPE_SEP),
       flgs = RTLD_NOW;
  
  if (shr_sym) flgs |= RTLD_GLOBAL;

  if (sep) {
    path  = lib_spec;
  } else {
    path  = OS_LIB_PRFX;
    path += lib_spec;
    path += OS_LIB_SHRD;
  }
 retry:
  if (handle = dlopen(path,flgs)) {
    if (init) Call(init);
  } else if (!done++) {
    error = dlerror();
    if (0 !=  strncmp(OS_ROOT,lib_spec,strlen(OS_ROOT))) {
      const char *a0 = Arg0();
      path  = "${" V2K_ROOT "}lib" OS_DIR_SEP "${" V2K_OS_OBJ "}";
      if (sep) {
	path += lib_spec;
      } else {
	path  = OS_LIB_PRFX;
	path += lib_spec;
	path += OS_LIB_SHRD;
      }
      envExpand(&path);
      char buff[100 + path.len()];
      if ( 0 == readlink(path.str(),buff,sizeof(buff)-1)) {
	path = buff;
      }
      goto retry;
    }
  }
#if DBGLVL > 1
  if (!handle) fprintf(stderr,"DL: %s (%s) - %s\n",
                                   lib_spec,path.str(),error.str());
#endif
  next = List;
  List = this;
}

DynObj::~DynObj()
{
  DynObj **lpp  = &List,
          *lp;

  while ((lp = *lpp) != this) lpp = &lp->next;

  *lpp = lp->next;
}

int DynObj::unload()
{
  Call("v2kUnload");
  delete(this);
}

int DynObj::unload(const char *targ)
{
  DynObj *lp  = List;
  int     sts = -1;

  for (; lp ; lp = lp->next) {
    if (0 == strcmp(lp->path,targ)) break;
  }

  if (lp) sts = lp->unload();

  return sts;
}

int DynObj::Call(const char *rtn)
{
  void *pr  = dlsym(handle,rtn);
  int   sts = -1;

  if (pr) {
    sts = (*(dyn_fn)pr)(this);
  }

  return sts;
}

void *DynObj::DlSym(const char *name)
{
  return dlsym(handle,name);
}

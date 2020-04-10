/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */

#undef  v2k_h_rcsid
#define v2k_h_rcsid() {return "$Id: v2k.h,v 1.20 2010/04/20 07:10:19 dkc Exp $";} /* RCS ID */

 
#ifdef ARGS_H

extern argHndlr v2kHndlrs[];

#ifdef V2K_DECL_MODE
V2K_DECL_MODE int setTop     (int *,const char ***,void *);
V2K_DECL_MODE int doElab     (int *,const char ***,void *);
V2K_DECL_MODE int doShell    (int *,const char ***,void *);
V2K_DECL_MODE int doCommand  (int *,const char ***,void *);
V2K_DECL_MODE int argMake    (int *,const char ***,void *);
V2K_DECL_MODE int codeGen    (int *,const char ***,void *);
V2K_DECL_MODE int startSim   (int *,const char ***,void *);
V2K_DECL_MODE int doSource   (int *,const char ***,void *);
V2K_DECL_MODE int autoLoad   (int *,const char ***,void *);
V2K_DECL_MODE int portMode   (int *,const char ***,void *);
V2K_DECL_MODE int jobLimit   (int *,const char ***,void *);
V2K_DECL_MODE int timeOut    (int *,const char ***,void *);
V2K_DECL_MODE int unmatchedOK(int *,const char ***,void *);
V2K_DECL_MODE int showTok    (int *,const char ***,void *);
V2K_DECL_MODE int showMode   (int *,const char ***,void *);
#endif

#endif

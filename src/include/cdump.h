/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  cdump_h_rcsid
#define cdump_h_rcsid() {return "$Id: cdump.h,v 1.6 2007/03/28 07:23:25 dkc Exp $";} /* RCS ID */

 
#ifdef __cplusplus
extern "C" {
#endif

eSTS cDumpC (int *argc,const char ***argv,void *var);

#ifdef __cplusplus
extern "C" }
#endif

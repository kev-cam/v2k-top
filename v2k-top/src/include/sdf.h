/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  sdf_h_rcsid
#define sdf_h_rcsid() {return "$Id: sdf.h,v 1.15 2007/03/28 07:23:25 dkc Exp $";} /* RCS ID */

 
#ifdef __cplusplus
extern "C" {
#endif

int  InitSdfTok();
eSTS tokSDF    (FILE *,fwrite_fn,FILE *);
int  prsSDF    (int *argc,const char ***argv,void *var,int,int);
int  sdfSetSz  (int *argc,const char ***argv,void *var,int,int);
int  sdfPiped  (int *argc,const char ***argv,void *var,int,int);

extern char *strChk[];

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
eSTS tokSDF    (Stream *,Stream *);
#endif

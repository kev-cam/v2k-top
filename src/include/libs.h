/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  libs_h_rcsid
#define libs_h_rcsid() {return "$Id: libs.h,v 1.8 2009/05/22 21:14:25 dkc Exp $";} /* RCS ID */

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _LibInfo {
  struct _LibInfo *next;
  const char      *name;
  const char      *version;
} LibInfo;


extern LibInfo  V2KlibCommon,
                V2KlibLang,
               *V2KloadedLibs;

#ifdef __cplusplus
}
#endif

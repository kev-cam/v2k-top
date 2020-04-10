/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  language_h_rcsid
#define language_h_rcsid() {return "$Id: language.h,v 1.12 2009/11/25 02:47:22 dkc Exp $";} /* RCS ID */

#ifndef LANGUAGE_H
#define LANGUAGE_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

extern const char *LangName[];

extern void  InitLang(const char *,char *);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* LANGUAGE_H */

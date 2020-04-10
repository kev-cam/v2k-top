/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * ldlib_cpp_rcsid() {return "$Id: ldlib.cpp,v 1.8 2007/02/01 06:46:48 dkc Exp $";}

#include "v2k_mem.h"
#include "error.h"
#include "env.h"
#include "file.h"
#include "libs.h"
#include "version.h"

#ifdef __cplusplus
extern "C" {
#endif

LibInfo V2KlibCommon = {0,"Common",V2K_STR_VERSION};

LibInfo  V2KlibLang    = {&V2KlibCommon,"Languages",V2K_STR_VERSION},
        *V2KloadedLibs = &V2KlibLang;

#ifdef __cplusplus
}
#endif

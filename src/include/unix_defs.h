/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  unix_defs_h_rcsid
#define unix_defs_h_rcsid() {return "$Id: unix_defs.h,v 1.22 2009/11/25 01:20:55 dkc Exp $";} /* RCS ID */

 
#define UNIX

#define OS_DIR_SEP  "/"
#define OS_TYPE_SEP "."
#define OS_CWD      "."
#define OS_PARENT   ".."
#define OS_ROOT     "/"
#define OS_EXE_PATH "PATH"
#define OS_TMP_EXT  "+"
#define OS_LIB_PRFX "lib"
#ifdef OS_SO_EXT
#define OS_LIB_SHRD OS_SO_EXT
#else
#define OS_LIB_SHRD ".so"
#endif
#define SH_LST_SEP  ":"

#define DEV_NULL    "/dev/null"

#ifdef RLIMIT_NOFILE
#ifdef __cplusplus
extern "C"
#endif
int getRlimitInt(int);
#define MAX_DESC    (getRlimitInt(RLIMIT_NOFILE))
#endif

#ifdef DEBUG
# define USE_PIPES 0
#endif

#ifndef MAIN_THREAD_ID
# define MAIN_THREAD_ID 1
# define SET_MAIN_THREAD_ID
# define DECL_MAIN_THREAD_ID
#endif

#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif

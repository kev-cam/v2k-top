/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  tokpool_h_rcsid
#define tokpool_h_rcsid() {return "$Id: tokpool.h,v 1.53 2012/10/16 22:30:00 cvs Exp $";} /* RCS ID */

 
#ifndef TOKPOOL_H
#define TOKPOOL_H

#ifndef SYSTEM_H
# include "system.h"
#endif

#ifndef ERROR_H
# include "error.h"
#endif

#ifndef POOL_H
# include "pool.h"
#endif

typedef enum /* eFixedPool */ {
# define FXD_POOL(P,N,U,L) U##_POOL,
#include "fxd_pool.h"

  FIRST_FREE_POOL,
  FIRST_USER_POOL  = 128
} eFixedPool;

#ifdef LITTLE_ENDIAN
#define TOKEN_ADD_POOL(P)  ((P) << 16)
#define TOKEN_POOL(T)      ((T) >> 16)
#define TOKEN_ADD_INDEX(I) (I)
#define TOKEN_INDEX(T)     ((T) & 0xFFFF)
#else
#define TOKEN_ADD_POOL(P)  (P)
#define TOKEN_POOL(T)      ((T) & 0xFFFF)
#define TOKEN_ADD_INDEX(I) ((I) << 16)
#define TOKEN_INDEX(T)     ((T) >> 16)
#endif

#define TOK_INDEX(tbl,nm,prv,idx) (tbl##POOL_BASE + idx)

typedef struct {
  short pool,
        index;
} tokPI;

typedef enum /* eTOK */ {
  TOK_CHAR      =   0,
  TOK_FULL_REF  =  -1,
  TOK_POOL_SPEC =  -2,
  TOK_ARG       =  -3,
  TOK_LINE      =  -4,
  TOK_LINE_FULL =  -5,
  TOK_FILE      =  -6,
  TOK_FILE_FULL =  -7,
  TOK_INC_LEVEL =  -8,
  TOK_TRUNCATED =  -9,
  TOK_EOF       = -10,
  TOK_END_MACRO = -11
} eTOK;

typedef struct {
  eTOK  tok:16;
  char  ch,
        count;
} tokCH;

typedef struct {
  eTOK  tok:16;
  char  arg,
        depth;
} tokArg;

typedef struct /* tokExt */ {
  eTOK  tok:16; /*!< Pool index if > 0 */
  short extra;
} tokExt;

typedef struct {
  unsigned char b1,
                b2,
                b3,
                b4;
} tokByt;

typedef union {
  int32_t as_int;
  tokPI   pi;
  tokCH   ch;
  tokExt  xt;
  tokByt  bt;
  tokArg  at;
} Token;

#define SAME_TOK(t1,t2)    ((t1)->as_int == (t2)->as_int)
#define SAME_TOK_PI(t,p,i) ((t)->pi.pool == p && (t)->pi.index == i)

typedef struct {
  short length;
  char  buff[UNSIZED(2)];
} tokString;

typedef struct {
  tokExt    xt;
  tokString name;
} tokPoolRef;

#ifdef __cplusplus
extern "C" {
#endif

int          tokReconInMem(const Token *,int,FILE *);
int          tokReconstruct(FILE *,FILE *,char *);
int          tokStrSize(tokString *);
const char  *tokDeref2(const Token **,const poolRef *);
const char  *tokDeref(const Token *,const poolRef *
#ifdef __cplusplus
                                                   args = 0
#endif
);


#ifndef TOKMORE
#define TOKMORE Token
#endif

extern TOKMORE TokMore;
extern TOKMORE TokEOF;

#ifdef __cplusplus
}
#endif

#ifndef IGNORE_TOK_TABLE
#include "lang-list.h"

#ifndef TOK_TABLE
typedef struct {const char *name; Token id;} tokTable;
#endif

#endif

#endif /* TOKPOOL_H */

/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ const char * language_c_rcsid() {return "$Id: language.c,v 1.29 2012/10/16 22:38:45 cvs Exp $";}
 


#define LANGUAGE_C

#define NEED_WHITESPACE
#include "error.h"
#include "tokpool.h"
#include "strdb.h"
#include "libs.h"
#include "version.h"
#include "assertions.h"

const poolRef NlRef  ={WHITESPACE_POOL,WHT_NL};
const poolRef CrRef  ={WHITESPACE_POOL,WHT_CR};

const char *LangName[FIRST_FREE_POOL];

void InitLang(const char *mode,char *reg) {

  if (LangName[1] && !reg) return;

  ASSERT(1 == WHITESPACE_POOL);

# define FXD_POOL(P,N,U,L) Init##N##Tok(mode,reg ? reg[U##_POOL] : 1);
# define SKIP_NULL_POOL
# include "fxd_pool.h"

  InitStrDB(mode);
}

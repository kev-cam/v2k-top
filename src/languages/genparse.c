/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ const char * genparse_c_rcsid() {return "$Id: genparse.c,v 1.30 2012/10/16 22:38:45 cvs Exp $";}

/*! /file
    Code generator for tokenizer
*/

#include "system.h"
#include "error.h"
#include "v2k_mem.h"
#include "args.h"
#include "tokpool.h"
#include "poolmngr.h"
#include "strdb.h"
#include "language.h"

static int  maxLen = 0;
static char useLang[128];

static void checkLen(poolRef ref)
{
  char     *pool = (char *)PoolAddr(ref.pool);
  PoolStr  *s    = (PoolStr *)(pool + ref.index);

  if (useLang[ref.pool] && s->l > maxLen) maxLen = s->l;
}

static int *Counts;
static int *counts;

static void countSize(poolRef ref)
{
  char     *pool = (char *)PoolAddr(ref.pool);
  PoolStr  *s    = (PoolStr *)(pool + ref.index);

  if (useLang[ref.pool]) (Counts[s->l])++;
}

static poolRef **Table;

static void logRef(poolRef ref)
{
  char     *pool = (char *)PoolAddr(ref.pool);
  PoolStr  *s    = (PoolStr *)(pool + ref.index);

  if (useLang[ref.pool]) {
    int l = s->l,
        c = --counts[l];
    assert(c >= 0);
    Table[l][c] = ref;
  }
}

char *mkcall(char *from)
{
  static char name[1024] = "PRS_";

  char *cp = &name[4];

  for (;(*cp = *from++);cp++) if (!isalpha(*cp)) {
    sprintf (cp,"%02X",*cp);
    cp++;
  }

  return name;
}

void generateC(const int l,char *prev)
{
  int            i   = Counts[l],
                 j,
                 k,
                 any = 0;
  char           need[256];
  poolRef        ref[256];
  unsigned char *s;

  BZERO(need,sizeof(need));

  while (i-- > 0) {
    s = (unsigned char *)strDeref(Table[l][i]);
    if (0 == strncmp((char *)s,prev,l-1)) {
      need[s[l-1]] |= 1;
      any          |= 1;
      ref[s[l-1]]   = Table[l][i];
    }
  }

  for (j = l; j++ < maxLen ;) {
    for (k = Counts[j]; k-- > 0;) {
      s = (unsigned char *)strDeref(Table[j][k]);
      if (0 == strncmp((char *)s,prev,l-1)) {
        need[s[l-1]] |= 2;
        any          |= 2;
      }
    }
  }

  if (any)
  {
    if (Arg.verbosity & VRB_GEN) fprintf(stderr,"Length %d [%s]\n",l,prev);

    for (j = 256; j-- > 0;) if (2 & need[j]) {
      prev[l-1] = j; prev[l] = '\0';
      generateC(l+1,prev);
      prev[l-1] = '\0';
    }

    fprintf(stdout,"\nstatic InLine Token *%s(ParseData *pData%s) { /* %s */\nToken *tok = 0;\n",
                   mkcall(prev),(l > 1) ? "" : ",int ch", prev);
    if (l > 1) {
       fprintf(stdout,"\tint ch = nextChar(pData);\n");
     }
    fprintf(stdout,"\tswitch(ch) {\n");
    for (j = 256; j-- > 0;) {
      if (need[j]) {
        prev[l-1] = j; prev[l] = '\0';
        if (0 == strcmp(prev,"*/")) {
          fprintf(stdout,"\t\tcase %d: /* %c:%s */\n",j,j,"\\*\\/");
        } else {
          fprintf(stdout,"\t\tcase %d: /* %c:%s */\n",j,j,prev);
        }
        if (3 == need[j]) {
          fprintf(stdout,"\t\t\tif ((tok = %s(pData))) return tok;\n",mkcall(prev));
        }
        if (2 == need[j]) {
          fprintf(stdout,"\t\t\treturn %s(pData);\n",mkcall(prev));
        }
        if (1 & need[j]) {
          fprintf(stdout,"\t\t\treturn createTok(pData,%d,%d,%d);\n",
                         ref[j].pool,ref[j].index,l);
        }
        prev[l-1] = '\0';
      }
    }

    if (l > 1) {
      fprintf(stdout,"\t\tdefault:\n\t\t\treturn tok;\n\t}\n}\n");
    } else {
      if (!need['\n'])
        fprintf(stdout,"\t\tcase '\\n':\n\t\t\treturn handleNL(pData);\n");
      if (!need[' '])
        fprintf(stdout,"\t\tcase ' ':\n\t\t\treturn handleSP(pData);\n");
      if (!need['\t'])
        fprintf(stdout,"\t\tcase '\\t':\n\t\t\treturn handleTB(pData);\n");
      if (!need['`'])
        fprintf(stdout,"\t\tcase '`':\n\t\t\treturn handleBQ(pData);\n");
      if (!need['\''])
        fprintf(stdout,"\t\tcase '\\'':\n\t\t\treturn handleSQ(pData);\n");
      if (!need['"'])
        fprintf(stdout,"\t\tcase '\"':\n\t\t\treturn handleDQ(pData);\n");
      fprintf(stdout,"\t\tdefault:\n\t\t\treturn tok;\n\t}\n}\n");
    }
  }
}

static void setUseLang(const char *lang)
{
  int l = FIRST_FREE_POOL;

  while (l-- >= 0) {
    if (0 == strcasecmp(lang,LangName[l])) {
      useLang[l] = 1;
      return;
    }
  }

  fprintf(stderr,"Language %s not recognized\n",lang);
  exit(1);
}

static int addLang(int *argc,const char ***argv,void *data,
                   int _ignore1,int _ignore2)
{
  const char *arg = **argv,
             *eq  = argEquals(argc,argv),
             *cm;

  if (!eq) return STS_MSSNG_ARG;

  for (;(cm = strchr(eq,',')); eq = ++cm) {
    int  l = cm - eq;
    TMPARR_POD(char,lang,l+1);
    strncpy(lang,eq,l);
    lang[l] = '\0';
    setUseLang(lang);
  }

  setUseLang(eq);

  return 0;
}

static argHndlr argHandlrs[] = {
  {"lang", addLang, 0, AT_STR|AT_ARG},
  {0,0,0}
};

int main(int argc,const char **argv)
{
  int   sts,
        i,j;
  char *tmp;

  memset(useLang,-1,sizeof(useLang));
  InitLang("cw",useLang);
  memset(useLang, 0,sizeof(useLang));

  if (!(sts = processArgs(&argc,&argv,argHandlrs))) {
    InitLang("cw",useLang);
    strForAll(checkLen);
    if (Arg.verbosity & VRB_GEN) fprintf(stderr,"Max. String:\t%d\n",maxLen);
    Counts = Calloc(maxLen+1,sizeof(int));
    counts = Calloc(maxLen+1,sizeof(int));
    Table  = Calloc(maxLen+1,sizeof(poolRef *));
    strForAll(countSize);
    for (i = 0; i <= maxLen; i++) {
      if (Arg.verbosity & VRB_GEN) fprintf(stderr,"Length %5d\t%d\n",i, Counts[i]);
      counts[i] = Counts[i];
      Table[i]  = Calloc(Counts[i],sizeof(poolRef));
    }
    strForAll(logRef);
    for (i = 0; i <= maxLen; i++) {
      if (Arg.verbosity & VRB_GEN) fprintf(stderr,"Length %5d\n",i);
      j = Counts[i];
      while (j-- > 0) {
        if (Arg.verbosity & VRB_GEN) fprintf(stderr,"\t%s\n",strDeref(Table[i][j]));
      }
    }
    *(tmp = (char *)Malloc(maxLen+1)) = '\0';
    generateC(1,tmp);
  }

  return sts;
}

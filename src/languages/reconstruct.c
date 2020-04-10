/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ const char * reconstruct_c_rcsid() {return "$Id: reconstruct.c,v 1.31 2012/10/16 22:38:45 cvs Exp $";}

#include "system.h"
#include "error.h"
#include "args.h"
#include "pool.h"
#include "strdb.h"
#define  NEED_QUOTES
#define  NEED_WHITESPACE
#include "tokpool.h"
#include "poolmngr.h"
#include "language.h"

const char *tokDeref2(const Token **TP,const poolRef *args)
{
  static char  buff[64];  
  poolRef      Ref;
  const Token *T = *TP;

  (*TP)++;

  if (!T) return "null-tok";

  switch (Ref.pool = T->pi.pool) {
  case TOK_CHAR:      return "TOK_CHAR";
  case TOK_FULL_REF:  Ref.pool  = *(int *)((*TP)++);
                      Ref.index = *(int *)((*TP)++);
                      goto rr;
  case TOK_POOL_SPEC: return "TOK_POOL_SPEC";
  case TOK_LINE:      return "TOK_LINE";
  case TOK_LINE_FULL: return "TOK_LINE_FULL";
  case TOK_FILE:      return "TOK_FILE";
  case TOK_FILE_FULL: return "TOK_FILE_FULL";
  case TOK_TRUNCATED: return "TOK_TRUNCATED";
  case TOK_EOF:       return "TOK_EOF";
  case TOK_END_MACRO: return "TOK_END_MACRO";
  case TOK_ARG:       if (args) {
                        sprintf(buff,"%s",strDeref(args[T->at.arg]));
                      } else{
                        sprintf(buff,"TOK_ARG(%d,%d)",T->at.arg,T->at.depth);
                      }
                      return buff;
  }
  Ref.index = T->pi.index;
 rr:
  return strDeref(Ref);
}

const char *tokDeref(const Token *T,const poolRef *args)
{
  return tokDeref2(&T,args);
}

int tokReconstruct(FILE *in,FILE *out,char *sep)
{
  Token    T;
  poolRef  Ref,
           file = NullRef;
  short    s;
  int      l,
           line;
  Filename fn;
  char     pad[4],
          *mode = "ra";

  while (fread(&T,sizeof(T),1,in)) {
    if ((Ref.pool = T.pi.pool) > 0) {
      Ref.index = T.pi.index;
      if (Arg.verbosity & VRB_DUMP) {
        fprintf(out,"\n%14s:",Ref.pool <FIRST_FREE_POOL ? LangName[Ref.pool]
                                                        : "Strings");
      }
      fprintf(out,"%s",strDeref(Ref));
      if (WHITESPACE_POOL == Ref.pool) continue;
    } else switch (Ref.pool) {
    case TOK_CHAR: {
      int l = T.ch.count;
      while (l-- > 0) fprintf(out,"%c",T.ch.ch);
    } ; break;
    case TOK_FULL_REF: {
      fread(&Ref.pool, T.xt.extra,1,in);
      fread(&Ref.index,T.xt.extra,1,in);
      fprintf(out,"%s",strDeref(Ref));
    } ; break;
    case TOK_LINE:
      l    = line;
      line = T.xt.extra;
      goto catch_up;
    case TOK_LINE_FULL:
      l    = line;
      fread(&T,sizeof(T),1,in);
      line = T.as_int;
    catch_up:
      if (Arg.verbosity & VRB_DUMP) fprintf(out,"\n// Line %d\n",line);
      else                          fputs("\n",out);
      break;
    case TOK_FILE:
      fread(&T,sizeof(T),1,in);
      file.pool  = T.pi.pool;
      file.index = T.pi.index;
      goto pf;
    case TOK_FILE_FULL:
      fread(&T,sizeof(T),1,in);
      file.pool  = T.as_int;
      fread(&T,sizeof(T),1,in);
      file.index = T.as_int;
    pf:
      if (Arg.verbosity & VRB_DUMP) fprintf(out,"\n// File %s\n",strDeref(file));
      break;
    case TOK_POOL_SPEC: {
      Ref.pool = T.xt.extra;
      fread(&s,sizeof(s),1,in);
      fread(fn,l = s+1,1,in);
      l += 2;
      if (l &= 0x3) {
         fread(pad,4 - l,1,in); /* re-align */
      }
      l = PoolLoad(fn,Ref.pool,mode);
      if (l != Ref.pool) {
        assert(0);
      }
      continue;
    } ; break;
    default:
        assert(0);
    }
    if (sep) fputs(sep,out);
  }

  return 0;
}

int tokStrSize(tokString *ref)
{
  int l = 2 + strlen(ref->buff) +1,
      p = l & 0x3;

  if (p) l += (4-p);

  return l;
}

int tokReconInMem(const Token *tok,int count,FILE *out)
{
  const Token *T;
  poolRef      Ref;
  int          l,
               line  = 1,
               i     = 0,
               quote = 0,
               lines = -1;
  char        *mode  = "ra";
  const char  *str0,
	      *str;

  if (count < 0) lines = -count;

  if (!out) out = stderr;

  if (Arg.show_mode & 1) {
    fprintf(out,"%4d\t",line);
  }
  line++;

  while (i < count || count < 0) {
    T = &tok[i++];
    if ((Ref.pool = T->pi.pool) > 0) {
      Ref.index = T->pi.index;
    do_ref:
      str0 = str = strDeref(Ref);
      if (quote > 0) {
	char ch;
	while ((ch = *str++)) switch (ch) {
	  case '\n': fprintf(out,"\\n"); break;
	  case '\r': fprintf(out,"\\r"); break;
	  case '\a': fprintf(out,"\\a"); break;
	  case '\f': fprintf(out,"\\f"); break;
          default:   fputc(ch,out);
	}
        quote = -1;
	continue;
      } else {
	fprintf(out,"%s",str);
	while (*str) switch (*str++) {
	  case '\n':
	  case '\f':
	    if (Arg.show_mode & 1) fprintf(out,"%4d\t",line);	    
	    line++;
	}
      }
      if (WHITESPACE_POOL == Ref.pool 
                && WHT_NL == Ref.index) {
        if (0 == --lines) break;
      } else if (QUOTES_POOL == Ref.pool 
                 && QUT_BACK != Ref.index) {
       qref:
	quote = quote ? 0
	              : 1;
      }
    } else switch (Ref.pool) {
    case TOK_CHAR:
      l = T->ch.count;
      while (l-- > 0) {
	int ch = T->ch.ch;
	fprintf(out,"%c",ch);
	if ('\n' == ch) {
	  if (Arg.show_mode & 1) fprintf(out,"%4d\t",line);
	  line++;
	}
      }
      break;
    case TOK_LINE:
      fprintf(out,"`line %d\n",line = T->xt.extra);
      break;
    case TOK_FILE:
      Ref.pool  = (++T)->pi.pool;
      Ref.index =    T ->pi.index;
      fprintf(out,"`file '%s'\n",strDeref(Ref));
      break;
    case TOK_ARG:
      fprintf(out,"/* ARG %d */",T->at.arg);
      break;
    case TOK_INC_LEVEL:
      fprintf(out,"`level %d\n",T->xt.extra);
      break;
    case TOK_END_MACRO:
      fprintf(out,"/* END MACRO */");
      count = 0;
      break;
    case TOK_FULL_REF:
      T         = &tok[i++];
      Ref.pool  = T->as_int;
      T         = &tok[i++];
      Ref.index = T->as_int;
      goto do_ref;
    case TOK_EOF:
      fprintf(out,"/* EOF */");
      assert(i >= count);
      break;
    case TOK_TRUNCATED:
      fprintf(out,"...");
      assert(i >= count);
      break;
    case TOK_POOL_SPEC: {
      tokPoolRef *ref = (tokPoolRef *)T;
      i += tokStrSize(&ref->name)/sizeof(Token);
      l  = PoolLoad(ref->name.buff,ref->xt.extra,mode);
      if (l != ref->xt.extra) {
        assert(0);
      }
    } ; break;
    default:
        assert(0);
    }
  }

  return 0;
}


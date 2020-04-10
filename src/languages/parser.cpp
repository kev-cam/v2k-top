/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * parser_cpp_rcsid() {return "$Id: parser.cpp,v 1.25 2012/10/16 22:38:45 cvs Exp $";}

#include "error.h"
#include "strdb.h"
#include "file.h"
#define  NEED_WHITESPACE
#define  NEED_PUNCTUATION
#define  TOKMORE tokCH
#include "tokpool.h"
#include "parser.h"
#include "poolmngr.h"

extern "C" {
  tokCH TokMore = {TOK_TRUNCATED},
        TokEOF  = {TOK_EOF};
}

const Token *PrsrCntxt::rescanning(const Token *T0,int back_only)
{
  int count = 0;

  if (!T0) {
    rescan = 1;
  } else {
    const Token *T = T0;
    if (T <= Tnl) {
      while (T < Tnl) {
        if (T->pi.pool == TOK_FULL_REF) T += 2;
        else if (SAME_TOK_PI(T,WHITESPACE_POOL,WHT_NL)) count++;
        T++;
      }
      rescan = 1;
    }
  }

  if (count) {
    line -= count;
  }

  return T0; 
}

const char PrsrCntxt::RplcDir = '+';
const char PrsrCntxt::RplcDot = '=';

char *PrsrCntxt::encode(char *str)
{
  char *sp = str;

  for (; *sp ; sp++) if      (*OS_DIR_SEP  == *sp) *sp = RplcDir;
                     else if (*OS_TYPE_SEP == *sp) *sp = RplcDot;
  return str;
}

const char *PrsrCntxt::cch2src(const char *cch,Filename &src)
{
  Filename f(cch);

  f  -= OS_DIR_SEP;
  src = f.basename();

  char *cp = src;

  for (; *cp ; cp++) {
         if (*cp == RplcDir) *cp = *OS_DIR_SEP;
    else if (*cp == RplcDot) *cp = *OS_TYPE_SEP;
  }

  return src;
}

const char *PrsrCntxt::tok2src(const char *tok,Filename &src)
{
  Filename f(tok);

  f  -= FileTypes[FT_Token].str;
  f  -= OS_TYPE_SEP;
  src = f.basename();

  char *cp = src;

  for (; *cp ; cp++) {
         if (*cp == RplcDir) *cp = *OS_DIR_SEP;
    else if (*cp == RplcDot) *cp = *OS_TYPE_SEP;
  }

  return src;
}

const char *PrsrCntxt::source(Filename &src)
{
  const char *cp;

  if (NULL_REF(curr_file)) cp = cch2src(cache,src);
  else                     cp = strDeref(curr_file);

  return cp;
}

const poolRef PrsrCntxt::srcRef()
{
  if (!NULL_REF(curr_file)) return curr_file;

  return curr_file = strSaveStr(cache);
}

bool PrsrCntxt::setCache(const char *src_cache)
{
  if (!cache) {
    cache = src_cache;
    fmode = "ra";
    line  = 1;
    cache.changeType(FT_Dir);

    return 1;
  }

  return 0;
}

void PrsrCntxt::CmnInit()
{
  File pls("${V2K_REPOSITORY}/cache/","pools",FT_Token);
  pools = new MappedFile(pls,"aspcrw");
}

Token *PrsrCntxt::rgstrPool(tokPoolRef *nw)
{
  int         p    = 0,
              sz   = sizeof(Token) + tokStrSize(&nw->name);
  tokPoolRef *scan = (tokPoolRef *)pools->base();

  while (p < pools->Size()) {
    if (0 == strcmp(nw->name.buff,scan->name.buff)) goto next;
    p += sizeof(Token) + tokStrSize(&scan->name);
    scan = (tokPoolRef *)(pools->base() + p);
  }

  pools->Write(nw,sz);

next:
  return (Token *)(((char *)nw) + sz);
}

const Token *PrsrCntxt::tokGetRef(const Token *T,poolRef *Ref,
                                  int on_line,int ignr_wht,const Token *TL)
{
  Ref->pool  = 0;
  Ref->index = 0;

  if (!TL) TL = file_TL;

  while (T < TL) {
    if ((Ref->pool = T->pi.pool) > 0) switch (Ref->pool) {
    case WHITESPACE_POOL:
        if (!ignr_wht || (on_line && WHT_NL == T->pi.index)) {
	  Ref->index = T->pi.index;
          goto done;
	}
        T = nextTok(T,1);
        break;
    default:
        Ref->index =  T->pi.index;
        T          =  nextTok(T,1);
        goto done;
    } else switch (Ref->pool) {
    case TOK_FULL_REF:
      T++;
      Ref->pool  = T->as_int;
      T++;
      Ref->index = T->as_int;
      T          = nextTok(T,1);
      goto done;
   case TOK_EOF:
      goto eof;
    case TOK_POOL_SPEC: {
      tokPoolRef *ref = (tokPoolRef *)T;
      T     = rgstrPool(ref);
      int l = PoolLoad(ref->name.buff,ref->xt.extra,fmode);
      if (l != ref->xt.extra) {
        assert(0);
      }
    } ; break;
    default:
      assert(0);
    }
  }

eof: 
  Ref->pool = 0;
done:
  return T;
}

const Token *PrsrCntxt::findClsBr(const Token *T,const Token *TT,int *pitms,
                                  const Token **psns)
{
  int depth = 0,
      items = 0;

  for (; T < TT; T++) {
    switch (T->pi.pool) {
    case PUNCTUATION_POOL: switch(T->pi.index) {
                           case PUNC_OPN_BR: if (!depth++) {
			                       if (psns) psns[items] = T;
                                               items++;
			                     }
                                             break;
                           case PUNC_CLS_BR: if (0 > --depth) {
                                               if (pitms) *pitms = items;
			                       if (psns)   psns[items] = T;
                                               return T;
			                     }
                                             break;
                           }
                           break;
    case TOK_FULL_REF:
    case TOK_FILE_FULL:    T += 2;
                           break;
    case TOK_LINE_FULL:
    case TOK_FILE:         T++;
                           break;
    case TOK_TRUNCATED:    assert(("Premature end of token stream",0));
    }
  }

  if (pitms) *pitms = items;
  if (psns)  psns[items] = TT;
done:
  return 0;
}

const Token *PrsrCntxt::tokenize(const Token *T,const Token *TL,int *pitms,
                                 const Token **psns)
{
  int depth    = 0,
      items    = 0,
      non_blnk = 0;

  for (; T < TL; T++) {
    switch (T->pi.pool) {
    case WHITESPACE_POOL:  if (!depth) non_blnk = 0;
                           break;
    case PUNCTUATION_POOL: switch(T->pi.index) {
                           case PUNC_OPN_BR: if (!depth++) {
			                       if (!non_blnk) {
                                                 if (psns) psns[items] = T;
                                                 items++;
                                                 non_blnk = 1;
                                               }
			                     }
                                             break;
                           case PUNC_CLS_BR: if (0 > --depth) {
                                               if (pitms) *pitms = items;
			                       if (psns)  psns[items] = T;
                                               return T;
			                     }
                                             break;
                           }
                           break;
    case TOK_FULL_REF:     if (!non_blnk) {
                             if (psns) psns[items] = T;
                             items++;
                             non_blnk = 1;
                           }
    case TOK_FILE_FULL:    T += 2;
                           break;
    case TOK_LINE_FULL:
    case TOK_FILE:         T++;
                           break;
    default:               if (!non_blnk) {
                             if (psns) psns[items] = T;
                             items++;
                             non_blnk = 1;
                           }
                           break;
    }
  }

  if (pitms) *pitms = items;
  if (psns)  psns[items] = TL;
  return 0;
}

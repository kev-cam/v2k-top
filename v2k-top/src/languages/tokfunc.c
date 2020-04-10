/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ const char * tokfunc_c_rcsid() {return "$Id: tokfunc.c,v 1.35 2012/10/16 22:38:45 cvs Exp $";}

#include "system.h"
#include "error.h"
#include "pool.h"
#include "poolmngr.h"
#include "strdb.h"
#define NEED_PUNCTUATION
#define NEED_WHITESPACE
#define NEED_CHAR
#define NEED_PRP
#define NEED_QUOTES
#include "tokpool.h"
#include "veri_enum.h"
#include "tokfunc.h"

#include "tokfunc.icc"

Token *dumpChar(ParseData *pData)
{
   int l;

   pData->lastTok.ch.tok   = TOK_CHAR;
   pData->lastTok.ch.ch    = pData->buff[0];
   pData->lastTok.ch.count = 1;

   l = (*pData->fwrite)(&pData->lastTok,sizeof(Token),1,pData->out);

   if (l < 1) pData->sts = Error();

   pData->count--;
   assert(pData->count >= 0);
   for (l = 0 ; l < pData->count; l++ ) {
      pData->buff[l] = pData->buff[l+1];
   }
#ifdef DEBUG
   pData->buff[l] = '\0';
#endif
   pData->pos     = 0;

   return &pData->lastTok;
}

void saveToPos(ParseData *pData)
{
  int     l  = pData->pos-1,
          ch = pData->buff[l];
  poolRef ref;

  assert(l >= 0);

  pData->buff[l] = '\0';
  ref            = strSaveStr((const char *)(pData->buff));
  pData->buff[l] = ch;

  createTok(pData,ref.pool,ref.index,l);
}

void backToPos(ParseData *pData,int old_pos)
{
  pData->curr = pData->buff[pData->pos = old_pos];
  pData->eof  = 0;
}

void saveToPosESC(ParseData *pData)
{
  int     l  = pData->pos-1,
          ch = pData->buff[l];
  poolRef ref;

  assert(l >= 0);

  pData->buff[l] = '\0';
  ref            = strSaveStr((const char *)(pData->buff));
  pData->buff[l] = ch;

  if (QUOTES_POOL == ref.pool) {
    ref.pool = ESC_POOL;
  }

  createTok(pData,ref.pool,ref.index,l);
}

void delChars(ParseData *pData,int length)
{
  int p = pData->pos,
      c = pData->count;

  pData->count -= length;
  assert(pData->count >= 0);
  for (; p < c; p++ ) {
    pData->buff[p] = pData->buff[p+length];
  }
#ifdef DEBUG
  pData->buff[pData->count] = '\0';
#endif
}

#define FWRITE(p,s,n,fp) {int w = (*pData->fwrite)(p,s,n,fp);\
                          if (0 > w) pData->sts = Error();\
                          PD_COUNT(else,pData,w);}

Token *createTok(ParseData *pData,int pool,int index,int len)
{
   int     l,
           ch;
   poolRef ref;

   if (pool > LST_RSVRD_PL_ID)
   {
      if (pool > pData->maxp) {
         l             = pool > MIN_POOLS ? (pool+256)
                                          : MIN_POOLS;
         REALLOC2(pData->dumped,l,unsigned char);
         BZERO(&pData->dumped[pData->maxp],l - pData->maxp);
         pData->maxp   = l;
      }

      if (!pData->dumped[pool]) {
         short       s;
         const char *name;
         pData->lastTok.xt.tok   = TOK_POOL_SPEC;
         pData->lastTok.xt.extra = pool;
         FWRITE(&pData->lastTok,sizeof(Token),1,pData->out);
         name  = PoolName(pool);
         l = s = strlen(name);
         FWRITE(  &s,sizeof(s),1,pData->out);
         FWRITE(name,      ++l,1,pData->out);
         l += 2;
         if (l &= 0x3) {
            FWRITE("1234",4 - l,1,pData->out); /* re-align */
         }
         pData->dumped[pool] = 1;
      }

      if ((pool|index) > 0x7FFF) {
         pData->lastTok.xt.tok   = TOK_FULL_REF;
         pData->lastTok.xt.extra = sizeof(int);
         FWRITE(&pData->lastTok,sizeof(Token),1,pData->out);
         FWRITE(&pool,          sizeof(int),  1,pData->out);
         FWRITE(&index,         sizeof(int),  1,pData->out);
         goto skip;
      }
   } else switch (pool) {
   case PRP_POOL:
     if (PRP_HASH == index) break;
   case ACCTFVPI_POOL:
   case VERILOG_POOL:
   case BUILTIN_POOL:
   case GLOBAL_POOL:
   case LABELS_POOL:
   case TASKS_POOL:
   case MATH_POOL:
   case PRC_POOL:
   case VPP_POOL:
   case CPP_POOL:
     if (pData->count > len) ch = pData->buff[len];
     else                    ch = nextChar(pData);
     if (isalnum(ch) || '_' == ch || '$' == ch) {
       return 0;
     }
     break;
   }

   pData->lastTok.pi.pool  = pool;
   pData->lastTok.pi.index = index;
   FWRITE(&pData->lastTok,sizeof(Token),1,pData->out);

skip:
   pData->count -= len;
   assert(pData->count >= 0);
   for (l = 0 ; l < pData->count; l++ ) {
      pData->buff[l] = pData->buff[l+len];
   }
#ifdef DEBUG
   pData->buff[l] = '\0';
#endif
   pData->pos     = 0;

   return &pData->lastTok;
}

void collectStrD(ParseData *pData)
{
  poolRef ref;
  int     esc,n,d,s;

  for (;;) {
    int c = nextChar(pData);
    switch (c) {
    case '\\': d = 1; s = 1;
               switch (esc = nextChar(pData)) {
               case 'n': esc = '\n'; break;
               case 't': esc = '\t'; break;
               case '"': break;
               default:  for (d = n = 0;
                              isdigit(esc) && esc < '8';
                              esc = nextChar(pData)) {
		           d++;
                           n = (n * 8) + esc - '0';
	                 }
	 	         if (d) { esc = n;
                                  s   = 2; }
		         else   { d   = 1; }
               }
               pData->pos -= d + s;
               assert(pData->pos >= 0);
               delChars(pData,d);
               pData->buff[pData->pos++] = esc;
               break;
    case '"':  saveToPosESC(pData);
               createTok(pData,QUOTES_POOL,QUT_DOUBLE,1);
               return;
    case '\n':
    case EOF:
               goto done;
    }
  }
done:
  saveToPos(pData);
}

void collectStrS(ParseData *pData)
{
  poolRef ref;
  int     esc,n,d,s;

  for (;;) {
    int c = nextChar(pData);
    switch (c) {
    case '\\': d = 1; s = 1;
               switch (esc = nextChar(pData)) {
               case 'n':  esc = '\n'; break;
               case 't':  esc = '\t'; break;
               case '\'': break;
               default:   for (d = n = 0;
                               isdigit(esc) && esc < '8';
                               esc = nextChar(pData)) {
		            d++;
                            n = (n * 8) + esc - '0';
	                  }
	  	          if (d) { esc = n;
                                   s   = 2; }
			  else   { d   = 1; }
               }
               pData->pos -= d + s;
               assert(pData->pos >= 0);
               delChars(pData,d);
               pData->buff[pData->pos++] = esc;
               break;
    case '\'': saveToPosESC(pData);
               createTok(pData,QUOTES_POOL,QUT_SINGLE,1);
               return;
    case '\n':
    case EOF:
               goto done;
    }
  }
done:
  saveToPos(pData);
}

void collectEscName(ParseData *pData)
{
  poolRef ref;
  int     ch;

  while (isprint(pData->curr) && !isspace(pData->curr)) {
    nextChar(pData); 
  }

  if (1 == --pData->pos) { // just '\'
    createTok(pData,CHAR_POOL,CHR_BACKSLASH,1);
  } else {
    saveToPos(pData);
    pData->pos++;
  }
}


int collectNumberV(ParseData *pData)
{
  poolRef ref;
  int     ch,
          base = 10,
          here = pData->pos;

  switch (nextChar(pData)) {
  case 'b':
  case 'B': base =  2; break;
  case 'o':
  case 'O': base =  8; break;
  case 'd':
  case 'D': break;
  case 'h':
  case 'H': base = 16; break;
  default:  backToPos(pData,here);
            return 0;
  }

  ch = nextChar(pData);

#ifndef NO_SPACE_FROM_BASE
  while (isspace(ch)) ch = nextChar(pData);
#endif

  for (;;ch = nextChar(pData)) {
    switch (ch) {
    case '0':
    case '1':
    case '_':           continue;
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':           if (base <  8) goto fnshd;
                        break;
    case '8':
    case '9':           if (base < 10) goto fnshd;
                        break;
    case 'a': case 'A':
    case 'b': case 'B':
    case 'c': case 'C':
    case 'd': case 'D':
    case 'e': case 'E':
    case 'f': case 'F': if (base < 16) goto fnshd;
                        break;
    case '?':
    case 'z': case 'Z':
    case 'x': case 'X': if (10 == base) goto fnshd;
                        break;
    default:            goto fnshd;
    }
  }

fnshd:
  saveToPos(pData);
  return 1;
}


int collectNumberC(ParseData *pData,int base)
{
  poolRef ref;
  int     ch,
          here = pData->pos,
          u    = 0,
          l    = 0;

  ch = nextChar(pData);

  for (;;ch = nextChar(pData)) {
    switch (ch) {
    case '0':
    case '1':
    case '_':           continue;
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':           if (base <  8) goto trlng;
                        break;
    case '8':
    case '9':           if (base < 10) goto trlng;
                        break;
    case 'a': case 'A':
    case 'b': case 'B':
    case 'c': case 'C':
    case 'd': case 'D':
    case 'e': case 'E':
    case 'f': case 'F': if (base < 16) goto trlng;
                        break;
    case '?':
    case 'z': case 'Z':
    case 'x': case 'X': if (10 == base) goto trlng;
                        break;
    default:            goto trlng;
    }
  }

trlng:
  for (;;ch = nextChar(pData)) {
    switch (ch) {
    case 'u': case 'U': u++; break;
    case 'l': case 'L': l++; break;
    default:            goto fnshd;
    }
  }
fnshd:
  saveToPos(pData);
  return 1;
}


void completeName(ParseData *pData,int ch)
{
  poolRef ref;

  do ch = nextChar(pData); while (isalnum(ch) || '_' == ch || '$' == ch );

  saveToPos(pData);
}

void completeNumberC(ParseData *pData,int ch)
{
  poolRef ref;

  do ch = nextChar(pData); while (isdigit(ch) || '_' == ch);

  if ('x' == ch && 2 == pData->pos) {
    if (!collectNumberC(pData,16)) goto save_it;
    return;
  }

  if ('.' == ch) {
    do ch = nextChar(pData); while (isdigit(ch) || '_' == ch);
    
  }

  switch (ch) {
  case 'e': case 'E':
    ch = nextChar(pData);
    if ('-' == ch || '+' == ch || isdigit(ch)) {
      do ch = nextChar(pData); while (isdigit(ch) || '_' == ch);
    }
  case 'f': case 'F':
    switch (ch) {
    case 'f': case 'F': ch = nextChar(pData);
    }    
    goto l1;
  case 'u': case 'U':
  case 'l':  case 'L':
    ch = nextChar(pData);
  l1:
    switch (ch) {
    case 'u': case 'U':
    case 'l': case 'L': ch = nextChar(pData);
    }
  }
save_it:
  saveToPos(pData);
}

void completeNumberV(ParseData *pData,int ch)
{
  poolRef ref;
  int     box = 0;

  do ch = nextChar(pData); while (isdigit(ch) || '_' == ch);

  if ('\'' == ch) {
    if (!collectNumberV(pData)) goto save_it;
    return;
  } else {
    if ('.' == ch) {
      do ch = nextChar(pData); while (isdigit(ch) || '_' == ch);
    }

    if (!box) switch (ch) {
    case 'e':
    case 'E':
      ch = nextChar(pData);
      if ('-' == ch || '+' == ch || isdigit(ch)) {
	do ch = nextChar(pData); while (isdigit(ch) || '_' == ch);
      }
      break;
    case 'T':
    case 'G':
    case 'M':
    case 'K':
    case 'm':
    case 'u':
    case 'n':
    case 'p':
    case 'f':
    case 'a':
      ch = nextChar(pData);
    }
  }
save_it:
  saveToPos(pData);
}

void completeNonBlank(ParseData *pData,int ch)
{
  poolRef ref;

  do ch = nextChar(pData); while (ch >= 0 && !isspace(ch));

  saveToPos(pData);
}

int switchLang(ParseData *pPD,Token *(*PRS_)(ParseData *,int))
{
  char lang[16];
  int  quote = 0,
       ch;

  do {
    pPD->lastTok.pi.pool = 0;
    (*PRS_)(pPD,nextChar(pPD));
    if (QUOTES_POOL == pPD->lastTok.pi.pool) {
      quote++;
      nextChar(pPD);
      break;
    }
  } while(WHITESPACE_POOL == pPD->lastTok.pi.pool);

  while (isalnum(pPD->buff[pPD->pos -1]) || '+' == pPD->buff[pPD->pos -1]) {
    ch = nextChar(pPD);
  }

  strncpy(lang,(const char *)(pPD->buff),pPD->pos -1);
  lang[pPD->pos -1] = '\0';
  saveToPos(pPD);

  if (quote) for(;;ch = nextChar(pPD)) {
    (*PRS_)(pPD,ch);
    if (QUOTES_POOL == pPD->lastTok.pi.pool) { quote--; break; }
  }

  if (0 == strcmp(lang,"C++")) {
    tokPcPD(pPD);
  }

  return !quote;
}

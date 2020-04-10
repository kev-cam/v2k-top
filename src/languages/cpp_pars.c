/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ const char * cpp_pars_c_rcsid() {return "$Id: cpp_pars.c,v 1.10 2012/10/16 22:38:45 cvs Exp $";}
 

#include "system.h"
#include "error.h"
#include "pool.h"
#include "poolmngr.h"
#include "strdb.h"
#define NEED_PUNCTUATION
#define NEED_WHITESPACE
#define NEED_QUOTES
#define NEED_VPP
#include "tokpool.h"
#include "pc.h"
#define LANG_MODE ePCMD
#include "tokfunc.h"

static ParseData  NullPCP,
                 *pPCP    = &NullPCP; /* for debug only */

#include "tokfunc.icc"

#ifdef CPP_ONLY
#include "genparse-cpp.c"
#endif

eSTS tokPcPD(ParseData *pPD)
{
  for (;;) {
    if (PRS_(pPD,nextChar(pPD))) {
      switch (pPD->lastTok.pi.pool) {
      case QUOTES_POOL: switch(pPD->lastTok.pi.index) {
        case QUT_DOUBLE:
	  collectStrD(pPD);
          break;
        case QUT_SINGLE:
	  collectStrS(pPD);
          break;
        }
        break;
      case VPP_POOL: switch(pPD->lastTok.pi.index) {
        case VPP_LANGUAGE:
          switchLang(pPD,PRS_);
          break;
        }
        break;
      }
    } else {
      int ch;
      pPD->pos = 0;
      switch (ch = *pPD->buff) {
      case '\\': collectEscName(pPD); break;
      case '$':  dumpChar(pPD);       break;
      case 0:    if (pPD->eof) goto done;
      default:
        if (isalpha(ch) || '_' == ch)
          completeName(pPD,ch);
        else if (isdigit(ch) || '.' == ch)
          completeNumberC(pPD,ch);
        else
          completeNonBlank(pPD,ch);
      }
    }
  }
done:;

  return pPD->sts;
}

eSTS tokPC(FILE *in,fwrite_fn fw_fn,FILE *out,ePCMD mode)
{
  ParseData PCP;
  eSTS      sts;

  BZERO(pPCP = &PCP,sizeof(PCP));

  PCP.in     = in;
  PCP.fwrite = fw_fn;
  PCP.out    = out;
  PCP.mode   = mode;
  sts        = tokPcPD(&PCP);
#if DBGLVL > 1
  pPCP       = &NullPCP;
#endif
  return sts;
}

eSTS tokPcStr(const char *in,void **out,int *len,ePCMD mode)
{
  FILE        *tmp_in = tmpfile(),
              *tmp_out;
  eSTS         sts    = STS_NORMAL;
  struct stat  buf;
  int          l;

  if (tmp_in && (tmp_out = tmpfile())) {
    fputs(in,tmp_in);
    fflush(tmp_in);
    fseek(tmp_in,0,SEEK_SET);
    sts = tokPC(tmp_in,fwrite,tmp_out,mode);
    fclose(tmp_in);
    fflush(tmp_out);
    fstat(fileno(tmp_out),&buf);
    l = *len = buf.st_size;
    if ((*out = (void *)MALLOC2_N(l,char))) {
      fseek(tmp_out,0,SEEK_SET);
      if (l != read(fileno(tmp_out),*out,l)) sts = Error();
    } else {
      sts = Error();
    }
    fclose(tmp_out);
  } else {
    sts = Error();
  }

  return sts;
}

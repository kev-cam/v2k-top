/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ const char * sdf_pars_c_rcsid() {return "$Id: sdf_pars.c,v 1.14 2012/10/16 22:38:45 cvs Exp $";}
 


#include "system.h"
#include "error.h"
#include "pool.h"
#include "poolmngr.h"
#include "strdb.h"
#define NEED_PUNCTUATION
#define NEED_WHITESPACE
#define NEED_QUOTES
#include "tokpool.h"
#include "veri_enum.h"
#include "tokfunc.h"

static ParseData  NullVP,
                 *pVP    = &NullVP; /* for debug only */

#if defined(DEBUG) && !defined(PROFILE)
#undef  InLine
#define InLine
#endif

#include "tokfunc.icc"

#ifdef CPP_ONLY
#include "genparse-sdf.c"
#endif

eSTS tokSDF(FILE *in,fwrite_fn fw_fn,FILE *out)
{
  ParseData VP;

  BZERO(pVP = &VP,sizeof(VP));

  VP.in     = in;
  VP.out    = out;
  VP.mode   = VMD_NONE;
  VP.fwrite = fw_fn;

  for (;;) {
    if (PRS_(&VP,nextChar(&VP))) {
      switch (VP.lastTok.pi.pool) {
      case QUOTES_POOL: switch(VP.lastTok.pi.index) {
        case QUT_DOUBLE:
	  collectStrD(&VP);
          break;
        case QUT_SINGLE:
	  collectNumberV(&VP);
          break;
        }
        break;
      }
    } else {
      int ch;
      VP.pos = 0;
      switch (ch = *VP.buff) {
      case '\\': collectEscName(&VP); break;
      case '$':  dumpChar(&VP); break;
      case 0:    if (VP.eof) goto done;
      default:
        if (isalpha(ch))
           completeName(&VP,ch);
        else if (isdigit(ch) || '.' == ch)
           completeNumberV(&VP,ch);
        else
           completeNonBlank(&VP,ch);
      }
    }
  }
done:;
  pVP = &NullVP;

  fflush(out);

  return VP.sts;
}

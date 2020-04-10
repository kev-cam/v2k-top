/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  tokfunc_h_rcsid
#define tokfunc_h_rcsid() {return "$Id: tokfunc.h,v 1.13 2012/10/16 22:30:00 cvs Exp $";} /* RCS ID */

#ifndef LANG_MODE 
#define LANG_MODE int
#endif

typedef struct {
  FILE           *in,
                 *out;
  fwrite_fn       fwrite;
  int             pos,
                  curr,
                  count,
                  maxp,
                  eof;
#ifdef DEBUG
  int             bytes;
#define           PD_COUNT(x,p,w) x p->bytes += w
#else
#define           PD_COUNT(x,p,w)
#endif
  LANG_MODE       mode;
  eSTS            sts;
  Token           lastTok;
  unsigned char  *dumped,
                  buff[1024];

} ParseData;


Token *createTok       (ParseData *,int,int,int);
void   collectStrD     (ParseData *);
int    collectNumberV  (ParseData *);
int    switchLang      (ParseData *pPD,Token *(*)(ParseData *,int));
eSTS   tokPcPD         (ParseData *);
void   collectEscName  (ParseData *);
Token *dumpChar        (ParseData *);
void   completeName    (ParseData *,int);
void   completeNumberV (ParseData *,int);
void   completeNonBlank(ParseData *,int);

/* Copyright (c) 1998,1999,2001,2002,2003 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ const char * v2kprl_c_rcsid() {return "$Id: v2kprl.c,v 1.4 2003/01/14 02:26:23 dkc Exp $";}
 


#define UI_TYPES

#include "EXTERN.h"
#include "perl.h"

#include "args.h"

#define main static v2kDo
#include "../command/v2k.c"

void *V2KgateLib;

static init;

static const char *argv[2];
static int         argc = 1;

int PerlV2Kinit(const char *v2k_exe)
{
  eSTS sts = STS_EALREADY;

  if (!init) {
    init = 1;

    argv[0] = v2k_exe;

    initShell(argc,argv);

    sts = envPathDo(doV2kRC,V2K_RC_PATH,".v2krc");

    if (STS_NORMAL == sts) {

      verInit();
    }
  }

  return ERRNO(sts);
}

int PerlV2Kdo(int s, int e, const char *var)
{
  eSTS  sts = STS_BAD_ARG;
  AV   *ap  = get_av(var, 0);
  int   i   = ap ? av_len(ap)
                 : -1;

  if (ap) {
    TMPARR(char *,argv,i + 2);
    SV  **svp,
         *sv;
    int   l,
          c;
    char *sp,
         *a0 = Arg0();

    i = 0;

    strcpy(sp = argv[i++] = MALLOC2_N(1 + strlen(a0),char),a0);

    for (c = s; e < 0 || c <= e ; c++) {
      svp = av_fetch(ap,c,0);
      if (svp && (sv = *svp)) {
        l  = SvLEN(sv);
        strncpy(sp = argv[i++] = MALLOC2_N(l+1,char),SvPV(sv, l),l);
        sp[l] = '\0';
//	fprintf (stderr,"%d,%d: %s\n",c,l,sp);
      } else if (e < 0) break;
    }

    argv[c = i++] = 0;

    sts = v2kDo(c,argv);

    Arg.exe = a0;;

    while (i-- > 0) Free(argv[i]);
  }

  return ERRNO(sts);
}


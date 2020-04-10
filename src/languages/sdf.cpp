/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * sdf_cpp_rcsid() {return "$Id: sdf.cpp,v 1.43 2009/05/22 21:27:31 dkc Exp $";}

#define VERILOG3
#define VERILOG4

#include "system.h"
#include "args.h"
#include "env.h"
#include "error.h"
#include "language.h"
#include "poolmngr.h"
#include "strdb.h"
#include "v2k_mem.h"
#include "v2k.h"
#define  NEED_TIMING
#define  NEED_CHAR
#define  NEED_SDF
#define  NEED_PUNCTUATION
#define  NEED_QUOTES
#define  NEED_OPERATORS
#define  NEED_WHITESPACE
#define  NEED_EDGES
#define  NEED_INTEGER
#define  NEED_COMMENTS
#define  NEED_VERILOG
#include "tokpool.h"
#include "verilog.h"
#include "dump.h"
#include "sdf.h"
#define SDF_POOL_DESC
#include "sdf_pool-dsc.h"
#include "sdf_pool.h"
#include "parser.h"
#include "job.h"

typedef struct CellInst_s {
  PLCELLINST_FIELDS
  int               nxt_hsh_idx;
} CellInst;

typedef struct HashEnt_s {
  struct HashEnt_s *next,
                   *nxt_hsh;
  int               indx;
} HashEnt;

typedef struct {
  HashEnt *first,
          *table[1];
} HashList;

class SDFcontext;
class TmpSDF : public ContextObj {
public:

  eTSPC    tspc;

  inline void initSDF(int sz) {BZERO(&tspc,sz-(intptr_t)&((TmpSDF *)0)->tspc);};

  eDMD          mode;
  int           saved,
               *inst;
  plCheck      *chk;
  plCell        tmp_cell;
  poolRef       last_cll,
                last_chk;
  SDFcontext   *prsr;

#include "TemplateSdfPool-sdf.fld"

#define CI_HSH_SZ 0x1000
  int ci_hsh[CI_HSH_SZ];

#define HSH_TBL(n,sz) struct {HashEnt *f,\
                                      *t[sz];} n##_hsh;
  HSH_TBL(trp,0x1000)
  HSH_TBL(v1, 0x1000)
  HSH_TBL(v2, 0x1000)
  HSH_TBL(v3, 0x1000)
  HSH_TBL(pp, 0x1000)

#define HSH_SZ(n)     ((sizeof(n##_hsh.t)/sizeof(*n##_hsh.t))-1)

#define SDF_TBL(t,n) int n##_max;
#include "sdf.inc"

#define insts tmp_cell.insts
#define SDF_TBL(t,n) inline int new_##n(t *add,int by) {\
                       int i = n##s++;\
                       if (n##s >= n##_max) { t tmp; if (add) tmp = *add;\
                                              n##_max += by;\
                                              REALLOC2(n,n##_max,t);\
                                              if (add) n[i] = tmp;}\
                       else\
                                              if (add) n[i] = *add;\
                       return i;};
#include "sdf.inc"
#undef insts

  TmpSDF();
  ~TmpSDF();

  Expr    *evalData(InfoRef *,int);
  Expr    *collapseHier(Expr *,VlTypXtra);
  int      evalRef(Expr *,eRFF,eREF);
  hshdRef *logLocal(poolRef,eREF,int);
  void    *getPort(int);
  void     dumpPortRef(Stream *,int,int);

  void createPool(const char *);
  void saveInst();
  void saveCell();

#undef plCellInst
};

extern "C" {
  char *strChk[] = {"",
# define TIMING(t,s,c,p_min,p_max,v_min,v_max,in) s,
# include "timing.inc"
  };
}

BasePool *CoerceSdfPool(PoolIdentifier pi)
{
  BasePool  *pool = pi.pool();

  switch (pool->getMode() & PM_BaseModes) {
#define PM_ACTION(m) case PM_##m:   {m##SdfPool tmp;\
                                     CopyVirtual(&tmp,pool);\
                                     tmp.self = pool; tmp.Init(); break;};
#include "poolmode.inc"

    default:   assert(0);
  }

  return pool;
}

TmpSDF::TmpSDF()
{
  initSDF(sizeof(*this));
  memset(ci_hsh,-1,sizeof(ci_hsh));
  typ = AO_SDF;
  Init();
}

TmpSDF::~TmpSDF()
{
  int           n,
                sz;
  poolRef       ref;

  if (!NULL_REF(tmp_cell.cll_typ)) saveCell();

  HashEnt      *pt,
               *nxt;
  InMemSdfPool *pSdf = SdfPool(saved);

  pSdf->sdfversion  = sdfversion;
  pSdf->version     = version;
  pSdf->design      = design;
  pSdf->date        = date;
  pSdf->vendor      = vendor;
  pSdf->program     = program;
  pSdf->process     = process;

  pSdf->divider     = divider;
  pSdf->tsi         = tsi;
  pSdf->tse         = tse;
  pSdf->voltage     = voltage;
  pSdf->temperature = temperature;

  pSdf->cells       = cells;

  for (nxt = trp_hsh.f; pt = nxt ;) {nxt = pt->next; FREE(pt);}
  for (nxt = v2_hsh.f;  pt = nxt ;) {nxt = pt->next; FREE(pt);}
  for (nxt = v3_hsh.f;  pt = nxt ;) {nxt = pt->next; FREE(pt);}
  for (nxt = pp_hsh.f;  pt = nxt ;) {nxt = pt->next; FREE(pt);}

  for (n = paths; n-- > 0 ;) {
    int      e  = path[n].elems;
    poolRef *pr = (poolRef *)SdfPool(saved)->allocAligned(
                                                   sz = e * sizeof(poolRef),
                                                   sizeof(long),&ref,ALC_PERM);
    BCOPY(path[n].elem.ptr,pr,sz);
    FREE(path[n].elem.ptr);
    SdfPool(saved)->setRef((voidIref *)&path[n].elem,ref);
  }

#define COMP_ONLY
#define SDF_TBL(t,n) t##Iref n##_ref;
#include "sdf.inc"
  IREF(plCellInst) path_ref;

  plCellInst *ci  = (plCellInst *)SdfPool(saved)->allocAligned(
                                                   paths * sizeof(plCellInst),
                                                   sizeof(long),&ref,ALC_PERM);

  for (n = paths; n-- > 0;) ci[n] = *(plCellInst *)&path[n];

  SdfPool(saved)->setRef((voidIref *)&path_ref,ref);

#define ALLOC(t,p) t *p##_p = (t *)SdfPool(saved)->allocAligned(\
                                    sz = p##s * sizeof(t),sizeof(long),\
                                    &ref,ALC_PERM);\
                     BCOPY(p,p##_p,sz);\
                     SdfPool(saved)->setRef((voidIref *)&p##_ref,ref);
#define COMP_ONLY
#define SDF_TBL(t,n) ALLOC(t,n) FREE(n);
#include "sdf.inc"

  pSdf = SdfPool(saved);
#define COMP_ONLY
#define SDF_TBL(t,n) pSdf->n     = n##_ref;\
                     pSdf->n##s  = n##s;
#include "sdf.inc"
                     pSdf->path  = path_ref;
                     pSdf->paths = paths;

  dumper *dmp = (dumper *)DumpList;
  for (; dmp ; dmp = dmp->Next()) if (dmp->Flags() & DMP_SDF) {
     pSdf->dump(dmp->Strm(),this);
  }

  CoerceSdfPool(saved = PoolMngr->pool(saved)->reload(FM_READ));
}

class SDFcontext : public PrsrCntxt {
  int     mapped,
          no_realign,
#ifndef DBGLVL
# define  NO_REALIGN(b) no_realign = b
#else
# define  NO_REALIGN(b)
#endif
          bytes;
  Stream *tok;
  Token  *mtk;
public:

  const Token *NextTok(const Token *,int);

  TmpSDF  sdf;

  static int MinMap,
             MaxMap,
             Realign,
             ReadSize,
             UsePipes;

  SDFcontext(const char *,Stream *);
  ~SDFcontext();

  eSTS         prsSdfTok(poolRef name);
  void         reportError(eSTS,char *,...);
  const Token *skip2nl(const Token *);
  const Token *skip2ec(const Token *);
  const Token *getQuoted(const Token *,poolRef *,char *);
  const Token *getTimescale(const Token *);
  const Token *getChk(eCHK,const Token *,const Token *,int,int,int,int,eCHK,
                      plCheck *);
  const Token *getInstance(const Token *,const Token *);
  const Token *realignBuff(const Token *);
  const Token *closeBlock(const Token *);
  eRVT         getNumber(int,int,const char *,int *);
  int          isNumber(const char *);
  int          ppIndex(plPortPath *);
  int          prtIndex(poolRef);
  int          refIndex(poolRef);
  int          cllIndex(int,poolRef *);
  int          trpIndex(plRtriple *);
  int          val2Index(plRvalue *);
  int          val3Index(plRvalue *);
  int          getRanges(int,InfoRef *,int *);
  int          dblIndex(double);
  int          getPort(const Token *,const Token *,plPortPath *);
  const Token *getValue(const Token *,const Token *,plRvalue *,char *,int,int);
  int          hashAdd(HashList *,int,int);

# define HASH_ADD(n,i,h) hashAdd((HashList *)&sdf.n##_hsh,i,h)

};

#ifndef USE_PIPES
# define USE_PIPES 1
#endif

#define MINMAP 10000

int SDFcontext::MinMap   = MINMAP,
    SDFcontext::MaxMap   = 10000000,
    SDFcontext::Realign  = 1000,
    SDFcontext::ReadSize = 256,
    SDFcontext::UsePipes = USE_PIPES;

extern "C" int sdfSetSz(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  const char *eq   = argEquals(argc,argv);

  if (!eq) return STS_MSSNG_ARG;

  if (1 == sscanf(eq,"%d",&SDFcontext::MinMap)
#ifndef DBGLVL
      && SDFcontext::MinMap > MINMAP
      && SDFcontext::MinMap < SDFcontext::MaxMap
#endif
  ) {
    SDFcontext::Realign = SDFcontext::MinMap/10;
    return STS_NORMAL;
  }

  return STS_BAD_ARG;
}

extern "C" int sdfPiped(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  const char *eq   = argEquals(argc,argv);

  if (!eq) return STS_MSSNG_ARG;

  return argBool(eq,&SDFcontext::UsePipes);
}

const Token *SDFcontext::closeBlock(const Token *T)
{
  while (!findClsBr(T,file_TL,0,0)) {
    int t = T - mtk,
        l = file_TL - T;
    T     = file_TL;
    if (!nextTok(T,-2)) break;
    T     = mtk + t;
  }
  return T;
}

const Token *SDFcontext::realignBuff(const Token *T)
{
  int l = file_TL - T;

  if (l > Realign) {
    ASSERT(!no_realign);
    Token *tm  = mtk;
    while (l-- >= 0) *tm++ = *T++;
    file_TL = --tm;
    T       = mtk;
  }

  return T;
}

const Token *SDFcontext::NextTok(const Token *T,int needed)
{
  if (T && ++T >= file_TL) {
    const Token *TT;
    int          l,
                 li,
                 ti = T - mtk,
                 r,
                 gs;
    T  = 0;
  retry:
    TT = &mtk[mapped];
    l  = TT - file_TL;
    li = file_TL - mtk;

    if (--l > 0) {
      if (l > ReadSize) l = ReadSize;
      switch (mtk[li].ch.tok) {
      case TOK_TRUNCATED:
        gs = l *= sizeof(Token);
      reread:
        if ((r = tok->Read(&mtk[li],gs)) > 0) {
          assert(0 == r%sizeof(Token));
          bytes   += r;
          if (ti > li) ti = li;
	  li      += r/sizeof(Token);
          mtk[li]  = TokMore;
        } else {
          if (0 == r && (r = tok->Poll()) > 0) {
            goto reread;
          }
          if (ti > li) ti = li;
          mtk[li] = TokEOF;
        }
        break;
      case TOK_EOF:
        T = 0;
        break;
      default:
        assert(("Bad token",0));
      }
      file_TL = &mtk[li];
    } else {
      switch (needed) {
      case -2:
        if (!no_realign) break;
	ErrorMsg(S_FATAL(STS_UNDERRUN),"Unmatched '(' at line %d ? ",line);
        goto done;
      case -1:
        if (!no_realign) break;
	ExitMsg(S_FATAL(STS_UNDERRUN),"Use -sbsz=(>%d) too fix.",MinMap);
        goto done;
      case  0:
      default:
        gs = (l = ReadSize) * sizeof(Token);
        li = 0;
        ti = 0;
        goto reread;
      }
      mapped += MinMap;
      REALLOC2(mtk,mapped,Token);
      file_TL = &mtk[li];
      goto retry;
    }
    T = &mtk[ti];
  }
done:
  return T;
}

SDFcontext::SDFcontext(const char *src_cache,Stream *in)
{
  int l;

  CmnInit();
  setCache(src_cache);
  sdf.createPool(src_cache);
  line    = 1;
  mtk     = MALLOC2_N(mapped = MinMap,Token);
  mtk[0]  = TokMore;
  file_TL = mtk;
  tok     = in;
}

SDFcontext::~SDFcontext()
{
  FREE(mtk);
}

const Token *SDFcontext::skip2nl(const Token *T)
{
  T = nextTok(T,0);

  while (T) {
    if (T->pi.pool == WHITESPACE_POOL) switch(T->pi.index) {
    case WHT_NL: return nextTok(T,0);
    }
    T = nextTok(T,0);
  }

  return T;
}

const Token *SDFcontext::skip2ec(const Token *T)
{
  T = nextTok(T,1);

  while (T) {
    if (T->pi.pool == COMMENTS_POOL) switch(T->pi.index) {
    case CMT_ENDCMNT: return nextTok(T,1);
    } else if (T->pi.pool == WHITESPACE_POOL) switch(T->pi.index) {
    }

    T = nextTok(T,1);
  }

  return T;
}

eRVT SDFcontext::getNumber(int sign,int point,const char *str,int *pVal)
{
  int         n   = 0,
              i;
  eRVT        rvt = RVT_INT;
  const char *cp  = str;
  double      d   = 0,
              p   = 0.1;

  if (!point) {
    while (isdigit(i = *cp++)) {
      n = (n * 10) + i - '0';
    }

    switch (i) {
    default:   goto bad;
    case '\0': n *= sign;
               goto done;
    case '.':  break;
    }
  }

  while (isdigit(i = *cp++)) {
    d = d + p * (i - '0');
    p = p/10;
  }

  if (i) goto bad;

  if (0.0 != d) {
    d  += n;
    d  *= sign;
    n   = dblIndex(d);
    rvt = RVT_DOUBLE;
  } else {
    n  *= sign;
  }

done:
  *pVal = n;

  return rvt;

bad:
  reportError(S_ERROR(STS_SYNTAX),"Bad number: %s",str);
  return RVT_NONE;
}

void TmpSDF::saveCell()
{
  poolRef lc   = last_cll;

  saveInst();

  plCell *pCll = (plCell *)SdfPool(saved)->allocAligned(sizeof(plCell),8,
                                                        &last_cll,ALC_PERM);
  *pCll = tmp_cell;
  cells++;

  BZERO(&tmp_cell,sizeof(tmp_cell));

  InMemSdfPool *pSdf = SdfPool(saved);
  if (NULL_REF(lc)) {
    pSdf->setRef((voidIref *)&pSdf->cell,last_cll);
  } else {
    pCll = (plCell *)pSdf->deref(lc);
    pSdf->setRef((voidIref *)&pCll->next,last_cll);
  }

}

const Token *SDFcontext::getChk(eCHK ct,const Token *T0,const Token *TT,
                                int ports_min,int ports_max,
                                int vals_min, int vals_max, eCHK inner,
                                plCheck *pChk)
{
  int          items = 0;
  const Token *T,
              *TE    = findClsBr(T0,TT,&items,0);
  TMPARR(const Token *,TS,items+1);
  plCheck      chk;
  poolRef      ref;

  chk.mode    = sdf.mode;
  chk.next.pi = NullRef;

  if (!TE) reportError(S_ERROR(STS_SYNTAX),"Check not closed");

  NO_REALIGN(1);

  T = findClsBr(T0,TT,0,TS);

  if (inner) {
    if (items <= 0) {
      reportError(S_ERROR(STS_SYNTAX),"Bad %s",strChk[ct]);
    } else {
      const Token *TI = TT = TS[items-1];

      TI = tokGetRef(nextTok(TI,0),&ref,0,1);
      switch (inner) {
#define TIMING(t,s,c,p_min,p_max,v_min,v_max,in)\
      case CHK_##t: if (SDF_POOL != ref.pool || SDF_##t != ref.index) {\
                      reportError(S_ERROR(STS_SYNTAX),"Expected %s not %s",\
                                                      s,strDeref(ref));\
		    } else {\
                      T  = getChk(CHK_##t,TI,TE,p_min,p_max,v_min,v_max,in,&chk);\
		    }\
                    break;
#include "timing.inc"
      }

      chk.inner = inner;
      chk.typ   = ct;

      if (TT != sdf.getExpr(T0,TT,STMT_CNTX(SC_TOP|SC_COMPLETE))) {
        String xpr;
        reportError(S_ERROR(STS_SYNTAX),"Confused at end of: %s",
                    strDerefArr(sdf.vd()->expr,sdf.vd()->expr_len,&xpr," "));
      } else {
        if (sdf.cmplExpr() && sdf.evalExpr()) {
          int *pp_id = (int *)SdfPool(sdf.saved)->deref((voidIref *)&chk.port);
  	  plPortPath *pPP = &sdf.pp[pp_id[0]];
          assert(NULL_REF(pPP->cond.pi));
          XREF(plExpr) px = SdfPool(sdf.saved)->plSaveExpr(sdf.vd()->expr_val);
          pPP->cond.pi    = px.pi;
        }
      }
    }
  } else {
    int      i  = items,
             rv = 0;
#define MAX_RV 11
    plRvalue rvs[MAX_RV+1];

    chk.typ = ct;

    while (i > 0) {
      i--;
      BZERO(&rvs[MAX_RV-rv],sizeof(plRvalue));
      if (!getValue(TS[i]+1,TS[i+1],&rvs[MAX_RV-rv],0,0,1)) break;
      TT = TS[i];
      rv++;
    }

    if (vals_max >= 0 && rv < vals_min) {
      reportError(S_ERROR(STS_SYNTAX),"Too few values (%d) to %s",
                                      rv,strChk[ct]);
    } else if (rv > vals_max || rv < vals_min) {
      reportError(S_ERROR(STS_SYNTAX),"Wrong number of values (%d) to %s",
                                      rv,strChk[ct]);
    }

    switch (rv) {
    case 1:  rvs[0]     = rvs[MAX_RV];
             break;
    case 2:  rvs[0].val = val2Index(&rvs[(MAX_RV+1)-rv]);
             rvs[0].typ = RVT_VAL2;
             break;
    case 3:  rvs[0].val = val3Index(&rvs[(MAX_RV+1)-rv]);
             rvs[0].typ = RVT_VAL3;
             break;
    case 6:  rvs[0].val = val3Index(&rvs[(MAX_RV+1)-rv]);
             rvs[0].typ = RVT_VAL3;
             rvs[1].val = val3Index(&rvs[((MAX_RV+1)-rv)+3]);
             rvs[1].typ = RVT_VAL3;
             rvs[0].val = val2Index(rvs);
             rvs[0].typ = RVT_VAL6;
             break;
    case 12: rvs[0].val = val3Index(&rvs[(MAX_RV+1)-rv]);
             rvs[0].typ = RVT_VAL3;
             rvs[1].val = val3Index(&rvs[((MAX_RV+1)-rv)+3]);
             rvs[1].typ = RVT_VAL3;
             rvs[0].val = val2Index(rvs);
             rvs[0].typ = RVT_VAL6;
             rvs[1].val = val3Index(&rvs[(MAX_RV+1)-rv]+6);
             rvs[1].typ = RVT_VAL3;
             rvs[2].val = val3Index(&rvs[((MAX_RV+1)-rv)+9]);
             rvs[2].typ = RVT_VAL3;
             rvs[1].val = val2Index(&rvs[1]);
             rvs[1].typ = RVT_VAL6;
             rvs[0].val = val2Index(&rvs[(MAX_RV+1)-rv]);
             rvs[0].typ = RVT_VAL12;
             break;
    default: reportError(S_ERROR(STS_SYNTAX),"Invalid list of values (%s)",
                                             strChk[ct]);
    }

    chk.val = rvs[0];

    tokenize(T0,TT,&items,0);
    TMPARR(const Token *,TP,items+1);
    tokenize(T0,TT,0,TP);
    chk.ports  = items;
    int  p     = 0,
         err   = 0,
        *pp_id = (int *)SdfPool(sdf.saved)->allocAligned(items * sizeof(int),
                                                         sizeof(int),
                                                         &ref,ALC_PERM);

    SdfPool(sdf.saved)->setRef((voidIref *)&chk.port,ref);

    for (i = 0; i < items ; p++,i++) {
      plPortPath port;
      BZERO(&port,sizeof(plPortPath));
      if (getPort(TP[i],TP[i+1],&port)) {
        pp_id[i] = ppIndex(&port);
      } else {
        pp_id[i] = -1;
        if (!err++) reportError(S_ERROR(STS_SYNTAX),
                                "Too few ports for (%s)",strChk[ct]);
      }
    }
  }

  if (pChk) {
    *pChk = chk;
  } else {
    poolRef lc = sdf.last_chk;
    pChk       = (plCheck *)SdfPool(sdf.saved)->allocAligned(
                                                  sizeof(plCheck),8,
                                                  &sdf.last_chk,ALC_PERM);
    *pChk = chk;
    sdf.tmp_cell.chks++;

    if (NULL_REF(sdf.tmp_cell.chk.pi)) {
      SdfPool(sdf.saved)->setRef((voidIref *)&sdf.tmp_cell.chk,sdf.last_chk);
    } else {
      pChk = (plCheck *)SdfPool(sdf.saved)->deref(lc);
      SdfPool(sdf.saved)->setRef((voidIref *)&pChk->next,sdf.last_chk);
    }
  }

done:
  NO_REALIGN(0);
  return TE;
}

void TmpSDF::saveInst()
{
  if (tmp_cell.insts) {
    poolRef  ref,
             lc;
    int     *ids = (int *)SdfPool(saved)->allocAligned(
                                      tmp_cell.insts * sizeof(int),sizeof(int),
                                      &ref,ALC_PERM);

    BCOPY(inst,ids,tmp_cell.insts * sizeof(int));
    SdfPool(saved)->setRef((voidIref *)&tmp_cell.inst,ref);
  }
}

const Token *SDFcontext::getTimescale(const Token *T) {
  poolRef     ref;
  int         n = 0,
              i;

  T              = tokGetRef(T,&ref,0,1);
  const char *cp = strDeref(ref);

  while (isdigit(i = *cp++)) n = (n * 10) + i - '0';

  if (n && !i) {
    T  = tokGetRef(T,&ref,0,1);
    cp = strDeref(ref);
    i  = *cp++;
  }

  sdf.tsi = n;

  switch (i) {
  case 0:   goto test;
  case 'u': sdf.tse = -6;  break;
  case 'n': sdf.tse = -9;  break;
  case 'p': sdf.tse = -12; break;
  }
  i = *cp++;
test:
  if (!sdf.tsi || !sdf.tse || (i && ('s' != i || *cp))) {
    reportError(S_ERROR(STS_SYNTAX),"Bad timescale");
  } else if (i != 's') {
    const Token *T2 = tokGetRef(T,&ref,0,1);
    if (0 == strcmp(strDeref(ref),"s")) T = T2;
    else                                rescanning(T);
  }

  return T;
}

const Token *SDFcontext::getInstance(const Token *T0,const Token *TT)
{
  const Token *T    = T0;
  int          d    = 1,
               slsh = '/' == sdf.divider,
               dot  = '.' == sdf.divider;

  for (; T <= TT ; T++) switch (T->pi.pool) {
  case OPERATORS_POOL:   if (OP_DIVIDE   == T->pi.index && slsh) d++;
                         break;
  case PUNCTUATION_POOL: if (PUNC_PERIOD == T->pi.index && dot)  d++;
                         if (PUNC_CLS_BR == T->pi.index) goto get_nms;
                         break;
  }

  reportError(S_ERROR(STS_SYNTAX),"Bad INSTANCE");
  return T;

get_nms:
  TMPARR(poolRef,names,d);
  poolRef ref;
  long    punc = 1;

  BZEROA(names);

  for (T = T0,d = 0;;) {
    T = tokGetRef(T0 = T,&ref,0,1);
    switch (ref.pool) {
    case OPERATORS_POOL:   if (OP_DIVIDE   == ref.index && slsh) {
                             punc = 1; continue;
                           }
                           break;
    case PUNCTUATION_POOL: if (PUNC_PERIOD == ref.index && dot) {
                             punc = 1; continue;
                           }
                           if (PUNC_CLS_BR == ref.index) {
                             T = rescanning(T0);
                             goto save;
                           }
                           break;
    }
    if (!punc) {
      reportError(S_ERROR(STS_SYNTAX),"Unexpected multiple tokens (%s)",
                                      strDeref(ref));
    }
    names[d++] = ref;
  }
save:
  int id = d > 0 ? cllIndex(d,names)
                 : -1,
      in = sdf.new_inst(&id,4);

  return T;
}

int SDFcontext::cllIndex(int t,poolRef *refs)
{
  int  d;
  long hash = 0;

  for (d = 0; d < t ; d++) {
    hash ^= refs[d].index^refs[d].pool;
  }

  hash = (CI_HSH_SZ-1) & (hash ^ (hash >> 16));

  CellInst  *c0 = sdf.path,
            *ci;
  int        id,
             id_lst = sdf.ci_hsh[hash];

  for (; id_lst >=0 ; id_lst = ci->nxt_hsh_idx) {
    ci = &c0[id = id_lst];
    if (d == ci->elems && 0 == BCMP(refs,ci->elem.ptr,d * sizeof(poolRef))) {
      goto hv_alrdy;
    }
  }

  id = sdf.new_path(0,50);

  sdf.path[id].elems       = d;
  sdf.path[id].nxt_hsh_idx = id_lst;
  sdf.ci_hsh[hash]         = id;

  BCOPY(refs,sdf.path[id].elem.ptr = MALLOC2_N(d,poolRef),d * sizeof(poolRef));

hv_alrdy:
  return id;
}

int SDFcontext::trpIndex(plRtriple *trip)
{
  int     *pt  = (int *)trip,
           hsh = HSH_SZ(sdf.trp) & (pt[0]^(pt[1] << 2)^(pt[2] << 4)
                                                      ^(pt[2] << 6)),
           id;
  HashEnt *he  = sdf.trp_hsh.t[hsh];

  for (; he ; he = he->nxt_hsh) {
    plRtriple *trip2 = &sdf.trpl[he->indx];
    if (0 == BCMP(trip2,trip,sizeof(plRtriple))) {
      return he->indx;
    }
  }

  id = sdf.new_trpl(trip,200);

  HASH_ADD(trp,id,hsh);

  return id;
}

int SDFcontext::hashAdd(HashList *lst,int id,int hsh)
{
  HashEnt *he      = MALLOC2(HashEnt);
  he->next         = lst->first;
  lst->first       = he;
  he->nxt_hsh      = lst->table[hsh];
  lst->table[hsh]  = he;
  he->indx         = id;

  return 1;
}

int SDFcontext::val2Index(plRvalue *pv)
{
  int      hsh = HSH_SZ(sdf.v2) & (pv[0].typ^(pv[1].typ << 2)
                                   ^(pv[0].val << 4)^(pv[1].val << 6)),
           id;
  HashEnt *he  = sdf.v2_hsh.t[hsh];

  for (; he ; he = he->nxt_hsh) {
    plRval2 *val2 = &sdf.val2[he->indx];
    if (0 == BCMP(val2,pv,sizeof(plRval2))) {
      return he->indx;
    }
  }

  id = sdf.new_val2(0,200);
  BCOPY(pv,&sdf.val2[id],sizeof(plRval2));

  HASH_ADD(v2,id,hsh);

  return id;
}

int SDFcontext::val3Index(plRvalue *pv)
{
  int      hsh = HSH_SZ(sdf.v3)
                  & (pv[0].typ^(pv[1].typ << 2)^(pv[2].typ << 4)
                     ^(pv[0].val << 6)^(pv[1].val << 8)^(pv[2].val << 10)),
           id;
  HashEnt *he  = sdf.v3_hsh.t[hsh];

  for (; he ; he = he->nxt_hsh) {
    plRval3 *val2 = &sdf.val3[he->indx];
    if (0 == BCMP(val2,pv,sizeof(plRval3))) {
      return he->indx;
    }
  }

  id  = sdf.new_val3(0,200);
  BCOPY(pv,&sdf.val3[id],sizeof(plRval3));

  HASH_ADD(v3,id,hsh);

  return id;
}

int SDFcontext::ppIndex(plPortPath *pPP)
{
  int      hsh = HSH_SZ(sdf.pp)
                  & (pPP->typ^(pPP->edge << 2)
                     ^(pPP->inst << 10)^(pPP->name)),
           id;
  HashEnt *he  = sdf.pp_hsh.t[hsh];

  for (; he ; he = he->nxt_hsh) {
    plPortPath *pPP2 = &sdf.pp[he->indx];
    if (0 == BCMP(pPP2,pPP,sizeof(plPortPath))) {
      return he->indx;
    }
  }

  id = sdf.new_pp(pPP,200);

  HASH_ADD(v3,id,hsh);

  return id;
}

int SDFcontext::dblIndex(double dbl)
{
  int top = sdf.dbls,
      bot = 0,
      tst;

  while (top > bot) {

    tst = (top + bot)/2;

    if (dbl == sdf.dbl[tst]) goto done;
    if (dbl  > sdf.dbl[tst]) bot = tst+1;
    else                     top = tst-1;
  }

  top = sdf.new_dbl(0,200);

  for (tst = top ; tst > 0 && dbl < sdf.dbl[tst] ; tst--) {
    sdf.dbl[tst] = sdf.dbl[tst-1];
  }

  sdf.dbl[tst] = dbl;

done:
  return tst;
}

int SDFcontext::prtIndex(poolRef ref)
{
  int top = sdf.ports,
      bot = 0,
      tst;
  I64 ll  = *(I64 *)&ref,
     *lp  = (I64 *)sdf.port;

  while (top > bot) {

    tst = (top + bot)/2;

    if (ll == lp[tst]) goto done;
    if (ll  > lp[tst]) bot = tst+1;
    else               top = tst-1;
  }

  top = sdf.new_port(0,200);
  lp  = (I64 *)sdf.port;

  for (tst = top ; tst > 0 && ll < lp[tst] ; tst--) {
    lp[tst] = lp[tst-1];
  }

  lp[tst] = ll;

done:
  return tst;
}

int SDFcontext::refIndex(poolRef ref)
{
  int top = sdf.ports,
      bot = 0,
      tst;
  I64 ll  = *(I64 *)&ref,
     *lp  = (I64 *)sdf.name;

  while (top > bot) {

    tst = (top + bot)/2;

    if (ll == lp[tst]) goto done;
    if (ll  > lp[tst]) bot = tst+1;
    else               top = tst-1;
  }

  top = sdf.new_name(0,200);
  lp  = (I64 *)sdf.name;

  for (tst = top ; tst > 0 && ll < lp[tst] ; tst--) {
    lp[tst] = lp[tst-1];
  }

  lp[tst] = ll;

done:
  return tst;
}

INLINE int SDFcontext::isNumber(const char *str)
{
  int ch;

  while (ch = *str++) {
    if (!isdigit(ch) && '.' != ch) return 0;
  }

  return 1;
}

int SDFcontext::getRanges(int exprl,InfoRef *pIrf,int *pRng)
{
  int         r = 0;
  const char *s;

  for (; exprl-- > 0 ; pIrf++) {
    if (1 != sscanf(s = strDeref(*pIrf++),"%d",&pRng[r++])) {
      reportError(S_ERROR(STS_SYNTAX),"Bad number: %s",s);
      break;
    }
    if (exprl-- > 0 && !SAME_REF_PI(*pIrf,PUNCTUATION_POOL,PUNC_COLON))  {
      reportError(S_ERROR(STS_SYNTAX),"Expected ':' after '%s'",s);
      break;
    }
  }

  return r;
}

int SDFcontext::getPort(const Token *T0,const Token *TT,plPortPath *pPP)
{
  poolRef      ref;
  const Token *T;
  eEDG         edge = EDG_NONE;
  int          xpr  = 0,
               slsh = '/' == sdf.divider,
               cond = -1;

  sdf.vd()->posn.line = line;

  T = sdf.getExpr(T0,TT,STMT_CNTX(SC_TOP|SC_COMPLETE));

  int      t     = sdf.vd()->expr_len,
           clsr  = -1,
           br0   = -1,
           br1   = -1,
           idx   = -1,
           depth = 0,
           exprl = sdf.vd()->expr_len;
  InfoRef *pIrf  = sdf.vd()->expr;

  if (T != TT) goto confused;

  while (t-- > 0) switch (pIrf[t].pool) {
  case OPERATORS_POOL:   if (slsh && OP_DIVIDE == pIrf[t].index) {
                           if (xpr < 1) xpr = -1;
                         } else {
                           xpr = 1;
                         }
                         break;
  case INTEGER_POOL:     if (1 == t && clsr == (exprl -1) &&
                             xpr < 1 && INT_10 == pIrf[t].index) {
                           edge = EDG_10;
                         }
                         break;
  case EDGES_POOL:       if (1 == t && clsr == (exprl -1)) {
                           switch (pIrf[t].index) {
                           case EDGE_01: edge = EDG_01; break;
                           case EDGE_0Z: edge = EDG_0Z; break;
                           case EDGE_Z1: edge = EDG_Z1; break;
                           case EDGE_1Z: edge = EDG_1Z; break;
                           case EDGE_Z0: edge = EDG_Z0; break;
                           }
                         }
                         break;
  case TIMING_POOL:      if (1 == t && clsr == (exprl -1)) {
                           switch (pIrf[t].index) {
                           case TIM_POSEDGE: edge = EDG_POSEDGE; break;
                           case TIM_NEGEDGE: edge = EDG_NEGEDGE; break;
                           }
                         }
                         break;
  case SDF_POOL:         if (SDF_COND == pIrf[t].index) cond = t;
                         break;
  case PUNCTUATION_POOL: switch (pIrf[t].index) {
                         case PUNC_OPN_BR: if (!--depth) clsr = -1;
                                           if (cond < 0) br1  = t;
                                           br0 = t;
                                           break;
                         case PUNC_CLS_BR: if (!depth++) clsr = t;
                                           break;
                         case PUNC_OPN_CR:
                         case PUNC_CLS_CR: xpr = 1;
                                           break;
                         case PUNC_OPN_SQ: idx = t;
                                           break;
                         }
  }

  if (cond >= 0) {
    if (cond != 1 || br0 != 0 || br1 <= cond +1) goto confused;

    sdf.vd()->cmpld_expr = sdf.cmplExpr(sdf.vd()->expr     + br1,
                                        sdf.vd()->expr_len - (br1 +1),
                                        VT_VOID,sdf.vd()->cntxt);

    if (sdf.vd()->cmpld_expr && sdf.evalExpr()) {
      XREF(plExpr) px = SdfPool(sdf.saved)->plSaveExpr(sdf.vd()->expr_val);
      pPP->cond.pi    = px.pi;
    }
    exprl  = br1 -2;
    pIrf  += 2;
  } else if (xpr > 0) {
    goto confused;
  } else if (pPP->edge = edge) {
    pIrf     += 2;
    exprl    -= 3;
    idx      -= 2;
    pPP->typ  = PSP(pPP->typ|PSP_EDGE);
  }

  if (idx >= 0) {
    if (!SAME_REF_PI(pIrf[exprl-1],PUNCTUATION_POOL,PUNC_CLS_SQ)) {
      goto confused;
    }
    pPP->ranges = getRanges((exprl-idx)-2,&pIrf[idx+1],pPP->range);
    exprl       = idx;
  }

  t   = exprl;
  ref = pIrf[--t];

  if (t) {
    int     i    = 0,
            d    = 0,
            slsh = '/' == sdf.divider,
            dot  = '.' == sdf.divider;
    TMPARR(poolRef,refs,t);

    pPP->name = refIndex(ref);

    for(; d < t ; d++) {
      ref = pIrf[d];
      switch (ref.pool) {
      case OPERATORS_POOL:   if (OP_DIVIDE   == ref.index && slsh) {
                               continue;
                             }
                             break;
      case PUNCTUATION_POOL: if (PUNC_PERIOD == ref.index && dot) {
                               continue;
                             }
                             break;
      }
      refs[i++] = ref;
    }
    pPP->inst = cllIndex(i,refs);
  } else {
    pPP->name = prtIndex(ref);
    pPP->inst = -1;
  }

  return 1;

confused: {
    String       xpr;
    reportError(S_ERROR(STS_SYNTAX),"Confused at end of: %s",
                                    strDerefArr(pIrf,exprl,&xpr," "));
  }
  return 0;
}

const Token *SDFcontext::getValue(const Token *T0,const Token *TT,
                                  plRvalue *pVal,char *ap,int i,int max)
{
  poolRef      ref;
  const Token *T;
  plRvalue     values[12];
  plRtriple    trip;

  for (; i < max && T0 < TT ; i++) {
    int n = 0;
    T     = tokGetRef(T0,&ref,0,1);

    if (PUNCTUATION_POOL == ref.pool) {

      if (PUNC_CLS_BR == ref.index) goto done;

      if (PUNC_OPN_BR == ref.index) {
        T = getValue(T,TT,&values[i],ap,i,max);
        T = tokGetRef(T,&ref,0,1);
        if (PUNCTUATION_POOL != ref.pool || PUNC_CLS_BR != ref.index) {
          if (!ap) return 0;
          reportError(S_ERROR(STS_SYNTAX),"Missing ')' for %s",ap);
        }
        continue;
      }
    }

    BZERO(&trip,sizeof(trip));
    while (n < 3) {
      int point = 0;
      if (PUNCTUATION_POOL == ref.pool) {
        switch (ref.index) {
        default:          if (!ap) return 0;
                          reportError(S_ERROR(STS_SYNTAX),
                                      "Badly formed number for %s",ap);
        case PUNC_CLS_BR: T = rescanning(T0);
                          goto end_num;
        case PUNC_COLON:  n++;
                          T = tokGetRef(T0 = T,&ref,0,1);
                          continue;
        case PUNC_PERIOD: point = 1;
                          T     = tokGetRef(T0 = T,&ref,0,1);
                          break;
        }
      }
      int s = 1;
      if (OPERATORS_POOL == ref.pool && OP_MINUS == ref.index) {
        s = -1;
        T = tokGetRef(T,&ref,0,1);
        if (PUNCTUATION_POOL == ref.pool && PUNC_PERIOD == ref.index) {
          point = 1;
          T     = tokGetRef(T0 = T,&ref,0,1);
        }
      }
      const char *str = strDeref(ref);
      if (!isNumber(str)) {
        if (!ap) return 0;
        reportError(S_ERROR(STS_SYNTAX),"Badly formed number (%s) for %s",
                                        str,ap);
      } else {
        trip.typ[n] = getNumber(s,point,str,&trip.val[n]);
        T           = tokGetRef(T0 = T,&ref,0,1);
      }
    }
  end_num:
    if (n > 0) {
      values[i].typ = RVT_TRIPLE;
      values[i].val = trpIndex(&trip);
    } else {
      values[i].typ = RVT(trip.typ[0]);
      values[i].val = trip.val[0];
    }
  }

done:
  switch (i) {
  case 1:  *pVal = values[0]; break;
  default: if (!ap) return 0;
           reportError(S_ERROR(STS_SYNTAX),"Badly formed list for %s",ap);
  }
  return T;
}

const Token *SDFcontext::getQuoted(const Token *T,poolRef *pRef,char *ap)
{
  poolRef      ref;
  int          q,
               ti = T - mtk;

  T = tokGetRef(T,&ref,0,1);

  if (QUOTES_POOL == ref.pool) {
    q = ref.index;
    T = tokGetRef(T,&ref,0,1);
    if (QUOTES_POOL      == ref.pool ||
	PUNCTUATION_POOL == ref.pool) {
      *pRef = NullRef;
    } else {
      *pRef = ref;
      T = tokGetRef(T,&ref,0,1);
      if (QUOTES_POOL != ref.pool || q != ref.index) {
	reportError(S_ERROR(STS_SYNTAX),"Badly formed string for %s",ap);
      }
    }
  } else if (PUNCTUATION_POOL == ref.pool && PUNC_CLS_BR == ref.index) {
    *pRef = NullRef;
    return &mtk[ti];
  } else {
    reportError(S_WARNING(STS_SYNTAX),"Badly formed string for %s",ap);
    *pRef = ref;
  }

  return T;
}

void SDFcontext::reportError(eSTS sts,char *format,...)
{
  va_list pvar;
  va_start(pvar, format);
  String msg(format,pvar);
  ErrorMsg(sts,"%s",msg.str());
  va_end(pvar);

  Filename    tmp;
  const char *src = source(tmp);

  if (!*src) {
    source(tmp);
    src = tmp.str();
  } else {
    cch2src(src,tmp);
  }

  ErrorMsg(sts," @ %s:%d",tmp.str(),line_no());
}

eSTS SDFcontext::prsSdfTok(poolRef nm)
{
  poolRef      Ref;
  tokPoolRef  *tpr;
  int          l,
               br  = 0;
  const Token *T   = mtk;

  curr_file = nm;

  sdf.setSource(curr_file = nm);
  sdf.pushScope(0,nm);

  sdf.prsr = this;

  while (T) {
    poolRef ref;
    ref.pool  = T->pi.pool;
    ref.index = T->pi.index;
  again:
    switch (ref.pool) {
    case TOK_EOF:
      T = 0;
      if (!br) continue;
      break;
    case TOK_TRUNCATED: 
      T = NextTok(T,1);
      continue;
    case WHITESPACE_POOL:
      T = nextTok(T,br > 0);
      continue;
    case PUNCTUATION_POOL:
      switch (ref.index) {
	case PUNC_OPN_BR: switch (++br) {
                        }
                        goto next;
	case PUNC_CLS_BR: --br;
                        goto next;
      }
      break;
    case SDF_POOL:
      switch (ref.index) {
      case SDF_DELAYFILE:    if (br != 1) goto unex;
                             goto next;
      case SDF_SDFVERSION:   if (br != 2) goto unex;
                             T = getQuoted(nextTok(T,0),&sdf.sdfversion,"SDFVERSION");
                             goto chk_cls;
      case SDF_PROCESS:      if (br != 2) goto unex;
                             T = getQuoted(nextTok(T,0),&sdf.process,"PROCESS");
                             goto chk_cls;
      case SDF_DESIGN:       if (br != 2) goto unex;
                             T = getQuoted(nextTok(T,0),&sdf.design,"DESIGN");
                             goto chk_cls;
      case SDF_DATE:         if (br != 2) goto unex;
                             T = getQuoted(nextTok(T,0),&sdf.date,"DATE");
                             goto chk_cls;
      case SDF_VENDOR:       if (br != 2) goto unex;
                             T = getQuoted(nextTok(T,0),&sdf.vendor,"VENDOR");
                             goto chk_cls;
      case SDF_PROGRAM:      if (br != 2) goto unex;
                             T = getQuoted(nextTok(T,0),&sdf.program,"PROGRAM");
                             goto chk_cls;
      case SDF_VERSION:      if (br != 2) goto unex;
                             T = getQuoted(nextTok(T,0),&sdf.version,"VERSION");
                             goto chk_cls;
      case SDF_DIVIDER:      if (br != 2) goto unex;
                             T           = tokGetRef(nextTok(T,0),&Ref,0,1);
                             sdf.divider = *strDeref(Ref);
                             goto chk_cls;
      case SDF_VOLTAGE:      if (br != 2) goto unex;
                             T = getValue(nextTok(T,0),file_TL,&sdf.voltage,
                                                 "VOLTAGE",0,1);
                             goto chk_cls;
      case SDF_TEMPERATURE:  if (br != 2) goto unex;
                             T = getValue(nextTok(T,0),file_TL,&sdf.temperature,
                                                 "TEMPERATURE",0,1);
                             goto chk_cls;
      case SDF_TIMESCALE:    if (br != 2) goto unex;
                             T = getTimescale(nextTok(T,0));
                             goto chk_cls;
      case SDF_CELL:         if (br != 2) goto unex;
	                     if (!NULL_REF(sdf.tmp_cell.cll_typ)) {
                               sdf.saveCell();
                             }
                             sdf.last_chk = NullRef;
                             T            = realignBuff(T);
                             goto next;
      case SDF_CELLTYPE:     if (br != 3) goto unex;
                             T = getQuoted(nextTok(T,0),&sdf.tmp_cell.cll_typ,
                                               "CELLTYPE");
                             goto chk_cls;
      case SDF_INSTANCE:     if (br != 3) goto unex;
                             T = realignBuff(T);
                             T = closeBlock(T);
                             T = getInstance(nextTok(T,0),file_TL);
                             goto chk_cls;
      case SDF_DELAY:        if (br != 3) goto unex;
                             sdf.tspc = TSPC_DELAY;
                             goto next;
      case SDF_TIMINGCHECK:  if (br != 3) goto unex;
                             sdf.tspc = TSPC_TIMINGCHECK;
                             goto next;
      case SDF_ABSOLUTE:     if (br != 4) goto unex;
                             sdf.mode = DMD_ABS;
                             goto next;
      case SDF_INCREMENT:    if (br != 4) goto unex;
                             sdf.mode = DMD_INCR;
                             goto next;
#define TIMING(t,s,c,p_min,p_max,v_min,v_max,in)\
      case SDF_##t:          if (br != 5 && TSPC_##c != sdf.tspc) goto unex;\
                             T = realignBuff(T);\
                             T = closeBlock(T);\
                             T = getChk(CHK_##t,nextTok(T,0),file_TL,p_min,p_max,v_min,v_max,in,0);\
                             goto chk_cls;
#include "timing.inc"
      }
      break;
    case COMMENTS_POOL:
      switch (T->pi.index) {
 	  case CMT_EOL:      T = skip2nl(T); continue;
	  case CMT_STRTCMNT: T = skip2ec(T); continue;
	}
      break;
    case TOK_CHAR:
      if (isspace(T->ch.ch)) {
        T = nextTok(T,br > 0);
      } else {
        assert(T->ch.count);
        assert(0);
      }
      continue;
    case TOK_FULL_REF:
      ref.pool  = (++T)->as_int;
      ref.index = (++T)->as_int;
      goto again;
    case TOK_POOL_SPEC:
      T = rgstrPool(tpr = (tokPoolRef *)T);
      l = PoolLoad (tpr->name.buff,tpr->xt.extra,fmode);
      assert(l == tpr->xt.extra);
      continue;
    }
  chk_cls:
    while (T && T->pi.pool == WHITESPACE_POOL) T = nextTok(T,1);
    if (!T) {
      reportError(S_ERROR(STS_TRUNC),"Premature end of file (%s)",tok->Name());
      return STS_TRUNC;
    } else if (PUNCTUATION_POOL == T->pi.pool && PUNC_CLS_BR == T->pi.index) {
      br--;
      goto next;
    }
  unex:
    reportError(S_ERROR(STS_SYNTAX),"Unexpected token (%s)",tokDeref(T));
  next:
    T = nextTok(T,br > 0);
  }

  sdf.popScope(0,nm);
  return bytes ? STS_NORMAL
               : STS_EMPTY;
}

InMemSdfPool *SdfPool(int id)
{
  return (InMemSdfPool *)(PoolMngr->pool(id));
}

void TmpSDF::createPool(const char *src_cch)
{
  File f(src_cch);

  f.changeType(FT_Cdf);

  CoerceSdfPool(saved = PoolMngr->newPool(PM_OnDisk,&f,"pcw",
                -1,sizeof(OnDiskSdfPool)));
}

Expr *TmpSDF::collapseHier(Expr *xpr,VlTypXtra in_use)
{
  int n = xpr->countLeaves();

  { TMPARR(poolRef,Refs,n);
    Expr *rx = new Expr(Refs,(XData)(XD_FLAG|XD_HREF),0);

    int id = prsr->cllIndex(n -1,Refs),
        ir = prsr->refIndex(Refs[n-1]);

    rx->typ           = VT_REFL;
    rx->rtyp          = REF_SDF_PORT;
    rx->prrf()->scp   = id;
    rx->prrf()->index = ir;

    return rx;
  }

  xpr->xtra = VL_TYP_XTRA(in_use|xpr->xtra);
  return xpr;
}

int TmpSDF::evalRef(Expr *rx,eRFF rff,eREF cr)
{
  poolRef nm  = rx->ref();
  int     ret = prsr->prtIndex(nm);

  rx->typ           = VT_REFL;
  rx->rtyp          = REF_SDF_PORT;
  rx->prrf()->scp   = -1;
  rx->prrf()->index = ret;

  return ret;
}

void TmpSDF::dumpPortRef(Stream *s,int i,int p)
{
  InMemSdfPool *pSdf = SdfPool(saved);

  pSdf->dumpPortRef(s,i,p);
}

hshdRef *TmpSDF::logLocal(poolRef ref,eREF rt,int i)
{
  assert("Inappropriate call" == 0);
  return 0;
}

#define EVAL_CLASS TmpSDF
#include "eval_data.inc"

eSTS tokSDF(Stream *in,Stream *out)
{
  return tokSDF(in->Fp(),out->FwriteFn(),out->Fp(STRM_OUT));
}


typedef struct {
#ifdef PTHREADS
  pthread_t      thread;
  pthread_attr_t attr;
#endif
  Stream         in,
                 out;
  eSTS           sts,
                 sts_thrd;
} SdfData;

#ifdef PTHREADS

void *tokSDFthrd(void *arg)
{
  SdfData *dp = (SdfData *)arg;

  LwpCount(1);

  dp->sts_thrd = tokSDF(dp->in.Fp(),dp->out.FwriteFn(),dp->out.Fp(STRM_OUT));
  dp->out.Close(STRM_OUT);

  LwpCount(-1);

  pthread_exit(&dp->sts_thrd);

  return 0;
}

#endif

int prsSDFld(const char *file)
{
  Filename  src(file);
  File      f(file);

  envExpand(src,sizeof(src));

  if (!src.Exists()) {
    return ErrorMsg(S_ERROR(STS_BAD_ARG),"File missing - %s",file);
  }

  poolRef src_nm = strSaveStr(src);

  SDFcontext::encode(src);

  File     tok("${V2K_REPOSITORY}/cache/",src,FT_Token),
          *fp = &f;
  SdfData  sd;

  if (sd.in.Open(fp,"r"))
  {
    SDFcontext cntxt(tok,&sd.out);

    if (SDFcontext::UsePipes && 0 == sd.out.Pipe(FM_NONE)) {
#ifndef PTHREADS
      int fllw_chld = 0; // Debug
      Job *job = new Job(Fork(fllw_chld),"SDF Tokenizing");
      switch(job->Pid()) {case -1: sd.sts = Error();
                                   goto failed;
                          case  0: if (fllw_chld) goto main;
                                   else           goto tknz;
                          default: if (fllw_chld) goto tknz;
                                   else           goto main;}

      tknz:    ErrControl.am_chld = 1;
               sd.out.Close(STRM_IN);
               sd.sts = tokSDF(&sd.in,&sd.out);
               Exit(sd.sts);

      main:    sd.out.Close(STRM_OUT);
               sd.out.setName(f);
               sd.out.setJob(job);
               if (!(sd.sts = cntxt.prsSdfTok(src_nm)) &&
                   !(sd.sts = job->Status())) {
                 strPoolShared(0);
                 goto done;
               }
               strPoolShared(0);
               ErrorMsg(S_WARNING(sd.sts),"Piped parse failed for %s",f.str());
#else
      sd.out.setName(f);
      pthread_attr_init(&sd.attr);

      if (pthread_create(&sd.thread,&sd.attr,tokSDFthrd,&sd)) {
        sd.sts = Error();
      } else {
        void *ep;
        sd.sts = cntxt.prsSdfTok(src_nm);
        pthread_join(sd.thread,&ep);
        if (!sd.sts && !(sd.sts_thrd)) goto done;
      }
      ErrorMsg(S_WARNING(sd.sts),"MT/Piped parse failed for %s",f.str());
#endif
    failed:
      sd.in.Open(fp,"r");
    }

    if (!sd.out.Open(fp = &tok,"pw|")) goto missing;

    if (!(sd.sts = tokSDF(&sd.in,&sd.out))) {
      if (!sd.out.Open(fp,FM(FM_RAW|FM_READ))) {
        ExitMsg(sd.sts = Error(),"can't re-open %s",fp->str());
      } else {
        sd.sts = cntxt.prsSdfTok(src_nm);
      }
    }

    tok.Unlink();

  done:
    sd.in.Close();
    sd.out.Close();
    return sd.sts;
  }

missing:
  return ExitMsg(Error(),"Can't open %s",fp->str());
}

extern "C" int prsSDF(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  const char *file = *(*argv)++;

  (*argc)--;

  InitLang(defPoolMode,0);

  return prsSDFld(file);
}

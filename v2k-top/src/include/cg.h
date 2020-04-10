/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  cg_h_rcsid
#define cg_h_rcsid() {return "$Id: cg.h,v 1.13 2009/07/08 08:34:54 dkc Exp $";} /* RCS ID */

#ifndef CG_H
#define CG_H

#include "cg_enums.h"

#define CG_FIELDS eCGT     cgt:8;\
                  char     init,\
                           vrsn;\
                  int      chksum;\
                  Inst     *ip;

class Inst;
class CgData;
class CgDataCln {
 public:
  CG_FIELDS
  CgData **of;

  inline CgDataCln(CgData **_of) { cgt = CGT_CLONE ; of = _of; init = 0; };
};

class CgDataCC;
class CgDataO;
class CgDataSO;
class CgData {
 public:

  CG_FIELDS

  inline CgDataCln *cln() {return (CgDataCln *)this;};
  inline CgDataCC  *cc()  {if (CGT_CLONE == cgt) return (*cln()->of)->cc();
                           ASSERT(CGT_CC == cgt);
                           return (CgDataCC *)this;};
  inline CgDataO   *o()   {if (CGT_CLONE == cgt) return (*cln()->of)->o();
                           ASSERT(CGT_O  == cgt);
                           return (CgDataO  *)this;};
  inline CgDataSO  *so()  {if (CGT_CLONE == cgt) return (*cln()->of)->so();
                           ASSERT(CGT_SO == cgt);
                           return (CgDataSO *)this;};
};

class CgDataCC : public CgData {
 public:
  static CgDataCC *Pend;

  eSTS      cc_sts;
  File      code;
  CgDataCC *next;

  inline CgDataCC(const char *cp,Inst *pi,int csum,int v) {
    ip     = pi;
    cgt    = CGT_CC;
    cc_sts = STS_ALLBAD;
    code   = cp;
    chksum = csum;
    vrsn   = v;
    next   = Pend;
    Pend   = this;
  };

  ~CgDataCC();

  int  Gather(eSTS *sts,File *mk_lib = 0);
  eSTS LoadObj();
};

inline int GatherCC(eSTS *sts) {
  return CgDataCC::Pend->Gather(sts);
}

class DynObj;
class _SimObj;
class CgDataO : public CgData {
 public:
  
  DynObj  *obj;
  _SimObj *md_cls;

  inline CgDataO(CgData *b,DynObj *o = 0) {BCOPY(b,this,sizeof(CgData));
                                           cgt    = CGT_O;
                                           md_cls = 0;
                                           obj    = o;}
  void *init(void *);

  ~CgDataO();
};

class CgDataSO : public CgDataO {
 public:
  char init;
  int  users;

  inline CgDataSO(CgData *b,DynObj *o = 0) : CgDataO(b,o) {cgt   = CGT_SO;
                                                           users = 0;};
};

typedef struct {
  int         epc;
  int         idx;
  const char *s;
} prcData;

typedef enum {
  CS_PRE,
  CS_DECL,
  CS_CODE,
  CS_CNTXT,
  CS_INIT,
  CS_LAST
} eCS;

typedef enum {
  CGX_ALL     = ~0,
  CGX_LHS     = 1,
  CGX_PLI_ARG = 2,
  CGX_PRT_ARG = 4,
  CGX_PRT_ASS = 8
} eCGX;

class cgData {
  prcData    *c_data;
  int         cntxts;
 public:
  eCGS        scp:8;
  eCG         sts:8;
  OPR         prnt_op;
  char        flags;
  Stream     *strm[CS_LAST];
  Module     *mod;
  Inst       *ip;
  const char *dbg_nl,
             *c_line_frmt;
  int         prcs,
              file_id,
              line,
              rpt,
              rpts,
              fr_dpth,
              cat_trgt,
              cntxt_ln;
  char        cntxt_nm[1024];

  inline  cgData() {
    c_data      = 0; 
    cntxts      = 0;
    scp         = CGS_NONE;
    sts         = CG_OK;
    prnt_op     = OPR_NULL;
    BZEROA(strm);
    mod         = 0;
    ip          = 0;
    dbg_nl      = 0;
    c_line_frmt = 0;
    prcs        = 0;
    file_id     = 0;
    line        = 0;
    rpt         = 0;
    rpts        = 0;
    fr_dpth     = 0;
    cat_trgt    = 0;
    cntxt_ln    = 0;
    BZEROA(cntxt_nm);
  };
  inline ~cgData() {FREE(c_data);};

  inline Stream *P() {return strm[CS_PRE];}
  inline Stream *D() {return strm[CS_DECL];}
  inline Stream *C() {return strm[CS_CODE];}
  inline Stream *X() {return strm[CS_CNTXT];}
  inline Stream *I() {return strm[CS_INIT];}

  inline void setFlag(int f) {flags |= f;}
  inline void clrFlag(int f) {flags &= ~f;}

  inline int flgs()     {return flags;}
  inline int lhs()      {return flags & CGX_LHS;}
  inline int pli_arg()  {return flags & CGX_PLI_ARG;}

  inline Stream *Replace(eCS si,Stream *stream) {
    Stream *was = strm[si];
    strm[si]    = stream;
    return was;
  }

  inline void Restore(eCS si,Stream *stream = 0) {
    strm[si] = stream;
  }

  int newCntxt(const char *,int idx = -1);

  inline int         Cntxt()      {return cntxts -1;};
  inline int         newSusp()    {return c_data[Cntxt()].epc++;};
  inline int         Susp()       {return c_data[Cntxt()].epc -1;};
  inline const char *str()        {return c_data[Cntxt()].s;};
  inline int         Susp(int c)  {return c_data[c].epc;};
  inline int         C_Idx(int c) {return c_data[c].idx;};
  inline const char *str(int c)   {return c_data[c].s;};

  inline void addCntxt(Stream *out,const char *pream = "",
                                   const char *post  =".",const char *alt = 0) {
    if (cntxt_ln) out->printf("%s%s%s",pream,cntxt_nm,post);
    else if (alt) out->printf("%s",alt);
  }

  void addCntxtC(Stream *,const char *pream = "",const char *post ="__",
                          const char *alt   = 0);

};

extern "C" eSTS LoadCode();

#define MAX_STR_TYPE 64
const char *cppType(typDesc *,char *,const char *msk_cls="%s");
const char *cppType(int,char *);
const char *EscpdCname(const char *);
const char *EscpdCname(poolRef ref);

#endif

/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */ 
#undef  pc_h_rcsid
#define pc_h_rcsid() {return "$Id: pc.h,v 1.71 2020/04/06 00:42:28 dkc Exp $";} /* RCS ID */

#include "list.h"
#include "strfunc.h"
#include "cppops.h"

#ifdef  PC_ANALYSIS_ONLY
#define PC_NA_BODY {assert(0);} 
#else
#define PC_NA_BODY 
#endif

#ifdef PC_NO_SYSTEMC
#define PC_SC_BODY {assert(0);} 
#else
#define PC_SC_BODY
#endif

typedef enum {
#define PC_OT_DECL(e,s,l,f) e,
#include "pc_o_typ.h"
PCO_TypMax
} ePcObjTyp;
extern const eFT         PcObjTypFT [PCO_TypMax+1];
extern const char *const PcObjTyp   [PCO_TypMax+1],
                  *const PcObjTypStr[PCO_TypMax+1];

typedef enum {
  PCX_NONE           =          0,
  PCX_CONST          =          1,
  PCX_EXTERN         =          2,
  PCX_C              =          4,
  PCX_CPP            =          8,
  PCX_TYPEDEF        =       0x10,
  PCX_VOLATILE       =       0x20,
  PCX_REGISTER       =       0x40,
  PCX_FIELD          =       0x80,
  PCX_DECL           =      0x100,
  PCX_AUTODECL       =      0x200,
  PCX_PUBLIC         =      0x400,
  PCX_PRIVATE        =      0x800,
  PCX_PROTECTED      =     0x1000,
  PCX_PMASK          =     PCX_PRIVATE|PCX_PUBLIC|PCX_PROTECTED,
  PCX_INLINE         =     0x2000,
  PCX_METHOD         =     0x4000,
  PCX_CNSTRCTR       =     0x8000,
  PCX_DESTRCTR       =    0x10000,
  PCX_STATIC         =    0x20000,
  PCX_VIRTUAL        =    0x40000,
  PCX_EXPLICIT       =    0x80000,
  PCX_TEMPLATE       =   0x100000,
  PCX_TMPLT_ARG      =   0x200000,
  PCX_T_INST         =   0x400000,
  PCX_M_INST         =   0x800000,
  PCX_OPERATOR       =  0x1000000,
  PCX_EXTENSION      =  0x2000000,
  PCX_TYPE_ONLY      =  0x4000000,
  PCX_RET_BASE_TYPE  =  0x8000000,
  PCX_FN_PTR         = 0x10000000,
  PCX_FN_CAST        = 0x20000000,
  PCX_CAST           = 0x40000000,
  PCX_UNAMBIGUOUS    = 0x80000000,

  PCX_FN_ATTR        = PCX_VIRTUAL|PCX_INLINE|PCX_STATIC|PCX_PMASK|PCX_FN_CAST
                                  |PCX_CNSTRCTR|PCX_DESTRCTR|PCX_FN_PTR

} ePcMod;
#define PCX(x) ((ePcMod)(x))

typedef enum {
  PCMD_NONE    =  0,
  PCMD_C       =  1,
  PCMD_CPP     =  2,
  PCMD_PAR     =  4,
  PCMD_I       =  8,
  PCMD_SYSC    = 16,

  PCMD_CI      = PCMD_I,
  PCMD_PC      = PCMD_CPP|PCMD_PAR,
  PCMD_PI      = PCMD_CPP|PCMD_PAR|PCMD_I,

  PCMD_UNKNOWN = -1
} ePCMD;

typedef enum{
  CPRS_Top          =        1, 
  CPRS_Block        =        2,
  CPRS_Type         =        3,
  CPRS_TypeOrBlock  =        4,
  CPRS_Decl         =        5,
  CPRS_Enum         =        8,
  CPRS_Stmt         =     0x10,
  CPRS_Class        =     0x20,
  CPRS_Union        =     0x40,
  CPRS_Struct       =     0x80,
  CPRS_Entity       =    0x100,
  CPRS_Architecture =    0x200,
  CPRS_Module       =  CPRS_Architecture|CPRS_Entity,
  CPRS_Obj          =  CPRS_Union|CPRS_Class|CPRS_Struct|CPRS_Module,
  CPRS_Args         =    0x400,
  CPRS_Friends      =    0x800,
  CPRS_Func         =   0x1000,
  CPRS_FuncArg      =   0x2000,
  CPRS_TypeExprOK   =   0x4000,
  CPRS_Expr         =   0x8000,
  CPRS_Cast         =  0x10000,
  CPRS_Procedural   =  0x20000,
  CPRS_Fork         =  0x40000,
  CPRS_Label        =  0x80000,
  CPRS_ModBody      = 0x100000,
  CPRS_ModTop       = CPRS_ModBody|CPRS_Module,
  CPRS_EntTop       = CPRS_ModBody|CPRS_Entity,
  CPRS_ArchTop      = CPRS_ModBody|CPRS_Architecture
} eCPRS;
#define CPRS(e) ((eCPRS)(e))

typedef enum{
  CSTMT_None       =    0,
  CSTMT_return     =    1,
  CSTMT_delete,
  CSTMT_del_arr,
  CSTMT_if,
  CSTMT_catch,
  CSTMT_for,
  CSTMT_fork,
  CSTMT_goto,
  CSTMT_case,
  CSTMT_break,
  CSTMT_cont,
  CSTMT_while,
  CSTMT_switch,
  CSTMT_process,
  CSTMT_At,
  CSTMT_Before,
  CSTMT_try,
  CSTMT_throw,
  CSTMT_Block,
  CSTMT_Label,
  CSTMT_Using,
  CSTMT_Type,
  CSTMT_TmplRef,
  CSTMT_Fn2Mod,
  CSTMT_ScCmnt,
  CSTMT_ScInst,
  CSTMT_ScProc,
  CSTMT_ScThread,
  CSTMT_Decl,
  CSTMT_Expr,
  CSTMT_Last
} eCSTMT;

typedef enum {
  AZR_OK       =   0,
  AZR_NONE     =   0,
  AZR_BUILTIN  =   1,
  AZR_INIT1    =   2,
  AZR_INST_M   =   4,
  AZR_SIGNAL   =   8,
  AZR_DRIVER   =  16,
  AZR_SIGREF   =  AZR_SIGNAL|AZR_DRIVER,
  AZR_PIPE     =  32,
  AZR_FN2MOD   =  64,
  AZR_RANGE    = 128,
  AZR_STTC_RNG = 256
} eAZR;
#define AZR(x) ((eAZR)(x))

typedef enum {
  AZC_NONE       =     0,
  AZC_DO_SCOPE   =     1,
  AZC_DECL       =     2,
  AZC_TYPEREF    =     4,
  AZC_BEFORE     =     8,
  AZC_SYSC       =  0x10,
  AZC_FN2MOD     =  0x20,
  AZC_BIND       =  0x40,
  AZC_FORK       =  0x80,
  AZC_IN_FN_T    = 0x100,
  AZC_IN_FN_E    = 0x200,
  AZC_IN_FN      = AZC_IN_FN_T|AZC_IN_FN_E,
  AZC_LHS        = 0x400

} eAZC;
#define AZC(x) ((eAZC)(x))

#ifdef __cplusplus
extern "C" {
#endif

int tokPC (FILE *,fwrite_fn,FILE *,ePCMD mode);
int prsCi(int *,const char ***,void *,int,int);
int prsPC(int *,const char ***,void *,int,int);
int prsPCi(int *,const char ***,void *,int,int);
int prsSysC(int *,const char ***,void *,int,int);
int prsPCextra(int *,const char ***,void *,int,int);
int pcEfile(int *,const char ***,void *,int,int);
int compilePC(int *,const char ***,void *,int,int);
int outputPC(int *,const char ***,void *,int,int);
int prsIncludeC(int *,const char ***,void *,int,int);
int prsDefineC(int *,const char ***,void *,int,int);

extern Filename PostProcessC;

#ifdef __cplusplus
}
#endif

typedef enum {
  PCT_NONE = 0,
  PCT_MODULE,
  PCT_ENTITY,
  PCT_ARCHITECTURE,
  PCT_PROCESS,
  PCT_STRUCT,
  PCT_CLASS,
  PCT_NAMESPACE,
  PCT_ENUM,
  PCT_QUAL_SCLR,
  PCT_SCALAR,
  PCT_UNION,
  PCT_TYPEDEF,
  PCT_TEMPLATE
} eCT;

typedef enum {
  WFLG_NONE        =          0,
  WFLG_MINIMAL     =          1,
  WFLG_ADD_CR      =          2,
  WFLG_NO_CLOSE    =          4,
  WFLG_NO_LOCATION =          8,
  WFLG_NO_TYPE     =       0x10,
  WFLG_TEMPLATE    =       0x20,
  WFLG_INST_T      =       0x40,
  WFLG_INST_M      =       0x80,
  WFLG_FRIEND      =      0x100,
  WFLG_FUNCTION    =      0x200,
  WFLG_PROC_STTC   =      0x400,
  WFLG_PROC_DYN    =      0x800,
  WFLG_PROC_ANY    =      WFLG_PROC_DYN|WFLG_PROC_STTC,
  WFLG_CLASS       =     0x1000,
  WFLG_INPROC      =     0x2000,
  WFLG_NO_PPP      =     0x4000,
  WFLG_CONSTRUCT   =    0x10000,
  WFLG_DESTRUCT    =    0x20000,
  WFLG_SYSC2PC     =    0x40000,
  WFLG_BODY_ONLY   =    0x80000,
  WFLG_ADD_SCOLON  =   0x100000,
  WFLG_COERCE      =   0x200000,
  WFLG_FORK0       =   0x400000,
  WFLG_FORK1       =   0x800000,
  WFLG_FORK        =      WFLG_FORK0|WFLG_FORK1,
  WFLG_NO_NL       =  0x1000000,
  WFLG_DISAMBIG    =  0x2000000,
  WFLG_DEBUG       = 0x80000000
} eWFLG;
#define WFLG(f) ((eWFLG)(f))

typedef enum {
  TYPX_NONE     = 0,
  TYPX_GEXT_PCX = 1,
  TYPX_GEXT     = 2,
  TYPX_NOEXCEPT = 4
} eTYPX;
#define TYPX(f) ((eTYPX)(f))
typedef enum {
  VIS_NONE    = 0, // not specified
  VIS_DEFAULT = 1
} eVIS;

typedef enum {
  TAG_NONE    = 0, // not specified
  TAG_ABI     = 1
} eTag;

typedef enum {
  INIT_NONE      =    0,
  INIT_CNSTRCT_0 =    1,
  INIT_CNSTRCT_1 =    2,
  INIT_CNSTRCT_2 =    3,
  INIT_CNSTRCT_3 =    4,
  INIT_CNSTRCT_L = INIT_CNSTRCT_3,
  INIT_DRX       =    8,
  INIT_KIDS_0    =    9,
  INIT_KIDS_1    =   10,
  INIT_INIT_C    =   11,
  INIT_INIT_0    =   12,
  INIT_INIT_1    =   13,

  INIT_DSTRCT    = 0x0F,
  INIT_PHASE     = 0x0F,
  INIT_ASSIGN    = 0x00,
  INIT_MTHD0     = 0x10,
  INIT_MTHD1     = 0x20,
  INIT_MTHDA     = 0x30,
  INIT_MTHD_MB   = 0x40,
  INIT_DLT       = 0x50,
  INIT_DLT_ARR   = 0x60,
  INIT_NULL      = 0x70,
  INIT_BIND      = 0x80,
  INIT_IF_NULL   = 0x90,
  INIT_DLT_DFLT  = 0xA0,
  INIT_CLL_INIT  = 0xB0,
  INIT_CONST     = 0xC0,
  INIT_VAL       = 0xD0,

  INIT_INT_0     = INIT_CNSTRCT_0|INIT_NULL,
  INIT_DRV_V     = INIT_INIT_0|INIT_VAL,
  INIT_DRV_A     = INIT_INIT_0|INIT_ASSIGN,
  INIT_DRV_0     = INIT_INIT_0|INIT_MTHD0,
  INIT_DRV_1     = INIT_INIT_0|INIT_MTHD1,
  INIT_DRV_2     = INIT_INIT_0|INIT_CLL_INIT,
  INIT_RX_0      = INIT_INIT_0|INIT_ASSIGN,
  INIT_RX_D      = INIT_DSTRCT|INIT_DLT,
  INIT_RX_DA     = INIT_DSTRCT|INIT_DLT_ARR,
  INIT_INST_0    = INIT_CNSTRCT_0|INIT_NULL,
  INIT_INST_D    = INIT_CNSTRCT_0|INIT_IF_NULL,
  INIT_INST_M    = INIT_KIDS_0|INIT_MTHDA,
  INIT_INST_B    = INIT_KIDS_1|INIT_BIND,
  INIT_CHILD     = INIT_CNSTRCT_3|INIT_MTHD0,
  INIT_PIPE_0    = INIT_CNSTRCT_0,
  INIT_PIPE_1    = INIT_CNSTRCT_1|INIT_MTHD1,
  INIT_PROC_0    = INIT_CNSTRCT_0,
  INIT_PROC_1    = INIT_INIT_0|INIT_MTHD0,
  INIT_PROC_2    = INIT_CNSTRCT_2,
  INIT_PROC_C    = INIT_INIT_C,
  INIT_INFNTCD   = INIT_DSTRCT|INIT_DLT,
  INIT_SIG_DLT   = INIT_DSTRCT|INIT_DLT_DFLT
} eINIT; 

#ifdef __cplusplus

class Location;
class CppContext;
class CppType;
class CppScope;
class CppTypeRef;
class CppTypeCls;

typedef  StringListItem CompilerOpt;

class CompilerOpts {
  simpleList<CompilerOpt> includes;
  simpleList<CompilerOpt> defines;
 public:
  void addIncDir(const char*);  
  void addDef(const char*);  

  inline const CompilerOpt *include0() {return includes.first();}
  inline const CompilerOpt *define0()  {return defines.first();}

  CompilerOpts();
};

class CppStream;
class Location {
  friend class CppContext;
  const Token *tok;
 public:
  poolRef      file_name;
  int          file_num[4];

  //file_num[0]
#define FNM_NOWHERE  0
#define FNM_RCYCLD  -2
#define FNM_PERM    -3

  inline Location() : file_name(NullRef), tok(0) {}

  inline Location &set(const Token *t) { tok = t;
                                         return *this;}

  inline const Token *Tok() const {return tok;}

  const Token *Write(Stream &,const Token *T0 = 0,eWFLG flgs = WFLG_NONE) const PC_NA_BODY;

  const char *describe(char *) const;
  const void  dump(Stream *)   const;
};

class OffsetLoc : public Location {
 public:
  int offset;
  const Token *lckd;
  const Token *pending;
  const Token *rescan;

  inline OffsetLoc() : offset(0), lckd(0), pending(0), rescan(0) {}
};

extern Location Nowhere;
extern Location Permanent;

class dump_ctrl;
class CppStrmData : public UsrStrmData {
 public:
  Location     loc;
  Location     dbg_loc;
  File        *cpp;
  const Token *curr;
  const char  *nl;
  dump_ctrl   *dmp;
  eWFLG        flgs;

  CppStrmData(File *out,const Token *T, dump_ctrl *pd = 0,eWFLG f = WFLG_NONE);

  void *UserFn1(Stream *,void *);
# define FLUSH_OUT(out,T) ((const Token *)out.UserFn1(const_cast<Token *>(T)))

  void *UserFn2(Stream *,void *);
  static const char *CsdNL(Stream &out,const Location *loc = 0);

  //# define NL CppStrmData::CsdNL
  //# define NL_SYNC NL

# define NL(cs)           ((const char *)cs.UserFn2(0))
# define NL_DBG(cs)       (dbg ? "" : (const char *)cs.UserFn2(0))
# define NL_SYNC(cs,loc)  ((const char *)cs.UserFn2(const_cast<Location *>(loc)))

};


class CppIndex;
class CppStmtType;
class CppTypeV {
 public:
  poolRef   name;  // Keep order
  CppTypeV *next;  //
  Location  source;
  CppTypeV *ovrld; // ring

  virtual       CppTypeRef *cRef()                             {return 0;}
  virtual const CppTypeRef *cRef()                       const {return 0;}
  virtual       CppType    *cTyp()                             {return 0;}
  virtual const CppType    *cTyp()                       const {return 0;}
  virtual       CppTypeCls *cTypCls(int fllw = 0,
                                        int *indrct = 0)       {return 0;}
  virtual const CppTypeCls *cTypCls(int fllw = 0,
                                        int *indrct = 0) const {return 0;}
  virtual       int         isA(int,int)                 const {return 0;}
  virtual eCT               getCT()                      const {return PCT_NONE;};
  virtual int               isTmplt()                    const {return 0;};
  virtual int               isTypNm()                    const {return 0;}
  virtual int               isTmpltArg()                 const {return 0;};
  virtual int               isScope()                    const {return 0;};
  virtual int               isNmSpc()                    const {return 0;};
  virtual int               isDecl()                     const {return 0;};
  virtual int               isAddr()                     const {return 0;};
  virtual int               isInst()                     const {return 0;};
  virtual int               isModMthd()                  const {return 0;};
  virtual int               isAnon()                     const {return 0;};
  virtual int               isFunc()                     const {return 0;};
  virtual int               isUnres()                    const {return 0;};
  virtual int               isAny(int clr = 0)           const {return 0;};
  virtual int               setFunc(int f)               const {return 0;};
  virtual int               isStrctr()                   const {return 0;};
  virtual int               funcClsd(int cls = 0)              {assert(0);
                                                                return -1;};
  virtual int               closeFunc()                        {assert(0);
                                                                return 0;}
  virtual int               isTypdf()                    const {return 0;};
  virtual int               isRgstrd()                   const {return -1;};
  virtual int               isDclrd()                          {return 0;};
  virtual void              setTypdf()                         {};
  virtual int               hasBod()                     const {return -1;};
  virtual const char       *describe(char *bf = 0)       const {return strDeref(name);}
  virtual const void        dump(Stream *)               const {};
  virtual const Location   *Where()                      const {return 0;}
  virtual const Token      *Write(Stream&, const Token*,eWFLG f = WFLG_NONE,
                                const char **dcl_nm = 0) const {return 0;}
  virtual void              WriteAliases(Stream &,const char *) 
                                                         const {}
  virtual int               WriteInit0(Stream &,const char *,
                                                const char *punc = " : ")
                                                         const {};
  virtual void              WriteInitX(Stream &,const char *,
                                       eINIT phase = INIT_CNSTRCT_1,
                                       eWFLG flgs  = WFLG_NONE)
                                                         const {};
  virtual       CppScope   *Scope(int fllw = 0)                {return 0;}
  virtual const CppScope   *Scope(int fllw = 0)          const {return 0;}
  virtual const CppTypeV   *findEntity()                 const {return 0;}
  virtual const CppTypeV   *Inherits()                   const {return 0;}
  virtual const CppTypeV  **Friends()                    const {return 0;}
  virtual       CppTypeV  **Inherits0()                        {return 0;}
  virtual int               isBuiltin()                  const {return 0;}
  virtual int               indices()                    const {return 0;}
  virtual int               copyIndices(CppIndex **)     const {return 0;}

  virtual int               isTypename(poolRef *ret)     const {return 0;}

  virtual void              set_bod(CppScope *scp_p=0,CppScope *scp_u=0,
                                    CppStmtType *stmt = 0)     {assert(0);}
  virtual void              set_rgstrd(int r = 1)              {assert(0);}
  virtual void              set_dclrd()                        {assert(0);}
  virtual poolRef           Name()                       const {return name;}

  virtual int              maybeCnstrctr(const CppTypeV *,
                                    const CppTypeRef *ref = 0,const CppScope **ret = 0) const PC_NA_BODY;

  virtual eAZR             Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
                                    int level = 0,eAZC flgs = AZC_NONE) = 0;

  void addOverload(CppTypeV *) PC_NA_BODY;

  CppTypeV(poolRef nm = NullRef)           : name(nm), next(0), source(Nowhere), ovrld(0) { }
  CppTypeV(poolRef nm,const Location *src) : name(nm), next(0), source(*src),    ovrld(0) { }

  void recycle();

  virtual CppTypeV **recycleList() {return 0;}

};

class CppTypeX : public CppTypeV {
 public:
  CppTypeX(poolRef nm) : CppTypeV(nm) { }
  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
                          int level = 0,eAZC flgs = AZC_NONE) { assert(0); }
};

typedef enum {
  XPRF_NONE     =      0,
  XPRF_COMMON   =      1,
  XPRF_RPLCMNT  =      2,
  XPRF_ANALYZED =      4,
  XPRF_SUB_L    =      8,
  XPRF_SUB_R    =   0x10,
  XPRF_SUB_BOTH = XPRF_SUB_L|XPRF_SUB_R,
  XPRF_RCYCLD   = 0x4000,
  XPRF_PERM     = 0x8000,
  XPRF_LEAVE    = 0xF000,

  // CXPR_ATTR
  XPRF_ASM      = 1,
  XPRF_ASM_     = 2

} eXPRF;

#define XPRF(f) ((eXPRF)(f))

struct DupContext {
 public:
  CppScope *scope;
  int      *depth;

  inline DupContext(CppScope *scp,int *pd) : scope(scp), depth(pd) {}
};

class CppDecl;
class CppExpr {
 public:
  union {
    I64        i64;
    U64        u64;
    poolRef    ref;
    double     d;
    CppTypeV  *typ;
    struct {
      CppExpr *l,
              *r;
    } hs;
    struct {
      const Token *l,
                  *r;
    } tk;
    CppDecl   *dcl;
    CppScope  *scp;
  } x;
  eCXPR    opr:16;
  eXPRF    flgs:16;
 
  inline void init(eCXPR o) { opr    = o;
                              flgs   = XPRF_NONE;
                              x.hs.l = x.hs.r = 0; }
  
 public:
  CppExpr(poolRef);
  CppExpr(poolRef,poolRef);
  CppExpr(CppDecl *);
  CppExpr(const CppExpr *,DupContext *cntxt = 0);
  CppExpr(const Token *T,const Token *TL);
  CppExpr(eCXPR,CppExpr *l,CppExpr *r = 0);
  CppExpr(eCXPR,I32);
  CppExpr(eCXPR,U32);
  CppExpr(eCXPR,I64);
  CppExpr(eCXPR,U64);
  CppExpr(eCXPR,double);
  CppExpr(eCXPR,poolRef);
  CppExpr(eCOPR);
  CppExpr(CppTypeV *,int nm_only = 0);

  ~CppExpr();

  I32 x_i32() const;
  U32 x_u32() const;

  const Token *Write(Stream &,const Token *T0 = 0,eWFLG f = WFLG_NONE) const PC_NA_BODY;
  const char  *describe(char *bf = 0)                                  const PC_NA_BODY;
  const void   dump(Stream *)                                          const PC_NA_BODY;
  const Token *WritePortBind(Stream &,const Token *,eWFLG,const CppExpr *) const;

  void MarkBind();

  eAZR Analyze    (CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
                   int level = 0,eAZC flgs = AZC_NONE);
  eAZR fixSigValue(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
                   int level = 0,eAZC flgs = AZC_NONE);

  static eAZR (*AnalysisFn)(CppExpr *,CppType *,CppScope *,const Location *,int,eAZC);

  int sameAs(CppExpr *);
  int eval(const CppExpr *);

  inline void setCmmn()     {flgs = XPRF(flgs|XPRF_COMMON);}
  inline void setPerm()     {flgs = XPRF(flgs|XPRF_COMMON|XPRF_PERM);}
  inline void setRplcd()    {flgs = XPRF(flgs|XPRF_RPLCMNT);}
  inline void setAnalyzed() {assert(!(flgs & XPRF_ANALYZED));
                             flgs = XPRF(flgs|XPRF_ANALYZED);}

  inline bool    isRef(poolRef Ref);
  inline eCXPR   getRef(poolRef *Ref);
  inline poolRef getRef();
  inline void    setRef(poolRef Ref) {opr = CXPR_REF; x.ref = Ref;}

  CppExpr *addAfter(CppExpr *);
  CppExpr *addAtEnd(CppExpr *);
  CppExpr *sysc2pc();
  CppExpr *duplicate(DupContext *cntxt = 0);

  bool isAddr() const;

  static void InitCommon();
  static CppExpr *ListAdd(CppExpr **,CppExpr *);

  static CppExpr *VoidP0;
  static CppExpr *Int0;
  static CppExpr *Int1;
  static CppExpr *RefMB;

  void recycle();
  static CppExpr *RecycleList;
};

class CppIndex {
 public:
  CppIndex *next;
  CppExpr  *expr;     

  inline CppIndex(CppIndex *nxt,CppExpr *xpr = 0) : next(nxt), expr(xpr) {}
  inline CppIndex() : next(0), expr(0) {}

  CppIndex *copy() {return this;}
};

typedef enum {
  STMTF_NONE     = 0,
  STMTF_ANALYZED = 1
} eSTMTF;
#define STMTF(f) ((eSTMTF)(f))

class CppStmt {
 public:
  CppStmt  *next;
  Location  source;
  eCSTMT    typ:16;
  eSTMTF    flgs:16;

  inline CppStmt(const Location *_source,eCSTMT _typ) 
    : next(0),typ(_typ),flgs(STMTF_NONE) { if (_source) source = *_source; }

  inline CppStmt(const CppStmt *stmt) 
    : next(0),typ(stmt->typ),flgs(stmt->flgs), source(stmt->source) {}

  inline const Location *Where() {return this ? &source
                                              : 0;}
  inline void setAnalyzed() {assert(!(flgs & STMTF_ANALYZED));
                             flgs = STMTF(flgs|STMTF_ANALYZED);}

  CppStmt *addStmt(CppStmt *,CppScope *,int posn = 1);

  virtual CppStmt *duplicate(DupContext *cntxt = 0) {assert(0); throw "Can't duplicate";}

  virtual const Token *Write(Stream&, const Token*,eWFLG) const PC_NA_BODY;

  virtual eAZR fixSystemC(CppScope *a_scp,const Location *where,int level,eAZC flgs) PC_SC_BODY;
  virtual eAZR Analyze   (CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
                          int level = 0,eAZC flgs = AZC_NONE);

  virtual void fixScWait (CppExpr *,CppStmt **) {};

  virtual ~CppStmt() { if (next) delete next; }
};

class CppStmtTmplRef : public CppStmt {
 public:
  CppTypeV *tmpl[2];
  ePcMod    xi;

  inline CppStmtTmplRef(CppTypeV **typ,const Location *where,ePcMod flgs = PCX_NONE) 
    : CppStmt(where,CSTMT_TmplRef), xi(flgs) 
  { tmpl[0] = typ[0];
    tmpl[1] = typ[1]; }

  const Token *Write(Stream&, const Token*,eWFLG) const PC_NA_BODY;
};

class CppStmtType : public CppStmt {
 public:
  CppTypeV *tv;
  int       fwrd;

  inline CppStmtType(CppTypeV *_tv,const Location *where,int fw = 0) 
    : CppStmt(where,CSTMT_Type), tv(_tv), fwrd(fw) 
  {
    tv->set_dclrd();}

  const Token *Write(Stream&, const Token*,eWFLG) const PC_NA_BODY;

  eAZR fixSystemC(CppScope *a_scp,const Location *where,int level,eAZC flgs) PC_SC_BODY;
  eAZR Analyze   (CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
                  int level = 0,eAZC flgs = AZC_NONE);
};

class CppStmtExpr : public CppStmt {
 public:
  CppExpr *expr;

  inline CppStmtExpr(const Location *_source,eCSTMT _typ,CppExpr *_expr = 0) 
    : CppStmt(_source,_typ), expr(_expr) {}

  CppStmtExpr(const CppStmtExpr *);

  const Token *Write(Stream&, const Token*,eWFLG) const PC_NA_BODY;

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE);

  void fixScWait(CppExpr *,CppStmt **) PC_SC_BODY;

  CppStmt *duplicate(DupContext *cntxt = 0);
};

class CppStmtLabel : public CppStmt {
 public:
  poolRef name;

  inline CppStmtLabel(const Location *_source,eCSTMT _typ,poolRef _name)
    : CppStmt(_source,_typ), name(_name) {}

  const Token *Write(Stream&, const Token*,eWFLG) const PC_NA_BODY;

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE);
};

class CppDecl;
class CppInit {
 public:
  CppDecl  *decl;
  CppTypeV *super;
  CppExpr  *expr;

  inline CppInit() {decl = 0; super = 0; expr = 0;}

  inline eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
                      int level = 0,eAZC flgs = AZC_NONE) {
                                        return expr->Analyze(proc,scp);}
};

class CppInitList {
 public:
  int      count;
  CppInit *list;

  inline CppInitList(int c = 0) : count(c) { list = c > 0 ? new CppInit [c+1]
			                                  : 0; }

  CppInit *Add(CppDecl *,CppTypeV *,CppExpr *,int idx = -1) PC_NA_BODY;
};

class CppTypeRef : public CppTypeV {

 public:
  const CppTypeRef *sub;
  const CppTypeV   *typ;
  CppIndex         *index;
  CppExpr          *attr;
  typedef union {
    struct {
      unsigned int  rgstrd:2,
	            dclrd:1,
                    perm:1,
	            dfnd:1,
	            tmplt:1,
	            tmplt_arg:1,
	            frnd_arg:1,
	            inst:1,
	            inst_ref:1,
	            ambig:1,
	            unres:1,
	            typdf:1,
	            typnm:1,
 	            cnstrctr:1,
	            destrctr:1,
	            nmspc:1,
	            scp:1,
	            any:1,
	            ext:1,
	            bod:1,
                    ref:1,
                    lng:1,
                    vol:1,
                    reg:1,
	            mod:1,
                    mut:1,
                    prvt:1,
                    prot:1,
	            pblc:1,
                    vrtl:1,
                    pure:1,
                    inln:1,
                    xplct:1,
                    sttc:1,
                    cnst_src:2,
                    cnst_trg:2,
                    gnu_cnst_src:2,
                    gnu_cnst_trg:2,
                    gnu_rstrct:1,
                    gnu_xn:1,
                    anon:2,
                    stub:1,
#define XF_FN_OPEN 1
#define XF_FN_CLSD 2
#define XF_FN      3
#define XF_FN_PTR  4
#define XF_FN_ALL  7
#define XF_FN_FWD  8
                    func:3,
                    cast:1,
                    typof:2,
                    dcl:1,
                    dflt:1,
                    op:1,
                    ddd:1,
                    thrw:1,
	            width:1,
	            sgnd:2, // < 0 => unsigned
                    rewrite:1,
                    nox:1;
      char          indrct;
    } f;
    U64             as_chrs[1];
  } flags;
  flags             x;
  CppDecl          *args;
  CppInitList      *inits;
  CppScope         *scp;

  CppTypeRef       *cRef()             {return this;}
  const CppTypeRef *cRef()       const {return this;}
  eCT               getCT()      const {return typ ? typ->getCT()
                                                   : PCT_NONE;};
  int               isTmplt()    const {if (sub) { return sub->isTmplt(); }
                                        int t = x.f.tmplt;
                                        if (0 == t && typ) { t = typ->isTmplt(); }
                                        return t;}
  int               indices()    const {int i = 0; CppIndex *scan = index;
                                        while (scan) {i++; scan=scan->next;}
					return i;}
      
  CppTypeCls       *cTypCls(int fllw = 0,int *indrct = 0) {
                      if (!fllw) return 0;
		      if (indrct) { *indrct |= (x.f.ref | x.f.indrct); }
                      return sub ? const_cast<CppTypeRef *>(sub)->cTypCls(fllw,indrct)
		     	         : typ ? const_cast<CppTypeV *>(typ)->cTypCls(fllw,indrct)
			               : 0;}
  const CppTypeCls *cTypCls(int fllw = 0,int *indrct = 0) const {
                      if (!fllw) return 0;
		      *indrct |= (x.f.ref | x.f.indrct);
                      return sub ? sub->cTypCls(fllw,indrct)
		                 : typ ? typ->cTypCls(fllw,indrct)
			               : 0;}

  int               isTypNm()    const {return x.f.typnm;};
  int               isScope()    const {return x.f.scp;};
  int               isInst()     const {return x.f.inst;};
  int               isNmSpc()    const {return x.f.nmspc;};
  int               isDecl()     const {if (sub) return sub->isDecl();
                                        return x.f.dcl;};
  int               isAny(int clr =0)
                                 const {int r = x.f.any || (sub && sub->isAny());
                                        if (clr) const_cast<CppTypeRef *>(this)->x.f.any = 0;
                                        return r;}
  int               setFunc(int f)     {if (typ && typ->isFunc()) 
					  const_cast<CppTypeV *>(typ)->setFunc(f);
					if (sub && sub->isFunc()) 
					  const_cast<CppTypeRef *>(sub)->setFunc(f);
                                        return x.f.func = f;}
  int               funcClsd(int cls = 0)
                                       {int f = isFunc(),
					    r = (XF_FN_CLSD|XF_FN_FWD) & f; 
					if (f && cls && !r) { closeFunc(); }
                                        return r;}
  int               hasBod()     const {return x.f.bod;};
  int               isModMthd()  const {return x.f.mod;};
  int               isStrctr()   const {if (x.f.cnstrctr) return  1;
                                        if (x.f.destrctr) return -1;
                                        if (x.f.func ||
                                            x.f.typdf)    return  0;
                                        return typ ? typ->isStrctr()
					           : 0;}
  int               isAnon()     const {return x.f.anon;};
  int               isUnres()    const {return x.f.unres || (sub && sub->isUnres());};
  int               isTypdf()    const {return x.f.typdf;};
  void              setTypdf()         {x.f.typdf = 1;};
  int               isRgstrd()   const {return x.f.rgstrd ||
                                               (sub && sub->isRgstrd());}
  int               isDclrd()          {return x.f.dclrd;}
  bool              onlyInst()   const {flags  y;
                                        memset(y.as_chrs,0,sizeof(flags));
                                        y.f.inst = 1;
					return memcmp(y.as_chrs,x.as_chrs,sizeof(flags));}
  bool              onlyRgstrd() const {flags   y;
                                        memset(y.as_chrs,0,sizeof(flags));
                                        y.f.rgstrd = 1;
                                        return memcmp(y.as_chrs,x.as_chrs,sizeof(flags));}

  void               regType(CppScope *prnt,CppScope *use_scp) PC_NA_BODY;
  void               init(const Location *,const Token *T = 0,
                          const CppTypeV *t = 0)               PC_NA_BODY;
  const Token       *Write(Stream&, const Token *,eWFLG f = WFLG_NONE,
                           const char **dcl_nm = 0)      const PC_NA_BODY;
  void               WriteAliases(Stream &,const char *) const PC_NA_BODY;

  static CppTypeRef *create(const Location *,const Token *,
                            const CppTypeV *,int indirect = 0,
                            int *cnst = 0,int *sgnd = 0,eTYPX *xtra = 0) PC_NA_BODY;

  inline void fix_sc(int *cnst,int *sgnd,eTYPX *xtra) {
    if (sgnd && *sgnd) { if (*sgnd > 0) set_sgnd();
                         else           set_unsgnd();     *sgnd = 0;}
    if (cnst && *cnst) { int c = *cnst;
                         if (c > 0) { if (1 & c) set_cnst_s();
                                      if (2 & c) set_gnu_cnst_t();  *cnst = 0;}
                         else       { c = -c;
                                      if (1 & c) set_cnst_s(2);
                                      if (2 & c) set_gnu_cnst_t(2); *cnst = 0;}
                         }
    if (xtra && *xtra) { if (TYPX_GEXT & *xtra) set_gnu_xn();
                         *xtra = TYPX_NONE;}
  }

  inline CppInit *addInit(CppDecl *dcl,CppExpr *xpr,int idx = -1) {
    if (!inits) inits = new CppInitList;
    inits->Add(dcl,0,xpr,idx);
  }

  inline const CppTypeRef *Sub() const { if (sub) return sub->Sub();
                                         return this; }

  CppTypeRef(const Location *,const Token *,const CppTypeV *) PC_NA_BODY;
  CppTypeRef(const Location *,const Token *,const CppDecl  *) PC_NA_BODY;
  CppTypeRef(const Location *,poolRef)                        PC_NA_BODY;
  CppTypeRef(const Location *,CppScope *)                     PC_NA_BODY;
  CppTypeRef(const Location *,CppExpr  *)                     PC_NA_BODY;

  void set_rgstrd(int r = 1)  {x.f.rgstrd = r;}
  void set_dclrd()            {x.f.dclrd  = 1;}

  void set_bod(CppScope *p=0,CppScope *u=0,CppStmtType *stmt = 0) PC_NA_BODY;

  inline void set_mut()                  {x.f.mut          = 1;}
  inline void set_pblc()                 {x.f.pblc         = 1;}
  inline void set_prot()                 {x.f.prot         = 1;}
  inline void set_prvt()                 {x.f.prvt         = 1;}
  inline void set_vrtl()                 {x.f.vrtl         = 1;}
  inline void set_anon(int a = 1)        {x.f.anon         = 1;}
  inline void set_stub()                 {x.f.stub         = 1;}
  inline void set_pure()                 {x.f.pure         = 1;}
  inline void set_inln()                 {x.f.inln         = 1;}
  inline void set_sttc()                 {x.f.sttc         = 1;}
  inline void set_cast()                 {x.f.cast         = 1;}
  inline void set_cnstrctr()             {x.f.cnstrctr     = 1;}
  inline void set_destrctr()             {x.f.destrctr     = 1;}
  inline void set_ref()                  {x.f.ref          = 1;}
  inline void set_ext()                  {x.f.ext          = 1;}
  inline void set_xplct()                {x.f.xplct        = 1;}
  inline void set_lng()                  {x.f.lng          = 1;}
  inline void set_tmplt()                {x.f.tmplt        = 1;}
  inline void set_tmplt_arg()            {x.f.tmplt_arg    = 1;}
  inline void set_frnd_arg()             {x.f.frnd_arg     = 1;}
  inline void set_inst(CppDecl *a=0)     {x.f.inst         = 1; args = a;}
  inline void set_ambig()                {x.f.ambig        = 1;}
  inline void set_unres()                {x.f.unres        = 1;}
  inline void set_typdf()                {x.f.typdf        = 1;}
  inline void set_typof(bool __=false)   {x.f.typof        = 1 + __;}
  inline void set_typnm(poolRef nm)      {x.f.typnm        = 1; name = nm;}
  inline void set_reg()                  {x.f.reg          = 1;}
  inline void set_mod()                  {x.f.mod          = 1;}
  inline void set_vol()                  {x.f.vol          = 1;}
  inline void set_cnst_s(int c = 1)      {x.f.cnst_src     = c;}
  inline void set_cnst_t(int c = 1)      {x.f.cnst_trg     = c;}
  inline void set_gnu_cnst_s(int c = 1)  {x.f.gnu_cnst_src = c;}
  inline void set_gnu_cnst_t(int c = 1)  {x.f.gnu_cnst_trg = c;}
  inline void set_gnu_rstrct()           {x.f.gnu_rstrct   = 1;}
  inline void set_gnu_xn()               {x.f.gnu_xn       = 1;}
  inline void set_noexcept()             {x.f.nox          = 1;}
  inline void set_dcl(CppDecl *dcl = 0)  {if (dcl) { assert(!args);
					             args = dcl; }
                                          x.f.dcl          = 1;}
  inline void set_func(int f=XF_FN_OPEN) {x.f.func         = f;}
  inline void set_func(poolRef nm)       {assert(!args); 
                                          set_func(); name = nm;}
  inline void set_ddd()                  {x.f.ddd          = 1;}
  inline void set_rewrite()              {x.f.rewrite      = 1;}
  inline void set_width(CppExpr *t)      {x.f.width        = 1; 
                                                 assert(!attr) ; attr = t;}
  inline void set_throw(CppExpr *t)      {x.f.thrw         = 1; 
                                                 assert(!attr) ; attr = t;}
  inline void set_dflt(CppExpr *t)       {x.f.dflt         = 1; 
                                                 assert(!attr) ; attr = t;}
  inline void set_sgnd()                 {x.f.sgnd         = 1;}
  inline void set_unsgnd()               {x.f.sgnd         = -1;}
  inline void set_any()                  {x.f.any          = 1;}
  inline void set_scp(CppScope *s,int n = 0)
                                         {assert(scp == s || !scp);
                                          scp              = s;
                                          x.f.scp          = n;}
  inline void set_nmspc(CppScope *s = 0) {if (s) set_scp(s);
                                          x.f.nmspc        = 1;}
  const CppTypeV *set_typ(CppScope *scope,CppTypeV *t) {
                                          assert(0 == typ && t);
                                          return typ = t; }

  const Location *Where()          const {return &source;}

  poolRef         Name()           const {if (NULL_REF(name) && typ) return typ->Name();
                                          return name;}

  int             closeFunc(eTYPX *xtra = NULL) PC_NA_BODY;

  int             isBuiltin()      const PC_NA_BODY;
  int             isA(int,int)     const PC_NA_BODY;
  int             isAddr()         const PC_NA_BODY;
  int             isFunc()         const PC_NA_BODY;

  int isTypename(poolRef *ret) const { if (x.f.typnm) {*ret = name; return 1;}
                                       return 0;}

  static CppTypeRef *RListTypeRef;
  CppTypeV      **recycleList()             PC_NA_BODY;
  CppScope       *Scope(int fllw = 0)       PC_NA_BODY;
  const CppScope *Scope(int fllw = 0) const {
                              return (const_cast<CppTypeRef *>(this))->Scope(fllw);}

  const char *describe(char *bf = 0)  const PC_NA_BODY;
  const void  dump(Stream *)          const PC_NA_BODY;

  int copyIndices(CppIndex **)        const PC_NA_BODY;

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE);

  static eAZR (*AnalysisFn)(CppTypeRef *,CppType *,CppScope *,const Location *,int,eAZC); 

  static CppTypeRef  Ellipsis;
  static CppTypeRef  VaList;

 private:
  CppTypeRef() { init(&Nowhere);
                 x.f.perm = 1; }
};

class CppDecl {
 public:
  poolRef      name; // Keep order
  CppDecl     *next; //
  CppTypeV    *typ;
  int          prvt:1;
  int          pblc:1;
  int          prot:1;
  int          dot:1;
  int          is_arg:1;
  int          is_alias:1;
  unsigned int assgnd:2;
#define ASSGN_EQ   0x1
#define ASSGN_ARR  0x2
#define ASSGN_DFLT 0x4
  int          vals:4;
  eINIT        init0:8;
  eINIT        init1:8;
  eINIT        init2:8;
  eINIT        init3:8;
  CppExpr     *val[1];  // Keep last
  
  inline void setPPP(ePcMod ppp) {assgnd = 1; dot = is_arg = is_alias = 0;
                                  if (ppp && PCX_PUBLIC)    pblc =1;
                                  if (ppp && PCX_PRIVATE)   prvt =1;
                                  if (ppp && PCX_PROTECTED) prot =1;}

  CppDecl(poolRef nm,ePcMod ppp = PCX_NONE) : name(nm) {setPPP(ppp);}
  CppDecl(CppTypeV *t,poolRef nm,CppExpr *vl = 0,ePcMod ppp = PCX_NONE,eINIT i0 = INIT_NONE) 
    : name(nm), next(0), typ(t), 
      init0(i0), init1(INIT_NONE), init2(INIT_NONE)
    {assert(t || (ppp & PCX_T_INST)); setPPP(ppp); vals = 0; val[0] = vl;}

  inline void setAssgnd(int a) {assgnd = a;}
  inline void setIsArg()       {is_arg = 1;}

  inline CppExpr *setVal(int i,CppExpr* xpr)  { assert(i <= vals);
                                                return val[i] = xpr; }
  inline CppExpr *Val(int i = 0)  const {return val[i <= vals ? i
						              : vals];}
  inline eINIT    Init(int i = 0) const {return 0 == i ? init0
                                                       : 1 == i ? init1
						                : 2 == i ? init2
                                                                         : init3;}
  int sameThing(CppDecl *);

  const char  *describe(char *bf = 0) const;
  const void   dump(Stream *)         const;
  const Token *Write(Stream &out,const Token *,
                     eWFLG flgs = WFLG_NONE,const char *sep = ",") const PC_NA_BODY;

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE);
  void addBind(CppScope *,CppExpr *);

  inline const Location *Where() {return typ ? typ->Where()
                                             : 0;}

  inline void recycle() {delete this;}
};

inline bool CppExpr::isRef(poolRef Ref)
{ 
  return ((opr == CXPR_REF     && SAME_REF(x.ref,Ref)) 
       || (opr == CXPR_REF_DCL && SAME_REF(x.dcl->name,Ref)));
}

inline eCXPR CppExpr::getRef(poolRef *Ref)
{ 
  switch (opr) {
  case CXPR_REF:       *Ref = x.ref;       return opr; 
  case CXPR_REF_DCL:   *Ref = x.dcl->name; return opr; 
  case CXPR_TYPE_NAME: *Ref = x.typ->name; return opr; 
  }
  return (eCXPR)0;
}

inline poolRef CppExpr::getRef()
{ 
  switch (opr) {
  case CXPR_REF:     return x.ref;
  case CXPR_REF_DCL: return x.dcl->name;
  }
  return NullRef;
}

class CppDeclAlias : public CppDecl {
 public:
  CppDecl *alias;
  CppDeclAlias(CppDecl *to) :
    CppDecl(to->typ,to->name), alias(to) {assert(!to->is_alias);
                                          is_arg   = to->is_arg;
                                          is_alias = 1;}
};

class CppDecl2 : public CppDecl {
 public:
  CppExpr  *val1;
  CppDecl2(CppTypeV *t,poolRef nm,CppExpr *vl1,CppExpr *vl2,ePcMod ppp = PCX_NONE) :
    CppDecl(t,nm,vl1,ppp) {val[++vals] = vl2;}
};

class CppDecl3 : public CppDecl2 {
 public:
  CppExpr  *val1;
  CppDecl3(CppTypeV *t,poolRef nm,CppExpr *vl1,CppExpr *vl2,CppExpr *vl3,ePcMod ppp = PCX_NONE) :
    CppDecl2(t,nm,vl1,vl2,ppp) {val[++vals] = vl3;}
};

class CppStmtDecl : public CppStmt {
 public:
  CppDecl *dcl;

  inline CppStmtDecl(CppDecl *_dcl) 
    : CppStmt(_dcl->Where(),CSTMT_Decl), dcl(_dcl) {}

  const Token *Write(Stream &out,const Token *,
                     eWFLG flgs,const char *sep = 0) const PC_NA_BODY;
  const Token *Write(Stream &out,const Token *T,eWFLG flgs) const {
    return Write(out,T,flgs,";");
  }
  eAZR fixSystemC(CppScope *a_scp,const Location *where,int level,eAZC flgs) PC_SC_BODY;
  eAZR Analyze   (CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
                  int level = 0,eAZC flgs = AZC_NONE);
};

class CppType : public CppTypeV {
 public:
  eCT        typ;
  typedef union {
    struct {
      unsigned int  rgstrd:2,
	            dclrd:1,
	            fwd:1,
                    bod:1,
                    tmplt:1,
                    tmplt_arg:1,
 	            typdf:1;
    } f;
    int             as_int;
  } flags;
  flags             x;
  CppType    *sub;
  CppTypeRef *refs;

  int             isA(int,int) const PC_NA_BODY;
  CppType        *cTyp()             {return this;}
  const CppType  *cTyp()       const {return this;}
  eCT             getCT()      const {return typ;}
  const Location *Where()      const {return &source;}
  int             isScope()    const {return 1;}

  void set_bod(CppScope *p=0,CppScope *u = 0,
               CppStmtType *stmt = 0) {x.f.bod       = 1;}
# define REG_CNTXT 1
# define REG_OVRLD 2
  void set_rgstrd(int r = REG_CNTXT)  {x.f.rgstrd    = r;}
  void set_dclrd()                    {x.f.dclrd     = 1;}
  void set_fwd()                      {x.f.fwd       = 1;}

  inline void set_tmplt()             {x.f.tmplt     = 1;}
  inline void set_tmplt_arg()         {x.f.tmplt_arg = 1;}
  inline void set_typdf()             {x.f.typdf     = 1;}

  CppType(const Location *_src,eCT _typ,poolRef nm = NullRef,int xtra = 0)
    : CppTypeV(nm,_src), typ(_typ), sub(0), refs(0)
  {
    x.as_int = 0;
  }

  virtual ~CppType() {}

  const char  *describe(char *bf = 0)       const PC_NA_BODY;
  const void   dump(Stream *)               const PC_NA_BODY;
  const Token *Write(Stream&, const Token*,eWFLG f = WFLG_NONE,
                   const char **dcl_nm = 0) const PC_NA_BODY;
  int          isTmplt()                    const {return x.f.tmplt;};
  int          isTmpltArg()                 const {return x.f.tmplt_arg;};
  int          isTypdf()                    const {return x.f.typdf;};
  void         setTypdf()                         {x.f.typdf = 1;};
  int          hasBod()                     const {return x.f.bod;};
  int          isRgstrd()                   const {return x.f.rgstrd;}

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE);
  eAZR fixEvtRef(CppExpr **,CppScope *scp,int level,eAZC flgs,
                 const Location *where,CppStmt *stmt = 0) PC_NA_BODY;

  int isBuiltin() const { switch (name.pool) {
                          case BUILTIN_POOL: return name.index; }
                          return 0; }

  static CppType *RListType;
  CppTypeV **recycleList() PC_NA_BODY;

  static CppType  *TypeName;
  static CppType  *Void;
  static CppType  *Bool;
  static CppType  *Char;
  static CppType  *Short;
  static CppType  *Int;
  static CppType  *Long;
  static CppType  *Float;
  static CppType  *Double;
 
  int isTypNm() const {return this == TypeName;}

  static CppTypeV *Signal;
  static CppTypeV *SigBool;
  static CppTypeV *Driver;
  static CppTypeV *Receiver;
  static CppTypeV *ReceiverP;
  static CppTypeV *PipeClient;
  static CppTypeV *PipeClientP;
  static CppTypeV *Module;
  static CppTypeV *ModuleP;
  static CppTypeV *CharP;
  static CppTypeV *ConstCharP;
  static CppTypeV *BitField;
  static void     findBuiltins(const CppScope *);

  static CppType  *cppType(poolRef) PC_NA_BODY;
};

class CppTypeCls : public CppType {
 public:
  CppTypeV   **frnds;
  CppTypeV    *inherits;
  CppTypeRef  *castFn;

  ~CppTypeCls();

  static CppTypeCls *RListTypeCls;
  CppTypeV **recycleList() PC_NA_BODY;

  inline CppTypeCls(const Location *src,eCT ct,poolRef nm = NullRef,int xtra = 0,
                    CppTypeV *inh = 0)
    : CppType(src,ct,nm,xtra), frnds(0), inherits(inh), castFn(0) {}

  const CppTypeV     *Inherits()   const {return inherits;}
  const CppTypeV    **Friends()    const {return const_cast<const CppTypeV **>(frnds);}
  CppTypeV          **Inherits0()        {return &inherits;}
  CppType            *cTyp()             {return this;}
  const CppType      *cTyp()       const {return this;}

  CppTypeCls         *cTypCls(int,int *indrct = 0)       {return this;}
  const CppTypeCls   *cTypCls(int,int *indrct = 0) const {return this;}

  const CppTypeV     *findEntity() const;

  int isBuiltin()  const;
  int isA(int,int) const;

  void                addCastFn(CppTypeRef *);
  const CppTypeRef   *findCastFn(CppTypeV **)                       const;
  void                WriteAliases(Stream &,const char *)           const PC_NA_BODY;
  int                 WriteInit0(Stream &,const char *,
                                          const char *punc = " : ") const PC_NA_BODY;
  void                WriteInitX(Stream &,const char *,
                                      eINIT phase = INIT_CNSTRCT_1,
                                      eWFLG flgs  = WFLG_NONE)      const PC_NA_BODY;
};

typedef enum {
  SCP_GLOBAL = 0,
  SCP_C,
  SCP_CPP,
  SCP_NAMESPACE,
  SCP_TEMPLATE,
  SCP_FUNCTION,
  SCP_TYPE,
  SCP_BLOCK,
  SCP_FOR
} eSCPT;

class CppScopeRef;
class CppScope {
 public:
  CppScope                   *next;
  CppScopeRef                *nm_spcs;
  eSCPT                       sc_typ;
  int                         anon_count;
  List<CppTypeV,LIST_RECYCLE> types;
  List<CppTypeV,LIST_RECYCLE> anon_types;
  List<CppDecl,LIST_USE_DEL>  decls;
  CppDecl                    *args;
  CppStmt                    *stmt0,
                            **p_stmt;
  CppScope                   *up;
#if DBGLVL > 0
  int                         visiting;
#endif

  CppScope(eSCPT st = SCP_BLOCK,CppScope *prnt = 0) 
    : next(0), nm_spcs(0), sc_typ(st), types(true), decls(true),
      args(0), stmt0(0), p_stmt(&stmt0), up(prnt) {
#   if DBGLVL > 0
      visiting = 0;
#   endif
  }
  CppScope(const CppScope *);
  virtual ~CppScope() { assert(!args); }

  CppTypeV    *addType(CppTypeV*);
  CppTypeV    *addTypeAnon(CppTypeV*);
  CppScope    *findNameSpace(poolRef);
  CppTypeV    *findType(poolRef,CppTypeRef::flags *flgs = 0,int lcl = 0) const;

  inline int   findObj(poolRef, CppTypeV **,CppDecl **,CppScope **,
		       CppTypeRef::flags *flgs = 0,int lcl = 0)          const PC_NA_BODY;
  CppStmt     *addStmt(CppStmt *,int posn = 1) PC_NA_BODY;
  CppDecl     *addDecl(CppDecl *,int posn = 1);
  CppDecl     *addConst(const char *,int val);
  const Token *Write(Stream &out,const Token *,eWFLG = WFLG_NONE,
                                                const CppTypeV *cls = 0) const PC_NA_BODY;
  eAZR         fixSystemC(CppScope *a_scp,const Location *where,int level,eAZC flgs) PC_SC_BODY;

  CppTypeV *findType(int p,int i,CppTypeRef::flags *flgs = 0,int lcl = 0) const {
               poolRef T; T.pool = p; T.index = i; return findType(T,flgs,lcl); }

  CppDecl  *findRef(poolRef Ref,CppTypeRef::flags *flgs = 0,int lcl = 0) const {
             CppDecl *ret = 0;
             findObj(Ref,0,&ret,0,flgs,lcl); return ret; }

  inline const Location *Where() const {return stmt0->Where();}

  void Show(Stream *out = 0,int top = 1,int vrb = 0);
  void fixScWait(CppExpr *,CppStmt **) PC_SC_BODY;
  void showScope(int dpth = 0);
  void showTypes();
  void showDecls();
  void moveStmts(CppScope *);
  void copyStmts(DupContext *,CppScope *) const;

  int  isParent(CppScope *) const; 

  virtual CppScope *duplicate(DupContext *,CppScope *rblk = 0);

  virtual poolRef   Name()            const {return NullRef;}
  virtual CppTypeV *Type()                  {return 0;}
  virtual CppTypeV *Func()                  {return 0;}
  virtual int       AnonCount()             {return -1;}
  virtual int       incrRsm()               {assert(0); return 0;}
  virtual int       forkRsm(int *fc)        {assert(0); return 0;}
  virtual void      setChan()               {}
  virtual void      setWait()               {}
  virtual void      setStts()               {}
  virtual int       addSttcRngs(int c=1)    {assert(0); return 0;}
  virtual int       hasSttcRngs()     const {return 0;}
  virtual int       rsmPoints()       const {return 0;}
  virtual int       forks()           const {return 0;}
  virtual int       forkWait(int i)   const {return 0;}
  virtual int       usesChan()        const {return 0;}
  virtual int       usesWait()        const {return 0;}
  virtual int       usesStts()        const {return 0;}

  virtual const CppTypeV *Type()      const {return 0;}
  virtual const CppTypeV *Ret()       const {return 0;}
  virtual const CppStmt  *lastStmt()  const {return 0;}
  virtual const CppStmt  *firstStmt() const {return stmt0;}
  virtual       void      clrCtrl()   const {}

  virtual eAZR       Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
                             int level = 0,eAZC flgs = AZC_NONE);

  const char *describe(char *) const;
  const char *str()            const;
  const void  dump(Stream *)   const;

  static const char *strScope[];


};

class CppTypeScp : public CppTypeCls, public CppScope {
 public:
  int  rsm_pts,
       frk_rsm,
       has_sttc_rngs,
      *fmap,
       msz;
  union {
    struct {
      int   uses_chan:1;
      int   uses_wait:1;
      int   uses_stts:1;
    } f;
    int     as_int;
  } flags; 

  inline CppTypeScp(const Location *src,eCT ct,eSCPT st = SCP_BLOCK,
                    poolRef nm = NullRef,int xtra = 0,CppTypeV *inherits = 0) 
    : CppTypeCls(src,ct,nm,xtra,inherits), CppScope(st),
    rsm_pts(0), frk_rsm(0), has_sttc_rngs(0), fmap(0), msz(0) {
                                   flags.as_int = 0; }

  ~CppTypeScp() {FREE(fmap);} 

  CppScope       *Scope(int fllw = 0)        {return this;}
  const CppScope *Scope(int fllw = 0) const  {return this;}

  CppTypeV       *Type()               {return this;}
  const CppTypeV *Type()        const  {return this;}
  int             incrRsm()            {return rsm_pts++;}
  int             forkRsm(int *fc)     {*fc = frk_rsm++; return incrRsm();}
  void            setChan()            {flags.f.uses_chan = 1;}
  void            setWait()            {flags.f.uses_wait = 1;}
  void            setStts()            {flags.f.uses_stts = 1;}
  int             addSttcRngs(int c=1) {return has_sttc_rngs += c;}
  int             hasSttcRngs() const  {return has_sttc_rngs;}
  int             rsmPoints()   const  {return rsm_pts;}
  int             forks()       const  {return frk_rsm;}
  int             forkWait(int) const  PC_NA_BODY;
  int             usesChan()    const  {return flags.f.uses_chan;}
  int             usesWait()    const  {return flags.f.uses_wait;}
  int             usesStts()    const  {return flags.f.uses_stts;}

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE);
  
};

class CppTypeScpR : public CppScope {
 public:
  Location    source;
  CppTypeCls *cls;

  inline CppTypeScpR(const Location *src,CppTypeCls *_cls,eSCPT st = SCP_BLOCK) 
    : CppScope(st), source(*src), cls(_cls) {}

  CppScope       *Scope(int fllw = 0)       {return this;}
  const CppScope *Scope(int fllw = 0) const {return this;}
  CppTypeV       *Type()                    {return cls;}
  const CppTypeV *Type()              const {return cls;}

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE);
};

class CppTypedScp : public CppScope {
 public:
  int         anon_args;

  inline CppTypedScp(eSCPT st) 
    : CppScope(st), anon_args(0) {}

  int AnonCount() {return ++anon_args;}
};

class CppFuncScp : public CppTypedScp {
 public:
  CppTypeRef       *func;
  const CppTypeV   *ret;
  int               anon_args;

  inline CppFuncScp(CppTypeRef *fn,const CppTypeV *rt) 
    : CppTypedScp(SCP_FUNCTION), func(fn), ret(rt) {assert(rt || fn->x.f.anon);
                                                    fn->set_scp(this);}

  CppTypeV       *Func()       {return func;}
  const CppTypeV *Ret()  const {return ret;}

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE) {return func->Analyze(proc,scp,where);}
};


class CppStmtBlock : public CppStmt, public CppScope {
 public:

  inline CppStmtBlock(const Location *_source,eCSTMT _typ = CSTMT_Block,
                                                 eSCPT st = SCP_BLOCK)
    : CppStmt(_source,_typ), CppScope(st) {}

  CppStmtBlock(const CppStmtBlock *);

  CppStmt *duplicate(DupContext *cntxt = 0);

  const Token *Write(Stream&, const Token*,eWFLG) const PC_NA_BODY;

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE);

  void fixScWait(CppExpr *xpr,CppStmt **strts) { CppScope::fixScWait(xpr,strts); }
};

class CppStmtNmdBlk;
class CppNamedScope : public CppScope {
 public:
  poolRef  name;
  
  CppStmtNmdBlk *ctrl_seg,
                *wr_seg;

  poolRef Name() const {return name;}

  const CppStmt *lastStmt()  const;
  const CppStmt *firstStmt() const;

  void clrCtrl() {ctrl_seg = 0;}

  CppNamedScope(poolRef nm) : CppScope(SCP_NAMESPACE), name(nm), ctrl_seg(0), wr_seg(0) {};
};

class CppStmtNmdBlk : public CppStmt {
 public:
  CppNamedScope *nm_spc;
  CppStmt       *start;
  CppStmtNmdBlk *next_seg;
  eVIS           vis:8;
  eTag           tag0:8;
  poolRef        tag[1];
   
  inline CppStmtNmdBlk(const Location *_source,CppNamedScope *nms,
                       eCSTMT _typ = CSTMT_Block)
    : CppStmt(_source,_typ), nm_spc(nms), start(0), 
      next_seg(0), vis(VIS_NONE) {}

  const Token *Write(Stream&, const Token*,eWFLG) const PC_NA_BODY;

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE);
};

class CppScopeRef {
 public:
  CppScopeRef *next;
  CppTypeV    *obj;

  inline CppScopeRef(CppScope *scp) : next(0) {
    obj = scp ? new CppTypeRef(&Nowhere,scp)
              : 0;
  }

  inline CppScopeRef(CppTypeV *typ = 0) : obj(typ), next(0) {}

  inline CppScope *Scope() {return obj ? obj->Scope()
                                       : 0;}

  inline poolRef   Name() const {return obj->Name();}
};

class CppStmtUsing : public CppStmt, public CppScopeRef {
 public:
  poolRef   pend;
  CppTypeV *item;
  int       nmspc:1;

  inline CppStmtUsing(const Location *_source,CppScope *scp,int n = 0)
    : CppStmt(_source,CSTMT_Using), 
    CppScopeRef(scp), pend(NullRef), item(0), nmspc(n) {}

  inline CppStmtUsing(const Location *_source,CppTypeV *typ,int n = 0)
    : CppStmt(_source,CSTMT_Using), 
    CppScopeRef(typ), pend(NullRef), item(0), nmspc(n) {}

  inline CppStmtUsing(const Location *_source,poolRef Ref)
    : CppStmt(_source,CSTMT_Using), CppScopeRef(), pend(Ref) {}

  const Token *Write(Stream&, const Token*,eWFLG) const PC_NA_BODY;

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE) {return AZR_OK;}
};

class CppStmtPragma : public CppStmt {
 public:
  const Token *start,
              *finish;

  inline CppStmtPragma(const Location *_source,const Token *s,const Token *f)
    : CppStmt(_source,CSTMT_Using), start(s), finish(f) {}

  const Token *Write(Stream&, const Token*,eWFLG) const PC_NA_BODY;

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE) {return AZR_OK;}
};

class CppStmtIf : public CppStmt {
 public:
  CppExpr   *cnd;
  CppScope   tru,
            *fls;

  inline CppStmtIf(const Location *_source,CppExpr *_cnd)
    : CppStmt(_source,CSTMT_if), cnd(_cnd), fls(0) {}

  inline CppStmtIf(CppStmt *stmt)
    : CppStmt(stmt) {}
    
  const Token *Write(Stream&, const Token*,eWFLG) const PC_NA_BODY;

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE);

  void fixScWait(CppExpr *xpr,CppStmt **strts) { tru.fixScWait(xpr,0);
                                                 if (fls) fls->fixScWait(xpr,0);}

  CppStmt *duplicate(DupContext *cntxt = 0);
};

class CppStmtSwitch : public CppStmtBlock {
 public:
  CppExpr *cnd;

  inline CppStmtSwitch(const Location *_source,
                      CppExpr *_cnd)
    : CppStmtBlock(_source,CSTMT_switch), cnd(_cnd) {}

  const Token *Write(Stream&, const Token*,eWFLG) const PC_NA_BODY;

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE);
};

class CppStmtWhile : public CppStmtBlock {
 public:
  CppExpr *cnd;

  inline CppStmtWhile(const Location *_source,
                      CppExpr *_cnd)
    : CppStmtBlock(_source,CSTMT_while), cnd(_cnd) {}

  const Token *Write(Stream&, const Token*,eWFLG) const PC_NA_BODY;

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE);
};

class CppStmtDo : public CppStmtBlock {
 public:
  CppExpr *cnd;

  inline CppStmtDo(const Location *_source,CppExpr *_cnd = 0)
    : CppStmtBlock(_source,CSTMT_while), cnd(_cnd) {}

  const Token *Write(Stream&, const Token*,eWFLG) const PC_NA_BODY;

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE);
};

class CppStmtFork : public CppStmtBlock {
 public:
  int                  join,
                       count,
                       replicated;
  CppExpr             *rplx;
  struct {int rp,wt;} *lbl;
  mutable int          active;

  inline CppStmtFork(const Location *_source,CppExpr *xpr = 0)
    : CppStmtBlock(_source,CSTMT_fork), join(-1), count(0), replicated(-1), 
                                                            rplx(xpr), lbl(0) {}

  ~CppStmtFork() {FREE(lbl);}

  void addLabel(int,int) PC_NA_BODY;
  inline int label(int i) const {return lbl[i].rp;}
  inline int wait(int i)  const {return lbl[i].wt;}

  const Token *Write(Stream&, const Token*,eWFLG) const PC_NA_BODY;

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE);
};

class CppStmtCatch : public CppStmtBlock {
 public:
  CppDecl *args;

  inline CppStmtCatch(const Location *_source,CppDecl *_args)
    : CppStmtBlock(_source,CSTMT_catch), args(_args) {}

  const Token *Write(Stream&, const Token*,eWFLG) const PC_NA_BODY;

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE);
};

class CppStmtAt : public CppStmtBlock {
 public:
  CppExpr *cnd;

  inline CppStmtAt(const Location *_source,CppExpr *_cnd)
    : CppStmtBlock(_source,CSTMT_At), cnd(_cnd) {}

  const Token *Write(Stream&, const Token*,eWFLG) const PC_NA_BODY;

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE);
};

class CppStmtBefore : public CppStmtBlock {
 public:
  CppExpr *cnd;

  inline CppStmtBefore(const Location *_source,
                      CppExpr *_cnd)
    : CppStmtBlock(_source,CSTMT_Before), cnd(_cnd) {}

  const Token *Write(Stream&, const Token*,eWFLG) const PC_NA_BODY;

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE);
};

class CppStmtFor : public CppStmtBlock {
 public:
  CppScope *cntrs;
  CppExpr  *init,
           *cnd,
           *tail;

  inline CppStmtFor(const Location *_source,CppScope *scp,
                    CppExpr *_init,CppExpr *_cnd,CppExpr *_tail,
                    CppStmt *_stmt = 0)
    : CppStmtBlock(_source,CSTMT_for),
      cntrs(scp), init(_init), cnd(_cnd), tail(_tail) {}

  const Token *Write(Stream&, const Token*,eWFLG) const PC_NA_BODY;

  eAZR Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
               int level = 0,eAZC flgs = AZC_NONE);
};

class LnDrctv : public OffsetLoc {
 public:
  int line;
};

class CppContext : public PrsrCntxt {
  int           stck_dpth;
  int           blk_dpth;
  ePCMD         cmode;
  MappedFile   *mtk;
  String       *dbg;
  OffsetLoc     source;
  CppScope      root,
               *current; //ring
  ePcMod        svd_mods;
  ePCMD         svd_mode;
  eCPRS         svd_prsng;
  const Token **line_map;
  int           max_line;
  LnDrctv      *line_drctv;
  int           last_drctv,
                max_drctv;
 public:
  CppContext();
  ~CppContext() { delete [] line_map; 
                  delete [] line_drctv;}

  inline int   depth() {return this ? stck_dpth : 0;};

  inline const Token *addToken(const Token *T,int needed,int nl = 1,int print = 1) {
#if DBGLVL > 2
    if (print) fprintf(stderr," %s",tokDeref(T));
#endif
    return nextTok(T,needed,nl);
  };

  inline const Location *Where()          {source.file_num[0] = line - source.offset;
                                           return &source;}
  inline const Location *Where(const Token *T)  
                                          {source.tok = T; return Where();}

  inline       CppScope *Root()           {return &root;}
  inline const CppScope *Root()     const {return &root;}

  CppScope      *pushDown(CppScope *down) {CppScope *ret = current; assert(down != ret);
                                           current = down;
					   if (!current->up) current->up = ret;
					   return ret; }

  inline void popScope(CppScope *svd = 0) {current = svd ? svd 
                                                         : current->up;}
  
  void popScope(CppScope *,CppScope *);

  inline CppDecl  *addDecl(CppDecl  *dcl,int posn = 1) {current->addDecl(dcl,posn);}
  inline CppTypeV *addType(CppTypeV *typ)              {current->addType(typ);}

  void           reportError(eSTS sts,const char *format,...);
  int            dumpLnMap(FILE *,const Token *,int flgs = 0);
  CppScope      *pushScope(eSCPT st = SCP_GLOBAL,CppTypeRef *f = 0,CppScope *use_scp = 0,const CppTypeV *r = 0);
  CppScope      *addScope(CppScope *) PC_NA_BODY;
  int            mtchInst(CppTypeRef *,int,CppDecl **,const CppTypeV *,int);
  void           fixTmpltTyp(CppTypeRef *);
  void           showScope();
  int            closeStmt(const Token *,const Token *,const Token **,int alt_stop = 0,int tmplt = 0);
  const Token   *prsAttr(const Token *,const Token *,CppExpr **);
  const Token   *prsASM(const Token *,const Token *,CppExpr **,eXPRF flgs=XPRF_ASM);
  const Token   *prsClass(const Token *,const Token *,eCPRS,ePcMod,ePCMD,
                          CppTypeV **pTyp = 0);
  const Token   *prsEnum(const Token *,const Token *,eCPRS,ePcMod,ePCMD,
                         CppTypeV **pTyp = 0);
  const Token   *getFnArg(const Token *,const Token *,eCPRS,ePcMod,ePCMD,
                          CppDecl **,CppTypeV *prnt = 0);
  const Token   *getFnArgs(const Token *,const Token *,eCPRS,ePcMod,ePCMD,
                           CppDecl **,CppTypeV *prnt = 0,int rgstr = 1);
  const Token   *getTmpltInstArgs(const Token *,const Token *,eCPRS,ePcMod,ePCMD,CppTypeRef *);
  const Token   *getIndices(const Token *,const Token *,CppTypeRef *);
  bool          checkName(poolRef);
  CppStmt       *addStmt(CppStmt *,int posn = 1);
  CppExpr       *prsExpr(const Token *,const Token *,eCPRS,ePcMod,ePCMD,CppDecl **pDcl = 0);
  int            typeList(const Token *,const Token *,eCPRS,ePcMod,ePCMD,
                          CppTypeV *typ = 0) PC_NA_BODY;
  CppExpr       *prsExprSub(poolRef *,const Token **,int,CppDecl **pDcl = 0,int typ_ok = -1);
  int            isTmplt(poolRef *,const Token **,int,CppScope **pStart = 0);
  CppExpr       *prsDataItem(poolRef *,const Token **,int,CppDecl **pDcl = 0,int typ_ok = -1);
  const Token   *prsDecl(const Token *,const Token *,eCPRS,ePcMod,ePCMD,
                         CppTypeV *prnt = 0,CppTypeV **pTyp = 0,CppDecl **pRet = 0);
  int           stmtTyp(const Token *,const Token *,eCPRS,ePcMod,ePCMD,CppTypeV *typ = 0);
  int            isPntr(const Token **,const Token *,poolRef *);
  int            tryClsFunc(const Token **T,const Token *TL,CppTypeV **pTyp);
  const Token   *prsDeclItem(const Token *,const Token *,eCPRS,ePcMod,ePCMD,
                             CppTypeV *prnt = 0,CppTypeV **pTyp = 0,CppDecl **pRet = 0);
  const Token   *prsType(const Token *,const Token *,eCPRS,ePcMod,ePCMD,
                         CppTypeV *prnt = 0,
                         CppTypeV **pRetT = 0,CppDecl **pRetD = 0) PC_NA_BODY;
  const Token   *getSub(const Token *,const Token *,eCPRS,ePcMod,ePCMD,CppTypeV *,
                         int nms_only = 0);
  const Token   *prsFriend(const Token *,const Token *,eCPRS,ePcMod,ePCMD,
                           CppTypeV *pCls);
  const Token   *prsCnstrctrs(const Token *,const Token *,eCPRS,ePcMod,ePCMD,
                              CppTypeRef *pFnc,int depth = 0);
  const Token   *prsStmts(const Token *,const Token *,eCPRS,ePcMod,ePCMD) PC_NA_BODY;
  const Token   *prsTypeDef(const Token *,const Token *,eCPRS,ePcMod,ePCMD);
  const Token   *closeTmpltArg(const Token **,const Token *,int strtd = 0);
  const Token   *prsTemplate(const Token *,const Token *,eCPRS,ePcMod,ePCMD,
                             CppTypeV *prnt = 0);
  const Token   *prsUsing(const Token *,const Token *,eCPRS,ePcMod,ePCMD);
  const Token   *prsNameSpace(const Token *,const Token *,eCPRS,ePcMod,ePCMD);
  const Token   *prsProcess(const Token *,const Token *,eCPRS,ePcMod,ePCMD,
                            CppTypeV **pTyp = 0);
  const Token   *prsClsObj(const Token *,const Token *,eCPRS,ePcMod,ePCMD,
                           CppTypeV **pTyp = 0);
  const Token   *skip1(const Token *,const Token *);
  const Token   *skipWht(const Token *,const Token *TL = 0,int nl = 1);
  const Token   *skip2nl(const Token *);
  const Token   *skip2ec(const Token *);
  const Token   *skipToken(const Token *,const Token *,const Token,const char *whr = 0);
  const Token   *findClose(const Token *,const Token *,const Token &tk = TOK_PUNC_SCOLON,int ti = 0);
  const Token   *NextTok(const Token *,int);
  const Token   *advanceTo(const Token *,const Token *);
  const Token   *prsLnDrctv(const Token *,int rescan = -1);
  const Token   *prsPCtok(const Token *,const Token *,eCPRS,ePcMod,ePCMD,CppTypeV *pTyp = 0);
  int            prsPCtok(const char *,File &,File *,ePCMD);

  eAZR           Analyze(CppType *proc = 0,CppScope *scp = 0,const Location *where = 0,
                         int level = 0,eAZC flgs = AZC_NONE);
  int            reWrite(File &,eWFLG) PC_NA_BODY;

  const Token *rescanning(const Token *T0,int back_only = 1);
  virtual const Token *nextPostNL(const Token *T,int l =0);

};

typedef eSTS (*CppFunc)(CppContext *);

class CppConst {
 public:
# define CONST_STR(n,s,p,i) static poolRef n;
# include "const_str.inc"
  static void setStrings();
};

int tokPC (Stream *,Stream *,ePCMD mode);

#endif // __cplusplus

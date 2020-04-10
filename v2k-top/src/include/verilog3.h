/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  verilog3_h_rcsid
#define verilog3_h_rcsid() {return "$Id: verilog3.h,v 1.101 2010/04/21 01:43:12 dkc Exp $";} /* RCS ID */

 
/*! \file
    Class definitions for Verilog parser
*/

#ifndef VERILOG3_H
#define VERILOG3_H

typedef enum {
  STMT_NULL = 0,
#define STATEMENT(e,t,m) e,
#include "statement.inc"
  STMT_LAST
} eSTMT;

#ifndef PLVALUE_FIELDS
#include "veri_pool-dsc.h"
#endif

#ifndef ARG_TYPE
#include "args.h"
#endif

class BaseExpr;

typedef union {
  PLVALUE_FF
  Token           tok;
  poolRef        *hrf;
  void           *ptr;
  struct sArray  *a;
  struct sCall   *c;
  class Expr     *expr;

} Value;

class ValueFn {
  Value value;
public:
  inline const Value *val()      const {return &value;};
  inline poolRef      piValue()  const {return value.ref;};
  inline rrfTup       rsiValue() const {return value.rrf;};

  const char *strValue(String *,VlType) const;
};

void fixTypDesc(const typDesc *,const typDesc *,typDesc *);

class Attr : public VerilogObj {
public:
  Attr *next,
       *attr;

  PLATTR_FF

  ~Attr();

  inline void setNT(poolRef nm,AttrType t) {typ   = t;
                                            pool  = nm.pool;
                                            index = nm.index;};
  inline Attr()                            {setNT(NullRef,ATTR_VOID);
                                            next = attr = 0;};
  inline AttrType at()                     {return (AttrType)typ;};
  inline VlType   valType() const          {return ATTR_VT(typ);};

  class Range *addRange(class Range *);
  ValueFn     *vf() const;
  int          hasValue() const;
  const char  *strValue(String *) const;
  Expr        *expr() const;
  poolRef      name() const;
};

class AttrU : public Attr {
public:
  poolRef nm;

  inline void setNT()                        {typ = ATTR_VOID;}
  inline void setNT(poolRef name,AttrType t) {index = pool = 0;
                                              nm  = name;
                                              typ = ATTR_TYPE(t|ATTR_USR);}
  inline poolRef name() const                {return nm;};
};

class AttrAnyU : public AttrU {
public:
  Value  value;
  inline AttrAnyU() {
           setNT(NullRef,ATTR_VOID);
         };
  inline AttrAnyU(poolRef nrf) {
           setNT(nrf,ATTR_VOID);
         };
  inline AttrAnyU(poolRef nrf,poolRef ref) {
           setNT(nrf,ATTR_REF);
           value.ref = ref;
         };
  inline AttrAnyU(poolRef nrf,poolRef ref,AttrType at) {
           setNT(nrf,at);
           value.ref = ref;
         };
  inline AttrAnyU(poolRef nrf,Expr *xpr) {
           setNT(nrf,ATTR_EXPR);
           value.expr = xpr;
         };
  inline AttrAnyU(poolRef nrf,double d) {
           setNT(nrf,ATTR_DOUBLE);
           value.d = d;
         };
  inline AttrAnyU(poolRef nrf,int i) {
           setNT(nrf,ATTR_INT);
           value.i = i;
         };
  inline AttrAnyU(poolRef nrf,I64 i64) {
           setNT(nrf,ATTR_I64);
           value.i64 = i64;
         };
  inline AttrAnyU(poolRef nrf,U64 u64) {
           setNT(nrf,ATTR_U64);
           value.u64 = u64;
         };
};

class AttrAnyU2 : public AttrAnyU {
public:
  Value value2;
  inline AttrAnyU2(poolRef nrf,Expr *xpr,Expr *xpr2) {
           setNT(nrf,ATTR_LOGIC);
           value.expr  = xpr;
           value2.expr = xpr2;
         };
};

class AttrAny : public Attr {
public:
  Value  value;
  inline AttrAny() {
           setNT(NullRef,ATTR_VOID);
         };
  inline AttrAny(poolRef nrf) {
           setNT(nrf,ATTR_VOID);
         };
  inline AttrAny(poolRef nrf,poolRef ref) {
           setNT(nrf,ATTR_REF);
           value.ref = ref;
         };
  inline AttrAny(poolRef nrf,poolRef ref,AttrType at) {
           setNT(nrf,at);
           value.ref = ref;
         };
  inline AttrAny(poolRef nrf,Expr *xpr) {
           setNT(nrf,ATTR_EXPR);
           value.expr = xpr;
         };
  inline AttrAny(poolRef nrf,Disc *disc,DiscIdx idx) {
           setNT(nrf,ATTR_DISC);
           value.i = idx;
         };
  inline AttrAny(poolRef nrf,Nature *nat,NatureIdx idx) {
           setNT(nrf,ATTR_NATURE);
           value.i = idx;
         };
  inline AttrAny(poolRef nrf,double d) {
           setNT(nrf,ATTR_DOUBLE);
           value.d = d;
         };
  inline AttrAny(poolRef nrf,int i) {
           setNT(nrf,ATTR_INT);
           value.i = i;
         };
  inline AttrAny(poolRef nrf,I64 i64) {
           setNT(nrf,ATTR_I64);
           value.i64 = i64;
         };
  inline AttrAny(poolRef nrf,U64 u64) {
           setNT(nrf,ATTR_U64);
           value.u64 = u64;
         };
};

class AttrObj : public VerilogObj {
public:
  Attr    *attr_list,
         **attr_next,
         **pending;
  eAOtype  typ:8;
  char     typ2;
  InfoRef  name;

  Attr *addAttr(poolRef,poolRef = NullRef);
  Attr *addAttr(poolRef,int);
  Attr *addAttr(poolRef,double);
  Attr *addAttr(poolRef,Nature *,NatureIdx);
  Attr *addAttr(poolRef,Disc *,DiscIdx);
  Attr *addAttr(poolRef,Expr *);
  Attr *addAttr(poolRef,poolRef,AttrType);

  void  addPendAttr(Attr    *);
  void  addPendAttr(AttrObj *);

  Attr *addAttrFront(poolRef,Expr *);

  poolRef attrVal(poolRef);

  inline AttrObj() {typ       = AO_UNKNOWN; attr_list = 0;
                    attr_next = &attr_list; name      = NullRef;
                    pending   = 0;};

  inline Attr *newAttr(Attr *ap) {
    if (!pending) {
      pending = attr_next;
    }
    *attr_next = ap; attr_next = &ap->next;
    return ap;
  }

};

inline void remove(AttrObj *pattr) {delete pattr;}

typedef enum {
#define LST(t,b,n,e) LL_##e,
#define LST_LOCAL
#include "lists.inc"

  LL_MAX
} eLL;

#define LST(t,b,n,e) typedef int t##Idx;
#define LST_LOCAL
#include "lists.inc"

class Port : public AttrObj {
public:
  Port     *next;
  BITF(PortDir,io,  16,short);
  BITF(PortTyp,ptyp, 8,char);
  BITF(SigType,styp, 8,char);
  DiscIdx   dsc_indx;
  Charge    chrg:8;
  Strength  ds0:8,
            ds1:8;
  int       pckd:8,
            unpckd:8,
            value:8;
  Expr    **rng,
           *dly;

  inline int  Rngs()    {return pckd + unpckd;};
  inline void recycle() {delete this;}

  Port(PortDir,PortTyp,SigType,Strength,int,DiscIdx);
};

class Branch : public VerilogObj {
public:
  class Branch *next;
  Expr         *p1,
               *p2;
  poolRef      *name;

  inline ~Branch()                {DELETE_ARR(name);};
  Branch(Expr *x1,Expr *x2,int c) {next    = 0;
                                   p1      = x1;
                                   p2      = x2;
                                   name    = 0;
                                   if (c >= 0) {name    = new poolRef[c+1];
                                                name[c] = NullRef;}};
};

class refList {
public:
  refList *next;
  poolRef  ref;

  inline refList(poolRef rf) {next = 0; ref = rf;};

  inline void recycle() {delete this;}
};

class Usage {
public:
  Usage    *next;
  BITF(eUSG,use,16,short);
  short     ignr;
  Expr     *xpr;

  ~Usage();
  Usage(eUSG,Expr *);

  inline Usage() { next = 0; xpr = 0; ignr = 0;};

  inline void recycle() {delete this;}
};

#if DBGLVL > 0
void dumpUsage(const Stmt *stmt);
#endif

class Unknown : public VerilogObj {
public:
  Unknown *next;
  int      scp;
  PortDir  io;
  eRFF     rff;
  eREF     rtyp;
  InfoRef  ref;
  Expr    *expr;
  rrfTup   fnd;

  inline void init(poolRef nm,eRFF f,Expr *xpr) {
    next      = 0;
    rff       = f;
    io        = PRT_NC;
    expr      = xpr;
    scp       = scp_id();
    ref.pool  = nm.pool;
    ref.index = nm.index;
    ref.file  = vd()->posn.file;
    ref.line  = vd()->posn.line;
    ref.level = vd()->posn.level;
    rtyp      = REF_UNKNOWN;
    fnd.index = -1;
  }
  inline Unknown(poolRef nm,eRFF f)           {init(nm,f,0);};
  inline Unknown(poolRef nm,eRFF f,Expr *xpr) {init(nm,f,xpr);};
};

class ParmList: public List<Attr,true>,public VerilogObj {
};

class ContextObj: public AttrObj {

  static Attr  *Saved,
              **SavedNext;

public:
  ListArrayItem<true> lists[LL_MAX];

  virtual Expr    *evalData(InfoRef *,int)        = 0;
  virtual hshdRef *logLocal(poolRef,eREF,int)     = 0;
  virtual int      evalRef(Expr *,eRFF,eREF)      = 0;
  virtual Expr    *collapseHier(Expr *,VlTypXtra) = 0;
  virtual void     dumpPortRef(Stream *,int,int)  = 0;

  void        popScope(Stmt *,poolRef);
  void        pushScope(Stmt *,poolRef);
  ContextObj *getScp(int);

  inline ContextObj() {InitLists(lists,LL_MAX);}
  ~ContextObj();

#define LST(t,b,n,e) inline t##List *n() const {\
                                       return (t##List *)&lists[LL_##e].list;};
#define LST_LOCAL
#include "lists.inc"

#define LST(t,b,n,e) inline b *n##_map(int i) const {\
                                       return (b *)(lists[LL_##e].map[i]);};
#define LST_LOCAL
#include "lists.inc"

  inline void extendExpr(InfoRef ref,const Token *T = 0) {
    if (vd()->expr_len + 1 >= vd()->expr_max) {
      vd()->expr_max += 64;
      REALLOC2(vd()->expr,vd()->expr_max,InfoRef);
    }
    int e = vd()->expr_len++;
    vd()->expr_term   = vd()->expr[e]; // in case we backed up
    vd()->expr[e]     = ref;
    vd()->expr[e].tok = T;    
  }

  void         deleteLists();
  Expr        *logUsage(eUSG,Expr *);
  Expr        *cmplExpr(InfoRef *,int,VlType,StmtCntx);
  Expr        *cmplExpr(VlType);
  Expr        *cmplExpr();
  Expr        *evalExpr(eREF);
  Expr        *saveExpr();
  Branch      *findBranch(Expr *,Expr *);
  const Token *endExpr(const Token *,const Token *);
  Disc        *findBrDisc(Expr *n1,Expr *n2);
  const Token *getExpr(const Token *,const Token *, StmtCntx,
		       InfoRef **ppRf=0, int *pLen=0);
  const Token *SaveAttr(const Token *,const Token *,
			int bail_none = 0,InfoRef *pRef = 0, int len = 0);
  void         addExprAttrSub();
  const Token *addExprAttr(const Token *,AttrObj *);
  const Token *addExprAttr(const Token *,Attr *);

  inline Expr *cmplExpr(AttrType t)      {return cmplExpr(ATTR_VT(t));};
  inline Expr *evalExpr()                {return evalExpr(REF_UNKNOWN);};
  inline void  extendExpr(poolRef ref)   {extendExpr(InfoRef(ref));};
  inline void  extendExpr(int p,int i)   {extendExpr(InfoRef(p,i));};
  inline void  endExpr()                 {extendExpr(NullRef);
                                          vd()->expr_len--;};
};

class VerCntxtObj : public ContextObj {
public:
  VerCntxtObj   *next;
  BITF(PortDir,io,16,short);
  int            saved,
                 found;
  long           stmt_count;

  VerCntxtObj();

  const Token *getDelayExpr(const Token *,const Token *);
  const Token *getEventExpr(const Token *,const Token *);
  const Token *getCase(const Token *,const Token *,StmtCase *,StmtCntx);
  const Token *getStmts(const Token *,const Token *,Stmt **,StmtCntx,long *);
  const Token *getForeign(const Token *,const Token *,InfoRef *,String *);
  hshdRef     *log_obj(poolRef,eREF,int);

  Expr        *evalData(InfoRef *,int);
  Expr        *collapseHier(Expr *,VlTypXtra);
  hshdRef     *logLocal(poolRef,eREF,int);
  int          evalRef(Expr *,eRFF,eREF);
  void         dumpPortRef(Stream *,int,int);

  inline void  recycle() {delete this;}
};

class Disc : public VerCntxtObj {
public:

  Expr    *evalData(InfoRef *,int);

  Disc      *next;
  DscFlgs    flags;
  DiscIdx    parent;
  NatureIdx  flow,
             potential;

  Disc(const Token *,const Token *,Disc *);
};

int getDisc(poolRef);

class SignalBase : public VerilogObj {
public:
  SigType typ:8;
};

class VectorBase : public SignalBase {
public:
  short dirn;
  int   start,
        finish;
};

class Scope : public VerilogObj {
public:
  Scope   *next;
  poolRef  name;
  Stmt    *stmt;
  int      up,
           dpth;

  inline Scope()         {next = 0; stmt = 0; up = dpth = -2;};
  inline Scope(Stmt *st) {next = 0; stmt = st;
                          up = scp_id(); dpth = scp_dpth();};
};

class VStmt;
typedef void *(*StmtCallBackFn)(void *,VStmt *);

typedef struct {
  int    port,
         count:8,
         dirn:24; // bit set for item positive
  Value *values;
} RngVec;

typedef struct sParmRng parmRng;

class Inst;

class Library : public AttrObj {

  static LibraryIdx current;

  Library  *next;
  Filename  path;
  eAT       pth_typ;
  int       used;

  friend class List<Library,true>;

public:
  Library(const char *name);
  Library(const char *name,const char *path);

  int setLibrary(int);

  inline const char *Name() { const char *nmp = strDeref(name);
                              return nmp ? nmp : ""; };
  inline const char *Path() { return path; };
};

class Module;
class PrtdObj : public VerCntxtObj {
public:

  inline PrtdObj()          {typ = AO_UNKNOWN;};
  inline PrtdObj(eAOtype a) {typ = a;};

  Library       *library;

  const Token *getPorts(const Token *,const Token *,
                        PortDir,PortTyp,SigType,Strength,int,DiscIdx,int);
  const Token *getParam(const Token *,const Token *);
  const Token *getPorts(const Token *,const Token *);
  const Token *getBranches(const Token *,const Token *);
  Attr        *getParm(poolRef,AttrType,poolRef,Expr*);
  Port        *findPort(poolRef);
  void         fixUnknowns();
  void         addInstRef(poolRef);
  int          parmIndex(poolRef);
  int          portCount();
  int          parmCount();
  poolRef      parmVal(Inst *,int,BaseExpr *,Value *);
  bool         parmRange(Inst *,int,int,parmRng *);
  PortDir      portMode(int);
  int          indxd();
  int          unk_count();
  eREF         unkRefType(int);
  bool         AnalogFunc(const Token *,const Token *);

  inline Module *AsMod() {return (Module *)this;};

  poolRef itemName(eREF,int);
  poolRef stmtLabel(int);
};

class Stmt : public VerCntxtObj {
public:

  Expr *evalData(InfoRef *,int);

  short  file_id,
         level;
  long   line;
  Stmt  *next;

  void        setLocation();
  const char *File(const ContextObj *) const;

  ~Stmt();
  inline Stmt() {typ = AO_STMT; typ2 = STMT_NULL; next = 0;
                 setLocation();};

  inline eSTMT   stmt_typ() const {return (eSTMT)typ2;};
};

class Module : public PrtdObj {
public:
  char     macro,
           specs,
           cell;
  int      tsi,tse,
           pri,pre;
  int      ref_indx,
           lib_indx;
  int      inst_count,
           def_prm;
  eVMD     vmode;
  Stmt    *stmts;
  poolRef  arch;

  ~Module();
  Module(const Token *,const Token *,bool);
  Module(int);

  void createPool();
  void minimize();

  void *forAllStmts(StmtCallBackFn,void *,int);
  int   evalIndices(Inst *,int,RngVec *);
  bool  bindPorts(Inst *,Inst *);
  eCG   codegen(Inst *,Stream *,Stream *,eCodeMode);
};

int loadModule(File *,int);

class InstRef : public VerilogObj {
public:
  InstRef *next;
  poolRef  name;
  int      mod_indx,
           refs,
           ref_max,
          *referer;

  inline ~InstRef() {FREE(referer);};
  InstRef(poolRef nm);

  InstRef *findRef(poolRef nm);
};

typedef struct s_table {
  struct s_table *next;
  char            entry[UNSIZED(0)];
} Table;

class Prim : public PrtdObj {
public:
  short  in,
         out;
  char   seq;
  int    tbl_sz,
         lib_indx;
  Table *table;
  Stmt  *stmt;

  void createPool(int);
  void minimize();

  ~Prim();
  Prim(const Token *,const Token *);
  Prim(int);

  const Token *readTable(const Token *,const Token *);
  eTable       tblConvert(int,poolRef);
  const char  *tblString(eTable t) const;
  int          evalIndices(Inst *,int,RngVec *);
};

int loadPrim(File *,int);

typedef struct sArray {
  int   dimensions;
  struct {
    int start,
        finish,
        step; // and direction
  }     dd[1];
} Array;

//! Constant Base Expression
typedef struct {
  PLBASEEXPR_FF
} CnstBaseExpr;

//! Base Expression Type
typedef enum {
# define BASE_EXPR(e,n,t,x,r,c,s) e,
# include "base_expr.inc"
  BX_LAST
} eBX;

//! Base Expression
class BaseExpr {
public:
  PLBASEEXPR_FF

  static CnstBaseExpr  CBE[BX_LAST+1];
  static const char   *typStr[VT_LAST+1];
  static ePrcsn        Precision[VT_LAST+1];
  static CllType       rtns[];
  static int           zero;

  inline VlTypXtra   constant()     {return VL_TYP_XTRA(VTX_CONST&xtra);};
  inline VlType      vt()           {return (VlType)typ;};
  inline eREF        rt()           {return (eREF)rtyp;};
  inline VlTypXtra   xt()           {return (VlTypXtra)xtra;};
  inline const char *typ_name()     {return typStr[typ];};
  inline BaseExpr   *set(eBX b)     {*(CnstBaseExpr *)this = CBE[b];
                                     return this;};
  inline             BaseExpr()     {set(BX_VOID);};
  inline void        set_vtx(int x) {xtra = (VlTypXtra)x;};

  ePrcsn             normalize(BaseExpr *);
  ePrcsn             normalize(BaseExpr *,BaseExpr *);
};

VlType Pt2Vt(PortTyp);

#define BASE_EXPR(e,n,t,x,r,c,s) BaseExpr *const n = (BaseExpr *)&BaseExpr::CBE[e];
#include "base_expr.inc"

//! Operator table entry
class Operator {
public:
  OPR       self,
            alt;
  eUSG      lval;
  StmtCntx  cntxt;
  poolRef   name;
  OprPrec   preced;
  BaseExpr *rtn,
           *left,
           *right;

  static Operator oprs[];

  inline Operator *get(OPR o) {return &oprs[o];};

  friend class Expr;
};

typedef struct sCall {

  poolRef   name;
  int       argc;
  BaseExpr *args;

} Call;

//! Semi-infinite 1/0/?/Z/X Logic #Value (Bit Map)
#define QZX_WDTH 64
typedef struct s_qzx {
  PLQZX_FIELDS
  struct s_qzx *next; // optional
} Qzx;

int printQzx(Qzx *,int,Stream *,const char *,int,int,int,int cb = 0);

typedef bool (*deAliasFn)(Expr *);

typedef struct deAliaser_s {
  struct deAliaser_s *next;
  deAliasFn           fn;
} deAliaser;

typedef struct rngEval_s {
  char l_cnst,r_cnst,inc;
  int  left,
       right;
} rngEval;

DECL_XREF(Expr);

//! Normal expression tree node
class Expr : public BaseExpr {

#define NON_EXPR(T)         (XD_FLAG & (intptr_t)(T))
#define IS_EXPR(P)          (!(1 & (int)P))
#define BASE_N_WIDTH(l,b,w) (XData)(((((b) << 11)|((w) << 16))|\
                                         XD_FLAG)|(l ? XD_LOGIC : 0))
#define IBASE(x)            ((((intptr_t)(x)) >> 11) & 0x1F)
#define IWIDTH(x)           (((intptr_t)(x)) >> 16)
#define BITS2LWORDS(n)      {if (n & 63) {n |= 63; n++;} n = n/64;}

#define EXPR_SUB Expr *
#define EXPR_ALIAS
#include "expr.h"

  static bool CheckInUse;

  inline Expr *lhs()      const {return left;};
  inline Expr *rhs()      const {return right;};
  inline bool  isInUse()        {if (xtra&VTX_ALIASSED) deAlias();
                                 return CheckInUse && (xtra&VTX_INUSE);};

  friend class InMemExpr;
};

typedef struct ExprAlias_s {
  struct   ExprAlias_s *next;
  Expr    *xpr;
  poolRef  ref;
} ExprAlias;

class Range : public VerilogObj {
public:
  Range  *next;
  int     exclude:8,
          inc_strt:8,
          inc_fnsh:8;
  Expr   *range;

  inline ~Range() {DELETE(range);};
  inline  Range() {next    = 0;
                   range   = 0;
                   exclude = inc_strt = inc_fnsh = 0;};
};

class RngdAttr : public AttrAnyU {
  inline void setNT(poolRef nam,AttrType t)
                                      {index = pool = 0;
                                       nm   = nam;
                                       typ  = ATTR_TYPE(t|ATTR_USR|ATTR_RNGD);}
public:
  Range  *rng_lst;

  inline RngdAttr(poolRef n)          {rng_lst = 0;
                                       setNT(n,ATTR_RNGD);};
  inline Range *addRange(Range *rng)  {rng->next      = rng_lst;
                                       return rng_lst = rng;};
  inline RngdAttr(poolRef n,Expr *x)  {rng_lst = 0;
                                       setNT(n,ATTR_TYPE(ATTR_EXPR|ATTR_RNGD));
                                       value.expr = x;};
  inline RngdAttr(poolRef n,double d) {rng_lst = 0;
                                       setNT(n,ATTR_DOUBLE);
                                       value.d = d;};
  inline RngdAttr(poolRef n,int i)    {rng_lst = 0;
                                       setNT(n,ATTR_INT);
                                       value.i = i;};
  inline RngdAttr(poolRef n,I64 i64)  {rng_lst = 0;
                                       setNT(n,ATTR_I64);
                                       value.i64 = i64;};
  inline RngdAttr(poolRef n,U64 u64)  {rng_lst = 0;
                                       setNT(n,ATTR_U64);
                                       value.u64 = u64;};
};

#define MAX_PLATTR_OBJ plRngdAttr

class Nature : public VerCntxtObj {
public:

  Expr *evalData(InfoRef *,int);

  Nature    *next;
  DscFlgs    flags;
  NatureIdx  parent;

  Nature(NatureIdx);

  void init(const Token *,const Token *,int);
  void getAttr(const Token *,const Token *,int,int);
};

#define STATEMENT(e,t,m) class t;
#include "statement.inc"

class StmtExpr : public Stmt {
public:
  Expr *expr;

  ~StmtExpr();
  inline StmtExpr(Expr *xpr,long *count)
                                    {typ2 = STMT_EXPR; expr = xpr; (*count)++;};
};

class StmtDefparam :  public Stmt{
public:
  Expr *expr;

  ~StmtDefparam();
  inline StmtDefparam(Expr *xpr,long *count)
                                {typ2 = STMT_DEFPARAM; expr = xpr; (*count)++;};
};

class StmtAnalog : public Stmt {
public:
  Stmt *child;

  ~StmtAnalog();
  inline StmtAnalog(long *count) {typ2 = STMT_ANALOG; child = 0; (*count)++;};
};

class StmtTask : public Stmt {
public:
  long glob,
       task_indx;

  ~StmtTask();
  inline StmtTask(long *count)
                      {typ2 = STMT_TASK; glob = 1; task_indx = -1; (*count)++;};
};

class StmtFunc : public Stmt {
public:
  long glob,
       func_indx;

  ~StmtFunc();
  inline StmtFunc(long *count)
                      {typ2 = STMT_FUNC; glob = 1; func_indx = -1; (*count)++;};
};

class StmtFuncA : public Stmt {
public:
  long glob,
       func_indx;

  ~StmtFuncA();
  inline StmtFuncA(long *count)
                      {typ2 = STMT_FUNC_A; glob = 1; func_indx = -1; (*count)++;};
};

class StmtDecl : public Stmt {
public:
  Stmt     *child;
  PrtdObj  *lcls;
  long      stmt_count;

  ~StmtDecl();
  inline StmtDecl() {lcls = 0; child = 0; stmt_count = 0;};

  inline PrtdObj *addLcls()   {if (!lcls) lcls = new PrtdObj(AO_LOCALS);
                               return lcls;};
};

class StmtSpec : public StmtDecl {
public:

  ~StmtSpec();
  inline StmtSpec(long *count) {typ2 = STMT_SPECIFY; (*count)++;};
};

class StmtBlock : public StmtDecl {
public:
  poolRef   name;

  ~StmtBlock();
  inline StmtBlock(poolRef nm,long *count)
                                    {typ2 = STMT_BLOCK; name = nm; (*count)++;};
};

class StmtFork : public StmtDecl {
public:
  poolRef   name;

  ~StmtFork();
  inline StmtFork(poolRef nm,long *count)
                                     {typ2 = STMT_FORK; name = nm; (*count)++;};
};

class StmtIf : public Stmt {
public:
  Expr *expr;
  Stmt *child_t,
       *child_f;

  ~StmtIf();
  inline StmtIf(Expr *xpr,long *count)
               {typ2 = STMT_IF; expr = xpr; child_t = child_f = 0; (*count)++;};
};

class StmtIfNone : public Stmt {
public:
  Expr *expr;
  Stmt *child_t,
       *child_f;

  ~StmtIfNone();
  inline StmtIfNone(Expr *xpr,long *count)
           {typ2 = STMT_IFNONE; expr = xpr; child_t = child_f = 0; (*count)++;};
};

class StmtAt : public Stmt {
public:
  Expr *expr;
  Stmt *child;

  ~StmtAt();
  inline StmtAt(Expr *xpr,long *count)
                           {typ2 = STMT_AT; expr = xpr; child = 0; (*count)++;};
};

class StmtInit : public Stmt {
public:
  Stmt *child;

  ~StmtInit();
  inline StmtInit(long *count) {typ2 = STMT_INITIAL; child = 0; (*count)++;};
};

class StmtDelay : public Stmt {
public:
  Expr *expr;
  Stmt *child;

  ~StmtDelay();
  inline StmtDelay(Expr *xpr,long *count)
                        {typ2 = STMT_DELAY; expr = xpr; child = 0; (*count)++;};
};

class StmtRepeat : public Stmt {
public:
  Expr *expr;
  Stmt *child;

  ~StmtRepeat();
  inline StmtRepeat(Expr *xpr,long *count)
                       {typ2 = STMT_REPEAT; expr = xpr; child = 0; (*count)++;};
};

class StmtFor : public Stmt {
public:
  Expr *ass1,
       *expr,
       *ass2;
  Stmt *child;

  ~StmtFor();
  inline StmtFor(Expr *a1,Expr *xpr,Expr *a2,long *count) {typ2  = STMT_FOR;
                                                           ass1  = a1;
                                                           expr  = xpr;
                                                           ass2  = a2;
                                                           child = 0;
                                                           (*count)++;};
};

class StmtGen : public Stmt {
public:
  poolRef  cntr;
  Expr    *expr;
  Stmt    *child;

  ~StmtGen();
  inline StmtGen(poolRef c,Expr *xpr,long *count) {typ2 = STMT_GEN; cntr  = c;
                                                   expr = xpr;      child = 0;
                                                   (*count)++;};
};

typedef struct CaseS {
  struct CaseS *next;
  Expr         *expr;
  Stmt         *child;
} case_s;

class StmtCase : public Stmt {
public:
  Expr   *expr;
  case_s *list;

  ~StmtCase();
  inline StmtCase()            {};
  inline StmtCase(long *count)  {typ2 = STMT_CASE;  (*count)++;};
};
class StmtCaseX : public StmtCase {
public:
  inline StmtCaseX(long *count) {typ2 = STMT_CASEX; (*count)++;};
};
class StmtCaseZ : public StmtCase {
public:
  inline StmtCaseZ(long *count) {typ2 = STMT_CASEZ; (*count)++;};
};

class StmtInst : public Stmt {
public:
  poolRef    name,
             drv;
  char       udp,
             prim;
#define IUDP_DRV   1
#define IUDP_DELAY 2
  DrvStrnth  ds[2];
  Expr      *param,
            *inst;
  ~StmtInst();
  StmtInst(poolRef,Expr *,PrtdObj *,long *count);
};

class StmtForever : public Stmt {
public:
  Stmt *child;

  ~StmtForever();
  inline StmtForever(long *count)
                                {typ2 = STMT_FOREVER; child = 0; (*count)++;};
};

class StmtAlways : public Stmt {
public:
  Stmt *child;

  ~StmtAlways();
  inline StmtAlways(long *count) {typ2 = STMT_ALWAYS; child = 0; (*count)++;};
};

class StmtWhile : public Stmt {
public:
  Stmt *child;
  Expr *expr;

  ~StmtWhile();
  inline StmtWhile(Expr *xpr,long *count)
                       {typ2 = STMT_WHILE; expr = xpr; child = 0; (*count)++;};
};

class StmtWait : public Stmt {
public:
  Stmt *child;
  Expr *expr;

  ~StmtWait();
  inline StmtWait(Expr *xpr,long *count)
                        {typ2 = STMT_WAIT; expr = xpr; child = 0; (*count)++;};
};

class StmtAssign : public Stmt {
public:
  Expr      *expr,
            *delay;
  DrvStrnth  ds[2];

  ~StmtAssign();
  inline StmtAssign(long *count) {typ2 = STMT_ASSIGN; expr = 0; delay = 0;
                                 ds[0].strnth = ds[1].strnth = DS_NONE;
                                 ds[0].lvl    = ds[1].lvl    = -1;
                                 (*count)++;};
  inline StmtAssign(Expr *xpr,long *count)
                                {typ2 = STMT_ASSIGN; expr = xpr; delay = 0;
                                 ds[0].strnth = ds[1].strnth = DS_NONE;
                                 ds[0].lvl    = ds[1].lvl    = -1;
                                 (*count)++;};
};

class StmtForce : public Stmt {
public:
  Expr *expr;

  ~StmtForce();
  inline StmtForce(Expr *xpr,long *count)
                                  {typ2 = STMT_FORCE; expr = xpr; (*count)++;};
};

class StmtDeassign : public Stmt {
public:
  Expr *expr;

  ~StmtDeassign();
  inline StmtDeassign(Expr *xpr,long *count)
                               {typ2 = STMT_DEASSIGN; expr = xpr; (*count)++;};
};

class StmtRelease : public Stmt {
public:
  Expr *expr;

  ~StmtRelease();
  inline StmtRelease(Expr *xpr,long *count)
                                {typ2 = STMT_RELEASE; expr = xpr; (*count)++;};
};

class StmtDisable : public Stmt {
public:
  Expr *expr;

  ~StmtDisable();
  inline StmtDisable(Expr *xpr,long *count)
                                {typ2 = STMT_DISABLE; expr = xpr; (*count)++;};
};

class StmtEvent : public Stmt {
public:
  Expr *expr;

  ~StmtEvent();
  inline StmtEvent(Expr *xpr,long *count)
                                  {typ2 = STMT_EVENT; expr = xpr; (*count)++;};
};

class StmtQC : public Stmt {
public:
  poolRef  lang;
  String  *str;

  ~StmtQC();
  inline StmtQC(poolRef l,String  *s,long *count)
                              {typ2 = STMT_QC; lang = l; str = s; (*count)++;};
};

class Func : public PrtdObj {
public:
  AttrObj  *parent;
  Stmt     *p_stmt,
           *stmts;
  char      glob;
  FuncRet   ret:8;
  Expr     *range;

  ~Func();
  Func(ContextObj *,const Token **,const Token *,StmtFunc *);
};

class Task : public PrtdObj {
public:
  AttrObj  *parent;
  Stmt     *p_stmt,
           *stmts;
  char      glob;

  ~Task();
  Task(ContextObj *,const Token **,const Token *,StmtTask *);
};

#endif

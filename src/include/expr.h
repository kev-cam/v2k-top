/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  expr_h_rcsid
#define expr_h_rcsid() {return "$Id: expr.h,v 1.28 2007/03/28 07:23:27 dkc Exp $";} /* RCS ID */
 
  Value    value;  //!< Node Value

protected:
  EXPR_SUB left;   //!< Operator LHS
  EXPR_SUB right;  //!< Operator RHS
#undef EXPR_SUB

public:

#undef EXPR
#undef PARM_EXPR

#ifdef EXPR_ALIAS
  static deAliaser *deAliasers;
  bool deAlias();
  void addDeAlias(deAliasFn);
#endif

  void clear();
  ~Expr() {clear();};

  Expr();
  Expr(poolRef);
  Expr(poolRef,XData);
  Expr(int);
  Expr(unsigned int);
  Expr(szdInt);
  Expr(U64);
  Expr(U64,XData,void*);
  Expr(poolRef *,XData,int);
  Expr(double);
  Expr(OPR, Expr *, Expr *);
  Expr(BaseExpr *, Expr *, Expr *);
  Expr(Expr *, Expr *, Expr *, VlTypXtra);
  Expr(eBX);
  Expr(eBX,int);
  Expr(VlTypXtra);

  void  copy(Expr *);
  void  copyUnique(Expr *);
  void  setXd(int);
  XData Xd() const;

  inline Expr(Expr *xpr) {copy(xpr);};

#undef NON_EXPR_LOC
#ifndef Expr

  bool  fixContrib(ContextObj *);
  Expr *collapseHier(ContextObj *,VlTypXtra);
  bool  collectNames(Expr *);
  eDIFF diff(Expr *);
  int   countLeaves();
  Expr *alias();

  inline bool same(Expr *x)                 {return DIFF_SAME == diff(x);};
  inline void reset(BaseExpr *b,poolRef nm) {FREE(value.hrf);
                                             *bx() = *b; value.ref = nm;
                                             left  = right = 0;};

  inline const Expr *Right() const {return right;}
  inline const Expr *Left()  const {return left;}

  inline void *pntr() const
  {
    return val()->ptr;
  }

# define EXPR(x)         ((Expr *)(x))
# define PARM_EXPR       Expr
# define NON_EXPR_LOC(p) &(p)

#else

# define EXPR(x)         ((THIS_EXPR *)(x))
# define PARM_EXPR       plExpr
# define NON_EXPR_LOC(p) &(p.ptr)

#endif

  Expr *eval(VlTypXtra,ContextObj *,eRFF,eREF);
  int   makeCall(Inst *,plExpr *,int,Expr *);
  int   bitSelect(PARM_EXPR *,PARM_EXPR *);
  int   nCat(PARM_EXPR *,PARM_EXPR *);

  inline Expr     *eval(VlTypXtra vtyp)    {return eval(vtyp,0,
                                                        RFF_ALL,REF_UNKNOWN);};
  inline BaseExpr *bx()                    {return (BaseExpr *)this;};
  inline void      set_bx(eBX cbe)         {*bx() = *(BaseExpr *)&CBE[cbe];};
  inline void      set_val(plValue *v)     {value = *(Value *)v;};
  inline int       rng_wdth()              {assert(VT_RANGE == typ);
                                            int i = value.r[1] - value.r[0];
                                            return i > 0 ? 1 + i
					                 : 1 - i;};
  inline int       max(int a,int b)        {return a > b ? a : b;};

  const char *strValue(String *);

  double eval_dbl() const;
  U64    eval_U64() const;
  I64    eval_I64() const;
  U64    dbl2U64()  const;
  I64    dbl2I64()  const;

  int coerce(BaseExpr *,Value *,void **,void **);
  inline int coerce(BaseExpr *b,Value *v) {return coerce(b,v,0,0);};

  eREF unambigous(VerilogObj *,eRFF);

  inline VlType         vt()   const {return (VlType)typ;};
  inline VlTypXtra      vtx()  const {return (VlTypXtra)xtra;};
  inline Value         *pval()       {return &value;};
  inline const Value   *val()  const {return &value;};
  inline const ValueFn *vf()   const {return (ValueFn *)&value;};

  inline Expr       *unalias() const {Expr *start = (Expr *)this;
                                      while (start->vt() == VT_ALIAS) {
                                        start = (Expr *)start->pntr();}
                                      return start;};

#if defined(INLINE_ALL_EXPR) || !defined(LD_BUG_01)
# define EXPR_IL_CLASS
# include "expr_il.h"
#else
  double dbl() const;
  I64    i64() const;
#endif

  inline unsigned int  tru()  const {return 0 != u64();};

  inline unsigned int  u32()  const {U64          ll = u64();
                                     unsigned int i  = ll;
                                     ll             -= i; assert (!ll);
                                     return i;};
  inline U64           u64()  const {switch(typ)
                                     {default:          return 1/zero;
                                      case VT_OPERATOR: return eval_U64();
                                      case VT_U64:      return value.u64;
                                      case VT_I64:      return value.i64;
                                      case VT_LUINT:    return value.lgc.i;
				      case VT_BOOL:
                                      case VT_UINT:     return value.u;
                                      case VT_INT:      return value.i;
                                      case VT_DOUBLE:   return dbl2U64();
                                      case VT_ALIAS:    return unalias()->u64();}};
  inline int           i32()  const {I64 ll = i64();
                                     int i  = ll;
                                     ll    -= i; assert (!ll);
                                     return i;};

  inline int  *nonXleft()  const {return (int *)NON_EXPR_LOC(left);}
  inline int  *nonXright() const {return (int *)NON_EXPR_LOC(right);}
  inline int   lgcWdth()   const {int lx = *nonXleft(),
				      lw = lx ? IWIDTH(lx)
                                              : value.lgc.w;
                                  return lw;}

  inline typDesc  *ptd()         {return &value.td;};
  inline poolRef  *pref()        {return &value.ref;};
  inline poolRef   ref()   const {return  value.ref;};
  inline rrfTup   *prrf()        {return &value.rrf;};
  inline rrfTup    rrf()   const {return  value.rrf;};
  inline refTup   *prtp()        {return &value.rtp;};
  inline refTup    rtp()   const {return  value.rtp;};
  inline OPR       op()    const {return VT_OPERATOR == typ ? value.op
                                                            : OPR_NULL;};
  inline Operator *opr()   const {return &Operator::oprs[op()];};

  inline void      dispose() {if (this && !(xtra&VTX_INUSE)) delete(this); };
  inline void      destroy() {if (this && !(xtra&VTX_PERM)) {
                                xtra = VL_TYP_XTRA(xtra|VTX_RM);
                                delete(this);}};

#undef Expr

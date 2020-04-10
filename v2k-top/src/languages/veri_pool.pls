# Copyright (c) 1998,1999,2001,2002,2003 Kevin Cameron
# Distributed under the GNU Lesser General Public License
# RCS ID: $Id: veri_pool.pls,v 1.37 2007/01/13 03:44:14 dkc Exp $
#


UNION plValue
  poolRef       ref;
  refTup        rtp;
  rrfTup        rrf;
  szdInt        lgc;
  int           i;
  typDesc       td;
  int           r[2];
  U32           u;
  double        d;
  OPR           op;
  I64           i64;
  U64           u64;
  IREF(void)    data;
  IREF(plExpr)  xpr;
  IREF(poolRef) hier;

STRUCT plBaseExpr
  VlType       typ:8;
  VlTypXtra    xtra:8;
  eREF         rtyp:8;
  CllType      call:8;

STRUCT plUsage
  eUSG         use:16;
  IREF(plExpr) xpr;

STRUCT plQzx
  U64 n; /* Used as base for array */
  U64 q;
  U64 z;
  U64 x;

STRUCT plExpr
  STRUCT       plBaseExpr
  plValue      value;
  IREF(void)   left;
  IREF(void)   right;

STRUCT DrvStrnth
  Strength     strnth:8;
  char         lvl;

UNION plStmtAny;

STRUCT plStmtHeader
  eSTMT        typ:8;
  short        file_id;
  I32          line;

STRUCT plStmtAnalog
  STRUCT          plStmtHeader
  IREF(plStmtAny) child;
  poolRef         name;
  I32             usages
  IREF(plUsage)   usage;

STRUCT plStmtDecl
  STRUCT          plStmtHeader
  I32             stmts;
  IREF(plStmtAny) stmt;
  IREF(plPrtdObj) lcls;

STRUCT plStmtBlock
  STRUCT          plStmtDecl

STRUCT plStmtFor
  STRUCT          plStmtHeader
  IREF(plExpr)    ass1;
  IREF(plExpr)    expr;
  IREF(plExpr)    ass2;
  IREF(plStmtAny) child;

STRUCT plStmtGen
  STRUCT          plStmtHeader
  poolRef         cntr;
  IREF(plExpr)    expr;
  IREF(plStmtAny) child;

STRUCT plStmtIf
  STRUCT          plStmtHeader
  IREF(plExpr)    expr;
  IREF(plStmtAny) child_t;
  IREF(plStmtAny) child_f;

STRUCT plStmtIfNone
  STRUCT          plStmtHeader
  IREF(plExpr)    expr;
  IREF(plStmtAny) child_t;
  IREF(plStmtAny) child_f;

STRUCT plCaseS
  IREF(plExpr)    expr;
  IREF(plStmtAny) child;

STRUCT plStmtCase
  STRUCT          plStmtHeader
  I32 	          casess;
  IREF(plCaseS)   VAR_ARRAY_PTR(cases);

STRUCT plStmtCaseX
  STRUCT          plStmtHeader
  I32 	          casess;
  IREF(plCaseS)   VAR_ARRAY_PTR(cases);

STRUCT plStmtCaseZ
  STRUCT          plStmtHeader
  I32 	          casess;
  IREF(plCaseS)   VAR_ARRAY_PTR(cases);

STRUCT plStmtExpr
  STRUCT          plStmtHeader
  IREF(plExpr)    expr;

STRUCT plStmtDefparam
  STRUCT          plStmtHeader
  IREF(plExpr)    expr;

STRUCT plStmtAlways
  STRUCT          plStmtHeader
  IREF(plStmtAny) child;
  poolRef         name;
  I32             usages
  IREF(plUsage)   usage;

STRUCT plStmtForever
  STRUCT          plStmtHeader
  IREF(plStmtAny) child;

STRUCT plStmtSpec
  STRUCT plStmtDecl

STRUCT plStmtFork
  STRUCT plStmtDecl

STRUCT plStmtRepeat
  STRUCT          plStmtHeader
  IREF(plExpr)    expr;
  IREF(plStmtAny) child;

STRUCT plStmtWhile
  STRUCT          plStmtHeader
  IREF(plExpr)    expr;
  IREF(plStmtAny) child;

STRUCT plStmtInit
  STRUCT          plStmtHeader
  IREF(plStmtAny) child;
  poolRef         name;
  I32             usages
  IREF(plUsage)   usage;

STRUCT plStmtAssign
  STRUCT          plStmtHeader
  IREF(plExpr)    expr;
  IREF(plExpr)    delay;
  DrvStrnth       ds[2];
  poolRef         name;
  I32             usages
  IREF(plUsage)   usage;

STRUCT plStmtDeassign
  STRUCT          plStmtHeader
  IREF(plExpr)    expr;

STRUCT plStmtDisable
  STRUCT          plStmtHeader
  IREF(plExpr)    expr;

STRUCT plStmtForce
  STRUCT          plStmtHeader
  IREF(plExpr)    expr;

STRUCT plStmtRelease
  STRUCT          plStmtHeader
  IREF(plExpr)    expr;

STRUCT plStmtWait
  STRUCT          plStmtHeader
  IREF(plExpr)    expr;
  IREF(plStmtAny) child;

STRUCT plStmtAt
  STRUCT          plStmtHeader
  IREF(plExpr)    expr;
  IREF(plStmtAny) child;

STRUCT plStmtDelay
  STRUCT          plStmtHeader
  IREF(plExpr)    expr;
  IREF(plStmtAny) child;

STRUCT plStmtEvent
  STRUCT          plStmtHeader
  IREF(plExpr)    expr;

STRUCT plStmtInst
  STRUCT          plStmtHeader
  poolRef         name;
  poolRef         drv;
  char            udp;
  char            prim;
  DrvStrnth       ds[2];
  IREF(plExpr)    param;
  IREF(plExpr)    inst;

STRUCT plStmtTask
  STRUCT        plStmtHeader
  I32           task_indx;

STRUCT plStmtFunc
  STRUCT        plStmtHeader
  I32           func_indx;

STRUCT plStmtFuncA
  STRUCT        plStmtHeader
  I32           func_indx;

STRUCT plStmtQC
  STRUCT        plStmtHeader
  poolRef       lang;
  STRUCT        plStr

UNION plStmtAny
  plStmtAlways  	Always;
  plStmtAnalog  	Analog;
  plStmtAssign  	Assign;
  plStmtAt		At;
  plStmtBlock   	Block;
  plStmtCase		Case;
  plStmtCaseX		CaseX;
  plStmtCaseZ		CaseZ;
  plStmtDeassign	Deassign;
  plStmtDecl		Decl;
  plStmtDefparam	Defparam;
  plStmtDelay		Delay;
  plStmtDisable		Disable;
  plStmtEvent		Event;
  plStmtExpr		Expr;
  plStmtFor		For;
  plStmtForce		Force;
  plStmtForever		Forever;
  plStmtFork		Fork;
  plStmtFunc		Func;
  plStmtGen		Gen;
  plStmtHeader		Header;
  plStmtIf		If;
  plStmtIfNone		IfNone;
  plStmtInit		Init;
  plStmtInst		Inst;
  plStmtQC		QC;
  plStmtRelease		Release;
  plStmtRepeat		Repeat;
  plStmtSpec		Spec;
  plStmtTask		Task;
  plStmtWait		Wait;
  plStmtWhile		While;

STRUCT plScope
  poolRef         name;
  IREF(plStmtAny) stmt;
  int             up;
  int             dpth;

STRUCT plAttr
  AttrType typ:8;
  char     pool;
  short    index;

STRUCT plAttrU
  STRUCT  plAttr
  poolRef nm;

STRUCT plAttrAnyU
  STRUCT  plAttrU
  plValue value;

STRUCT plAttrAny
  STRUCT  plAttr
  plValue value;

STRUCT plAttrAnyU2
  STRUCT  plAttrAnyU
  plValue value2;

STRUCT plRange
  char         exclude;
  char         inc_strt;
  char         inc_fnsh;
  IREF(plExpr) range;

STRUCT plRngdAttr
  STRUCT        plAttrAnyU
  int           rngs;
  IREF(plRange) VAR_ARRAY_PTR(rng);

STRUCT plAttrObj
  poolRef          name;
  eAOtype          typ:8;
  char             typ2;
  int              attrs;
  IREF(plRngdAttr) VAR_ARRAY_PTR(attr);

STRUCT plPort
  STRUCT        plAttrObj;
  PortDir       io:16;
  PortTyp       ptyp:8;
  SigType       styp:8;
  Charge        chrg:8;
  Strength      ds0:8;
  Strength      ds1:8;
  int           value:8;
  int           pckd:8;
  int           unpckd:8;
  poolRef       disc;
  IREF(poolRef) VAR_ARRAY_PTR(rng);
  IREF(plExpr)  dly;

STRUCT plBranch
  IREF(plExpr)  p1;
  IREF(plExpr)  p2;
  IREF(poolRef) name;

STRUCT plUnknown
  int      scp;
  eRFF     rff;
  eREF     rtyp;
  poolRef  nm;

STRUCT plPrtdObj
  poolRef          name;
  I32              ports;
  I32              scopes;
  I32              parms;
  I32              attrs;
  I32              branchs;
  I32              stmts;
  I32              usages
  IREF(plPort)     VAR_ARRAY_PTR(port);
  IREF(plScope)    VAR_ARRAY_PTR(scope);
  IREF(plRngdAttr) VAR_ARRAY_PTR(parm);
  IREF(plRngdAttr) VAR_ARRAY_PTR(attr);
  IREF(plBranch)   VAR_ARRAY_PTR(branch);
  IREF(plStmtAny)  VAR_ARRAY_PTR(stmt);
  IREF(plUsage)    VAR_ARRAY_PTR(usage);

STRUCT plFunc
  STRUCT plPrtdObj
  FuncRet         ret:8;
  IREF(plExpr)    range
  XREF(plAttrObj) parent;

STRUCT plTask
  STRUCT plPrtdObj
  XREF(plAttrObj) parent;

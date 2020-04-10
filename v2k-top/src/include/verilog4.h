/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  verilog4_h_rcsid
#define verilog4_h_rcsid() {return "$Id: verilog4.h,v 1.50 2009/07/08 08:37:43 dkc Exp $";} /* RCS ID */

 
/*! \file
    Access to in-mem or saved statements [for elaboration etc.]
*/

#ifndef VERILOG4_H
#define VERILOG4_H

#include "models.h"
#include "resolve.h"
#include "cg.h"

typedef enum {
  LS_NOT_FOUND = 0,
  LS_FOUND,
  LS_LOADED
} eLibSrch;

class Inst;

class VStmt {
public:
  Module *mod;
  int     index;
#undef VSTMT_VMODE
#define VSTMT_VMODE =0
#define NEW_XTRA
#include "vstmt.inc"

  static int StmtSize[];
};

#undef VSTMT_VMODE
#define VSTMT_VMODE
class VStmtSaved : public VStmt {
public:
  plStmtAny  stmt;
  plExpr    *get_parm(int);
#define NEW_XTRA stmt.Header.typ = STMT_NULL;
#define VStmt VStmtSaved
#include "vstmt.inc"
};

class VStmtInMem : public VStmt {
public:
  Stmt *stmt;
#define NEW_XTRA stmt = 0;
#define VStmt VStmtInMem
#include "vstmt.inc"
};
#undef VSTMT_VMODE

typedef struct PrmOvrd_s {
  struct PrmOvrd_s *next;
  int               index,
                    inst;
  Inst             *dfd_frm;
  BaseExpr          bx;
  Value             val;
} PrmOvrd;

typedef struct {
  union {
    eRFF need;
    eREF got;
  }      typ;
  int    scp,
         index;
} XmrData;

typedef struct Xmr_s {
  struct Xmr_s *next;
  Inst         *ip;
  int           index;
  XmrData       xmr;
} Xmr;

typedef struct {
  BITF(eBIND,typ,8,char);
  int    index:24;
  long   offset;
} PrtBnd;

typedef struct {
  Inst     *owner;
  PortInfo  target;
} UnkBnd;

typedef struct sParmRng {
  char     exclude,
           inc_strt,
           inc_fnsh;
  BaseExpr start_b,
           finish_b;
  Value    start_v,
           finish_v;
} ParmRng;

typedef void *(*InstCllBk)(Inst *,void *);
typedef void *(*SigCllBk)(Inst *,void *,int,int);
typedef void *(*ParmCllBk)(Inst *,void *,int);

class mod_model : public model,public VerilogObj {
public:
  long index;

  inline mod_model(long idx) {index = idx;};

#include "model.vfn"
};

class udp_model : public model,public VerilogObj {
public:
  long index;

  inline udp_model(long idx) {index = idx;};
#include "model.vfn"
};

class root_model : public model {
public:
#include "model.vfn"
};

class unm_model : public model {
public:
#include "model.vfn"
};

typedef enum {
  GLBL_XMR   = 0,
  GLBL_0     = 1,
  GLBL_1     = 2,
  GLBL_WIRES = 256
} eGLBL;

class CgData;
class SimData;
class Inst : public AttrObj {
  long           count;
  unsigned char *imr;
  model         *self;
  int           *extra;
  char           x_rng,
                 x_copy,
                 x_auto;
public:
  char           behavioral,
                 structural,
                 bnd_allcd;
  long           children,
                 stmt_id,
                *dp_stmt;
  int            parms,
                 dp_idx,
                 depth,
                 prt_wires,
                 lcl_wires,
                 imr_last,
                 glbl_strt,
                 sim_space;
  Inst          *next,
                *parent,
                *child,
                *next_dfp;
  int            po_count;
  PrmOvrd       *po;
  Xmr           *xmr;
  RngVec        *rv;
  int            rv_count;
  union {
    int          xpr;
    PrtBnd      *tbl;
  } bind;
  UnkBnd        *unk_tbl;
  CgData        *cg_data;
  SimData       *sim_data;

  static long        modules,
                     prims,
                     udps,
                     expanded,
                     def_prm_tot,
                     Depth,
                     MaxDepth,
                     Wires,
                     Unconnected,
                     NC0;
  static String     *Path;
  static Inst       *Root,
                    *def_prm,
                   **def_prm_nxt;
  static int         RunTime,
                     ElabDone;
  static const char *Sep,
                    *RootName;
  static char       *RootModName;
  static unm_model  *Unmatched;

  ~Inst();
  Inst(model *mp);
  Inst(long id);

  inline model  *Self()            {return self;};
  inline void    SetSelf(model *s) {self = s;};
  inline int     incr()            {return ++count;};
  inline int     index()           {return count;};
  inline int     mod_id()          {assert(REF_MODULE == self->Type());
                                    return self->Index();};
  inline Module *pmod()            {return module_map(mod_id());};
  inline bool    runtime()         {return RunTime;};
  inline void    noBindTbl()       {FREE(bind.tbl);};

  inline long    lstWire()         {return Wires;};
  inline void    incWires()        {Wires += lcl_wires + prt_wires;};
  inline model  *unmatched()       {if (!Unmatched) { Unmatched = new unm_model;}
                                    self = Unmatched; return Unmatched;};
  inline int     Imr(int index)    {return (index >= imr_last) ? 0 : imr[index];}

  int            setExtra(char *,int,int sz = 1,int offset = 0);
  int           *cloneExtra();
  int           *resetExtra();

  inline int     setCopy(int r)    {return setExtra(&x_copy,r);};
  inline int     rngSlv()          {return x_copy > 0 ? extra[x_copy]
				                      : 0; };
  inline Inst   *master()          {return &this[-rngSlv()];};

  inline int     setRngInc(int r)  {return setExtra(&x_rng,r,3);};
  inline int     rngIncSub()       {return x_rng > 0 ? extra[x_rng]
				                     : x_rng; };
  inline int     rngInc()          {return master()->rngIncSub();};

  inline int     setRngIdx (int r) {return setExtra(&x_rng,r,3,1);};
  inline int     rngIdxSub()       {return x_rng > 0  ? extra[x_rng + 1]
				                      : 0; };
  inline int     rngIdx0()         {return master()->rngIdxSub();};
  inline int     rngIdx()          {Inst  *ip0 = master();
                                    return ip0->rngIdxSub()
                                             + (rngSlv() * ip0->rngIncSub());};

  inline int     setRngMax(int r)  {return setExtra(&x_rng,r,3,2);};
  inline int     rngMaxSub()       {return x_rng > 0  ? extra[x_rng + 2]
				                      : 0; };
  inline int     rngMax()          {return master()->rngMaxSub();};

  inline int     setAuto(int r)    {return setExtra(&x_auto,r);};
  inline int     AutoName()        {return x_auto > 0 ? extra[x_auto]
				                      : x_auto; };

  int rngCnt();

  const char *path();
  const char *path(String *);
  const char *iname(String *);
  long        build();
  PrtBnd     *makeBindTbl(int);
  int         addNC(int);
  void        fixWires();
  int         bindPorts();
  void        makeSigInfo();
  int         getSigInfo();
  void        setImrFlags(int,int);
  void        addImrFlags(int,int);
  int         getImrFlags(int);
  RngVec     *extraRv(int);
  RngVec     *findRv(int);
  eLibSrch    findLibMod(poolRef);
  void        evalDfPrm(Module *,int);
  int         evalPortDim();
  Inst       *findXmr(int,poolRef *,XmrData *,eXMR);
  Inst       *haveXmr(int,XmrData *);
  void        logXmr(int,Inst *,XmrData *);
  int         setParm(XmrData *,Expr *,Inst *);
  int         findOvrrd(int);
  int         getPortInfo(poolRef,PortInfo *,int);
  poolRef     itemName(eREF,int);
  int         bindUnk(Inst *,PrtBnd *,PortInfo *);
  void        reportPosn(Inst *);
  int         portWidthInst(int,PortTyp *ptp = 0,RngVec **rvp_r = 0);
  int         sliceWidth(RngVec *,rngEval *,int);
  plPort     *getPort(int,int *pool = 0);
  PortDir     portMode(plPort *,int);
  PortDir     portMode(Port *,int);
  void       *forChildren(InstCllBk,void *);
  void       *forSigs(SigCllBk,void *);
  void       *forParms(ParmCllBk fn,void *dp);
  void       *forSpec(int *,int,const char **,InstCllBk,void *);
  void       *forSigSpec(int *,int,const char **,SigCllBk,void *);
  int         derefPort(PrtBnd *);
  int         derefSig(int,int,int,Inst *);
  int         derefUnk(int,int);
  int         derefXpr(int,int);
  void        delChildren();
  void        warnMixed(Inst *,poolRef);
  void        fillNC(int,int);
  int         getPortDisc(int);
  void        setSigInfo(SigInfoCllctr *,int,int,DrvInfo,int,int,int,void *,
                         int);
  eCG         codeGen(Module *,Stream *,Stream *);
  eCG         codeGen(bool,eCodeMode);
  eSTS        Compile(bool,bool dbg = 0);
  eSTS        initSim(eSTS sts = STS_NREADY);
  int         simSpace(int,int);
  int         simSize();
  const char *cppPortType(int,char *,char *unpkd = 0);

 private:
  Inst();
  void init_all();
};

class ModRef {
public:
  ModRef *next;
  int     id;

  inline  ModRef(int idx) {id = idx; next = 0;};

  inline void recycle() {delete this;}
};

int portWidth(RngVec *,int *dimn = 0);

typedef enum {
  LPM_RANGE    =   1,
  LPM_C        =   2
} eCBD_LPM;

void *prntParm(Stream *,Inst *,int,const char **,int *,int);

#endif

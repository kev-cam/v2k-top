/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * elaborate_cpp_rcsid() {return "$Id: elaborate.cpp,v 1.90 2012/10/16 22:38:45 cvs Exp $";}

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
#define  NEED_VERILOG
#define  NEED_COMMANDS
#include "tokpool.h"
#include "dyn.h"
#include "verilog.h"
#include "dump.h"
#include "mod_pool.h"
#include "shell.h"

#if (defined(PERL_LIB) || !defined(DYNLOADING))
extern
#endif
       GateLib *V2KgateLib;

extern "C" void V2KsetGateLib(GateLib *gp)
{
  V2KgateLib = gp;
}

long       Inst::modules,
           Inst::prims,
           Inst::udps,
           Inst::expanded,
           Inst::def_prm_tot,
           Inst::Depth,
           Inst::Wires        = GLBL_WIRES,
           Inst::Unconnected,
           Inst::NC0,
           Inst::MaxDepth;

int        Inst::RunTime,
           Inst::ElabDone;

String    *Inst::Path;

Inst      *Inst::Root,
          *Inst::def_prm,
         **Inst::def_prm_nxt = &Inst::def_prm;

const char *Inst::Sep         = ".",
           *Inst::RootName    = "";
char       *Inst::RootModName;

unm_model *Inst::Unmatched;

class SIsmall : public SigInfoCllctr {
public:
  SigInfo **SI;

  virtual void      setSigInfo(void *,int,int,DrvInfo,int,int,int,void *);
  virtual SigInfo **getSigInfo(void *,int,int);
  virtual SigInfo  *getSigInfo(void *,int);

  virtual ~SIsmall();

  inline SIsmall(SigInfo **p) { SI = p; }
};

class SIstrength : public SigInfoCllctr {
public:
  char *SI;

  SigInfo *Hash[0x10000];

  virtual void      setSigInfo(void *,int,int,DrvInfo,int,int,int,void *);
  virtual SigInfo **getSigInfo(void *,int,int);
  virtual SigInfo  *getSigInfo(void *,int);
  virtual ~SIstrength();

  inline SIstrength(char *p) { SI = p; BZEROS(Hash); }

  inline int hashIdx(void *ip,int pid,int si) {return si & (sizeof(Hash) -1);}
};

class SI4level : public SigInfoCllctr {
public:
  char *SI;

  SigInfo *Hash[0x10000];

  virtual void      setSigInfo(void *,int,int,DrvInfo,int,int,int,void *);
  virtual SigInfo **getSigInfo(void *,int,int);
  virtual SigInfo  *getSigInfo(void *,int);
  virtual ~SI4level();

  inline SI4level(char *p) { SI = p; BZEROS(Hash); }

  inline int hashIdx(void *ip,int pid,int si) {return si & (sizeof(Hash) -1);}
};

class SI2level : public SigInfoCllctr {
public:
  char *SI;

  SigInfo *Hash[0x10000];

  virtual void      setSigInfo(void *,int,int,DrvInfo,int,int,int,void *);
  virtual SigInfo **getSigInfo(void *,int,int);
  virtual SigInfo  *getSigInfo(void *,int);
  virtual ~SI2level();

  inline SI2level(char *p) { SI = p; BZEROS(Hash); }

  inline int hashIdx(void *ip,int pid,int si) {return si & (sizeof(Hash) -1);}
};


SigInfoCllctr *SIcllctr;

extern "C" {
  int V2kAuto,
      V2kPortMode,
      V2kWireMode,
      V2kUnmatchedOK;
}

int VStmt::StmtSize[] = {
 0,
#define STATEMENT(e,s,m) sizeof(pl##s),
#include "statement.inc"
 0
};

class HierShell : public Shell {
public:
  Inst   *curr_inst;
  String  path_buff;

  virtual eSTS parse(int,const char **,char *,char *,Stream **,Stream **,eSHX);

  HierShell(Shell **);
};

static int Missing;

int         root_model::Bind(void *,int,int *,prtCB) const { return 0; }
void        root_model::setPorts(int)                      {}
PortDir     root_model::portMode(int)                const { return PRT_NC;  }
PortTyp     root_model::portType(int)                const { return PT_NONE; }
bool        root_model::Xtra(void *)                 const { return 0; }
const char *root_model::procLabel(void *,int,char *) const { return ""; }
eCX         root_model::cmpXtra(void *,void *)       const { return CX_BAD; }
eREF        root_model::Type()                       const { return REF_ROOT; }
int         root_model::Index()                      const { return -1; }
eDM         root_model::drvMode()                    const { return DM_BAD; }
int         root_model::Wires(void *,int)            const { return 0; }
int         root_model::PortWidth(void *,PortInfo *,int)
                                                     const { return 0; }
void        root_model::getSigInfo(void *vp,eREF rft,int pi,int p,int s)
                                                     const {}
void        root_model::Unneeded()                   const { DELETE(SIcllctr); }

int         unm_model::Bind(void *,int,int *,prtCB)  const { return 0; }
void        unm_model::setPorts(int)                       {}
PortDir     unm_model::portMode(int)                 const { return PRT_NC; }
PortTyp     unm_model::portType(int)                 const { return PT_NONE; }
bool        unm_model::Xtra(void *)                  const { return 0; }
const char *unm_model::procLabel(void *,int,char *)  const { return ""; }
eCX         unm_model::cmpXtra(void *,void *)        const { return CX_BAD; }
eDM         unm_model::drvMode()                     const { return DM_BAD; }
eREF        unm_model::Type()                        const {
                                                        return REF_UNMATCHED; }
int         unm_model::Index()                       const { return -1; }
int         unm_model::Wires(void *vp,int w)         const { return w; }
int         unm_model::PortWidth(void *,PortInfo *,int)
                                                     const { return 0; }
void        unm_model::getSigInfo(void *vp,eREF rft,int pi,int p,int s)
                                                     const {}
void        unm_model::Unneeded()                    const {}

int mod_model::Bind(void *,int,int *,prtCB) const
{
  assert(0);
}

void mod_model::setPorts(int)
{  
}

PortDir mod_model::portMode(int p_idx) const
{
  Module *mod = module_map(index);

  if (mod->saved) {
    return PoPool(mod->saved)->portMode(0,p_idx);
  } else {
    assert(0);
  }
}

PortTyp mod_model::portType(int p_idx) const
{
  Module *mod = module_map(index);

  if (mod->saved) {
    return PoPool(mod->saved)->portType(0,p_idx);
  } else {
    assert(0);
  }
}

const char *mod_model::procLabel(void *,int st_idx,char *buf) const
{
  Module  *mod = module_map(index);
  poolRef  ref = mod->stmtLabel(st_idx);

  if (!NULL_REF(ref)) return strDeref(ref);

  sprintf(buf,"#%d",st_idx);
  return buf;
}

bool mod_model::Xtra(void *dsc) const
{
  intptr_t idx = (intptr_t)dsc;

  return idx >= 0;
}

eDM mod_model::drvMode() const
{
  return DM_FLAGS;
}

eCX mod_model::cmpXtra(void *d1,void *d2) const
{
  if (d1 != d2) return CX_DIFF;

  return CX_MERGE;
}

int mod_model::Index() const
{
  return index;
}

int mod_model::Wires(void *vp,int w) const
{
  Inst *ip = (Inst *)vp;

  if (w < 0) return ip->prt_wires;

  if (w != ip->prt_wires) {
    String path;
    eSTS   sts;

    ip->path(&path);
    if (w > ip->prt_wires) {
      if (V2kPortMode & PORT_NOEXCESS) sts = S_ERROR(STS_ELAB);
      else                             sts = S_WARNING(STS_ELAB);
      ErrorMsg(sts,"Too many connections for %s (%d vs %d)",
                   path.str(),w,ip->prt_wires);
      return ip->prt_wires;
    }
    if (V2kPortMode & PORT_NODANGLE) sts = S_ERROR(STS_ELAB);
    else                             sts = S_WARNING(STS_ELAB);
    ErrorMsg(sts,"Too few connections for %s (%d vs %d)",
                 path.str(),w,ip->prt_wires);
  }

  return w;
}

int mod_model::PortWidth(void *vp,PortInfo *pi,int width) const
{
  Inst *ip = (Inst *)vp;

  ip->getPortInfo(NullRef,pi,PRT_BNDSIG);

  if (pi->width < width) {
    width = pi->width;
  }

  return width;
}

void mod_model::getSigInfo(void *vp,eREF rft,int pi,int prts,int si) const
{
  Inst   *ip  = (Inst *)vp;
  Module *mod = module_map(index);

  if (mod->saved) {
    return PoPool(mod->saved)->getSigInfo(ip,rft,pi,prts,si);
  } else {
    assert(0);
  }
}

void mod_model::Unneeded() const
{
  mod_model *mp = (mod_model *)this;

  delete mp;
}

int udp_model::Bind(void *,int,int *,prtCB) const
{
  assert(0);
}

void udp_model::setPorts(int)
{
}

PortDir udp_model::portMode(int p_idx) const
{
  Prim *udp = prim_map(index);

  if (udp->saved) {
    return PoPool(udp->saved)->portMode(0,p_idx);
  } else {
    assert(0);
  }
}

PortTyp udp_model::portType(int p_idx) const
{
  Prim *udp = prim_map(index);

  if (udp->saved) {
    return PoPool(udp->saved)->portType(0,p_idx);
  } else {
    assert(0);
  }
}

const char *udp_model::procLabel(void *,int,char *) const
{
  return "";
}

bool udp_model::Xtra(void *) const
{
  return 0;
}

eCX udp_model::cmpXtra(void *,void *) const
{
  return CX_BAD;
}

int udp_model::Index() const
{
  return index;
}

eDM udp_model::drvMode() const
{
  return DM_FLAGS;
}

int udp_model::Wires(void *vp,int w) const
{
  Inst *ip = (Inst *)vp;

  if (w < 0) return ip->prt_wires;

  if (w != ip->prt_wires) {
    String path;
    eSTS   sts;
    ip->path(&path);
    if (w > ip->prt_wires) {
      if (V2kPortMode & PORT_NOEXCESS) sts = S_ERROR(STS_ELAB);
      else                             sts = S_WARNING(STS_ELAB);
      ErrorMsg(sts,"Too many connections for %s (%d vs %d)",
                   path.str(),w,ip->prt_wires);
      return ip->prt_wires;
    }
    if (V2kPortMode & PORT_NODANGLE) sts = S_ERROR(STS_ELAB);
    else                             sts = S_WARNING(STS_ELAB);
    ErrorMsg(sts,"Too few connections for %s (%d vs %d)",
                 path.str(),w,ip->prt_wires);
  }

  return w;
}

int udp_model::PortWidth(void *vp,PortInfo *pi,int width) const
{
  Inst *ip = (Inst *)vp;
  int   w  = ip->prt_wires;

  return w - pi->used >= width ? width
                               : w - pi->used;
}

void udp_model::getSigInfo(void *vp,eREF rft,int pi,int prts,int si) const
{
  Inst *ip  = (Inst *)vp;
  Prim *udp = prim_map(index);

  if (udp->saved) {
    return PoPool(udp->saved)->getSigInfo(ip,rft,pi,prts,si);
  } else {
    assert(0);
  }
}

void udp_model::Unneeded() const
{
  udp_model *mp = (udp_model *)this;

  delete mp;
}

extern "C" eSTS setTopMod(const char *tmn)
{
  Free(Inst::RootModName);
  strcpy(Inst::RootModName = (char *)Malloc(strlen(tmn)+1),tmn);

  return STS_NORMAL;
}

eSTS elaborate(int flags,int *all_done)
{
  Inst       *ti   = 0;
  InstRef    *rf   = ti->instLst()->first();
  int         id   = 0,
              n    = 0,
              d;
  Module     *mod;
  String      tmp_path;
  ModRef     *mr;
  eSTS        sts  = STS_NORMAL;

  for (d = ELAB_GTSI; ! (d & flags) ; d >>= 1);
  while (d) flags |= ( d >>= 1);

  if ((flags & ELAB_TOP) && !(Inst::ElabDone & ELAB_TOP))
  {
    prsVMdone();

    ti = Inst::Root = new Inst(new root_model);

    ti->Path = &tmp_path;
    ti->SetSelf(new root_model());
    ti->name = strFind(Inst::RootName);

    for (; rf ; rf = rf->next,id++) {
      poolRef nm  = rf->name;
      refTup  rt;
      if (ti->findGlobObj(RFF_MP,nm,&rt)) switch (rt.typ) {
      case REF_MODULE: mod           = ti->module_map(rt.index);
                       mod->ref_indx = id;
      default:         break;
      }
    }

    for (id = 0 ; (mod = ti->module_map(id)) ; id++)
    {
      if (Inst::RootModName) {
        const char *nm = strDeref(mod->name);
        if (0 == strcmp(Inst::RootModName,nm)) {
          ti->topLst()->add(new ModRef(id));
          ti->Root->children++;
        }
      } else if (mod->ref_indx < 0) {
        const char *hghlt = "<<<<<";
        if (mod->macro) {
          hghlt = "[Macro]";
//      } else if (mod->vmode & (VMD_CELL)) {
//        hghlt = "[Cell]";
        } else if (mod->vmode & (VMD_DASHV)) {
          hghlt = "[-v]";
        } else {
          ti->topLst()->add(new ModRef(id));
          ti->Root->children++;
        }
        if (Arg.verbosity & VRB_ELAB) {
          fprintf(stderr,"Module '%s' not instanced %s\n",
                       strDeref(mod->name),hghlt);
        }
      } else if (Arg.verbosity & VRB_ELAB) {
        InstRef *rf = ti->instLst_map(mod->ref_indx);
        int      i  = rf->refs;
        fprintf(stderr,"%s '%s' instanced by\n",
                       mod->inst_count ? "Module"
	                               : "Leaf module",
                       strDeref(mod->name));
        while (i >= 0) {
          mod = ti->module_map(rf->referer[i--]);
          fprintf(stderr,"\t%s\n",strDeref(mod->name));
        }
      }
    }

    ti = ti->Root->child = CALLOC2(ti->Root->children,Inst);
    Inst::ElabDone |= ELAB_TOP;
  }

  if ((flags & ELAB_HIER) && !(Inst::ElabDone & ELAB_HIER))
  {
    Inst::Depth = 1;

    for (n = id = 0 ; (mr = ti->topLst_map(id)) ; id++,ti++) {
      ti->typ        = AO_INST;
      ti->parent     = ti->Root;
      ti->depth      = Inst::Depth;
      ti->SetSelf(new mod_model(mr->id));
      ti->name       = ti->module_map(mr->id)->name;
      ti->setAuto(-1);
      n             += ti->build();
    }

    ti->topLst()->clear();

    if (Missing) {
      Missing = 0;
      if (!Arg.interactive && !V2kUnmatchedOK) {
        sts = STS_UNMATCH;
        DELETE(Inst::Root);
        goto done;
      }
    }

    assert (n == ti->modules + ti->prims + ti->udps);

    new HierShell(&Shell::RootShell);

    Inst::ElabDone |= ELAB_HIER;
  }

  if ((flags & ELAB_PARM) && !(Inst::ElabDone & ELAB_PARM))
  {
    ti->Path = 0;

    for (d = Inst::MaxDepth; d-->0;) {
      if ((ti = Inst::def_prm)) {
        for (ti = Inst::def_prm; ti ; ti = ti->next_dfp) {
          if (d ==  ti->depth) {
            mod    = ti->pmod();
            int p  = mod->def_prm;
            for (n = 0; n < p ; n++) {
              ti->evalDfPrm(mod,n);
	    }
          }
        }
      }
    }

    Inst::ElabDone |= ELAB_PARM;
  }

  ti->Path = &tmp_path;

  if ((flags & ELAB_DIMN) && !(Inst::ElabDone & ELAB_DIMN))
  {
    Inst::Root->evalPortDim();
    Inst::ElabDone |= ELAB_DIMN;
  }

  if ((flags & ELAB_BIND) && !(Inst::ElabDone & ELAB_BIND))
  {
    Inst::Root->bindPorts();
    Inst::ElabDone |= ELAB_BIND;
  }

  if ((flags & ELAB_MKSI) && !(Inst::ElabDone & ELAB_MKSI))
  {
    Inst::Root->makeSigInfo();
    Inst::ElabDone |= ELAB_MKSI;
  }

  if ((flags & ELAB_GTSI) && !(Inst::ElabDone & ELAB_GTSI))
  {
    Inst::Root->getSigInfo();
    Inst::ElabDone |= ELAB_GTSI;
  
    if (all_done) *all_done = 1;
    ((EnvItem *)0)->add("elaborated","1",ENV_LOCAL);
  }

  ti->Path = 0;

done:
  return sts;
}

Inst::~Inst()
{
  delChildren();

  if (bnd_allcd) FREE(bind.tbl);

  FREE(imr);

  Xmr *px = xmr,
      *pxn;
  for (; px ; px = pxn) { pxn = px->next;
                          Free(px); }

  Self()->Unneeded();
}

int Inst::addNC(int w)
{
  int u = Unconnected;

  Unconnected += w;

  return u;
}

void Inst::makeSigInfo()
{
  NC0     = Wires;
  Wires  += Unconnected;

  if (Wires) {
    SigInfo **dr = CALLOC(Wires,SigInfo *);

    if (dr) {
      SIcllctr = new SIsmall(dr);
    } else {
      char *cp = CALLOC(Wires,char);
      if (cp) {
        SIcllctr = new SIstrength(cp);
      } else if ((cp = CALLOC((1 + Wires)/2,char))) {
        SIcllctr = new SI4level(cp);
      } else if ((cp = CALLOC((7 + Wires)/8,char))) {
        SIcllctr = new SI2level(cp);
      } else {
        ExitMsg(S_FATAL(errno),"Unable to allocate signal table");
      }
    }
  }
}

int Inst::findOvrrd(int indx)
{
  int top  = po_count-1,
      bot  = 0,
      splt = po_count >> 1;

  while (top >= bot) {
    if (po[splt].index > indx) {
      top = splt -1;
    } else if (po[splt].index < indx) {
      bot = splt +1;
    } else {
      return splt;
    }
    splt = (top + bot) >> 1;
  }

  return -1;
}

int Inst::setParm(XmrData *rtp,Expr *xpr,Inst *from)
{
  int i,
      sts = 1;

  assert(REF_PARM == rtp->typ.got && rtp->index >= 0);

  if ((i = findOvrrd(rtp->index)) < 0) {
    int p = ++po_count;
    REALLOC2(po,p,PrmOvrd);
    for (i = p; --i > 0;) {
      if (po[i-1].index < rtp->index) break;
      po[i] = po[i-1];
    }
    BZERO(&po[i],sizeof(po[i]));
    po[i].index = rtp->index;
  }

  if (po[i].dfd_frm) {
    if (po[i].dfd_frm->depth < from->depth) {
      sts = -1;
    } else if (po[i].dfd_frm->depth == from->depth) {
      sts = 0;
    }
  }

  po[i].dfd_frm = from;
  po[i].bx      = *xpr->bx();
  po[i].val     = *xpr->val();

  return sts;
}

Inst *Inst::haveXmr(int indx,XmrData *rtp)
{
  Xmr *px = xmr;

  while (px && px->index <= indx) {
    if (px->index == indx) {
      *rtp = px->xmr;
      return px->ip;
    }
    px = px->next;
  }

  return 0;
}

void Inst::logXmr(int indx,Inst *ip,XmrData *rtp)
{
  Xmr **ppx = &xmr,
       *px;

  for (; (px = *ppx) && px->index > indx ; ppx = &px->next);

  px        = new Xmr;
  px->index = indx;
  px->ip    = ip;
  px->xmr   = *rtp;
  px->next  = *ppx;
  *ppx      = px;
}

const char *Inst::path()
{
  if (Path) return Path->str();

  assert(0);
}

const char *Inst::path(String *buff)
{
  if (parent && *parent->path(buff)) {
    *buff += Sep;
  }

  return iname(buff);
}

void Inst::init_all()
{
  count      = 0;
  imr        = 0;
  self       = 0;
  extra      = 0;
  x_rng      = 0;
  x_copy     = 0;
  x_auto     = 0;
  behavioral = 0;
  structural = 0;
  bnd_allcd  = 0;
  children   = 0;
  stmt_id    = 0;
  dp_stmt    = 0;
  parms      = 0;
  dp_idx     = 0;
  depth      = 0;
  prt_wires  = 0;
  lcl_wires  = 0;
  imr_last   = 0;
  glbl_strt  = 0;
  sim_space  = 0;
  next       = 0;
  parent     = 0;
  child      = 0;
  next_dfp   = 0;
  po_count   = 0;
  po         = 0;
  xmr        = 0;
  rv         = 0;
  rv_count   = 0;
  bind.tbl   = 0;
  unk_tbl    = 0;
  cg_data    = 0;
  sim_data   = 0;
  typ        = AO_INST;
}

Inst::Inst(model *mp)
{
  init_all();
  self       = mp;
  setAuto(-1);
}

Inst::Inst(long id)
{
  init_all();
  self       = new mod_model(id);
  setAuto(-1);
}

static void *InstCallBack(void *inst,VStmt *vs)
{
  Inst    *parent = (Inst *)inst,
          *ip     = 0;
  long     t      = 0,
           n,
           p      = 0,
           i;
  int      copy  =  0,
           i_idx = -1;
  eSTMT    st;
  poolRef  ref,
           inm;
  PrmOvrd *po     = 0;
  PrtdObj *m_p    = 0;

retry:
  switch (st = vs->stmt_typ()) {
  default:            parent->behavioral = 1;
                      goto ok;
  case STMT_DEFPARAM: if (!parent->dp_stmt) {
                        Module  *mod    = (mod = 0)->module_map(parent->Self()->Index());
                        parent->dp_stmt = MALLOC2_N(mod->def_prm,long);
                      }
                      parent->dp_stmt[parent->dp_idx++] = vs->index;
                      goto ok;
  case STMT_INST:     parent->structural = 1;
                      ip                 = &parent->child[i = parent->index()];
                      ip->parent         = parent;
                      ip->stmt_id        = vs->index;
                      ref                = vs->name();
                      p                  = vs->parm_count();
                      if (vs->set_mod(ip)) {
                        m_p = (PrtdObj *)m_p->demap(ip->Self());
                        goto fill;
                      } else {
                        switch (ref.pool) {
                        case VERILOG_POOL:
		          if (V2KgateLib) switch(ref.index) {
#define PRIM(g,l)         case VER_##g: ip->SetSelf((*V2KgateLib->info)(GATE_##g)); goto fill;
#include "vprim.inc"
		          }
                        }
                      library:
                        switch (ip->findLibMod(ref)) {
			case LS_LOADED:
			  goto retry;
                        default:
                          if (V2kAuto) {
                          }
                          ip->unmatched();
                          if (ErrorMsg(V2kUnmatchedOK ? S_WARNING(STS_ELAB)
                                                      : S_ERROR(STS_ELAB),
                                       "No match for %s",strDeref(ref)))
                          {
                            vs->reportPosn();
			  }
                          Missing++;
                        }
                        goto fill;
                      }
                      break;
  }
fill:
  n = vs->inst_count();
  if (p && !m_p) {
    if (ErrorMsg(S_WARNING(STS_ELAB),
             "Parameters ignored for: %s - %s",ip->path(),strDeref(ref))) vs->reportPosn();
  }

  {
    int any_unamed = 0,
        any_named  = 0;
    po             = CALLOC2(p,PrmOvrd);
    for (i = 0 ; i < p ; i++) {
      poolRef  pnm;
      plExpr   xpr;
      Expr    *px = (Expr *)&xpr;
      po[i].index = -1;
      px->set(BX_VOID);
      vs->parm_val(parent,i,px,&pnm);
      po[i].bx  = *px->bx();
      po[i].val = *px->val();
      if (NULL_REF(pnm)) {
        if (any_named) {
          ErrorMsg(S_ERROR(STS_ELAB),
                   "Can't mix parameter binding schemes [@ #%d]",i);
          vs->reportPosn();
        } else {
          po[i].inst  = 1;
          po[i].index = i;
          any_unamed  = 1;
        }
      } else {
        if (any_unamed) {
          ErrorMsg(S_WARNING(STS_ELAB),
                   "Mixing parameter binding schemes [@ #%d/.%s()]",
                   i,strDeref(pnm));
          vs->reportPosn();
        }
        if (m_p && (po[i].index = m_p->parmIndex(pnm)) < 0) {
          ErrorMsg(S_WARNING(STS_ELAB),"Invalid Parameter: %s",
                                       strDeref(pnm));
          vs->reportPosn();
        } else {
          any_named  = 1;
          po[i].inst = 1;
        }
      }
    }
    if (any_named) {
      int swps;
      do {
        swps = 0;
        for (i = 1 ; i < p ; i++) {
          if (po[i].index < po[i-1].index) {
            PrmOvrd swp = po[i-1];
            po[i-1]     = po[i];
            po[i]       = swp;
            swps++;
          }
        }
      } while (swps);
    }
  }

  for (i = 0 ; i < n ; i++, copy = 2) {
    int range[3], /* width/step,first,last */
        aut,
        p_idx,
        b_idx;

    range[0] = 0;
    inm      = vs->inst_name(i,range,&b_idx,ip);
    switch (*range) {
    case -1:
      if (NULL_REF(inm)) {
        ErrorMsg(S_ERROR(STS_ELAB), "Non-constant range for instance: %s - %s",
                                    ip->path(),strDeref(ref));
      } else {
        ErrorMsg(S_ERROR(STS_ELAB), "Non-constant range for instance: %s.%s",
                                    ip->path(),strDeref(inm));
      }
    case 0:
      range[1] = 0;
      range[2] = 1;
      break;
    default:
      int x = range[2] - range[1];
      if (x < 0) {
        x        = -x;
        range[0] = -1;
      } else {
        range[0] =  1;
      }
      if (x > 0) {
        int c  = parent->children,
            ci = ip - parent->child; assert(ci < c);
        parent->children += x;
        parent->expanded += x;
        REALLOC2(parent->child,parent->children,Inst);
        BZERO(&parent->child[c],x*sizeof(Inst));
        ip = &parent->child[ci];
      }
      range[1] -= range[0];
    }
    p_idx = parent->index();
    if (NULL_REF(inm)) {
      int x;
      aut = 0;
      for (ip = &parent->child[x = p_idx]; --x >= 0 ; ip--) {
        if (SAME_REF(ref,parent->child[x].name)) {
          if (ip->AutoName() >= 0) {
            aut = ip->AutoName() + 1;
            break;
          }
          aut++;
        }
      }
    } else {
      aut = -1;
    }
    int step = range[0];
    if (step) {
      if ((step > 0 && range[2] < range[1]) ||
          (step < 0 && range[2] > range[1])) {
        range[1] = range[2];
        // allowed ???
      }
    } else {
      step = range[2] - range[1];
    }
    for (;range[1] != range[2]; copy = 1) {

      range[1] += step;

      assert(p_idx < parent->children);

      ip = &parent->child[p_idx];

      if (p_idx) ip[-1].next = ip;

      switch (copy) {
      case 1:
        *ip = ip[-1];
	ip->resetExtra();
        ip->setCopy(++i_idx);
        break;
      case 2:
        ip->parent   = parent;
        ip->SetSelf(ip[-1].Self());
      default:
        if (ip->setRngInc(range[0])) {
          i_idx = 0;
          ip->setRngMax(1+((range[2]-range[1])/range[0]));
          ip->setRngIdx(range[1]);
	}
        ip->typ      = AO_INST;
        ip->po       = po;
        ip->po_count = p;
        ip->depth    = Inst::Depth;
        ip->bind.xpr = b_idx;

        ip->name = (ip->setAuto(aut) < 0) ? inm
	    	                          : ref;
      }

      t += ip->build();

      p_idx = parent->incr();
    }
  }
ok:
  return 0;
}

void Inst::reportPosn(Inst *chld)
{
  Module *mod = pmod();
  if (mod->saved) {
    VStmtSaved vs(mod,chld->stmt_id);
    vs.reportPosn();
  } else {
    VStmtInMem vs(mod,chld->stmt_id);
    vs.reportPosn();
  }
}

int Inst::setExtra(char *x,int val,int sz,int offset)
{
  if (val > 0 || val < -128 ||
      *x  > 0 || ((*x || val) && offset))
  {
    if (extra) {
      if (0 == offset) {
        *x       = *extra;
        *extra  += sz;
        extra    = (int *)Realloc(extra,*extra * sizeof(int));
      }
    } else {
      int old  = *x;
      *x       = 1;
      extra    = (int *)Calloc(1 + sz,sizeof(int));
      *extra   = 1 + sz;
      extra[1] = old;
    }
    extra[offset + *x] = val;
  } else {
    *x = val;
  }

  return val;
}

int *Inst::resetExtra()
{
   extra = 0;
   x_rng = x_copy = x_auto = 0;

   return (int *)0;
}

int *Inst::cloneExtra()
{
  int *old =  extra,
       sz  =  *old * sizeof(int);

  extra = (int *)Malloc(sz);
  BCOPY(old,extra,sz);

  return old;
}

const char *Inst::iname(String *pth) {
  char idx[8];

  if (this != Root) {

    *pth += strDeref(name);

    if (AutoName() >= 0) {
      sprintf(idx,"-%d",AutoName());
      *pth += idx;
    }

    if (rngInc()) {
      sprintf(idx,"[%d]",rngIdx());
      *pth += idx;
    }
  }

  return pth->str();
}

eREF mod_model::Type() const
{
  return REF_MODULE;
}

eREF udp_model::Type() const
{
  return REF_PRIM;
}

long Inst::build()
{
  Module  *mod;

  int      c,
           s  = Path->len(),
           n  = 1;
  long     m,
           p  = prims,
           u  = udps;

  if (s) *Path += Sep;

  iname(Path);

  if (Arg.verbosity & VRB_ELAB) {
    fprintf(stderr,"Inst: %s\n",path());
  }

  switch (self->Type()) {
  case REF_MODULE:
    m     = ++modules;
    mod   = module_map(self->Index());
    child = CALLOC2(c = children = mod->inst_count,Inst);
    if (++Depth > MaxDepth) MaxDepth = Depth;
    if (mod->forAllStmts(InstCallBack,this,0)) {
      if (!Arg.interactive) Exit(STS_NORMAL);
    }
    Depth--;
    count = (modules - m) + (prims - p) + (udps - u);
    n    += count;
    if (mod->def_prm) {
      def_prm_tot++;
      *def_prm_nxt = this;
      def_prm_nxt  = &next_dfp;
    }
    break;
  case REF_PRIM:
    m   = ++udps;
    break;
  case REF_VPRIM:
    prims++;
    break;
  case REF_UNKNOWN:
    assert(REF_MODULE == parent->self->Type());
  default:
    assert(0);
  case REF_UNMATCHED:;
  }

  Path->cut(s);

  return n;
}

RngVec *Inst::extraRv(int extra)
{
  if (extra) {
    int ix = rv_count;

    REALLOC(rv,rv_count += extra,RngVec);
    BZERO(&rv[ix],extra * sizeof(RngVec));

    return &rv[ix];
  }

  return 0;
}

RngVec *Inst::findRv(int port)
{
  RngVec *rv0 = rv;
  int     b   = 0,
          t   = rv_count -1;

  while (t >= b) {
    int     i   = (b + t)/2;
    RngVec *rvp = &rv0[i];
    if (port == rvp->port) return rvp;
    if (rvp->port > port) t = i - 1;
    else                  b = i + 1;
  }

  return 0;
}

int Inst::evalPortDim()
{
  Module  *mod;
  Prim    *prm;
  int      c,
           ok = 0,
           s  = Path->len();

  if (s) *Path += Sep;

  iname(Path);

  if (self) {
    ok = 1;
    switch (self->Type()) {
    case REF_MODULE:
      mod = module_map(self->Index());
      c   = mod->indxd();
      mod->evalIndices(this,c,extraRv(c));
      /* drop thru */
    case REF_ROOT:
      for (c = children; c-- > 0 ; child[c].evalPortDim());
      break;
    case REF_PRIM:
      prm = prim_map(self->Index());
      c   = prm->indxd();
      prm->evalIndices(this,c,extraRv(c));
      break;
    case REF_UNMATCHED:
    case REF_VPRIM:
      break;
    case REF_UNKNOWN:
      assert(REF_MODULE == parent->self->Type());
    default: assert(0);
    }
  }

  Path->cut(s);
  return ok;
}

void Inst::delChildren()
{
  int c;

  if (child) {
    switch (self->Type()) {
    case REF_MODULE:
    case REF_ROOT:
      for (c = children; c-- > 0 ;) child[c].~Inst();
      FREE(child);
      children = 0;
    }
  }
}

PrtBnd *Inst::makeBindTbl(int width)
{
  int w = self->Wires(this,width);

  if ((w > 0 && !bind.tbl) || w > prt_wires) {
    if (!bind.tbl) prt_wires = 0;
    REALLOC2(bind.tbl,w,PrtBnd);
    bnd_allcd = 1;
    BZERO(&bind.tbl[prt_wires],(w - prt_wires) * sizeof(PrtBnd));
    prt_wires = w;
  }

  return bind.tbl;
}

int FixPortWidth(void *vp,PortInfo *pi,int min,int w)
{
  Inst *ip  = (Inst *)vp;
  int   cnt = pi->offset + w;

  if (ip->prt_wires < cnt) {
    ip->makeBindTbl(cnt);
  }

  return w;
}

int Inst::bindPorts()
{
  Module  *mod = 0;
  Inst    *pc;
  int      s   = Path->len(),
           c,
           ok  = 0;

  if (s) *Path += Sep;

  iname(Path);

  if (self) {
    ok = 1;
    switch (self->Type()) {
    case REF_MODULE:
      mod = module_map(self->Index());
    case REF_ROOT:
      for (pc = child,c = children; c-- > 0 ; pc++) {
	if (mod) {
	  mod->bindPorts(this,pc);
	} else {
	  int     w  = pc->prt_wires;
	  PrtBnd *pb = pc->makeBindTbl(w),
	          bnd;
	  bnd.typ          = BND_GLBL;
	  bnd.index        = Root->lcl_wires;
	  bnd.offset       = 0;
	  Root->lcl_wires += w;
	  while (w-- > 0) {
	    *pb++ = bnd;
	    bnd.index++;
	  }
	}
	pc->bindPorts();
      }
      if (mod) fixWires();
      incWires();
      break;
    case REF_UNMATCHED:
    case REF_PRIM:
    case REF_VPRIM:
      break;
    case REF_UNKNOWN:
      assert(REF_MODULE == parent->self->Type());
    default: assert(0);
    }
  }

  Path->cut(s);
  return ok;
}

int Inst::derefSig(int idx0,int offst, int prt_allow,Inst *from)
{
  PortInfo  pi;

  if (from) {
    pi.used  = PIU_XMR;
  } else {
    XmrData  rtp;
    Inst    *rip = haveXmr(idx0,&rtp);
    if (rip) {
      return rip->derefSig(rtp.index,offst,prt_allow|PRT_XMR,this);
    }
    pi.used  = PIU_UNSET;
  }

  pi.index = idx0;
  pi.dimn  = 0;
  int idx  = getPortInfo(NullRef,&pi,prt_allow);

  if (pi.xmr) {
    return GLBL_XMR;    
  }

  assert(idx >= 0 && offst < pi.width);

  idx = pi.offset + offst;

  if (idx < prt_wires) {
    return derefPort(&bind.tbl[idx]);
  }

  idx -= prt_wires;
  assert(idx < lcl_wires);
  return glbl_strt + idx;
}

int Inst::derefUnk(int idx,int offst)
{
  UnkBnd *pub = &unk_tbl[idx];

  return glbl_strt + pub->target.index + offst;
}

int Inst::derefXpr(int idx,int offst)
{
  return glbl_strt + offst;
}

void Inst::fixWires()
{
  UnkBnd *pub = unk_tbl;
  int     s0  = prt_wires + lcl_wires,
          off = 0,
          w   = 0,
          n   = pmod()->unk_count(),
          i   = 0;

  if (pub) {
    for (; i < n ; pub++) {
      if (pmod()->unkRefType(i++) == REF_UNKNOWN) {
        pub->target.index   = s0 + off;
        pub->target.offset  = 0;
        w                   = pub->target.width;
        off                += w;
      } else {
        pub->target.index   = -1;
      }
    }
  } else {
    // Warn unused ???
  }

  lcl_wires += off;
}

void Inst::fillNC(int start,int used)
{
  PrtBnd  bnd,
         *pb   = bind.tbl;
  int     need = prt_wires - used;

  pb          += start;
  bnd.typ      = BND_SET_NC;
  bnd.index    = addNC(need);
  bnd.offset   = 0;

  while (need-- > 0) {
    if (BND_NC == pb->typ) { *pb++ = bnd; bnd.index++; }
  }
}

int Inst::derefPort(PrtBnd *pb)
{
  switch (pb->typ) {
  default:         assert(0);
  case BND_GLBL:   return pb->index + pb->offset;
  case BND_PORT:   return parent->derefSig(pb->index,pb->offset,PRT_BNDSIG,0);
  case BND_UNK:    return parent->derefUnk(pb->index,pb->offset);
  case BND_SET_NC: return NC0 + pb->index + pb->offset;
  case BND_CONSTZ:
  case BND_CONST0:
  case BND_CONST1:
  case BND_CONSTX:
  case BND_EXPR:   return parent->derefXpr(pb->index,pb->offset);
  case BND_NC:     break;
  }

  return -1;
}

void SIsmall::setSigInfo(void *vp,int pid,int si,DrvInfo dr,int idx,
                         int rcv_cnt,int drv_cnt,void *xtr)
{
  Inst *ip = (Inst *)vp;

  return ip->setSigInfo(this,pid,si,dr,idx,rcv_cnt,drv_cnt,xtr,0);
}

void Inst::setSigInfo(SigInfoCllctr *psi,int pid,int si,DrvInfo dr,int idx,
                      int rcv_cnt,int drv_cnt,void *xtr,int hshd)
{
  SigInfo  *psn  = 0,
           *ps   = 0,
          **pps  = psi->getSigInfo(this,pid,si),
          **ppsn;
  int       i    = 1,
            s    = 1;
  void     *x;
  eDM       dm   = self->drvMode();

  for (; (ps = *pps) ; pps = ppsn)  {
    if (!hshd || si == ps->Si()) {
      if (this == ps->Ip() && pid == ps->Pid()) {
        switch (self->cmpXtra(xtr,x = ps->Xtr())) {
        case CX_MERGE: switch (dm) {
	               case DM_FLAGS:
			 if (drv_cnt) ps->drv_info |= DRF_DRVN;
			 if (rcv_cnt) ps->drv_info |= DRF_RCVD;
                         goto all_done;
                       default:
                         i = s = ps->Items();
                         if (rcv_cnt != ps->Rcv() ||
                             drv_cnt != ps->Drv()) {
	                   if (!ps->rcv && rcv_cnt) s++;
	                   if (!ps->drv && drv_cnt) s++;
                           psn = psn->Create(s,ps);
                           if (!ps->rcv && rcv_cnt) {
                             psn->set(psn->rcv = ++i,rcv_cnt);
                           }
                           if (!ps->drv && drv_cnt) {
                             psn->set(psn->drv = ++i,drv_cnt);
                           }
	   		   *pps = psn;
                           ps->Destroy();
                           goto done;
                         }
	               }
        case CX_SAME:  goto all_done;
        case CX_DIFF:  break;
        default:       assert("Bad CX case" == 0);
        }
      }
    }
    if (!(ppsn = ps->pNext())) break;
  }

  if (self->Xtra(xtr)) s++;
  if (ps)              s++;
  if (hshd)            s++;
  if (pid >= 0)        s++;

  if (drv_cnt && (dm & DM_COUNT_DRV)) s++;
  if (rcv_cnt && (dm & DM_COUNT_RCV)) s++;

  psn = psn->Create(s);

  psn->set(psn->ip  = i++,this);

  if (self->Xtra(xtr)) psn->set(psn->xtr = i++,xtr);
  if (hshd)            psn->set(psn->si  = i++,si);
  if (pid >= 0)        psn->set(psn->pid = i++,pid);

  if (drv_cnt) psn->drv_info |= DRF_DRVN;
  if (rcv_cnt) psn->drv_info |= DRF_RCVD;

  if (ps) {
    SigInfo *pspn = ps->Next();
    if (pspn) {
      psn->set(psn->nxt = i++,pspn);
      ps->set(ps->nxt,psn);
      goto done;
    }
    psn->set(psn->nxt = i++,ps);
  }

  *pps = psn;
done:
  assert(s == (i = psn->Items()));
all_done:;
}

SigInfo **SIsmall::getSigInfo(void *ip,int pid,int si)
{
  assert(0 <= si && si < ((Inst *)ip)->Wires);

  return &SI[si];
}

SigInfo *SIsmall::getSigInfo(void *ip,int si)
{
  assert(0 <= si && si < ((Inst *)ip)->Wires);

  return SI[si];
}

SIsmall::~SIsmall()
{
  FREE(SI);
}

void SIstrength::setSigInfo(void *vp,int pid,int si,DrvInfo dr,int idx,
                            int rcv_cnt,int drv_cnt,void *xtr)
{
  Inst *ip = (Inst *)vp;

  return ip->setSigInfo(this,pid,si,dr,idx,rcv_cnt,drv_cnt,xtr,1);
}

SigInfo **SIstrength::getSigInfo(void *ip,int pid,int si)
{
  int i = hashIdx(ip,pid,si);

  return &Hash[i];
}

SigInfo *SIstrength::getSigInfo(void *ip,int si)
{
  int i = hashIdx(ip,-1,si);

  return Hash[i];
}

SIstrength::~SIstrength()
{
  FREE(SI);
}

void SI4level::setSigInfo(void *vp,int pid,int si,DrvInfo dr,int idx,
                          int rcv_cnt,int drv_cnt,void *xtr)
{
  Inst *ip = (Inst *)vp;

  return ip->setSigInfo(this,pid,si,dr,idx,rcv_cnt,drv_cnt,xtr,1);
}

SigInfo **SI4level::getSigInfo(void *ip,int pid,int si)
{
  int i = hashIdx(ip,pid,si);

  return &Hash[i];
}

SigInfo *SI4level::getSigInfo(void *ip,int si)
{
  int i = hashIdx(ip,-1,si);

  return Hash[i];
}

SI4level::~SI4level()
{
  FREE(SI);
}

void SI2level::setSigInfo(void *vp,int pid,int si,DrvInfo dr,int idx,
                          int rcv_cnt,int drv_cnt,void *xtr)
{
  Inst *ip = (Inst *)vp;

  return ip->setSigInfo(this,pid,si,dr,idx,rcv_cnt,drv_cnt,xtr,1);
}

SigInfo **SI2level::getSigInfo(void *ip,int pid,int si)
{
  int i = hashIdx(ip,pid,si);

  return &Hash[i];
}

SigInfo *SI2level::getSigInfo(void *ip,int si)
{
  int i = hashIdx(ip,-1,si);

  return Hash[i];
}

SI2level::~SI2level()
{
  FREE(SI);
}

int Inst::getSigInfo()
{
  Module  *mod   = 0;
  int      s     = Path->len(),
           c,
           ok    = 0;
  if (s)  *Path += Sep;

  iname(Path);

  if (self) {
    switch (self->Type()) {
    case REF_MODULE:
      mod = module_map(self->Index());
    case REF_ROOT:
      for (c = children; c-- > 0 ; ) {
	Inst *pc = &child[c];
	pc->getSigInfo();
      }
      if (!mod) goto done;
    }
    ok = 1;

    int      si_p,
             si_l  = glbl_strt,
             todo,
             p     = 0;
    PrtBnd  *pb    = bind.tbl;
    PortDir  pm;

    self->setPorts(prt_wires);

    for (todo = prt_wires + lcl_wires; todo > 0 ; p++) {
      int w;
      pm = self->portMode(p);
      if (pm & PRT_PORT) {
        int si_p = derefPort(pb++);
        self->getSigInfo(this,REF_PORT,p,-1,si_p);
        w     = portWidthInst(p);
        todo -= w;
      } else {
        PortTyp pt = self->portType(p);
        if ((pm & PRT_BNDSIG) || (XMR_BNDSIG & Imr(p))) {
          self->getSigInfo(this,REF_PORT,p,-1,si_l);
          w     = portWidthInst(p);
          si_l += w;
          todo -= w;
        }
      }
    } 

    ASSERT(0 == todo && si_l - glbl_strt == lcl_wires);
  }

done:
  Path->cut(s);
  return ok;
}

eLibSrch Inst::findLibMod(poolRef nm)
{
  int         l    = 0;
  Library    *plib = 0;
  Filename    tryp;
  String      tryn(OS_DIR_SEP);

  tryn += strDeref(nm);
  tryn += ".";

  for (; (plib = plib->libLst_map(l)) ; l++) {
    tryp  = plib->Path();
    tryp += tryn;
    tryp += "v";
    if (tryp.Exists()) {
      int oldl = plib->setLibrary(l),
          sts  = prsVM(tryp,VMD(VMD_IEEE|VMD_VA));
      plib->setLibrary(oldl);
      if (!sts) return LS_LOADED;
    }
  }

  return LS_NOT_FOUND;
}

void Inst::evalDfPrm(Module *mod,int n)
{
  if (mod->saved) {
    ModPool(mod->saved)->evalDfPrm(mod,this,n);
  } else {
    VStmtInMem vs(mod,n);
    vs.evalDfPrm(this);
  }
}

void Inst::setImrFlags(int index,int flgs)
{
  if (index >= imr_last) {
    REALLOC(imr,index + 1,unsigned char);
    while (imr_last <= index) imr[imr_last++] = 0;
  }

  imr[index] = flgs;
}

void Inst::addImrFlags(int index,int flgs)
{
  if (index >= imr_last) setImrFlags(index,0);

  imr[index] |= flgs;
}

Inst *Inst::findXmr(int n,poolRef *names,XmrData *ret,eXMR xio)
{
  int     c   = 0;
  eRFF    rrf = ret->typ.need;
  eREF    it  = self->Type();
  Module *mod = (REF_MODULE == it) ? pmod()
                                   : 0;
  assert(names->pool > 0);

  if (ret->scp > 0 && mod && SAME_REF(mod->name,*names) &&
      ModPool(mod->saved)->findImr(this,n-1,names+1,ret) >= 0) {
    goto local;
  }

  for (; c < children ; c++) {
    Inst *ip  = &child[c];
    if (SAME_REF(ip->name,*names)) {
      if (1 == n) {
        if (rrf & RFF_INST) {
          ret->typ.got = REF_INST;
          ret->scp     = -1;
          ret->index   = ip->self->Index();
          return ip;
        }
      } else if ((ip = ip->findXmr(n -1,names +1,ret,xio))) {
        return ip;
      }
      break;
    }
  }

  if (REF_MODULE == it) {
    if (ModPool(mod->saved)->findImr(this,n,names,ret) >= 0) {
    local:
      if (xio) {
        if (REF_PORT == ret->typ.got) {
          if (!(mod->portMode(ret->index) & PRT_BNDSIG)) {
            setImrFlags(ret->index,xio);
          }
        }
      }
      return this;
    }
  } else if (REF_ROOT != it) {
    assert("Need model virtual function" == 0);
  }

  return 0;
}

void Inst::warnMixed(Inst *child,poolRef prt_nm)
{
  String      buff;
  const char *sp  = strDeref(prt_nm);

  ErrorMsg(S_ERROR(STS_ELAB),"Unnamed port binding after named (%s after %s)",
                             child->path(&buff),sp ? sp : "?");
  reportPosn(child);
}

int portWidth(RngVec *rvp,int *dimn)
{
  int    w  = 1,
         i  = 0,
         s  = rvp->dirn,
         c  = rvp->count,
         x;
  Value *vp = rvp->values;

  for (; c-- > 0 ; s >>= 1) {
    int v1 = vp++->i,
        v2 = vp++->i,
        sn = (s & 1) ?  1
                     : -1,
        dw = 1 + sn * (v2 - v1);

    w *= dw;

    if (dimn) *dimn = dw;
  }
  return w;
}

int Inst::sliceWidth(RngVec *rvp,rngEval *rng,int idxs)
{
  int      w     = 1,
           w0,
           i     = 0,
           s     = rvp->dirn,
           c     = rvp->count,
           x;
  Value   *vp    = rvp->values;
  plPort  *prt   = getPort(rvp->port);
  rngEval *rng_p;

  if (prt->pckd && idxs == prt->pckd + prt->unpckd) {
    rng_p = &rng[idxs - 1];
    w  = rng_p->left - rng_p->right;
    w0 = vp[0].i     - vp[1].i;
    if (s & 1) { w  = -w;
                 w0 = -w0; }
    if (w <  0) goto null_range;
    if (w > w0) goto bad_range;
    w++;
    goto done;
  }

  assert(0);

 done:
  return w;

 null_range:
  return -1;

 bad_range:
  return -2;
}

VlType Pt2Vt(PortTyp pt)
{
  int v = VT_LUINT;

  switch (pt & PT_TYPE_MSK) {
  case PT_TIME: v = VT_U64;    break;
  case PT_REAL: v = VT_DOUBLE; break;
  }

  if (pt & PT_SIGNED) v |= VT_SIGNED; 

  return (VlType)v;
}

int Inst::portWidthInst(int p,PortTyp *ptp,RngVec **rvp_r)
{
  RngVec   *rvp = rv;
  PortInfo  pi;
  PortTyp   pt  = self->portType(p);
  int       w   = 1;

  if (ptp) *ptp = pt;

       if ((pt & PT_INT) && !(pt & PT_PACKED)) { w = 32; }
  else if (pt & PT_REAL)                       { w = 64; }

  if (rvp)
  {
    int i = 0,
        c = pmod()->indxd();

    for (; i <= p ; i++) {

      if (rvp->port < i) {
        rvp++;
        if (--c <= 0) break;
      }

      if (rvp->port >= p) {
        if (rvp->port == p) {
          w *= portWidth(rvp);
          goto done;
        }
        break;
      }
    }

    rvp = 0;
  }

 done:
  if (rvp_r) *rvp_r = rvp;
  return w;
}

poolRef Inst::itemName(eREF ref,int index)
{
  VerilogObj *vp = 0;
  PrtdObj    *p0 = (PrtdObj *)vp->demap(self);

  if (p0) {
    if (p0->saved) {
      return PoPool(p0->saved)->itemName(ref,index);
    } else {
      return p0->itemName(ref,index);
    }
  }

  return NullRef;
}

PortDir Inst::portMode(plPort *prt,int pidx)
{
  int pm = 0;
  if (prt) {
    pm          = prt->io;
  } else switch (self->Type()) {
  default:
    assert("Need model virtual function" == 0);
  case REF_MODULE:
    Module *mod = pmod();
    int     pl;
    prt         = PoPool(mod->saved)->getPort(pidx);
    pm          = prt->io;
  }
  if (pm & PRT_BNDSIG) {
    pm |= (PRT_BNDSIG);
  } else {
    pm |= Imr(pidx) << PRT_XMR_SHFT;
  }
  return (PortDir)pm;
}

PortDir Inst::portMode(Port *prt,int pidx)
{
  int pm = 0;
  if (prt) {
    pm          = prt->io;
  } else switch (self->Type()) {
  default:
    assert("Need model virtual function" == 0);
  case REF_MODULE:
    Module *mod = pmod();
    prt         = mod->port_map(pidx);
    pm          = prt->io;
  }
  if (pm & PRT_BNDSIG) {
    pm |= (PRT_BNDSIG);
  } else {
    pm |= Imr(pidx) << PRT_XMR_SHFT;
  }
  return (PortDir)pm;
}

int Inst::getPortInfo(poolRef nm,PortInfo *ret,int prt_allow)
{
  if (REF_UNMATCHED == self->Type()) return -1;

  VerilogObj *vp  = 0;
  Module     *mod = pmod();
  long        w   = 0;

  if (mod->saved) {
    return PoPool(mod->saved)->getPortInfo(this,nm,ret,prt_allow);
  } else {
    RngVec *rvp = rv;
    int     i   = 0,
            c   = mod->indxd(),
            pw;
    Port   *prt;

    for (; (prt = mod->port_map(i)) ; i++,w += pw) {

      pw = 1;

           if ((prt->ptyp & PT_INT) && !prt->pckd) pw = 32;
      else if ((prt->ptyp & PT_REAL))              pw = 64;

      if (!(portMode(prt,i) & prt_allow)) {
        pw = 0;
      }

      if (pw) {
        while (c > 0 && rvp->port < i) {rvp++;c--;}
        if (c > 0 && rvp->port == i) {
          pw *= portWidth(rvp);
        }

        if ((NULL_REF(nm) && ret->index == i) || SAME_REF(prt->name,nm)) {
          ret->index  = i;
          ret->offset = w;
          ret->width  = pw;
          return i;
        }
      }
    }
  }

  return -1;
}

int Inst::bindUnk(Inst *owner,PrtBnd *pb,PortInfo *target)
{
  UnkBnd *pub = unk_tbl;
  int     c   = pmod()->unk_count();

  if (!pub) { pub   = unk_tbl = CALLOC2(c,UnkBnd); }

  ASSERT(c > pb->index);

  pub += pb->index;

  if (pub->owner && pub->target.width >= target->width) {
    return pub->target.width;
  }

  pub->owner  = owner;
  pub->target = *target;

  return target->width;
}

int Inst::getPortDisc(int pidx)
{
  Module *mod = pmod();
  int     di;

  if (mod->saved) {
    di       = PoPool(mod->saved)->getPortDisc(pidx);
  } else {
    Port *pp = mod->port_map(pidx);
    di       = pp->dsc_indx;
  }

  return di;
}

HierShell::HierShell(Shell **rp)
{
  static ShellHelp hier_help[] = {
    {"pwi","show instance"},
    {"li", "list instances  [-h for more]"},
    {"ci", "change instance"},
    {"ss", "show signals    [-h for more]"},
    {"lpm","show parameters [-h for more]"},
    {0,0}
  };

  priority  = SHP_HIER;
  cntxt     = SHC_COMMAND;
  curr_inst = Inst::Root;
  name      = "hier";
  help_data = hier_help;

  addParser(rp);
}

typedef enum {
  LI_RECURSE   =   1,
  LI_FULLPATH  =   2,
  LI_ESC_BRKT  =   4,
  LI_CURR      =   8,
  LI_ROOT      =  16,
  LI_PRINT     =  32
} eCBD_LI;

typedef enum {
  SS_DISC      =    1,
  SS_NATURE    =    2,
  SS_LONG      =    4,
  SS_ALL_INST  =    8,
  SS_IO        =   16,
  SS_DRIVER    =   32,
  SS_RECEIVER  =   64,
  SS_VERBOSE   =  128,
  SS_FULL      =  256,
  SS_SHOW_SELF =  512,
  SS_DIMENSION = 1024,
  SS_TYPE      = 2048
} eCBD_SS;

typedef struct {
  int           matched,
                do_ret,
                wcnt,
                extra,
                depth;
  const char  **wild,
               *punc,
               *indent;
  Inst         *last;
  Stream       *out;
  String       *full;
  InstCllBk     fn;
} CbData;

plPort *Inst::getPort(int pidx,int *pool)
{
  switch (self->Type()) {
  case REF_MODULE:
  case REF_PRIM:
    Module *mod = pmod();
    if (mod->saved) {
      if (pool) *pool = mod->saved;
      return PoPool(mod->saved)->getPort(pidx);
    }
  }

  assert(0);
  return 0;
}

void *Inst::forSigs(SigCllBk fn,void *dp)
{
  void *ret = 0;

  switch (self->Type()) {
  case REF_MODULE:
  case REF_PRIM:
    Module *mod = pmod();
    int     n   = mod->portCount(),
            i   = 0,
            bi  = 0;

    for (; i < n ; i++) {
      int pm = mod->portMode(i),
          w  = portWidthInst(i);
      if (pm & PRT_BNDSIG) {
        if ((ret = (*fn)(this,dp,i,bi))) break;
        bi += w;
      } else {
        if ((ret = (*fn)(this,dp,i,-1))) break;
      }
    }
  }

  return ret;
}

void prntCtype(Stream *out,BaseExpr *bx,Value *pv) {
  switch (bx->typ) {
  case VT_DOUBLE: out->printf("PARAM_TYPE(DOUBLE)"); break;
  case VT_INT:    out->printf("PARAM_TYPE(INT)");    break;
  case VT_UINT:   out->printf("PARAM_TYPE(UINT)");   break;
  case VT_LINT:   out->printf("PARAM_TYPE(LINT)");   break;
  case VT_LUINT:  out->printf("PARAM_TYPE(LUINT)");  break;
  default:        out->printf("PARAM_TYPE(void)");   break;
  }
}

void prntExpr(Stream *out,BaseExpr *bx,Value *pv,int c) {
  switch (bx->typ) {
  case VT_DOUBLE: out->printf("%g",pv->d);     break;
  case VT_INT:    out->printf("%d",pv->i);     break;
  case VT_UINT:   out->printf("%u",pv->i);     break;
  case VT_LINT:   out->printf("%d",pv->lgc.i); break;
  case VT_LUINT:  out->printf("%u",pv->lgc.i); break;
  default:        out->printf(c ? "NaN"
                                : "?");        break;
  }
}

void *prntParm(Stream *out,Inst *ip,int flgs,const char **wpp,
               int *matched,int idx)
{
  void        *ret   = 0;
  Module      *mod   = ip->pmod();
  Expr         expr;
  Value       *pv    = expr.pval();
  poolRef      nm    = mod->parmVal(ip,idx,expr.bx(),pv);
  const char  *np    = EscpdCname(nm);
  int          c,
               rng   = 0;

  if (wpp) {
    const char *wp;
    while ((wp = *wpp++)) {
      if (wildCmp(wp,np)) goto mtchd;
    }
    goto done;
  }

 mtchd:
  (*matched)++;
  if (!(c = (flgs & LPM_C))) {
    out->printf("%s = ",np);
    prntExpr(out,expr.bx(),expr.pval(),c);
  }

  if (flgs & LPM_RANGE) {
    for (;; rng++) {
      parmRng rng_data;
      if (!mod->parmRange(ip,idx,rng,&rng_data)) break;
        if (c && 0 == rng) {
          out->printf("static const _param_range _rng_%s(",np);
        }
        if (rng_data.exclude) {
	  out->printf(c ? "-1,"
                        : " exclude ");
	  prntExpr(out,&rng_data.start_b, &rng_data.start_v,c);
	} else {
          if (c) {
            out->printf("%d,",rng_data.inc_strt +
			      rng_data.inc_fnsh << 1);
          } else {
            out->printf(" from %c ",rng_data.inc_strt ? '['
                                                      : '(');
          }
	  prntExpr(out,&rng_data.start_b, &rng_data.start_v,c);
	  out->printf(c ? ","
                        : " : ");
	  prntExpr(out,&rng_data.finish_b,&rng_data.finish_v,c);
          out->printf(c ? ","
                        :" %c",rng_data.inc_fnsh ? ']'
                                                 : ')');
	}
    }
    
  }

  if (c) {
    const char *typ = "";
    if (rng) out->printf("0);\n");
    switch (expr.bx()->typ) {
    case VT_DOUBLE: typ = "_dbl_"; break;
    }
    out->printf("static const %s_parameter %s(",typ,np);
    prntCtype(out,expr.bx(),expr.pval());
    if (rng) {out->printf(",&_rng_%s,",np);}
    else     {out->printf(",(_param_range *)0,");}
    prntExpr(out,expr.bx(),expr.pval(),c);
    out->printf(");\n");

  } else {
    out->printf("\n");
  }

 done:
  return 0;
}

static void *prntParm(Inst *ip,void *dp,int idx)
{
  CbData *data = (CbData *)dp;

  return prntParm(data->out,ip,data->extra,data->wild,&data->matched,idx);
}

void *Inst::forParms(ParmCllBk fn,void *dp)
{
  void *ret = 0;

  switch (self->Type()) {
  case REF_MODULE:
  case REF_PRIM:
    Module *mod = pmod();
    int     n   = mod->parmCount(),
            i   = 0;

    for (; i < n ; i++) {
      if ((ret = (*fn)(this,dp,i))) break;
    }
  }

  return ret;
}

void *Inst::forChildren(InstCllBk fn,void *dp)
{
  void *ret = 0;
  int   lmt = children,
        c;

  for (c = 0; c < lmt ; c++) {
    if ((ret = (*fn)(&child[c],dp))) break;
  }

  return ret;
}

static void *scanInst(Inst *,void *);
static void *scanSig(Inst *,void *,int,int);

static void *subSpec(Inst *ip, void *vp)
{
  CbData *dp = (CbData *)vp;

  return scanInst(ip,++dp);
}

void *Inst::forSigSpec(int *i,int argc,const char **argv,SigCllBk fn,void *vp)
{

  return 0;
}

void *Inst::forSpec(int *i,int argc,const char **argv,InstCllBk fn,void *vp)
{
  const char *sp;
  void       *ret  = 0;
  int         c    = *i,
              l    = strlen(Sep),
              sl   = 0,
              segs = 1,
              idx  = 0;

  for (;;) {
    for (sp = argv[c++]; *sp ; sl++) switch (*sp) {
    case '[': idx++;
              goto next;
    case ']': if (idx) idx--;
              goto next;
    default:  if (0 == strncmp(Sep,sp,l)) { segs++;
	                                    sp += l;
                                            continue;}
    next:     sp++;
    }
    if (c >= argc) break;
    if (0 == strcmp(argv[c],"[")) continue;
    if (!idx) break;
  }

  { TMPARR(char,spc2,sl+1);
    TMPARR(const char *,seg, segs + 1);
    TMPARR(CbData,data,segs + 1);

    char   *sp2 = spc2;
    CbData *dp  = (CbData *)vp;

    data[0]      = *dp;
    segs         = 0;
    data[0].wild = seg;
    *seg         = spc2;
    data[0].wcnt = 1;

    for (c = *i,idx = 0;;) {
      for (sp = argv[c++]; *sp ; sl++) switch (*sp2 = *sp) {
      case '[': idx++;
                goto next2;
      case ']': if (idx) idx--;
                goto next2;
      default:  if (0 == strncmp(Sep,sp,l)) {
	          sp               += l;
                  *sp2++            = 0;
                  data[segs].fn     = subSpec;
                  data[segs].extra &= ~LI_PRINT;
                  data[segs].extra |= LI_RECURSE;
    	          segs++;
                  seg[segs]         = sp2;
                  data[segs]        = *dp;
                  if (dp->extra & LI_FULLPATH) {
                    data[segs].punc = "\n";
                  }
                  data[segs].wild   = &seg[segs];
                  data[segs].wcnt   = 1;
                  continue;
                }
      next2:    sp++;
                sp2++;
      }
      if (c >= argc) break;
      if (0 == strcmp(argv[c],"[")) continue;
      if (!idx) break;
    }

    *sp2 = 0;
    *i   = c;

    if (*seg[0]) ret = forChildren(scanInst,data);
    else         ret = Root->forChildren(scanInst,&data[1]);

    *dp = data[segs];
  }

  return ret;
}

static void *scanSig(Inst *ip,void *dp,int idx,int bi)
{
  Module      *mod  = ip->pmod();
  CbData      *data = (CbData *)dp;
  poolRef      nm   = mod->itemName(REF_PORT,idx);
  const char  *np   = strDeref(nm),
              *pp   = data->punc,
             **ppw  = data->wild;
  int          i,
               xtra = data->extra,
               drv  = 0,
               rcv  = 0,
               w    = data->wcnt;

  if (w) {
    while (w--) {
      const char *wild = *ppw++;
      if (wildCmp(wild,np)) goto ok;
    }
    goto done;
  }

ok:
  if (xtra & SS_TYPE) {
    
  }

  data->out->printf("%s",np);

  if (xtra) {
    SigInfo    *pi0 = (bi >= 0) && SIcllctr ? SIcllctr->getSigInfo(ip,idx)
                                            : 0,
               *pi  = pi0;
    const char *sp;
    Inst       *ip2 = 0,
               *ipl;
    RngVec     *rvp;

    if ((xtra & SS_DIMENSION) && (rvp = ip->findRv(idx))) {
      int r = rvp->count;
      for (r = 0; r < rvp->count ; r++) {
        data->out->printf("[%d:%d]",rvp->values[2 * r].i,
			            rvp->values[2 * r + 1].i);
      }
    }

    while (pi && (idx == (i = pi->Si()) || i < 0)) {
      sp = "\t";
      if (ip == (ip2 = pi->Ip()) || (xtra & SS_ALL_INST))
      {
        if (xtra & SS_VERBOSE) {
          if ((ip != ip2) || (xtra & SS_SHOW_SELF)) {
            String      path;
            int         pid = pi->Pid();
            char        pbf[12];
            const char *pl  = ip2->Self()->procLabel(ip2,pid,pbf);

            data->out->printf("\n\t(%s%s%s)",
                              (xtra & SS_FULL) ? ip2->path(&path)
                                               : ip2->iname(&path),
                              *pl ? "."
                                  : "",
                              pl);
          } else if (ipl != ip2) {
            data->out->printf("\n\t(.)");
          }
        }

        if ((xtra & SS_DISC) && (i = pi->Dsc()) >= 0) {
          nm = mod->disc_map(i)->name;
          np = strDeref(nm);
          data->out->printf("%s%s",sp,np); sp = ",";
        }

        drv += pi->Drvn();
        rcv += pi->Rcvd();

        sp = "\t";
	if (xtra & SS_VERBOSE) {
	  if ((xtra & SS_DRIVER)) {
	    data->out->printf(" <%d",drv); drv = 0; sp = " ";
	  }
	  if ((xtra & SS_RECEIVER)) {
	    data->out->printf(" >%d",rcv); rcv = 0; sp = " ";
	  }
	}
      }
      pi = pi->Next();
    }

    sp = "\t";
    if (!(xtra & SS_VERBOSE)) {
      if ((xtra & SS_DRIVER)) {
        data->out->printf("%s<%-2d",sp,drv); sp = " ";
      }
      if ((xtra & SS_RECEIVER)) {
        data->out->printf("%s>%-2d",sp,rcv); sp = " ";
      }
    }
  }

  data->out->printf(pp);

done:
  return 0;
}

static void *scanInst(Inst *ip,void *dp)
{
  CbData     *data  = (CbData *)dp;
  int         mtch  = 0,
              w     = data->wcnt,
              all   = !w;
  const char *np    = strDeref(ip->name);
  char       *nc    = 0;
  void       *ret   = 0;

  if (ip->rngInc()) {
    int l  = strlen(np);
    nc     = new char[l + 12];
    if (data->extra & LI_ESC_BRKT) {
      sprintf(nc,"%s\\[%d\\]",np,ip->rngIdx());
    } else {
      sprintf(nc,"%s[%d]",np,ip->rngIdx());
    }
    np  = nc;
  }

  while (w-- > 0) {
    const char *wild = data->wild[w];
    if (!wildCmp(wild,np)) goto done;
    mtch++;
    data->last = ip;
    if (1 == data->wcnt && data->do_ret && !isWild(wild)) {
      ret = ip;
      goto done;
    }
  }

  if (all || mtch) {
    int     c;
    String *str = data->full;
    Stream *out = (data->extra & LI_PRINT) ? data->out
                                             : 0;

    if (data->matched && out) out->printf(data->punc);

    data->matched += mtch + all;

    if (str) {
      const char *cp = str->str();
      c              = str->len();
      if (*cp) {
        if (out) out->printf("%s",cp);
      } else if (data->extra & LI_ROOT) {
      }
      *str += np;
      *str += Inst::Sep;
    } else if (data->indent && out) {
      int d = data->depth;
      while (d-- > 0) out->printf(data->indent);
    }

    if (out) out->printf("%s",np);

    if (data->extra & LI_RECURSE) {
      data->depth++;
      ip->forChildren(data->fn,data);
      data->depth--;
    }

    if (str) str->cut(c);

  } else {

    data->matched += mtch;

  }

done:
  DELETE(nc);
  return ret;
}

eSTS HierShell::parse(int argc,const char **argv,char *space,char *qutd,
                      Stream **std,Stream **rdr,eSHX shx)
{
  CbData      cbd;
  Inst       *inst;
  poolRef     nm  = strFind(argv[0]);
  const char *cp;
  char       *tmp = 0;
  eSTS        sts = STS_NORMAL;
  int         pid = 0,
              i;

  InitLang(defPoolMode,0);

  switch (nm.pool) {
  case COMMANDS_POOL:
    switch (nm.index) {
    case SHL_PWI:
      rdr[STDOUT_FILENO]->printf("%s\n",path_buff.str());
      goto done;
    case SHL_CI:
      switch(argc) {
      case 1:  inst = Inst::Root;
               goto new_curr;
      case 2:  if (0 == strcmp("-h",argv[1])) {
                 rdr[STDOUT_FILENO]->printf(
                             "  ^\tUp to parent\n"
                             "  \tWith no argument goes to root\n");
                 goto ss_done;
               }
	       if (0 == strcmp("^",argv[1])) {
                 inst = curr_inst->parent;
                 goto new_curr;
               }
      default: BZEROS(cbd);
               i = 1;
               curr_inst->forSpec(&i,argc,argv,scanInst,&cbd);
               switch (cbd.matched) {
               case 1: inst = cbd.last; goto new_curr;
               case 0: std[STDERR_FILENO]->printf("ci: No such scope.\n");
                       sts = STS_NOTSCOPE;
                       goto done;
               }
               std[STDERR_FILENO]->printf("cs: Too many arguments.\n");
               goto syntax2;
      }
      break;
    case SHL_SS:
      cbd.extra = 0;
      cbd.out   = rdr[STDOUT_FILENO];
      cbd.punc  = " ";
      cbd.wcnt  = 0;
      switch(argc) {
      default: for (i=1; (cp = argv[i]) && '-' == *argv[i] ; i++) {
                 char ch;
                 while ((ch = *++cp)) switch(ch) {
                 default:
                   std[STDERR_FILENO]->printf("ss: invalid argument (-%c).\n",
                                              ch);
                   goto syntax2;
                   break;
                 case 'd': cbd.extra |= SS_DISC;               break;
                 case 'n': cbd.extra |= SS_NATURE;             break;
                 case 'a': cbd.extra |= SS_ALL_INST;           break;
                 case 'i': cbd.extra |= SS_IO;                 break;
                 case 'x': cbd.extra |= SS_DRIVER|SS_RECEIVER; break;
                 case 'v': cbd.extra |= SS_VERBOSE;            break;
                 case 't': cbd.extra |= SS_TYPE;               break;
                 case 'f': cbd.extra |= SS_FULL;               break;
                 case 'r': cbd.extra |= SS_DIMENSION;          break;
                 case 's': cbd.extra |= SS_SHOW_SELF;          break;
                 case 'h': rdr[STDOUT_FILENO]->printf(
                             "  -d\tDisciplines\n"
                             "  -n\tNatures\n"
                             "  -a\tFor all instances\n"
                             "  -i\tIO\n"
                             "  -x\tDrivers and receivers\n"
                             "  -f\tFull paths\n"
                             "  -r\tDimensions\n"
                             "  -s\tShow self as full path\n");
		           goto ss_done;
                 }
               }
               if (cp) {
                 SUBSH_FORK(0);
		 cbd.out    = rdr[STDOUT_FILENO];
		 if (cbd.extra || !isatty(cbd.out->Fd())
                               || (shx & (SHX_NOWAIT))) {
		   cbd.punc = "\n";
		 }
                 cbd.wcnt = argc - i;
	         cbd.wild = &argv[i];
    		 curr_inst->forSigs(scanSig,&cbd);
		 if ('\n' != *cbd.punc) rdr[STDOUT_FILENO]->printf("\n");
		 SUBSH_JOIN(STS_NORMAL);
		 LOG_PROC(pid,argc,argv);
                 goto ss_done;
               }
      case 1:  SUBSH_FORK(0);
               if (cbd.extra || !isatty(cbd.out->Fd())
                             || (shx & (SHX_NOWAIT))) {
                 cbd.punc = "\n";
               }
               curr_inst->forSigs(scanSig,&cbd);
               if ('\n' != *cbd.punc) rdr[STDOUT_FILENO]->printf("\n");
               SUBSH_JOIN(STS_NORMAL);
               LOG_PROC(pid,argc,argv);
               goto ss_done;
      }
    ss_done:
      goto done;

    case SHL_LI:
      BZEROS(cbd);
      cbd.indent = " ";
      cbd.punc   = " ";
      cbd.extra  = (LI_CURR|LI_PRINT);
      cbd.out    = rdr[STDOUT_FILENO];
      switch(argc) {
      default: for (i=1; (cp = argv[i]) && '-' == *argv[i] ; i++) {
                 char ch;
                 while ((ch = *++cp)) switch(ch) {
                 default:
                   std[STDERR_FILENO]->printf("li: invalid argument (-%c).\n",
                                              ch);
                   goto syntax2;
                 case 'e':
                   cbd.extra |= LI_ESC_BRKT;
                   break;
                 case 'R':
                   cbd.extra |= LI_RECURSE;
                   cbd.punc   = "\n";
                   cbd.fn     = scanInst;
                   break;
                 case 'f':
                   cbd.extra |= LI_FULLPATH;
                   cbd.full   = new String;
                   break;
                 case 'h':
                   rdr[STDOUT_FILENO]->printf("  -R\tRecurse\n"
                                              "  -f\tFull paths\n"
                                              "  -e\tEscape brackets\n");
		   goto li_done;
                 }
               }
               if (cp) {
                 SUBSH_FORK(0);
		 cbd.out    = rdr[STDOUT_FILENO];
		 if (!isatty(cbd.out->Fd()) || (shx & (SHX_NOWAIT))) {
		   cbd.punc = "\n";
		 }
                 for (i =1; (cp = argv[i]);) {
    		   curr_inst->forSpec(&i,argc,argv,scanInst,&cbd);
	         }
		 rdr[STDOUT_FILENO]->printf("\n");
		 SUBSH_JOIN(STS_NORMAL);
		 LOG_PROC(pid,argc,argv);
                 goto li_done;
               }
      case 1:  SUBSH_FORK(0);
               if (!isatty(cbd.out->Fd()) || (shx & (SHX_NOWAIT))) {
                 cbd.punc = "\n";
               }
               curr_inst->forChildren(scanInst,&cbd);
               rdr[STDOUT_FILENO]->printf("\n");
               SUBSH_JOIN(STS_NORMAL);
               LOG_PROC(pid,argc,argv);
               goto li_done;
      }
    li_done:
      DELETE(cbd.full);
      goto done;
    case SHL_LPM:
      BZEROS(cbd);
      cbd.indent = " ";
      cbd.punc   = " ";
      cbd.out    = rdr[STDOUT_FILENO];
      switch(argc) {
      default: for (i=1; (cp = argv[i]) && '-' == *argv[i] ; i++) {
                 char ch;
                 while ((ch = *++cp)) switch(ch) {
                 default:
                   std[STDERR_FILENO]->printf("lpm: invalid argument (-%c).\n",
                                              ch);
                   goto syntax2;
                 case 'r':
                   cbd.extra |= LPM_RANGE;
                   break;
                 case 'C':
                   cbd.extra |= LPM_C;
                   break;
                 case 'h':
                   rdr[STDOUT_FILENO]->printf("  -r\tShow ranges\n");
		   goto lpm_done;
                 }
               }
               if (cp) {
		 cbd.wild = &argv[1];
               }
      case 1:  SUBSH_FORK(0);
               if (!isatty(cbd.out->Fd()) || (shx & (SHX_NOWAIT))) {
                 cbd.punc = "\n";
               }
               curr_inst->forParms(prntParm,&cbd);
               if (!cbd.matched) {
		 if (cbd.wild) std[STDERR_FILENO]->printf("lpm: No match.\n");
		 else          rdr[STDOUT_FILENO]->printf("\n");
               }
               SUBSH_JOIN(STS_NORMAL);
               LOG_PROC(pid,argc,argv);
               goto lpm_done;
      }
    lpm_done:
      DELETE(cbd.full);
      goto done;
    }
  }

  sts = STS_NOTCOMM;
  goto done;

syntax:
  std[STDERR_FILENO]->printf("Command not understood.\n");
syntax2:
  sts = STS_SYNTAX;
  goto done;

new_curr:
  if (curr_inst != inst) {
    path_buff = "";
    (curr_inst = inst)->path(&path_buff);
  }
done:
  DELETE(tmp);
  return sts;
}

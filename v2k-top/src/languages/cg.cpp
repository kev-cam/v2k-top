/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * cg_cpp_rcsid() {return "$Id: cg.cpp,v 1.13 2012/10/16 22:38:45 cvs Exp $";}

#define VERILOG3
#define VERILOG4

#include "system.h"
#define  HAVE_CODE_MODE
#include "error.h"
#include "args.h"
#include "env.h"
#include "error.h"
#include "language.h"
#include "poolmngr.h"
#include "strdb.h"
#include "v2k_mem.h"
#include "v2k.h"
#include "dyn.h"
#define NEED_VERILOG
#define NEED_COMMANDS
#include "tokpool.h"
#include "verilog.h"
#include "dump.h"
#include "mod_pool.h"
#include "cg.h"
#include "primes.h"
#include "dyn.h"
#include "shell.h"
#include "simulation.h"
#include "sim_kern.h"

const char *SimLibName = "${" V2K_SIM_LIB "}";

extern "C" eSTS v2kCodegen(eCodeMode cm)
{
  Inst *ti  = Inst::Root;
  eSTS  sts = loadSimKrnl(SimLibName);

#if DBGLVL > 0
  if (Simulator::Kernel) fprintf(stderr,"Sim library: %s\n",Simulator::Kernel->whoami());
#endif

  if (sts) return sts;

  if (ti) {
    int n = Inst::Root->children;
    ti    = ti->Root->child;
    for(; n-- > 0 ; ti++) {
      switch (ti->codeGen(true,cm)) {
      case CG_OK: continue;
      default:    goto failed;
      }
    }

    while (CgDataCC::Pend) {
      GatherCC(&sts);
    }
  }

 failed:
  return sts;
}

eSTS Inst::initSim(eSTS sts)
{
  int   n   = children;
  Inst *ci  = child;
  if (cg_data) switch (cg_data->cgt) { 
  case CGT_O:   
  case CGT_SO: if (!cg_data->o()->md_cls) {
    Module *mod   = module_map(self->Index());
    char    buff[256],
           *ep_nm = Simulator::Kernel->simModEp(
                                    EscpdCname(mod->name),cg_data->chksum,
                                                          cg_data->vrsn,buff);
    void *ep = cg_data->o()->obj->DlSym(ep_nm);

    cg_data->o()->md_cls = (*(CodeInst)ep)(this);

    sts = STS_NORMAL;
  }}

  if (STS_NORMAL == sts) for(; n-- > 0 ; ci++) if (sts = ci->initSim()) break;

  return sts;
}

eSTS initSim()
{
  if (Simulator::Kernel && 
      Simulator::Kernel->Status() >= SMST_INITIALIZED) return STS_NORMAL;

  eSTS sts = Inst::Root->initSim(STS_NORMAL);

  if (STS_NORMAL == sts) {
    if (!Simulator::Kernel){
      sts = loadSimKrnl(SimLibName);
    }
  }

  return sts;
}

int CheckSum(const char *str)
{
  int c = 0;

  while (*str) c += Prime256[*str++];

  return c;
}

CgDataCC *CgDataCC::Pend;

CgDataCC::~CgDataCC()
{
  if (CGT_CC_CUT != cgt) {
    CgDataCC **pp = &Pend;

    while (*pp != this) {
      pp = &((*pp)->next);
    }

    *pp = next;
  }
}

int CgDataCC::Gather(eSTS *sts,File *mk_lib)
{
  CgDataCC  *dp,
           **p_nxt,
            *nxt;
  int       todo   = 0;

  while (dp = *(p_nxt = &Pend)) {
    for (; dp ; dp = *p_nxt) {
      switch (dp->cc_sts) {
      case STS_NORMAL: if (mk_lib) continue;
	               dp->LoadObj();
                       break;
      case STS_ALLBAD: todo++;
                       p_nxt = &dp->next;
                       continue;
      }
      if (STS_NORMAL != dp->cc_sts) *sts = dp->cc_sts;
      *p_nxt  = dp->next;
      dp->cgt = CGT_CC_CUT;
      delete dp;
    }
    if (todo) Shell::CSH->WaitJobs(1);    
  } 

  if (mk_lib && !todo) {
    assert (0);
  }

  return todo;
}

eSTS CgDataCC::LoadObj()
{
  File o_f(code.str());

  o_f.changeType(FT_Obj);

  envExpand(o_f);

  DynObj *obj = new DynObj(o_f);

  eSTS sts = obj->Status();

  switch (sts) {
  case STS_NORMAL: ip->cg_data = new CgDataO(this,obj); break;
  default:         delete obj;    
  }

  return cc_sts = sts;
}

void *CgDataO::init(void *ip)
{
  return 0;
}

CgDataO::~CgDataO()
{
  delete obj;
}

eSTS Inst::Compile(bool bg,bool dbg)
{
  String        cc;
  StringStream  ss(&cc);
  CgDataCC     *ccp = cg_data->cc(); 
  Filename      stem(ccp->code.basename());

  stem.changeType();

  cc.printf("(%s cd %s; exec $V2K_CC_SIM -o %s.o -I. %s.cpp) %s\n",
            dbg ? "set echo=1;" 
                : "",
            ccp->code.dir(),stem.str(),stem.str(),
            bg ? "&"
               : "");

  if (VRB_CODE & Arg.verbosity) {
    envExpand(&cc,Stream::Stdio(STDERR_FILENO));
    fprintf(stderr,"Compiling: %s\n",cc.str());
  }

  ccp->cc_sts = STS_ALLBAD;

  eSTS sts = Shell::CSH->start(&ss,
                               Stream::Stdio(STDIN_FILENO),
                               Stream::Stdio(STDOUT_FILENO),
                               Stream::Stdio(STDERR_FILENO),
                               0,SMT_NONE,bg ? &ccp->cc_sts
                                             : 0);
  if (!bg) {
    ccp->cc_sts = sts;
  }

  return sts;
}

eCG Inst::codeGen(bool recurse,eCodeMode cm)
{
  const char *cm_ft = "cpp",
             *cm_mn = "module.cpp";

  if (cm & CM_PARC) {
    cm_ft = "pc"; 
    cm_mn = "module.pc";
  }

  switch (self->Type()) {
  case REF_MODULE:
  {
    String        params;
    StringStream  sp(&params); 
    Module       *mod = module_map(self->Index());
    int           m,
                  n   = mod->parmCount();
    while (n-- > 0) {
      prntParm(sp.strm(),this,LPM_C|LPM_RANGE,0,&m,n);
    }
    int   chksum = CheckSum(params);
    File  ph;
    const char *mn = EscpdCname(mod->name);
    reposPath(&ph,"${" V2K_CODE_REPOS "}",mn,"",OBJ_Null);
    char param[64];
    int v = 0;
    for (;; v++) {
      sprintf(param,"param+%x+%x.%s",chksum,v,cm_ft);
      break;
    }

    String       cpp;
    StringStream m_cls(&cpp);
    Stream       hcf;

    ph += cm_mn;
    hcf.Open(&ph,FM(FM_WRITE|FM_CREATE|FM_MKDIR)); 
    mod->codegen(this,&m_cls,&hcf,cm);
    hcf.printf("\n"
               "#define _M_CLASS_ _MODULE_NAME(%s,%x,%d)\n"
               "class _M_CLASS_ : public _SimObj {\n"
               "public:\n"
               "  _Inst *ip;\n",
               mn,chksum,v);
    hcf.printf("%s\n"
	       "static inline _M_CLASS_ *new_inst(_Inst *ip) {\n"
               "  return new _M_CLASS_(ip);\n"
               "}\n\n"
               "extern \"C\" _M_CLASS_ *_M_CLASS_(_Inst *ip) {\n"
               "  return new_inst(ip);\n"
               "}\n",cpp.str());
    hcf.Close();
    ph -= cm_mn;

    ph += &param[0];
    hcf.Open(&ph,FM(FM_WRITE|FM_CREATE)); 
    hcf.printf("\n#define _MODULE  %s",mn);
    hcf.printf("\n#define _PRM_SET %x",chksum);
    hcf.printf("\n#define _VERSION %d\n",v);
    hcf.printf("\n#include \"simulation.h\"\n\n",v);
    hcf.Write(params.str(),params.len());
    hcf.printf("\n#include \"module.%s\"\n",cm_ft);
    hcf.Close();

    cg_data = new CgDataCC(ph,this,chksum,v);

    Compile(Job::JobLimit > 1,Arg.verbosity & DMP_VERBOSE);

    if (recurse) {
      int   n  = children;
      Inst *ip = child;
      for(; n-- > 0 ; ip++) {
        switch (ip->codeGen(true,cm)) {
        case CG_OK: continue;
        default:    break;
        }
      }      
    }
    break;
  } 
  case REF_PRIM:
    break;
  case REF_VPRIM:
    break;
  case REF_UNKNOWN:
    assert(REF_MODULE == parent->self->Type());
  default:
    assert(0);
  case REF_UNMATCHED:;
  }

  return CG_OK;  
}

int cgData::newCntxt(const char *cp,int index)
{
  REALLOC(c_data,cntxts+1,prcData);
  c_data[cntxts].epc = 0;
  c_data[cntxts].idx = index;
  c_data[cntxts].s   = cp;
  rpts = 0;

  return cntxts++;
}

void cgData::addCntxtC(Stream *out,
		       const char *pream,const char *post,const char *alt)
{
  if (alt && !cntxt_ln) {
    out->printf(alt);
  } else {
    out->printf("%s",pream);
    char *cp = cntxt_nm;
    for (; *cp ; cp++) {
      out->printf('.' == *cp ? "__"
                             : "%c",*cp);
    }
    out->printf("%s",post);
  }
}

static char ret_buf[8][128];
static int  rbi;

const char *EscpdCname(const char *escpd)
{
  const char *ep = escpd;

  if ('\\' == *ep) {
    int   ch,
          i;
    char *rp = &ret_buf[i = rbi++][0];
    *rp++ = '_';
    while (ch = *ep++) {
      if (isalnum(ch)) {
        *rp++ = ch;
      } else {
        sprintf(rp,"_%02x",ch);
        rp += 3;
      }
    }
    return &ret_buf[i][0];
  }

  return escpd;
}

const char *EscpdCname(poolRef ref)
{
  const char *escpd = strDeref(ref);
  const char *ep    = escpd;

  if ('\\' == *ep || CPP_POOL == ref.pool) {
    int   ch,
          i;
    char *rp = &ret_buf[i = rbi++][0];
    *rp++ = '_';
    while (ch = *ep++) {
      if (isalnum(ch)) {
        *rp++ = ch;
      } else {
        sprintf(rp,"_%02x",ch);
        rp += 3;
      }
    }
    return &ret_buf[i][0];
  }

  return escpd;
}

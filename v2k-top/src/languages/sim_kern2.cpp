/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * sim_kern2_cpp_rcsid() {return "$Id: sim_kern2.cpp,v 1.16 2009/07/08 08:43:38 dkc Exp $";}

#define VERILOG3
#define VERILOG4

#include "system.h"
#include "args.h"
#include "env.h"
#include "error.h"
#include "language.h"
#include "strdb.h"
#include "v2k_mem.h"
#include "tokpool.h"
#include "verilog.h"
#define  SIM_IP Inst
#include "sim_extra.h"
#include "simulation.h"
#include "sim_kern.h"

void *v2kSim::Bucket[29];
Pond *v2kSim::Ponds[29];

void v2kSim::fillBucket(int b)
{
  int sz = 1 << (b + 2);
  switch (sz) {
  case 4:  sz  = 8; break;
  default: sz += 8; break;
  }
  int items  = 4000/sz;
  if (!items) items = 1;
  Pond *pond = (Pond *)Malloc(sizeof(Pond) + (items * sz));
  pond->next = Ponds[b];
  Ponds[b]   = pond;
  char *bp   = (char *)pond;
  bp        += sizeof(Pond);
  Bucket[b]  = bp;
  while (--items > 0) { 
    *(void **)bp = &bp[sz];
    bp += sz;
  }
  *(void **)bp = 0;
}

void v2kSim::Recycle(void *ptr)
{
  char *cp = (char *)ptr;
  int   b  = cp[-1];

  switch (b) {
  case 0:  cp -= 4; break;
  default: cp -= 8; break;
  }

  *(void **)cp = Bucket[b];
  Bucket[b]    = (void *)cp;
}

extern "C" void v2kSimEval(Inst *ip,int si,int drv_i,void *drv_v)
{
  V2kSimData *sd = (V2kSimData *)ip->sim_data;
  v2kSimItem *sp = (v2kSimItem *)&sd->data[si];

  switch (sp->typ) {
  case SI_SIG: { v2kSI_SIG *sig = (v2kSI_SIG *)sp;
                 switch (sig->size) {
                 case 1: { char *i_drv_v = (char *)drv_v,
                                *rslvd   = (char *)sig->data;
                           switch (sig->drv) {
                           case 1: *rslvd   = i_drv_v[drv_i];
                           }
                         } break;
                 case 4: { int *i_drv_v = (int *)drv_v,
                               *rslvd   = (int *)sig->data;
                           switch (sig->drv) {
                           case 1: *rslvd   = i_drv_v[drv_i];
                           }
                         } break;
                 }
                 v2kSense *sense = sig->sense;
                 if (sense) switch (sense->typ) {
                 case SNS_CNTXT: sense->C()->activate(); break;
	         default:        assert(0);
                 }
               } break;
  default:     assert(0);
  }
}

void v2kSim::Sensitize(_Context *cntxt,_Sense *sens)
{
  cntxt->sense = sens;

  v2kSense *sense = (v2kSense *)sens;

  switch (sense->typ) {
  case SNS_CNTXT:   sense->C()->cntxt = cntxt; break;
  case SNS_ARRAY: { int c = sense->A()->count;
                    while (c-- > 0) Sensitize(cntxt,
                                              (_Sense *)sense->A()->sense[c]);
                  } break;
  default:          assert(0);
  }  
}

void v2kSim::Desensitize(_Context *cntxt)
{
  if (cntxt->sense) {
    v2kSense *sense = (v2kSense *)cntxt->sense;
    cntxt->sense    = 0;
    switch (sense->typ) {
    }
  }  
}

v2kSenseCntxt::v2kSenseCntxt(_Context *ctx)
{
  typ   = SNS_CNTXT;
  cntxt = ctx;
}

v2kSense *v2kAddSensEl(v2kSenseArray **p_array,v2kSense *sense)
{
  v2kSenseArray *array;

  if (p_array) {
    array    = *p_array;
    array    = (v2kSenseArray *)Realloc(array,sizeof(v2kSenseArray)
                                        + array->count * sizeof(v2kSense *));
    *p_array = array;
  } else {
    array        = (v2kSenseArray *)Malloc(sizeof(v2kSenseArray));
    array->count = 0;
    array->typ   = SNS_ARRAY;
  }

  array->sense[array->count++] = sense;

  return (v2kSense *)array;
}

void v2kSimItem::addSense(_Sense **p_sens)
{
  v2kSense *sns0  = (v2kSense *)*p_sens,
           *sense = new v2kSenseCntxt();

  if (sns0) {
    switch (sns0->typ) {
    case SNS_CNTXT:   *p_sens = (_Sense *)v2kAddSensEl(0,sense);   break;
    case SNS_ARRAY:   v2kAddSensEl((v2kSenseArray**)p_sens,sense); break;
    default:          assert(!"Bad Sense Object");
    }
  } else {
    *p_sens = (_Sense *)sense;
  }

  switch (typ) {
  case SI_SIG: { v2kSI_SIG *sp = (v2kSI_SIG *)this;
                 if (sp->sense) {
                   assert(("NIY",0));
                 } else {
                   sp->sense = sense;
                 }
               } break;
  default:     assert(0);
  }    
}

extern "C" void v2kSimAddSense(Inst *ip,_Sense **p_sens,_PortFlags *pf)
{
  V2kSimData *sd    = (V2kSimData *)ip->sim_data;
  v2kSimItem *sp    = (v2kSimItem *)&sd->data[pf->index];

  sp->addSense(p_sens);
}

extern "C" void v2kSimSigInit(Inst *ip,_PortRefBase *prt_b,
                              int idx,int pf,int drv,int vsz)
{
  V2kSimData *sd = (V2kSimData *)ip->sim_data;
  char       *cp = (char *)sd;

  if (!sd) {
    int max      = ip->simSize() + sizeof(V2kSimData);
    cp           = MALLOC2_N(max,char);
    sd           = (V2kSimData *)cp;
    sd->max      = max;
    sd->used     = 0;
    ip->sim_data = sd;
  }

  if (PRT_NC == pf) {
    int used = sd->used,
        need = vsz + sizeof(v2kSI_SIG),
	inc  = need/sizeof(*sd->data);
    if (inc * sizeof(v2kSimItem) < need) inc++; 
    need = ((used+inc) * sizeof(v2kSimItem)) + sizeof(V2kSimData);
    if (need >= sd->max) {
      cp = (char *)sd;
      REALLOC2(cp,need,char);
      ip->sim_data = (SimData *)cp;
      sd           = (V2kSimData *)cp;
      sd->max      = need;
    }
    v2kSI_SIG *sp = (v2kSI_SIG *)&sd->data[prt_b->pf.index = used];
    assert(0 == ((intptr_t)sp%sizeof(void*)));
    sp->sns   = SNS_SIM_ITEM;
    sp->typ   = SI_SIG;
    sp->sense = 0;
    sp->size  = vsz;
    sp->drv   = drv;
    sp->idx   = idx;
    if (prt_b->vp) BCOPY(prt_b->vp,sp->data,vsz);
    else           BZERO(sp->data,vsz);
    prt_b->vp = sp->data;
    sd->used += inc;
    assert(sd->used <= sd->max);
  } else {
    assert(0);
  }
 
}

int Inst::simSize()
{
  return sim_space;
}

int Inst::simSpace(int sets,int wires)
{
  if (wires <= 8) {
    if (sets < 2) return 1;
    if (sets < 3) return 2;
    if (sets < 5) return 4;
  }
  if (wires <= 16) {
    if (sets < 2) return 2;
    if (sets < 3) return 4;
    if (sets < 5) return 8;
  }
  if (wires <= 32) {
    if (sets < 2) return 4;
    if (sets < 3) return 8;
    if (sets < 5) return 16;
  }

  if (wires & 31) wires = (wires | 31) + 1;

  return sets * (wires/32);
}

const char *cppType(typDesc *ptd,char *ret_buff,const char *msk_cls)
{
  const char *frmt = ",%d,1",
             *typ  = "int";
  int         wrs1 = ptd->size,
              wrs2 = wrs1;

  if (wrs1 <= 8)  { typ = "char";  goto done; }
  if (wrs1 <= 16) { typ = "short"; goto done; }
  if (wrs1 <= 32) { typ = "int";   goto done; }

  if (wrs1 & 31) wrs2 = (wrs1 | 31) + 1;
  frmt = ",%d,%d";

 done:
  char *rp = ret_buff;
  sprintf(rp,msk_cls,typ);
  while (*rp) rp++;
  sprintf(rp,frmt,wrs1,wrs2);

  return ret_buff;
}

const char *cppType(int wires,char *ret_buff)
{
  typDesc td;

  td.size     = wires;
  td.indirect = 0;
  td.vt       = VT_VOID;

  return cppType(&td,ret_buff,"%s");
}

const char *Inst::cppPortType(int idx,char *ret_buff_p,char *ret_buff_u)
{
  PortInfo info;
  int      dimn[32];

  info.index = idx;
  info.width = 0;
  info.dimn  = dimn;

  *dimn = -1;

  getPortInfo(NullRef,&info,PRT_ANY);

  ASSERT(info.width);

  if (ret_buff_u) *ret_buff_u = '\0';
  
  int p = info.pckd;
  for (; info.unpckd-- > 0 ; p++) {
    info.width /= dimn[p];
    if (ret_buff_u) {
      sprintf(ret_buff_u,"[%d]",dimn[p]);
      ret_buff_u += strlen(ret_buff_u);
    }
  }

  return cppType(info.width,ret_buff_p);
}


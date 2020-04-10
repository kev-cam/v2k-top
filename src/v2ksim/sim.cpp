/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * sim_cpp_rcsid() {return "$Id: sim.cpp,v 1.25 2012/10/16 23:16:34 cvs Exp $";}

#include "system.h"
#include "error.h"

#include "fenum.h"
#include "strfunc.h"
#include "error.h"
#include "file.h"
#include "dyn.h"
#include "v2k_mem.h"
#include "sigtrap.h"

#include "sim_extra.h"
#include "simulation.h"
#include "sim_kern.h"

v2kSim v2kSimKrnl;

extern "C" {

extern void V2KsetSimKrnl(Simulator *);

int v2kInit(DynObj *lib_handle)
{
  if (! *(void **)&v2kSimKrnl) {
    v2kSim tmp; bcopy(&tmp,&v2kSimKrnl,sizeof(void *));
    v2kSimKrnl.init();
  }

  V2KsetSimKrnl(&v2kSimKrnl);

  return 0;
}

int v2kUnload(void *p)
{
  V2KsetSimKrnl(0);

  return 0;
}

}


_parameter::_parameter(int typ,_param_range *rng,int val)
{
  switch (typ) {
  case PARAM_TYPE(UINT):  
  case PARAM_TYPE(LUINT): value = val;
                          break;
  default:                assert(("NIY",0));
  }
}

void _PortRefBase::SigInit(int idx,int pf,int drv,int vsz)
{
  v2kSimKrnl.SigInit(this,idx,pf,drv,vsz);
}

inline void v2kSim::StartProc(_Context *cntxt,C_entry ep,_SimObj *sp)
{
  BZEROP(cntxt);
  *Q_nxt                  = cntxt;
  *(Q_nxt = &cntxt->next) = 0;
  cntxt->call             = ep;
  cntxt->Push(0);
}

void _SimObj::StartProc(_Context *cntxt,C_entry ep)
{
  cntxt->lbl = 0;
  v2kSimKrnl.StartProc(cntxt,ep,this);
}

bool _SimObj::At(_Context *cntxt,int lbl,C_entry ep,int val)
{
  if (cntxt->data.c.waiting) {
    if (val) {
      cntxt->data.c.waiting = 0;
      return false;
    }
  } else {
    cntxt->data.c.waiting = 1;
  }
  cntxt->lbl = lbl;
  return true;
}

bool _SimObj::At(_Context *tf,int lbl,_Context *cntxt,int val)
{
  if (cntxt->data.c.waiting) {
    if (val) {
      cntxt->data.c.waiting = 0;
      return false;
    }
  } else {
    cntxt->data.c.waiting = 1;
  }
  tf->lbl = lbl;
  return true;
}

bool _SimObj::Wait(_Context *cntxt,int lbl,C_entry ep,int val)
{
  if (cntxt->data.c.waiting) {
    if (val) {
      cntxt->data.c.waiting = 0;
      return false;
    }
  } else {
    cntxt->data.c.waiting = 1;
  }
  cntxt->lbl = lbl;
  return true;
}

bool _SimObj::Wait(_Context *tf,int lbl,_Context *cntxt,int val)
{
  if (cntxt->data.c.waiting) {
    if (val) {
      cntxt->data.c.waiting = 0;
      return false;
    }
  } else {
    cntxt->data.c.waiting = 1;
  }
  tf->lbl = lbl;
  return true;
}

inline int v2kSim::Suspend(_Context *cntxt,int lbl,C_entry ep,_SimObj *ip)
{
  cntxt->call = ep;
  return cntxt->lbl  = lbl;
}

int _SimObj::Suspend(_Context *cntxt,int lbl,C_entry ep)
{
  return v2kSimKrnl.Suspend(cntxt,lbl,ep,this);
}

inline int v2kSim::Delay(_Context *cntxt,int lbl,C_entry ep,_SimTime time,_SimObj *ip)
{
  cntxt->call = ep;
  cntxt->time = time;
  cntxt->lbl  = lbl;

  Schedule(cntxt,time);

  return lbl;
}

inline int v2kSim::Delay(_Context *cntxt,int lbl,_Context *ep,_SimTime time,_SimObj *ip)
{
  ep->time = time;
  return cntxt->lbl = lbl;
}

int _SimObj::Delay(_Context *cntxt,int lbl,C_entry ep,_SimTime time)
{
  return v2kSimKrnl.Delay(cntxt,lbl,ep,time,this);
}

int _SimObj::Delay(_Context *cntxt,int lbl,_Context *ep,_SimTime time)
{
  return v2kSimKrnl.Delay(cntxt,lbl,ep,time,this);
}

bool _SimObj::Posedge(_PortFlags *pf)
{
  if (pf->edge != 1) return false;

  return 1;
}

bool _SimObj::Negedge(_PortFlags *pf)
{
  if (pf->edge != -1) return false;

  return 1;
}

bool _SimObj::Anyedge(_PortFlags *pf)
{
  if (pf->edge == 0) return false;

  return 1;
}


v2kSim::v2kSim()
{
  init();
  sim_log = 0;
}    

Stream *v2kSim::SimLog()
{
  return sim_log ? sim_log
                 : sim_log->Stdio(STDOUT_FILENO);
}

void v2kSim::init()
{
  status = SMST_LOADED;
  Irq    = 0; 
  Q_nxt  = &Q;
  Q      = 0;
}

void v2kSim::SigInit(_PortRefBase *prt,int idx,int pf,int drv,int vsz)
{
  v2kSimSigInit(ip,prt,idx,pf,drv,vsz);
}

void v2kSim::setIrq(int flags)
{
  Irq |= flags;
}

static SigDo sim_int(SigMap map,void *sp)
{
  ((v2kSim *)sp)->setIrq(SIM_IRQ_USR);

  return SGD_CONT;
}

eSMST v2kSim::Status()
{
  return status;
}

void v2kSim::setStatus(eSMST sim_state)
{
  *(int *)&status |= sim_state;
}

void v2kSim::clrStatus(eSMST sim_state)
{
  *(int *)&status &= ~sim_state;
}

eSTS v2kSim::Start(bool from_shell)
{
  _Event  *dq;
  eSMST    cause;
  SigTrap  int_trap(SM_INT,sim_int,SA_NONE,this,this);

  if ((cause = (eSMST)setjmp(top))) {
    setStatus(cause);
    goto stopped;
  }

  setStatus(SMST_RUNNING);

  while ((dq = Q)) {

    if (Irq) {
      if (Irq & SIM_IRQ_USR) {
        Irq &= ~SIM_IRQ_USR;
        return STS_INTERRUPT;
      }
    }

    Time = Q->time;
    Q    = Q->next;

    (*dq->call)(dq);

  }

 stopped:
  clrStatus(SMST_RUNNING);

  return STS_NORMAL;
}

void v2kSim::Schedule(_Event *event,_SimTime reltime)
{
  _Event **ppq = &Q,
          *pq;

  event->time.t = (reltime.t += Time.t);

  while ((pq = *ppq)) {
    if (reltime.t < pq->time.t) break; 
    ppq = &pq->next;
  }

  event->next = pq;
  *ppq        = event;
}

const char *v2kSim::whoami()
{
  return "v2k";
}

char *v2kSim::simModEp(const char *mod,int chksum,int vrsn,char *rbuff)
{
  sprintf(rbuff,"%s__%x__%d",mod,chksum,vrsn);

  return rbuff;
}

int _Context::Push(int lbl)
{
  if (dpth == dmax) {
    dmax += 4;
    REALLOC(stck,dmax,int);
  }
  return stck[dpth++] = lbl;
}

int _Context::Pop()
{
  return stck[--dpth];
}

inline void v2kSim::setIp(void *crrnt_ip)
{
  ip = crrnt_ip;
}

extern "C" void setSimIp(void *ip)
{
  v2kSimKrnl.setIp(ip);
}

inline void v2kSim::addSense(_Sense **p_sens,_PortFlags *pf)
{
  v2kSimAddSense(ip,p_sens,pf);
}

void _SimObj::PortUsage(_Sense **p_sens,_PortFlags *pf,int usg)
{
  if (usg & USG_SENSE) {
    v2kSimKrnl.addSense(p_sens,pf);
  }
}

void _SimObj::PortUsageAuto(_Sense **p_sens,_RefBase *prt,int usg)
{
  if (usg & (USG_SENSE|USG_READ)) {
    v2kSimKrnl.addSense(p_sens,&prt->pb()->pf);
  }
}

void _SimObj::Sensitize(_Context *cntxt,_Sense *sens)
{
  v2kSimKrnl.Sensitize(cntxt,sens);
}

void _SimObj::Desensitize(_Context *cntxt)
{
  v2kSimKrnl.Desensitize(cntxt);
}

void _SimObj::PushPliArgs(int i,void **args)
{
  v2kSimKrnl.pli_arg_count = i;
  v2kSimKrnl.pli_args      = args;
}

void _SimObj::PopPliArgs()
{
}

void DqNBA(void *ep)
{
  EventNBA *nba = (EventNBA *)ep;

  _PortFlags *prt   = nba->prt;
  int         drv_n = nba->drv_n;
  void       *ip    = nba->ip,
             *val   = nba->val;
  PortAssFn   afn   = nba->afn;

  v2kSimKrnl.Recycle(nba->val);

  nba->next          = v2kSimKrnl.freeNBA;
  v2kSimKrnl.freeNBA = nba;

  (*afn)(prt,val);
}

void _SimObj::NBA(_PortFlags *prt, int drv_n, void *val, PortAssFn afn, _SimTime delay,
                  int indices, ...)
{
  EventNBA *nba = v2kSimKrnl.freeNBA;

  if (nba) {
    v2kSimKrnl.freeNBA = (EventNBA *)nba->next;
  } else {
    nba       = new EventNBA();
    nba->call = DqNBA;
  }

  nba->prt   = prt;
  nba->drv_n = drv_n;
  nba->ip    = v2kSimKrnl.ip;
  nba->val   = val;
  nba->afn   = afn;

  v2kSimKrnl.Schedule(nba,delay);
}

const char _NullArg[2] = " ";

static void v2k_print(int i,void **args,int nl = 0)
{
  Stream *out = v2kSimKrnl.SimLog();

  _PLI_argU str_d(0,"%d");

  while (i > 0 ) {
    _PLI_argU *format;
    _PLI_argBase *pv = (_PLI_argBase *)*args++; i--;
    switch (pv->typ) {
    case PLI_ARG_U_PORT:
      format = &str_d;
      args--;
      goto as_str; 
    case PLI_CHAR_PTR: {
      format = (_PLI_argU *)pv;
     as_str:
      const char *fp0 = (const char *)format->val.ptr,
                 *fp  = fp0;
      int         dsh;
      char        frmt[16],
                  ch,
                 *fp2;

      while ((ch = *fp)) {
        switch (ch) {
        case '%': 
          out->Write(fp0,fp - fp0);
          *(fp2 = frmt) = '%';
          fp0 = fp++;
          if ((dsh = ('-' == (ch = *fp)))) {*++fp2 = ch;
                                    ch     = *++fp;}
          while (isdigit(ch))              {*++fp2 = ch;
                                    ch     = *++fp;}
          switch (ch) {
          case 'h':
          case 'b': ch = 'x';
          case 'x':
          case 'd': *++fp2 = ch;
              	    *++fp2 = '\0';
                    fp0    = ++fp;
            	    { _PLI_argU *dp; 
 	   	      int s = 0;
                      I64 n = 0;
		      *(void **)&dp = *args++; i--;
                      switch (dp->typ) {
                      case PLI_ARG_U_TIME:
                        break;
                      case PLI_ARG_U_S_PORT:                     
                      case PLI_ARG_U_S_VAR: s = 1;
                      case PLI_ARG_U_PORT:                     
		      case PLI_ARG_U_VAR: {
                        if (s) switch (dp->val.var.t) {
                        case 1: n = *(int8_t   *)dp->val.var.ptr; break;
                        case 2: n = *(int16_t  *)dp->val.var.ptr; break;
                        case 3: n = *(int32_t  *)dp->val.var.ptr; break;
                        case 4: n = *(int64_t  *)dp->val.var.ptr; break;
                        } else switch (dp->val.var.t) {
                        case 1: n = *(uint8_t  *)dp->val.var.ptr; break;
                        case 2: n = *(uint16_t *)dp->val.var.ptr; break;
                        case 3: n = *(uint32_t *)dp->val.var.ptr; break;
                        case 4: n = *(uint64_t *)dp->val.var.ptr; break;
                        }
                        if ((n > 0 && n >> 32) || (n < 0 && ~n >> 32)) {
                           assert(0);
                        } else {
			   int i = n;
                           out->printf(frmt,i);
		        }
                      } break;
                      default:
                        assert(0);
                      }
                    }
                    break;
          case 't': *++fp2 = 'l';
              	    *++fp2 = 'd';
              	    *++fp2 = '\0';
                    fp0    = ++fp;
            	    { _PLI_argU *dp; 
		      *(void **)&dp = *args++; i--;
                      switch (dp->typ) {
                      case PLI_ARG_U_TIME: {
                        int *pi = (int *)dp->val.ptr;
                        out->printf(frmt,*pi);
                      } break;
                      default:
                        assert(0);
                      }
                    }
                    break;
	          }
          break;
        default:  fp++;
        }
      }
      out->printf("%s",fp0);
    } break;
    default:
      assert(0);
    }
  } 

  if (nl) out->printf("\n");   
}

static void v2k_display(int i,void **args)
{
  v2k_print(i,args,1);
}
_v2k_sys_call v2k_sys_display = v2k_display;

static void v2k_write(int i,void **args)
{
  v2k_print(i,args,0);
}
_v2k_sys_call v2k_sys_write = v2k_write;

static void v2k_finish(int i,void **args)
{
  longjmp(v2kSimKrnl.top,SMST_FINISHED);
}
_v2k_sys_call v2k_sys_finish  = v2k_finish;

static _SimTime v2k_time(int i,void **args)
{
  return v2kSimKrnl.Time;
}
_v2k_sys_time v2k_sys_time  = v2k_time;

static const char *v2k_typeof(int i,void **args)
{
  static char buff[64];

  _PLI_argU *pa = (_PLI_argU *)*args;

  buff[0] = '\0';
  if (i && PLI_INT_PTR == pa->typ) {
    int        *ip = (int *)pa->val.ptr;
    const char *nm = "?";
    switch (ip[0]) {
#   define AV_TYPE(t,p,s) case VT_##t: nm = s; break;
#   include "av_types.h"  
    }
    int j = ip[1];
    while (j--) strcat(buff,"*");
    sprintf(&buff[ip[1]],"%s[%d]",nm,ip[2]);
  }

  return buff;
}
_v2k_sys_str v2k_sys_typeof = v2k_typeof;

void *v2kSim::Allocate(unsigned int size)
{
  char *ret;
#define ALLOC_FROM(B,O)\
  if (size <= 1 << (B+2)) {\
    while (!(ret = (char *)Bucket[B])) { fillBucket(B); }\
    Bucket[B] = *(void **)ret; ret += O;\
    ret[-1] = B;\
    return (void *)ret ;}
  ALLOC_FROM( 0,4)
  ALLOC_FROM( 1,8)
  ALLOC_FROM( 2,8)
  ALLOC_FROM( 3,8)
  ALLOC_FROM( 4,8)
  ALLOC_FROM( 5,8)
  ALLOC_FROM( 6,8)
  ALLOC_FROM( 7,8)
  ALLOC_FROM( 8,8)
  ALLOC_FROM( 9,8)
  ALLOC_FROM(10,8)
  ALLOC_FROM(11,8)
  ALLOC_FROM(12,8)
  ALLOC_FROM(13,8)
  ALLOC_FROM(14,8)
  ALLOC_FROM(15,8)
  ALLOC_FROM(16,8)
  ALLOC_FROM(17,8)
  ALLOC_FROM(18,8)
  ALLOC_FROM(19,8)
  ALLOC_FROM(20,8)
  ALLOC_FROM(21,8)
  ALLOC_FROM(22,8)
  ALLOC_FROM(23,8)
  ALLOC_FROM(24,8)
  ALLOC_FROM(25,8)
  ALLOC_FROM(26,8)
  ALLOC_FROM(27,8)
  ALLOC_FROM(28,8)

  assert(0);
}


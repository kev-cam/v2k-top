/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * parc_cpp_rcsid() {return "$Id: parc.cpp,v 1.45 2020/04/06 00:32:01 dkc Exp $";}

#include <stdio.h>
#ifndef __APPLE_CC__
#include <malloc.h>
#endif
#include <strings.h>
#include <stdarg.h>
#include <sched.h>
#include <stdlib.h>
#include <stdlib.h>
#include <cxxabi.h> 

#include "parc.h"
#include "pc_analysis.h"

extern "C" void exit(int);

namespace parc {

Proc::~Proc()
{
}

void Proc::cancel(Event *event) { 
  if (nop != event->fn) Kern->Desched(event);
}

int Proc::suspend(ResumeFn fn,Event *event)
{
  event->fn = fn;  
  return 1;
}

int Proc::suspend(ResumeFn fn,Event *event,void_pipe &chn)
{
  event->fn = fn;
  chn.add_writer(event);
  return 1;
}

int Proc::wait(RelTime delay,ResumeFn fn,Event *event)
{
  event->fn = fn;
  assert(0 == event->next &&
         0 == event->prev);
  Kern[cpu].Schedule(event,delay);
  return 1;
}

void Proc:: unWait(Event *event)
{
  assert(0 == event->next &&
         0 == event->prev);

  event->fn = Assert;
}

int Proc::initFork(Event *_wait,int j,int count,...)
{
  Event    &join(_wait[j]);
  intptr_t *ip = (intptr_t *)&join.fn;
  va_list   ap;
  int       w1;

  va_start(ap, count);
  for (int c = 0; c < count ; c++ ) {
    int      w  = va_arg(ap, int);
    _wait[w].fn = va_arg(ap, ResumeFn);
    if (c) {
      Kern[cpu].Schedule(&_wait[w],SCHED_IMMED);
    } else {
      w1 = w;
    }
  }
  va_end(ap);

  *ip = count; 

  return w1;
}

int  Proc::joinFork(Event *_wait,int j)
{
  Event    &join(_wait[j]);
  intptr_t *ip   = (intptr_t *)&join.fn;
  int       left = --(*ip);

  return left <= 0;
}

int podCopy(const void *src, void *dst, size_t sz)
{
  bcopy(src,dst,sz);

  return 0;
}

rx_base *podUpdate(void_signal *sig, void *src, size_t sz)
{
  void *dst = sig->any()->pValue();

  bcopy(src,dst,sz);

  return sig->receivers;
}

void void_signal::setFlag(eSigFlag flg,int w,void *xtra)
{
  (*StFlgFn())(this,flg,w,xtra);
}
  
void void_signal::setBridged(bridge *brdg) 
{
  cpu_id ownr = CPU();

  assert(MyKern()->CPU() == ownr);

  bridged = 1;

  rx_base         *sr0 = receivers;
  bridge::element *be  = (brdg->a.sig == this) ? &brdg->a
                                               : &brdg->b;
  be->mark = be->sig->mark;

  if (sr0) {
    if (SNS_BRIDGE == sr0->sns_typ) {
      if (be->master > 0) {
        bridge::element *be0 = (bridge::element *)sr0;
	assert(be0->master <= 0);
	sr0 = sr0->prev;
      } else {
	receivers = be;
      }
    } else {
      receivers = be;
    }
    be->next       = sr0->next;
    be->prev       = sr0->prev;
    be->prev->next = be;
    be->next->prev = be;
  } else {
    receivers           = be;
    be->next = be->prev = be;
  }
}

void podSigSet(void_signal *sig,eSigFlag flg,int width,void *xtra)
{
  while (width-- > 0) {
    switch (flg) {
    case SGF_BRDGD: sig->setBridged((bridge *)xtra); 
                    if (width) {
		      assert(0);
		    }
                    break;
    }
    sig = sig->sigIndex(1);
  }
}

void vecSigSet(void_signal *sig,eSigFlag flg,int width,void *xtra)
{
  switch (flg) {
  case SGF_BRDGD: sig->setBridged((bridge *)xtra); break;
  }
}

KernGlob Kernel::G;

void Kernel::Sync(int *count)
{
  pthread_mutex_lock(&G.count_lock);
  int c = ++(*count);
  pthread_mutex_unlock(&G.count_lock);
  while (G.cpus != c) {
    pthread_mutex_lock(&G.count_lock);
    c = *count;
    pthread_mutex_unlock(&G.count_lock);
  }
}

Kernel::Kernel()
 : cpu(this - Kern),
   sts(0),
   outstanding(0),
   pQ(0),
   channels(0)
{
  memset((char *)flgi,0,sizeof(flgi));
  memset((char *)flgo,0,sizeof(flgo));

  if (G.cpus <= 0) {
    char *cp = getenv("PARC_CPUS");
    G.cpus   = cp ? atoi(cp)
                  : 1;
  }

  if (cpu < G.cpus) {
    G.activity[cpu] = ACT_GO;

    if (cpu > 0) {
      pthread_create(&id,0,Start,(void *)this);
    } else {
      id  = thread_self();
      sts = KS_RUNNING;
    }
  }
}

void *Kernel::Start(void *vp)
{
  Kernel *kp = (Kernel *)vp;

  kp->Sync(&G.running);
  kp->sts = KS_RUNNING;

  kp->Continue();

  return vp;
}

Module Module::Root;

void Module::addChild(Module *pc)
{
  pc->next   = child;
  child      = pc;
  pc->parent = this;
}

void Module::detach(Module *pc)
{
  Module **scan = &child,
          *chld;
 
  while ((chld = *scan) != pc) scan = &chld->next;

  *scan        = chld->next;
  chld->parent = 0;
}

Module::~Module()
{
  if (parent) parent->detach(this);
}

Module *Module::init(Module *up,const char *nm)
{
  name = nm; 

  if (!up) {
     up = &Root;
     if (up == this) goto done;
  }

  up->addChild(this);

 done:
  return this;
}

static Module    *CurrMod;
static Proc      *CurrProc;
static Connected *CurrNetwork;
static int        CurrPos,
                  logUsage;

static uintptr_t incrCount(Module *inst,int *count)
{
  (*count)++;

  return 0;
}

static uintptr_t addInst(Module *inst,Connected **scan)
{
  Connected &last(**scan);

  (*scan)++;
  (*scan)->index = last.index+1;
  (*scan)->self  = inst;

  return 0;
}

RefTracker *Module::LogUsage()
{
  logUsage = 1;

  int count = 0;

  forSelfThenAll((scan_fn)incrCount,&count);

  Connected  *network = new Connected [count+1],
             *scan    = network;

  scan[0].index    = 0;
  forSelfThenAll((scan_fn)addInst,&scan);
  network[0].index = count;

  CurrMod     = this;
  CurrNetwork = network;
  CurrPos     = 0;

  return new RefTracker(network);
}

static uintptr_t init_processes(Module *inst,void *data)
{
  CurrMod  = inst;
  CurrProc = 0;

  inst->init_prcs();

  return 0;
}

void Module::StartAll()
{
  InitAll();
  RunAll();
}

void Module::RunAll()
{
  Kern->Start(Kern->Master());
}

void Module::InitAll()
{
  Root.forSelfThenAll(init_processes,(void *)0);
}

static EntItem *entities;

Module *NewArch(Module *up,const char *nm,const char *ent,const char *arch)
{
  EntItem   *ep  = entities;
  create_fn  cfn,
             dflt = 0;

  for (; ep ; ep = ep->next) {
    if (0 == strcmp(ep->name,ent)) {
      Arch *ap = ep->list;
      for (; ap ; ap = ap->next) {
        cfn = ap->c_fn;    
        if (arch) {
	  if (0 == strcmp(ap->name,arch)) return (*cfn)(up,nm);
        } else if (!dflt) {
          dflt = cfn;
	} else {
          throw ERR_ELAB_AMBG_ARCH;
	}
      }
    }
  }  

  if (dflt) return (*cfn)(up,nm);

  throw ERR_ELAB_NO_ARCH;
}

EntItem::EntItem(const char *nm)
{
  next = 0;
  name = nm;
}

Arch::Arch(const char *ent,const char *arch,create_fn fn)
{
  c_fn = fn;
  name = arch;

  EntItem **ep  = &entities,
              *scan;

  while ((scan = *ep)) {
    if (0 == strcmp(ent,scan->name)) goto found;
  }

  scan = *ep = new EntItem(ent);

 found:
  next       = scan->list;
  scan->list = this;
}

void Kernel::Schedule(Event *event,eSCHED where)
{
  switch (where) {
  default: assert(0);
  case SCHED_IMMED: {
      if ((event->next = pQ)) {
        event->time = pQ->time;
      } else {
        event->time = Now();
      }
      pQ = event;
    }
  }
}

void Kernel::Schedule(Event *event,Time t)
{
  Event **scan = &pQ,
         *evt,
         *last = 0;

  while ((evt = *scan) && evt->time <= t) {
    scan = &(last = evt)->next;
  }
  assert(event != evt);
  if ((event->next = evt)) {
    evt->prev = event;
  }
  if ((event->prev = last)) {
    last->next = event;
  }
  *scan = event;
}

void Kernel::Schedule(Event *event)
{
  Schedule(event,event->time);
}

void Kernel::Schedule(Event *event,RelTime delay)
{
  Time t(G.abs_time);

  t += delay;
  event->time = t;

  Schedule(event,t);
}

void Kernel::ScheduleDelta(Event *event)
{
  Time t = G.abs_time;

  t.incDelta();

  event->time = t;

  Schedule(event,t);
}

void Kernel::Desched(Event *event)
{
  Time    t    = event->time;
  Event **scan = &pQ,
         *evt;

  if (event->prev) {
    if (pQ == event) {
      pQ = event->next;
    }
    event->prev->next = event->next;
    if (event->next) {
      event->next->prev = event->prev;
    }
    event->next = event->prev = 0;
  } else if (event->next) {
    while ((evt = *scan) != event) {
      scan = &evt->next;
    }
    *scan = evt = event->next;
    if (evt) {
      evt->prev = 0;
    }
    event->next = 0;
  }
}

int Kernel::read(int frm_cpu)
{
  int        act = ACT_NO;
  kern_pipe &kp  = xcmd[frm_cpu];

   while (kp.rd_posn != kp.wr_posn) {
     int         rp = kp.rd_posn;
     const void *vp;
# define READ_VP vp = kp.data[rp++]; rp &= (K_PIPE_SIZE-1); kp.rd_posn =rp;
     READ_VP
     eKMSG km = (eKMSG)(long)vp;
     switch (km) {
     case KM_MIGRATE:  { READ_VP
                         Event    *evt  = (Event *)vp;
                         evt->proc->cpu = cpu;
                         (*evt->fn)(evt->proc);
                         write(frm_cpu,(void *)KM_DONE);
                       } break;
     case KM_PP_OWN_R: { READ_VP
                         void_pipe   *v_p  = (void_pipe *)vp;
                         READ_VP
                         pipe_client *clnt = (pipe_client *)vp;
			 if (id == v_p->rd_thrd) {
                           v_p->rd_thrd    = Kern[frm_cpu].id;
                           write(frm_cpu,(void *)KM_PP_CLNT_O,(void *)clnt);
                           act |= ACT_PIPE;
			 } else {
                           assert(0);
			 }
                       } break;
     case KM_PP_CLNT_O:
     case KM_PP_CLNT:  { READ_VP
                         pipe_client *clnt = (pipe_client *)vp;
	                 if (clnt->Ready()) {
                           Event       *evt  = clnt->cllbk;
                           if (evt->proc->CPU() == cpu) {
                             (*evt->fn)(evt->proc);
                             act |= ACT_PROC;
			   } else {
                             assert(0);
			   }
			 }
                         cpu_id n_cpu = clnt->Notify();
                         if (n_cpu >= 0) {
                           if (n_cpu == cpu) {
                             clnt->clrNotify();
			   } else {
                             write(n_cpu,(void *)KM_PP_ACK,(void *)clnt);
			   }
			 }
                       } break;
     case KM_PP_ACK:   { READ_VP
                         pipe_client *clnt = (pipe_client *)vp;
	                 clnt->clrNotify();
                       } break;
     case KM_PP_OWN_W: { READ_VP
                         void_pipe *v_p = (void_pipe *)vp;
                         READ_VP
                         Event     *evt = (Event *)vp;
			 if (id == v_p->wr_thrd) {
                           v_p->wr_thrd = Kern[frm_cpu].id;
			 }
                         write(frm_cpu,(void *)KM_EVENT,(void *)evt);
                         act |= ACT_PIPE;
                       } break;
     case KM_WAKE_RD:  { READ_VP
                         pipe_client *clnt = (pipe_client *)vp;
                         if (clnt->Ready()) {
                           Event *evt = clnt->cllbk;
                           if (evt->proc->CPU() == cpu) {
                             (*evt->fn)(evt->proc);
                             act |= ACT_PROC;
			   } else {
                             assert(0);
			   }
			 }
                       } break;
     case KM_WAKE_WR:  { READ_VP
                         void_pipe *pp = (void_pipe *)vp;
                         pp->wakeup_wr();			 
                       } break;
     case KM_SIG:
     case KM_RCVR:
     case KM_PROC:     { act |= ACT_PROC;
                         READ_VP
                         Event *evt = (Event *)vp;
                         if (evt->proc->CPU() == cpu) {
                           (*evt->fn)(evt->proc);
			 } else {
                           assert(0);
			 }
                         write(frm_cpu,(void *)KM_DONE);
                       } break;
     case KM_EVENT:    { act |= ACT_PROC;
                         READ_VP
                         Event *evt = (Event *)vp;
                         if (evt->proc->CPU() == cpu) {
                           (*evt->fn)(evt->proc);
			 } else {
                           assert(0);
			 }
                       } break;
     case KM_DONE:       outstanding--;
                         break;
     case KM_ENABLE:   { READ_VP
                         receiver *rcv = (receiver *)vp;
			 READ_VP
                         eImplicit i = (eImplicit)(long)vp;
			 rcv->enable(this,i);
                       } break;
     case KM_DISABLE:  { READ_VP
                         receiver *rcv = (receiver *)vp;
			 rcv->disable();
                       } break;
     case KM_DRV_OWN:  { READ_VP
                         void_driver *drv = (void_driver *)vp;
	                 drv->setThrd(Kern[frm_cpu].id);
                       } break;
     case KM_DRV_UPD:  { READ_VP
                         any_driver *drv = (any_driver *)vp;
	                 drv->UpdateDriver(drv);
			 write(frm_cpu,(void *)KM_DONE);
                       } break;
     default:          assert(0);
     }
   }
 
   return act;
}

int Kernel::CheckKP()
{
  int   c      = G.cpus/sizeof(long),
        i      = 0,
        active = ACT_NO;
  long *fi     = (long *)flgi;
  long *fo     = (long *)flgo;

  do {
    if (fo[i]) {
      int cf = sizeof(long);
      int i2 = cf * i;
      for (; cf > 0 ; cf--,i2++) {
        if (flgo[i2]) {
          kern_pipe &kp = Kern[i2].xcmd[cpu];
          if (kp.rd_posn == kp.wr_posn) {
            Kern[i2].flgi[cpu] = 0;
            flgo[i2]           = 0;
	  } else {
            active |= ACT_IO;
	  }
	}
      }
    }
    if (fi[i]) {
      int cf = sizeof(long);
      int i2 = cf * i;
      for (; cf > 0 ; cf--,i2++) {
        if (flgi[i2]) {
          active |= read(i2);
	}
      }
    }
  } while (++i < c);

  return active;
}

int Kernel::anyActive(volatile unsigned char *g_act,int act)
{
  int  c = G.cpus,
       q = 0;
  Time t(G.abs_time); t += TIME_MAX_REL;
  
  while (c-- > 0) {
    if (flgo[c]) {
      act |= ACT_IO; goto done;
    }
    if (flgi[c]) {
      act |= ACT_IO; goto done;
    }
    int a = G.activity[c];
    if (a) {
      if (a & ACT_SYNC_T) {
        q++;
        Time t2(this[c-cpu].abs_next);
        if (t2 < t) {
          t = t2;
        }
      } else {
        act |= ACT_OTHER; goto done;
      }
    }
  }

  if (q) {
    assert(t >= G.abs_time);
    (Time &)G.abs_time = t;
    act |= ACT_GO;
  }

 done:
  return *g_act = act;
}

void Kernel::Continue()
{
  volatile unsigned char &activity = G.activity[cpu];
  int                     cpus     = G.cpus;
  int                     act;
# ifndef NDEBUG
  Event                  *last;
  cpu_id                  self     = cpu;
# endif

  do {
    void_pipe *chan;
    Event     *curr;

    act = ACT_Q;
   top:
    int chn_actv = 1;
    while (chn_actv && (chan = channels)) {
      chn_actv = 0;
      channels = chan->next;
      if (0 == chan->wakeup_rd()) {
        chn_actv += chan->wakeup_wr();
      }
    }
    activity = act|CheckKP();
    while ((curr = pQ)) {
      if (cpus > 1) {
        abs_next = curr->time;
        if (abs_next > G.abs_time) {
          act = anyActive(&activity,outstanding ? ACT_SYNC_0
			                        : ACT_SYNC_T);
          goto top;
	}
        if (act & (ACT_SYNC_T|ACT_SYNC_0)) {
          activity = act = ACT_Q;
	}
      }
      if ((pQ = curr->next)) {
        pQ->prev = 0;
      }
      curr->next = curr->prev = 0;
      (Time &)G.abs_time = curr->time;
#     ifndef NDEBUG
      curr->time.delta   = -(1 + curr->time.delta);
#     endif
      (*curr->fn)(curr->proc);
      assert(curr->proc->CPU() == self ||
             (ResumeFn)any_driver::UpdateDriver == curr->fn);
#     ifndef NDEBUG
      last             = curr;
#     endif
    }
    activity &= ~ACT_Q;
    if (!(act = activity)) {
      act = anyActive(&activity);
    }
  } while (act);

  sts = KS_STOPPED;
}

void Kernel::Active(void_pipe *chan)
{
  chan->next = channels;
  channels   = chan;
}

int nop(Proc *)
{
  return 0;
}

int Assert(Proc *)
{
  assert(0);

  return -1;
}

Kernel Kern[MIN_THREADS];

#define DEF_CHAN_SIZE 0x2

void_pipe::void_pipe(int pkt_sz)
 : ctyp(CHNT_CP),
   sts(PSTS_NONE),
   busy(0),
   error(0)
{
  if (pkt_sz) {
    int sz        = DEF_CHAN_SIZE * pkt_sz;
    data.pc       = (pipe_buff *)calloc(1,sizeof(pipe_buff) + sz);
    data.pc->size = sz;
  } else {
    data.pc       = 0;
  }
  clients_rd      = 0;
  clients_wr      = 0;

  rd_thrd = wr_thrd = thread_self();
}

int void_pipe::wakeup_wr()
{
  thread_id self = thread_self();

  if (wr_thrd == self) {
    Event *evt0 = clients_wr,
          *evt;

    busy = 0;

    if (evt0) {
      do {
        clients_wr   = (evt = clients_wr)->next;
        Kernel *kp   = MyKern();
        int     cpu  = kp->CPU();
        int     ownr = evt->proc->CPU();
        if (ownr == cpu) {
          (*evt->fn)(evt->proc);
        } else {
          kp->outStanding();
          kp->write(ownr,(void *)KM_PROC,(void *)evt);
        }
      } while (clients_wr && evt != clients_wr);
    }
  } else {
    int cpu = Kern->GetCPU(self);

    Kern[cpu].write(Kern->GetCPU(wr_thrd),(void *)KM_WAKE_WR,(void*)this);
  }

  return busy;
}

int void_pipe::wakeup_rd()
{
  pipe_client *clnt0 = clients_rd,
              *clnt;

  busy = 0;

  if (clnt0) {
    do {
      clients_rd = (clnt = clients_rd)->next;
      if (clnt->ready) {
        Kernel *kp   = MyKern();
        int     cpu  = kp->CPU();
        Event  *evt  = clnt->cllbk;
        int     ownr = evt->proc->CPU();
        if (ownr == cpu) {
          (*evt->fn)(evt->proc);
        } else {
          kp->write(ownr,(void *)KM_WAKE_RD,(void *)clnt);
        }
      }
    } while (clnt0 != clients_rd);
  }

  return busy;
}

void pipe_client::disable()
{
  if (cllbk) cllbk->fn = 0;
}

int pipe_client::set_cllbk(ResumeFn fn,int sz)
{
  cllbk->fn = fn;
  ready     = sz;

  chan->wakeup_wr();

  return cllbk->fn == fn;
}

int pipe_client::set_cllbk(void_pipe *chn,Event *wt,ResumeFn fn,int sz)
{
  (cllbk=wt)->fn = fn;
  ready          = sz;

  init(chn,wt);

  chan->wakeup_wr();

  return cllbk->fn == fn;
}

eChanSts void_pipe::readV(void *dp,int cnt,pipe_client *clnt,
			                   Event *wt,ResumeFn fn,int pkt)
{
  pipe_buff *bf   = data.pc;
  thread_id  thrd = thread_self();   
  int        ret  = 0;

  if (clnt->sts & CSTS_LOCK) {
    goto ret_block;
  }

  if (rd_thrd != thrd) {
    if (clnt->Notify() < 0) {
      int ownr = Kern->GetCPU(rd_thrd);
      int self = Kern->GetCPU(thrd);
      clnt->setNotify(self);
      Kern[self].write(ownr,(void *)KM_PP_OWN_R,(void *)this,(void *)clnt);
    }
    goto block;
  }

  int retry;
 retry:
  if (bf->rd_posn != bf->wr_posn) {
    int   split = bf->size - bf->rd_posn,
          done  = clnt->done;
    char *bp    = (char *)dp;  
    int   sz    = pkt * cnt;

    if (done > 0) {
      sz -= done;
      bp += done;
    }

    int used = bf->wr_posn - bf->rd_posn;
    if (used < 0) used += bf->size;

    if (used >= sz) {
      ret += sz/pkt;
      if (split <= sz) {
	bcopy(&bf->buff[bf->rd_posn],bp,split);
	bf->rd_posn = 0;
	sz         -= split;
        if (sz) {
  	  bp       += split;
          bcopy(&bf->buff[0],bp,sz);
	}
      } else {
        bcopy(&bf->buff[bf->rd_posn],bp,sz);
      }
      bf->rd_posn += sz;

      busy         = 1;
      clnt->ready  = 0;

      if (!(clnt->sts & CSTS_KEEP)) clnt->detach();
      goto done;
    }

    int u0 = used;
    if (split <= used) {
      bcopy(&bf->buff[bf->rd_posn],bp,split);
      bf->rd_posn = 0;
      used       -= split;
      if (used) {
        bp       += split;
        bcopy(&bf->buff[0],bp,used);
      }
    } else {
      bcopy(&bf->buff[bf->rd_posn],bp,used);
    }
    bf->rd_posn += used;
    clnt->done  += u0;
    ret         += u0/pkt;
  }

 block:
  clnt->sts = CSTS(clnt->sts|CSTS_LOCK);
  if (!clnt->set_cllbk(this,wt,fn,cnt-ret)) {
    assert(rd_thrd != wr_thrd || !clnt->ready);
  }
  retry     = clnt->sts & CSTS_RETRY;
  clnt->sts = CSTS(clnt->sts & ~(CSTS_LOCK|CSTS_RETRY));
  if (retry) {
    if (!(clnt->sts & CSTS_KEEP)) clnt->detach();
    goto retry;
  }

 ret_block:
  ret = CHNS_BLOCK;

 done:
  if (ret >= 0 && !(clnt->sts & CSTS_KEEP)) {
    assert(!clnt->next);
  } 

  return (eChanSts)ret;
}

void void_pipe::check_clients(pipe_client *start,pipe_buff *pb,int cpu)
{
  pipe_client *scan = start;
  int          spin = 0;
  Kernel      *kp   = 0;

  do {
    pipe_client *cnxt = scan->next;
    int          clld = 0;
    while (pb->rd_posn != pb->wr_posn && scan->ready) {
      if (!kp) {
        if (cpu >= 0) {
          spin = 1;
          kp   = &Kern[cpu];
        } else {
          kp   = MyKern();
          cpu  = kp->CPU();
        }
      }
      Event  *evt  = scan->cllbk;
      cpu_id  ownr = evt->proc->CPU();
      if (ownr == cpu) {
	if (scan->sts & CSTS_LOCK) {
	  if (scan->ready) {
	    scan->sts = CSTS(scan->sts | CSTS_RETRY);
            return;
	  }
	} else {
	  scan->ready =  0;
	  clld        = -1;
	  (*scan->cllbk->fn)(scan->cllbk->proc);
	  if (cnxt == scan) {
	    cnxt  = clients_rd; 
	    clld  = 0;
	    start = 0;
	  }
	}
      } else {
        if (scan->notify < 0) {
          scan->notify = cpu;
          kp->write(ownr,(void *)KM_PP_CLNT,(void *)scan);
	}
        if (!spin) break;
      }
    }
    if ((scan = cnxt)) {
      if (clld) clients_rd = cnxt;
    } else {
      break;
    }
    if (scan == start) break;
    if (!start) start = scan;
  } while (pb->rd_posn != pb->wr_posn);
}

pipe_client * void_pipe::find_client(pipe_client *start,int sz)
{
  pipe_client *scan = start;

  do {
    if (scan->ready >= sz) {
      return scan;
    }
    scan = scan->next;
  } while (scan != start);

  return 0;
}

eChanSts void_pipe::writeV(const void *dp,int cnt,int pkt,Event *wt,ResumeFn fn,eChanWM wm)
{
  pipe_buff  *bf   = data.pc;
  const char *bp   = (const char *)dp;
  int         sz   = pkt * cnt,
              rp,
              used,
              avail;
  thread_id   thrd = thread_self();   

  while (wr_thrd != thrd) {
    int ownr = Kern->GetCPU(wr_thrd);
    int self = Kern->GetCPU(thrd);
    Kern[self].write(ownr,(void *)KM_PP_OWN_W,(void *)this,(void *)wt);
    if (wt) goto failed;
  }

  do {
    rp   = bf->rd_posn;
    used = bf->wr_posn - rp;

    if (used < 0) used += bf->size;

    avail = (bf->size - used) - pkt;

    if (sz > avail && used && clients_rd) {
      check_clients(clients_rd,bf,wt ? -1
		                     : Kern->GetCPU(thrd));
    }

    if (CWM_SYNC == wm) {
      pipe_client *clnt = 0;
      if (!clients_rd || bf->wr_posn != bf->rd_posn
                      || !(clnt = find_client(clients_rd,cnt))) {    
        goto failed;
      }
      if (clnt) {
        clients_rd = clnt;
      }
    }

  } while (rp != bf->rd_posn);

  if (sz <= avail) {

    int wp  = bf->wr_posn,
        wp2 = wp + sz;

    if (wp2 >= bf->size) {
       wp2      -= bf->size;
       int split = bf->size - wp;
       bcopy(bp,&bf->buff[wp],split);
       sz -= split;
       if (sz > 0) {
         bp += split;
         bcopy(bp,&bf->buff[0],sz);
       }
    } else {
       bcopy(bp,&bf->buff[wp],sz);
    }
    bf->wr_posn = wp2;

    if (clients_rd) {
      check_clients(clients_rd,bf);
    }

    busy = 1;

    if (wt && wt->next) {
      wt->next->prev = wt->prev;
      wt->prev->next = wt->next;
      if (wt == clients_wr) {
        clients_wr = (wt->next == wt) ? 0
	                              : wt->next;
      }
      wt->next = wt->prev = 0;
    }

    return (eChanSts)cnt;
    
  } else if (CWM_QUEUE == wm) {
    data.pc = (pipe_buff *)realloc(data.pc,sizeof(pipe_buff)
                                                    + (data.pc->size += sz));
    assert(data.pc);
    return writeV(dp,cnt,pkt,wt,fn,wm);
  }

 failed:
  if (!wt) throw CHNS_BLOCK;
  if ((wt->next = clients_wr)) {
    wt->prev         = clients_wr->prev;
    wt->prev->next   = wt;
    clients_wr->prev = wt;
  } else {
    clients_wr = wt->next = wt->prev = wt;
  }
  Kern->Active(this);
  wt->fn = fn;
 block:
  return CHNS_BLOCK;
}

int void_pipe::pktSize()
{
  assert(this == &NullDev);

  return 0;
}

void_pipe void_pipe::NullDev;

void void_pipe::add_writer(Event *add)
{
  thread_id self = thread_self();

  assert(self == wr_thrd);

  Event **scan = &clients_wr,
         *more;
 
  while ((more = *scan)) scan = &(more->next);

  (*scan = add)->next  = 0;
}

const Event NullEvent;

pipe_client::pipe_client()
{
  sns_typ  = SNS_CHAN;
  chan     = &void_pipe::NullDev;
  next     = 0;
  sts      = CSTS_NONE;
  cllbk    = (Event *)&NullEvent;
  notify   = -1;
  ready    = 0;
  done     = -1;
}

void pipe_client::init(void_pipe *chn,Event *evt)
{
  chan  = chn;
  cllbk = evt;

  if (chn && evt) {
    if (chn) {
      if (chn->clients_rd) {
	next                   = chn->clients_rd->next;
	chn->clients_rd->next  = this;
      } else {
	next = this;
      }
      chn->clients_rd = this;
    }
  }
}

pipe_client::~pipe_client()
{
}

void pipe_client::detach()
{
  if (next) {
    pipe_client **scan0 = &next,
                **scan  = scan0;

    while (*scan != this) {
      scan = &(*scan)->next;
    }

    if (scan == scan0) {
      chan->clients_rd = 0;
    } else {
      if (chan->clients_rd == this) {
	chan->clients_rd = *scan = next;
      } else {
	*scan = next;
      }
    }

    next = 0;
  }
}

int pipe_client::callback()
{
  return (*cllbk->fn)(cllbk->proc);
}

void sliceProp(void_signal *s1,void_signal *s2,int edge)
{
  s2->propagate(0,0,edge);
}

void bridge::element::propagate(int edge)
{
  bridge::element *e_o = &this[other];
  if (!e_o->active) {
    active = 1;
    (*propFn)(sig,e_o->sig,edge);
    active = 0;
  }
}

void void_signal::propagate(size_t sz,const void *src,int edge)
{
  rx_base     *rx,
              *rx_next;
  Event       *evt;
  void_signal *si   = this;
  Kernel      *kp   = MyKern();
  cpu_id       self = kp->CPU(),
               ownr = si->CPU();

  assert(self == ownr);

  if (implicit && (si = implicit[0])->before) {
    si->active = 1;
    if ((rx = si->receivers)) {
      do {
	evt  = rx->cllbk;
        rx   = rx->next;
        ownr = evt->proc->CPU();
        if (ownr == self) {
  	  (*evt->fn)(evt->proc);
	} else {
          kp->outStanding();
          kp->write(ownr,(void *)KM_RCVR,(void *)evt);
          assert(0);
	}
      } while (rx && rx != si->receivers);
    }
    si->active = 0;
  }

  rx = (*UpdtFn())(this,src,sz);

  mark++;
  // fprintf(stderr,">0x%016llx %s %d %d\n",this,sigType(this),active,mark);
  active = 1;
  if (rx) {
    do {
      char diff = mark - rx->mark;
      // fprintf(stderr," 0x%016llx 0x%016llx\n",this,rx);
      assert(1 == diff);
      rx->mark = mark;
      rx_next  = rx->next;
      switch (rx->sns_typ) {
      case SNS_RCVR: {
	receiver *rcv = (receiver *)rx;
	if ((!rcv->edge) || rcv->edge == edge) {  
	  if ((evt = rcv->cllbk)) {
	    ownr = evt->proc->CPU();
	    if (ownr == self) {
	      (*evt->fn)(evt->proc);
	    } else {
	      kp->outStanding();
	      kp->write(ownr,(void *)KM_RCVR,(void *)evt);
	    }
	  }
	}
      } break;
      case SNS_BRIDGE: {
	bridge::element *be = (bridge::element *)rx;
        if (be->active) {
          // fprintf(stderr," 0x%016llx 0x%016llx [skipped]\n",this,rx);
	} else {
          active = 3;
          be->propagate(edge);
          active = 1;
	}
      } break;
      default: assert(0);  
      }
    } while ((rx = rx_next) && rx->mark != mark);
  }

 done:
  // fprintf(stderr,"<0x%016llx %s %d %d\n",this,sigType(this),active,mark);
  active = 0;
}

static wave_element *WEfreeList[32];

static wave_element *newWave(int sz)
{
  wave_element *we;
  int           bckt = 0;

  if (sz < 4) sz = 4;

  while (4 << bckt < sz) bckt++;
  
  sz = 4 << bckt;

  if ((we = WEfreeList[bckt])) {
    WEfreeList[bckt] = we->next_we;
  } else {
    we = (wave_element *)malloc(sizeof(wave_element) + sz);
    assert(we);
  }

  return we;
}

static void recycleWave(wave_element *we,int sz)
{
  int  bckt = 0;

  if (sz < 4) sz = 4;

  while (4 << bckt < sz) bckt++;

  we->next_we      = WEfreeList[bckt];
  WEfreeList[bckt] = we;
}

void any_driver::UpdateDriver(any_driver *drv)
{
  Kernel    *kp    = MyKern();
  thread_id  d_own = drv->Thrd();
 
  if (d_own == thread_self()) {
    any_signal *sig   = drv->sig;
    cpu_id      s_own = sig->cpu,
                d_cpu = Kern->GetCPU(d_own);

    if (d_cpu == s_own) {
      wave_element *we = drv->Waveform();
      if (drv->SetWaveform(we->next_we)) {
        assert(kp->ThreadID() == drv->Thrd());
        kp->Schedule(drv->Waveform());
      }
      int  sz   = drv->Size(),
           diff;
      char buff[sz];

      drv->copy(we->value,drv->pValue(),sz);

      recycleWave(we,sz);  

      if (sig->resolve(buff) >= 0) {
        if ((diff = (*sig->CompFn())(buff,sig->pValue(),sz))) {
          sig->propagate(sz,buff,sig->Edge(drv->pValue()));
        }
      } else {
        if ((diff = (*sig->CompFn())(drv->pValue(),sig->pValue(),sz))) {
          sig->propagate(sz,drv->pValue(),sig->Edge(drv->pValue()));
        }
      }
    } else {
      drv->setThrd(Kern[s_own].ThreadID());
      kp->outStanding();
      kp->write(s_own,(void *)KM_DRV_UPD,(void *)drv);
    }
  } else {
    kp->outStanding();
    kp->write(Kern->GetCPU(d_own),(void *)KM_DRV_O_U,(void *)drv);
  }
} 

int void_driver::_ba(void *pval,int sz,CopyFn cpy_fn)
{
  any_driver  *drv = (any_driver *)this;
  any_signal  *sig = drv->Sig();
  int          diff;
  char         buff[sz];
  thread_id    self = thread_self();

  if (thrd != self) {
    Kernel *kp = GetKern(self);
    kp->write(Kern->GetCPU(thrd),(void *)KM_DRV_OWN,(void *)this);
    do {
      kp->CheckKP();
    } while (thrd != self);
  }

  int sts = (*cpy_fn)(pval,drv->pValue(),sz);

  if (0 == sts) {
    CompareFn cmp = sig->CompFn();
    if (sig->resolve(buff) >= 0) {
      if ((diff = (*cmp)(buff,sig->pValue(),sz))) {
        sig->propagate(sz,buff,sig->Edge(drv->pValue()));
      }
    } else {
      if ((diff = (*cmp)(drv->pValue(),sig->pValue(),sz))) {
        sig->propagate(sz,drv->pValue(),sig->Edge(drv->pValue()));
      }
    }
  } else switch (sts) {
    default: assert(0);
             break;
  }

  return sts;
}

wave_element *void_driver::_nba(int sz,RelTime t)
{
  any_driver    *any  = (any_driver *)this;
  void_signal   *sig  = any->Sig();
  wave_element **pwe0 = &waveform,
               **pwe  = pwe0,
                *wes;
  Kernel        *kp   = MyKern();
  Time           t2,
                 ta   = kp->Now() + t;
  thread_id      self = thread_self();

  if (thrd != self) {
    Kernel *kp = GetKern(self);
    kp->write(Kern->GetCPU(thrd),(void *)KM_DRV_OWN,(void *)this);
    do {
      kp->CheckKP();
    } while (thrd != self);
  }

  if (0 == t) {
    ta.incDelta();
  }

  while ((wes = *pwe)) {
    t2 = wes->When();
    if (t2 > t) break;
      pwe = &(wes->next_we);
  }

  if (wes) {
    wave_element *we1,
                 *we2 = wes->next_we;
    while ((we1 = we2)) {
      we2 = we1->next_we;
      recycleWave(we1,sz);
    }
    if (ta != t2) {
      wes->SetTime(ta);
      kp->Schedule(wes,ta);
      if (pwe == pwe0) {
        kp->Desched(wes);
        kp->Schedule(wes);
      }
    }
  } else {
    *pwe = wes = newWave(sz);
    wes->proc  = (Proc *)this;
    wes->fn    = (ResumeFn)any_driver::UpdateDriver;
    wes->SetTime(ta);
    if (pwe == pwe0) kp->Schedule(wes,ta);
  }

  wes->next_we = 0;

  return wes;
}

void receiver::index(int top,int bottom)
{
  receiver    *root = &this[-top];
  void_signal *rsg  = root->sig;
  int          max  = root->max_idx;

  if (top > max) max = root->max_idx = top;

  while (++bottom <= top) {
    void_signal *sgp = rsg->sigIndex(bottom);
    root[bottom].sig = sgp;
  }
}

void void_signal::enableBridge()
{
}

void void_signal::disableBridge()
{
}

receiver *receiver::prc_init(void_signal &s,int sig_sz,int mb)
{
  receiver *rcv = new receiver [mb],
           *rp  = rcv;
  while (mb-- > 0) rp++->init(s,sig_sz);
  return rcv;
}

receiver *receiver::prc_init(void_signal *s,int sig_sz,int mb)
{
  receiver *rcv = new receiver [mb],
           *rp  = rcv;
  while (mb-- > 0) rp++->init(s,sig_sz);
  return rcv;
}

pipe_client *pipe_client::prc_init(void_pipe *p0,int p_sz,int indrct,Event *_wait,eCUSG rw,int mb)
{
  pipe_client *pc = new pipe_client [mb],
              *pp = pc;
  void_pipe   *p  = p0;
  int          c  = 0;

  for (int i = indrct; i--;) p = *(void_pipe **)p;

  while (mb-- > 0) pp++->init(p,_wait);

  do {
    if (p) {
      c++;
      if (logUsage) {
	p->log(CurrMod,CurrProc,rw);
      }
    }
    if (indrct) {
      void_pipe **pp = (void_pipe **)p0;
      pp++ ; p_sz -= (sizeof(pp));
      p = p0 = (void_pipe *)pp;
      for (int i = indrct; i--;) p = *(void_pipe **)p;
    } else {
      p++  ; p_sz -= (sizeof(*p));
    }
  } while (p_sz > 0); 

  return pc;
}

void Proc::initWait(Event *evt,int n,Module *m)
{
  assert(CurrMod == m);

  CurrProc = this;

  while (n-- > 0) evt++->proc = this;
}

void receiver::enable(Kernel *kp,eImplicit i)
{
  assert(!next);

  void_signal *si   = sig;
  cpu_id       ownr = si->CPU();

  assert(kp->CPU() == ownr);

  if ((imp = i)) si = sig->Implicit(imp);

  mark = si->Mark();  

  rx_base *sr0 = si->receivers;
  if (sr0) {
    next          = sr0->next;
    prev          = sr0->prev;
    prev->next    = this;
    next->prev    = this;
  } else {
    si->receivers = this;
    next = prev   = this;
  }

  if (si->Bridged()) {
    si->enableBridge();
  }
}

void receiver::enable(Event *evt,int for_edg,eImplicit i)
{
  assert(!(next || prev || cllbk));

  void_signal *si   = sig;
  Kernel      *kp   = MyKern();
  cpu_id       ownr = si->CPU();

  cllbk = evt;
  edge  = for_edg;

  if (kp->CPU() == ownr) {

    if ((imp = i)) si = sig->Implicit(imp);

    mark = si->Mark();

    rx_base *sr0 = si->receivers;
    if ((next = sr0)) {
      prev          = sr0->prev;
      sr0->prev     = this;
      prev->next    = this;
    } else {
      si->receivers = this;
      next = prev   = this;
    }

    if (si->Bridged()) {
      si->enableBridge();
    }

  } else { 
    assert(!(next && prev));

    kp->write(ownr,(void *)KM_ENABLE,(void *)this,(void *)i);

    do {
      kp->CheckKP();
    } while(!(next && prev));
  }
}

void receiver::disable(Event *evt)
{
  if (evt) evt->fn = nop;

  void_signal *si   = sig;
  Kernel      *kp   = MyKern();
  cpu_id       ownr = si->CPU();

  cllbk = 0;

  if (kp->CPU() == ownr) {

    if (imp) si = sig->Implicit(imp);

    rx_base *sr0 = si->receivers;

    if (sr0 == this) {
      if ((si->receivers = sr0->next) == this) {
        si->receivers = 0;
        goto done;
      }
    }

    next->prev = prev;
    prev->next = next;

   done:
    if (si->Bridged()) {
      si->disableBridge();
    }

    next = prev = 0;

  } else {
    assert(next && prev);

    kp->write(ownr,(void *)KM_DISABLE,(void *)this);

    do {
      kp->CheckKP();
    } while(next && prev);
  }
}

int void_pipe::Ready()
{
  pipe_buff *bf = data.pc;

  int used = bf->wr_posn - bf->rd_posn;

  if (used) {
     return used > 0 ? used
                     : used + bf->size;
  }

  if (clients_wr) return -1;

  return 0;
}

bool pipe_client::Sense()
{
  if (chan->Ready()) return false;

  return true;
}

static void reEnable(int idx,receiver *old_r,receiver *new_r)
{
  receiver *old    = &old_r[idx];
  rx_base  *in_use = old->next;

  if (in_use) old->disable();

  if (idx > 0) reEnable(idx-1,old_r,new_r);

  receiver *np = &new_r[idx];
  np->sig = old->sig;

  if (in_use) np->enable(old->cllbk);
}

void Kernel::write(int to_cpu,const void *op)
{
  int offset = to_cpu - cpu;

  assert(offset);

  kern_pipe &kp = this[offset].xcmd[cpu];

  int avail,
      used,
      wp = kp.wr_posn;

  do {
    used = wp - kp.rd_posn;
    if (used < 0) used += K_PIPE_SIZE;
    avail = K_PIPE_SIZE - used;
  } while (avail < 4);
  
  kp.data[wp++] = op; wp &= (K_PIPE_SIZE-1);

  kp.wr_posn = wp;

  this[offset].flgi[cpu] = 1;
  flgo[to_cpu]           = 1;
}

void Kernel::write(int to_cpu,const void *op,const void *d1)
{
  int offset = to_cpu - cpu;

  assert(offset);

  kern_pipe &kp = this[offset].xcmd[cpu];

  int avail,
      used,
      wp = kp.wr_posn;

  do {
    used = wp - kp.rd_posn;
    if (used < 0) used += K_PIPE_SIZE;
    avail = K_PIPE_SIZE - used;
  } while (avail < 3);
  
  kp.data[wp++] = op; wp &= (K_PIPE_SIZE-1);
  kp.data[wp++] = d1; wp &= (K_PIPE_SIZE-1);

  kp.wr_posn = wp;

  this[offset].flgi[cpu] = 1;
  flgo[to_cpu]           = 1;
}

void Kernel::write(int to_cpu,const void *op,const void *d1,const void *d2)
{
  int offset = to_cpu - cpu;

  assert(offset);

  kern_pipe &kp = this[offset].xcmd[cpu];

  int avail,
      used,
      wp = kp.wr_posn;

  do {
    used = wp - kp.rd_posn;
    if (used < 0) used += K_PIPE_SIZE;
    avail = K_PIPE_SIZE - used;
  } while (avail < 4);
  
  kp.data[wp++] = op; wp &= (K_PIPE_SIZE-1);
  kp.data[wp++] = d1; wp &= (K_PIPE_SIZE-1);
  kp.data[wp++] = d2; wp &= (K_PIPE_SIZE-1);

  kp.wr_posn = wp;

  this[offset].flgi[cpu] = 1;
  flgo[to_cpu]           = 1;
}

int Proc::migrate(ResumeFn fn,Event *evt,int trgt_cpu)
{
  Kernel *kp = MyKern();

  assert(cpu == kp->CPU());

  if (trgt_cpu < 0) {
    trgt_cpu = kp->CPU() + 1;
    if (trgt_cpu >= kp->CPUs()) {
      trgt_cpu = 0;
    }
  }

  if (trgt_cpu != cpu) {    
    cpu     = -1;
    evt->fn = fn;
    kp->outStanding();
    kp->write(trgt_cpu,(void *)KM_MIGRATE,(void *)evt);
    return 1;
  }

  return 0;
}

eSigSts Proc::waitSig(Event *evt,ResumeFn fn,sense *sns,...)
{
  va_list pvar;

  va_start(pvar, sns);

  evt->fn = fn;

  int     edge = 0;
  eSigSts sts  = SIG_BLCK;

  while (sns) {
    receiver    *rcv;
    pipe_client *clnt;
    int          sz = -1;

    if (sns == RCVR_ARRAY) {
     arr:
      receiver **pr = 0;
      pr            = va_arg(pvar,receiver **);
      receiver *r0  = *pr;
      int       _W  = va_arg(pvar,int); assert(!_W);
      rcv           = va_arg(pvar,receiver *);
      int index = rcv - r0,
          i0    = r0->max_idx;
      if (index > r0->max_idx) {
        receiver *nr = new receiver[1+(r0->max_idx = index)];
        rcv = &(*pr = nr)[index];
        reEnable(i0,r0,nr);
        rcv->index(index,i0);
        delete[] r0;
      }
    } else if (sns == RCVR_NEGEDGE) {
      edge = -1; goto nxt;
    } else if (sns == RCVR_POSEDGE) {
      edge =  1; goto nxt;
    } else switch(sns->sns_typ){
    case SNS_CHAN: 
      clnt = (pipe_client *)sns;
      if (!clnt->Sense()) {
        sts = SIG_READY;
      }
      goto nxt;
    default:
      rcv  = (receiver *)sns;
    }

    rcv->enable(evt,edge);
    edge = 0;
  nxt:
    sns = va_arg(pvar,sense *);
  }

  va_end(pvar);

  return sts;
}

eSigSts Proc::waitSigPre(Event *evt,ResumeFn fn,sense *sns,...)
{
  va_list pvar;

  va_start(pvar, sns);

  evt->fn = fn;

  int     edge = 0;
  eSigSts sts  = SIG_BLCK;

  while (sns) {
    receiver    *rcv;
    pipe_client *clnt;
    int          sz = -1;

    if (sns == RCVR_ARRAY) {
      receiver **pr = 0;
      pr  = va_arg(pvar,receiver **);
      receiver	*r0 = *pr;
      rcv = va_arg(pvar,receiver *);
      int index = rcv - r0,
          i0    = r0->max_idx;
      if (index > r0->max_idx) {
        receiver *nr = new receiver[1+(r0->max_idx = index)];
        rcv = &(*pr = nr)[index];        
        reEnable(i0,r0,nr);
        rcv->index(index,i0);
        delete[] r0;
      }
    } else if (sns == RCVR_ARRAY_SZ) {
      assert(0);
    } else if (sns == RCVR_NEGEDGE) {
      edge = -1; goto nxt;
    } else if (sns == RCVR_POSEDGE) {
      edge =  1; goto nxt;
    } else switch(sns->sns_typ){
    case SNS_CHAN: 
      clnt = (pipe_client *)sns;
      if (!clnt->Sense()) {
        sts = SIG_READY;
      }
      goto nxt;
    default:
      rcv  = (receiver *)sns;
    }

    rcv->enable(evt,edge,IMP_BEFORE);
    edge = 0;
  nxt:
    sns = va_arg(pvar,sense *);
  }

  va_end(pvar);

  return sts;
}

void Proc::unWaitSig(Event *evt,sense *sns,...)
{
  va_list pvar;

  va_start(pvar, sns);

  int edge = 0;

  while (sns) {
    receiver    *rcv;
    pipe_client *clnt;
    int          sz = -1;

    if (sns == RCVR_ARRAY) {
      receiver **pr = 0;
      pr            = va_arg(pvar,receiver **);
      int        _W = va_arg(pvar,int); assert(!_W);
      receiver	*r0 = *pr;
      rcv = va_arg(pvar,receiver *);
    } else if (sns == RCVR_ARRAY_SZ) {
      assert(0);
    } else if (sns == RCVR_NEGEDGE) {
      edge = -1; goto nxt;
    } else if (sns == RCVR_POSEDGE) {
      edge =  1; goto nxt;
    } else switch(sns->sns_typ){
    case SNS_CHAN: 
      clnt = (pipe_client *)sns;
      clnt->disable();
      goto nxt;
    default:
      rcv  = (receiver *)sns;
    }

    rcv->disable();
    edge = 0;
  nxt:
    sns = va_arg(pvar,sense *);
  }

  va_end(pvar);

  evt->fn = Assert;
}

//void Proc::unWaitSigPre(Event *evt,sense *r0,...)
//{
//  evt->fn = nop;
//}

receiver::~receiver()
{
  if (next) disable();
}

void_driver::~void_driver()
{
  void_driver **pdrv = &next,
               *drv;
  any_driver   *self = Any();
  void_signal  *sig  = self->Sig();

  if (sig) {
    self->clrSig();

    while ((drv = *pdrv) != this && drv) {
      pdrv = &drv->next;
    }

    if (drv && this == (sig->drivers = *pdrv = drv->next)) sig->drivers = 0;

    next = 0;
  } else {
    assert(0 == next);
  }
}

void bind(Module *mod,const type_info &r,void_signal **ref,
                      const type_info &s,void_signal *sig) {
  if (s != r) {
    const char *ss = s.name(),
               *rs = r.name();

    int r_p = 0;
    while ('P' == *rs) {rs++; r_p++;}
    int s_a = 0,
        dim[8];
    while ('A' == *ss) {
      ss++;
      dim[s_a] = 0;
      while (*ss >= '0' && *ss <= '9') { dim[s_a] *= 10;
	                                 dim[s_a] += *ss++ - '0';}
      s_a++;
      ss++;
    }
 
    if (r_p == s_a && 0 == strcmp(ss,rs)) {
      goto match;
    }

    Converter *cnv_rs = Converter::find(r,s),
              *cnv_sr = Converter::find(s,r);

    if (!(cnv_rs && cnv_sr)) {
      throw ERR_BIND_MISMATCH;
    }

    int bf = -1;
    (*cnv_sr->from.sizes)(sig,&bf);
    int bt = bf;
    (*cnv_sr->to.sizes)  (  0,&bt);

    assert(bf == bt);

    void_signal *s2 = (*cnv_sr->to.crt_sig)(0,0,bt,0,0,0);

    new bridge(sig,cnv_sr->bridge_fn,
                s2,cnv_rs->bridge_fn,bt,cnv_sr->fMaster());

    sig = s2;
  }

 match:
  *ref = sig;
}

void Module::setIdx(int dimn, ...)
{
  va_list pvar;

  va_start(pvar, dimn);

  assert(!index && dimn > 0);

  index       = new int [dimn+1];
  index[dimn] = -1;

  while(dimn-- > 0) {
    index[dimn] = va_arg(pvar, int);
  }

  va_end(pvar);
}

void_driver *void_signal::attach(void_driver *drv,const type_info &t,void_signal **p_sig)
{
  *p_sig = this;

  void_driver *drv0 = drivers;

  if (drv0) {
    drv->next  = drv0->next;
    drv0->next = drv;

    char temp[dataSize()];

    if (resolve(temp) < 0) {
      assert(!"multiple drivers for non-resolved type");
    }

  } else {
    drv->next = drv;
    drivers   = drv;
  }

  return drv;
}

void_signal *void_signal::Implicit(eImplicit i)
{
  assert(i);

  imp_signal *si = 0;

  int c = i_count;

  switch (i) {
  case IMP_BEFORE: if (c) {
                     if ((si = implicit[0])->before) goto done;
                     imp_signal **ni      = new imp_signal*[++i_count];
                     for (;c;c--) ni[c+1] = implicit[c];
                     implicit[0]     = si = new imp_signal;
                   } else {
                     (implicit = new imp_signal*[1])[i_count++] 
                                     = si = new imp_signal;
                   }
                   si->before = 1;
                   break;
  }

 done:
  return si;
}

template<>
int l_signal<bool>::Edge(const void *new_val) {
  bool b = *(const bool *)new_val;

  return (b - Value());
}

template<>
int c_signal<bool>::Edge(const void *new_val) {
  bool b = *(const bool *)new_val;

  return (b - Value());
}

template<>
int l_signal<logic>::Edge(const void *new_val) {
  return logic_edge(*(logic *)new_val,Value());
}

template<>
int c_signal<logic>::Edge(const void *new_val) {
  const logic *np = (logic *)new_val;

  int edge = (np->value - Value().value);

  return edge;
}

template<>
int r_signal<logic>::resolve(void *ret)
{
  logic         *pl           = (logic *)ret;
  driver<logic> *drv0         = Drivers(),
                *drv          = drv0;
  int            max_strength = -1,
                 res_value    = -1,
                 res_known    =  0;

  do {
    logic dv = drv->Value();
    if ((int)dv.strength > max_strength) {
      max_strength = dv.strength;
      res_value    = dv.value;
      res_known    = dv.known;
    } else if (dv.strength == max_strength) {
      if (!dv.known || dv.value != res_value) {
	res_known  = 0;
      }
    }   
    drv = drv->Next();
  } while (drv != drv0);  

  pl->strength = max_strength;
  pl->value    = res_value;
  pl->known    = res_known;

  return 1;
}

template<>
r_signal<logic> *r_signal<logic>::sttcSlice(Slice **p_slice,int l,int r)
{
  Slice *nw_slc = new Slice(this,this->any()->pValue(),l,r);
  nw_slc->next  = *p_slice;
  *p_slice      = nw_slc;
  return &this[l];
}

template<>
int rl_signal<logic>::resolve(void *ret)
{
  logic         *pl           = (logic *)ret;
  driver<logic> *drv0         = Drivers(),
                *drv          = drv0;
  int            max_strength = -1,
                 res_value    = -1,
                 res_known    =  0;

  do {
    logic dv = drv->Value();
    if ((int)dv.strength > max_strength) {
      max_strength = dv.strength;
      res_value    = dv.value;
      res_known    = dv.known;
    } else if (dv.strength == max_strength) {
      if (!dv.known || dv.value != res_value) {
	res_known  = 0;
      }
    }   
    drv = drv->Next();
  } while (drv != drv0);  

  pl->strength = max_strength;
  pl->value    = res_value;
  pl->known    = res_known;

  return 1;
}

int mt_printf(const char *format,...)
{
  static pthread_mutex_t lock;
  
  pthread_mutex_lock(&lock);

  va_list ap;
  va_start(ap, format);
  int r = vprintf(format, ap);
  va_end(ap);

  fflush(stdout);

  pthread_mutex_unlock(&lock);

  return r;
}

int logic::sizes(void_signal *sig,int *bits)
{
  if (bits) {
    int b = *bits;
    if (b >= 0) {
      return b;
    }
  }

  return 1;
}

void_signal *logic::crt_sig(void_signal *sig,Slice **p_slc,int bits,int sgn,int l, int r)
{
  assert(0 == p_slc);
  return new signal<logic>[bits];
}

int logic_vec_slice::sizes(void_signal *sig,int *bits)
{
  signal<logic_vec_slice> *lvs = (signal<logic_vec_slice> *)sig;

  if (lvs) {
    const Slice &slc(*lvs->pValue());
    int b = 1 + (slc.r - slc.l);
    if (bits) *bits = b;
    return sizeof(logic_word) * (1 + b/(8 * sizeof(long)));
  }
  return 1;
}

void_signal *logic_vec_slice::crt_sig(void_signal *sig,Slice **p_slc,int w,int s,int l,int r)
{
  if ((r-l)+1 > w) {
    throw ERR_BAD_SLICE;
  }

  logic_vec_slice slc(sig,sig->any()->pValue(),l,r);

  signal<logic_vec_slice,logic_vec_slice,
         logic_vec_fn_slice,logic_vec_fn_slc2sig> *ss
                    = new signal<logic_vec_slice,logic_vec_slice,
                                 logic_vec_fn_slice,logic_vec_fn_slc2sig>(slc);

   new bridge(sig,sliceProp,ss,sliceProp,w,0,1);

   return ss;
}

void_driver *logic_vec_slice::crt_drv(void_driver *drv,void_signal **sig,int w,int s,int l,int r)
{
  driver<logic_vec_slice> *lvs_drv = static_cast<driver<logic_vec_slice> *>(drv);

  logic_vec_slice *slc = lvs_drv->pValue();

  slc->set_lr(l,r);

  assert(0);
}

void init_drv(driver<LOGIC_VEC_SLICE> &drv,signal<LOGIC_VEC_SLICE_SIG> *sig)
{
  const logic_vec_slice *ss = &sig->Value();
  logic_vec_slice       *ds =  drv.pValue();

  drv.set_sig(sig);

  ds->mstr_sig = ss->mstr_sig;
  ds->l        = ss->l;
  ds->r        = ss->r;

  ds->data = new logic_word[logic_word::words_for(1+(ss->r-ss->l))];
}

TypeDataSlc *TypeDataSlc::list;

void TypeDataSlc::reg()
{
  next = list;
  list = this;
}

TypeDataSlc TypeDataSlc::KnownSlc[] = {
  {0,"parc::logic_vec<%d, %d>",logic_vec_slice::crt_sig,logic_vec_slice::crt_drv},
  {0,"parc::logic_vec_slice",  logic_vec_slice::crt_sig,logic_vec_slice::crt_drv}
};

TypeDataSlcArray TypeDataSlcArray::KnownSlcCount;

TypeDataSlcArray::TypeDataSlcArray()
{
  int n = sizeof(TypeDataSlc::KnownSlc)/sizeof(TypeDataSlc);
  count = 0;
  while (count < n) TypeDataSlc::KnownSlc[count++].reg();
}

void_signal *void_signal::sttcSlice(Slice **p_slc,int l,int r,
                                    const type_info &t,const type_info &s)
{
  int         sts;
  const char  *nm0  = t.name(),
              *nm1  = abi::__cxa_demangle(t.name(), 0, 0, &sts);
  TypeDataSlc *scan = TypeDataSlc::list;
  void_signal *sig  = 0;
  for (; scan ; scan = scan->next) {
    int w,
        s = 0;
    if (sscanf(nm1,scan->name,&w,&s)) {
      assert(0 == sig);
      sig = (*scan->crt_sig)(this,p_slc,w,s,l,r);      
    }
  }

  return sig;
}

void logic_vec_slice::attach(void_driver *drv,const type_info &t,
                             void_signal **p_sig)
{
  int         sts;
  const char  *nm0  = t.name(),
              *nm1  = abi::__cxa_demangle(t.name(), 0, 0, &sts);
  TypeDataSlc *scan = TypeDataSlc::list;
  int          mtch = 0;
  for (; scan ; scan = scan->next) {
    int w = -1,
        s =  0;
    if (0 == strcmp(nm1,scan->name) ||
             sscanf(nm1,scan->name,&w,&s)) {
      assert(0 == mtch); mtch++;
      (*scan->crt_drv)(drv,p_sig,w,s,l,r);  
    }
  }

  assert(mtch);
}

enum eSSop {
  SSO_DIFF,
  SSO_COPY
};

static inline int  lvs_op(const logic_vec_slice *src,
		                logic_vec_slice *dst, size_t sz,eSSop op = SSO_DIFF)
{
  int sl = src->l,
      sr = src->r,
      dl = dst->l,
      dr = dst->r,
      ls = (sr - sl) +1,
      ld = (dr - dl) +1;

  logic_word *sd = (logic_word *)src->data,
             *dd = (logic_word *)dst->data;

  assert(sd != dd);

  if (sl == dl) {
    assert(sr >= dr);
    while (sl >= logic_word::BITS_PW) {
      sd++; dd++; sl -= logic_word::BITS_PW;
    }
    for (;;) {
      unsigned long *sv   = &sd->value,
 	            *dv   = &dd->value,
	             diff,
 	             mask,
                     val;
      for (int vkd = 0; vkd < 3 ; vkd++,sv++,dv++) {
	diff = *sv ^ (val = *dv);
	if (diff) {
          if (logic_word::BITS_PW - sl >= ls) {
            mask = 1; mask <<= ls; mask--;
	    mask <<= sl;
            if (diff & mask) {
              switch (op) { case 0: return 1;
	                    case 1: *dv = val ^ (diff & mask);
                                    break;}
            }
          } else {
            assert(0);
          }
        }
      }
      if ((ls -= logic_word::BITS_PW) <= 0) break;
        sd++; dd++;
    } 
  } else {
    while (sl >= logic_word::BITS_PW) {
      sd++; sl -= logic_word::BITS_PW;
    }
    while (dl >= logic_word::BITS_PW) {
      dd++; dl -= logic_word::BITS_PW;
    }
    for (;;) {
      unsigned long *sv   = &sd->value,
 	            *dv   = &dd->value,
	             diff,
 	             mask,
	             val,
	             sw,
                     dw;
      for (int vkd = 0; vkd < 3 ; vkd++,sv++,dv++) {
        if (logic_word::BITS_PW - sl >= ld) {
          sw = *sv; sw >>= sl;
          dw = *dv; dw >>= dl;
          mask = 1; mask <<= ld; mask--;
          if (ls < ld) {
	    assert(0); // need sign
	  }
          diff = (val = dw) ^ sw;
          if (diff & mask) {
            switch (op) { case 0: return 1;
	                  case 1: *dv = (val ^ (diff & mask)) << dl;
                                  break; }
          }	  
	} else {
	  assert(0);
        }
      }
      if ((ld -= logic_word::BITS_PW) <= 0) break;
        sd++; dd++;
    }     
  }

  return 0;
}

int logic_vec_slice::compare(const logic_vec_slice *src,
                                   logic_vec_slice *dst, size_t sz)
{
  return lvs_op(src,dst,sz,SSO_DIFF);  
}

int logic_vec_slice::copy(const logic_vec_slice *src,
                                logic_vec_slice *dst, size_t sz)
{
  return lvs_op(src,dst,sz,SSO_COPY);
}

rx_base *logic_vec_slice::update(void_signal *sig,logic_vec_slice *src, size_t sz)
{
  if (0 == src) {
    return sig->receivers;
  }

  signal<logic_vec_slice_1> *ss = (signal<logic_vec_slice_1> *)sig;
  any_signal                *sm = ss->pValue()->mstr_sig->any();

  CompareFn cmp = ss->SlcCompFn();
  rx_base  *rx  = 0;
  if ((*cmp)(src,sm->pValue(),sz)) {
    UpdateFn  updt = ss->SlcUpdtFn();
    (*updt)(sm,src,sz);
    rx = sig->receivers;
  }

  return rx;
}

static inline int slc2sig_op(const logic_vec_slice *src,
			           logic_vec_1w    *dst, size_t sz,
                                       eSSop op = SSO_DIFF, int l = -1)
{
  logic_vec_slice ds(0,dst,src->l,src->r,src->mstr_sig->Sgn());

  return lvs_op(src,&ds,sz,op);
}

int logic_vec_slice::comp2sig(const logic_vec_slice *src,
                                    logic_vec_1w    *dst, size_t sz)
{
  return slc2sig_op(src,dst,sz,SSO_DIFF);
}

rx_base *logic_vec_slice::copy2sig(void_signal *sig,
                                   const logic_vec_slice *src, size_t sz)
{
  // sigType(sig)

  signal<logic_vec_1w> *s1 = (signal<logic_vec_1w> *)sig;

  logic_vec_1w *dst = s1->pValMod();

  slc2sig_op(src,dst,sz,SSO_COPY,sig->Bits());

  return s1->receivers;
}

template<>
void nba<logic_vec_slice>(driver<LOGIC_VEC_SLICE> *drv,logic_vec_slice *data,RelTime t) 
{
  int bytes = (8 + (data->r - data->l))/8;

  wave_element *wes = drv->_nba((3 * bytes) + sizeof(logic_vec_slice),t);

  logic_vec_slice_1 *lvs = (logic_vec_slice_1 *)wes->value;

  const logic_vec_slice *dd = drv->pValue();

  lvs->l        = dd->l;
  lvs->r        = dd->r;
  lvs->sgn      = drv->Sgn();
  lvs->mstr_sig = 0;  
  lvs->data     = lvs->w0;
  logic_vec_slice::copy(data,lvs,bytes);
}

#define SET_FN_CLASS(f,cpy,updt,cmp,stf)\
          CopyFn    f::CpyFn   = (CopyFn)cpy;\
          UpdateFn  f::UpdtFn  = (UpdateFn)updt;\
          CompareFn f::CompFn  = (CompareFn)cmp;\
          SetFlagFn f::StFlgFn = (SetFlagFn)stf;

SET_FN_CLASS(pod,                 podCopy,podUpdate,memcmp,podSigSet)
SET_FN_CLASS(logic_vec_fn_slice,  logic_vec_slice::copy,
	                          logic_vec_slice::update,
	                          logic_vec_slice::compare,
                                  vecSigSet)
SET_FN_CLASS(logic_vec_fn_sig,    logic_vec_slice::copy,
	                          logic_vec_slice::update,
	                          logic_vec_slice::compare,
                                  vecSigSet)
SET_FN_CLASS(logic_vec_fn_slc2sig,logic_vec_slice::copy,
 	                          logic_vec_slice::copy2sig,
 	                          logic_vec_slice::comp2sig,
                                  vecSigSet)

template<>
int  ba<logic_vec_slice>(driver<LOGIC_VEC_SLICE> *drv,logic_vec_slice *data)
{
  int bytes = (8 + (data->r - data->l))/8,
      sz    = (3 *bytes)+sizeof(logic_vec_slice),
      sts   = drv->_ba(data,(3 *bytes)+sizeof(logic_vec_slice),
                                      (CopyFn)logic_vec_slice::copy);

  return sts;
}

cnvSlcVc2Lgc cnvSlcVc2Lgc::reg;
cnvLgc2SlcVc cnvLgc2SlcVc::reg;

void cnvSlcVc2Lgc::brdg_prp(signal<logic_vec_slice> *fs,signal<logic> *ts,int edge)
{
  const logic_vec_slice *slc = fs->pValue();

  int shift = slc->l,
      bits  = 1 + (slc->r - slc->l);

  const logic_word *ws = (logic_word *)slc->data;

  unsigned long sv = ws->value,
                sk = ws->known,
                sd = ws->driven;

  for (; bits-- > 0 ; shift++,ts++) {
    logic l;
    l.value    = sv >> shift;
    l.known    = sk >> shift;
    l.strength = (1 & (sd >> shift)) * LGC_STRN_NORM;
    if (l.diff(ts->pValue())) {
      ts->propagate(sizeof(logic),&l,ts->Edge(&l));
    }
  }
}

void cnvLgc2SlcVc::brdg_prp(signal<logic> *fs,signal<logic_vec_slice> *ts,int edge)
{
  logic_vec_slice *slc = ts->pValMod();

  assert(0);
}

TYPE_DATA_P(logic)
TYPE_DATA_P(logic_vec_slice)

Converter *Converter::list;

Converter::Converter(const TypeData &f,int fm,const TypeData &t,
                     BridgePropFn bp_fn)
 : next(list),
   f_master(fm),
   bridge_fn(bp_fn),
   from(f),
   to(t)
{
  list = this;
}

void Converter::reg(TypeData &f,int fm,TypeData &t,BridgePropFn brdg_fn)
{
  int sts;

  if (!f.name) f.name = abi::__cxa_demangle(f.info.name(), 0, 0, &sts);
  if (!t.name) t.name = abi::__cxa_demangle(t.info.name(), 0, 0, &sts);

  new Converter(f,fm,t,brdg_fn);
}

static char *trimType(const char *cp,char *buff)
{
  char *bp  = buff;
  int   cpy = 1;
  for (; *cp != '>' ; cp++) {
    if (',' == *cp) cpy = 0;
    if (cpy) *bp++ = *cp;
  }
  while (*cp++)      *bp++ = *cp;
  return  buff;
}

Converter *Converter::find(const type_info &frm,const type_info &to)
{
  Converter  *scan = list;
  int         sts;
  const char *ff   = frm.name(),
             *df   = abi::__cxa_demangle(ff, 0, 0, &sts),
             *ft   = to.name(),
             *dt   = abi::__cxa_demangle(ft, 0, 0, &sts),
             *cp;
  char        buff1[strlen(df)],
              buff2[strlen(dt)],
             *bp;


  if ((cp = strstr(cp = df,"signal<"))) {
    df = trimType(cp + 7,buff1);
  }

  if ((cp = strstr(cp = dt,"signal<"))) {
    dt  = trimType(cp + 7,buff2);
  }

  for (; scan ; scan = scan->next) {
    const char *sf   = scan->from.name,
               *st   = scan->to.name;
    if (0 == strcmp(df,sf) && 0 == strcmp(dt,st)) {
      int bits = -1;
      return scan;
    }
  }

  char *f_nm = abi::__cxa_demangle(frm.name(), 0, 0, &sts),
       *t_nm = abi::__cxa_demangle(to.name(),  0, 0, &sts);

  return 0;
}

uintptr_t Module::forSelfThenAll(scan_fn fn,void *data)
{
  uintptr_t r = (*fn)(this,data);

  if (!r) {
    for (Module *chld = child; chld ; chld = chld->next) {
      if ((r = chld->forSelfThenAll(fn,data))) break;
    }
  }

  return r;
}

uintptr_t Module::forAllThenSelf(scan_fn fn,void *data)
{
  uintptr_t r = 0;

  for (Module *chld = child; chld ; chld = chld->next) {
    if ((r = chld->forAllThenSelf(fn,data))) break;
  }

  return r ? r 
           : (*fn)(this,data);
}

Connected *RefTracker::findConn(Module *inst)
{
  int b = 0,
      t = network->index - 1,
      m;

  for (;;) {
    assert(b <= t);
    m = (b+t)/2;
    if (inst > network[cref[m]].self) {
      b = m+1;
    } else if (inst < network[cref[m]].self) {
      t = m-1;
    } else {
      break;
    }
  }

  return &network[cref[m]];
}

BackRef *RefTracker::addBackRef(void_pipe *pipe,Module *inst)
{
  int        b    = 0,
             t    = b_top,
             m    = (b+t)/2,
             i;
  Connected *conn = inst ? findConn(inst)
                         : 0;

  while (b <= t) {
    if (pipe > bref[m].pipe) {
      b = m+1;
    } else if (pipe < bref[m].pipe) {
      t = m-1;
    } else {
      goto have_it;
    }
    m = (b+t)/2;
  }

  if (!conn) return 0;

  if (m < 0) m++;
  
  if (m < b_top && pipe > bref[m].pipe) m++;

  i = ++b_top;
  if (b_top >= brefs) {
    int b  = brefs;
    brefs *= 2;
    BackRef *bggr = new BackRef [brefs];
    while (b-- > 0) bggr[b] = bref[b];
    delete [] bref;
    bref = bggr;
  }

  for (; i > m; i--) bref[i] = bref[i-1];

  bref[m].pipe    = pipe;
  bref[m].conn    = new Connected * [bref[m].cnt = 1];
  bref[m].conn[0] = conn;
  return &bref[m];
  
 have_it: 
  BackRef &br(bref[m]);

  if (conn) {
    for (i = br.cnt; i-- > 0 ;) {
      if (br.conn[i] == conn) goto done;
    }
    
    Connected **bggr = new Connected * [br.cnt +1];
    
    for (i = br.cnt; i-- > 0; ) bggr[i] = br.conn[i];

    delete [] br.conn;
    (br.conn = bggr)[br.cnt++] = conn;
  }

 done:
  return &br;
}

BackRef *RefTracker::findBackRef(void_pipe *pipe)
{
  return addBackRef(pipe,(Module *)0);
}

RefTracker::RefTracker(Connected *net)
 : network(net) {
  int t = net[0].index,
      b = 0,
      m;

  bref = new BackRef[brefs = 2 * t]; b_top = -1;
  cref = new int[t];
  int o = 0;
  for (m = t; --m > 0 ;) if (network[m+1].self > network[m].self) o += 1;
                         else                                     o -= 1; 
  if (o > 0) { // preconditioning
     for (m = t; m-- > 0 ;) { cref[m] = m+1; }
  } else {
     for (m = t; m-- > 0 ;) { cref[m] = t-m; }
  }
  int swaps,s;
  do { // bubble-sort (need something better)
    swaps = 0;
    for (m = b; ++m < t;) {
      if (network[s = cref[m-1]].self > network[cref[m]].self) {
	swaps++;
	cref[m-1] = cref[m]; cref[m] = s;
      } 
    }
    m = t--;
    for (; --m > b;) {
      if (network[s = cref[m-1]].self > network[cref[m]].self) {
	swaps++;
	cref[m-1] = cref[m]; cref[m] = s;
      } 
    }
    m = ++b;
  } while (swaps);
}

int Module::Analyze(RefTracker *trk)
{
  int n   = 0,
      l   = trk->network[0].index;

  while (++n <= l) {
    Connected &c(trk->network[n]);
    PipeRef   *pr = c.pipes;
    for (; pr ; pr = pr->next) {
      trk->addBackRef(pr->pipe,c.self);
    }
  }

  return 0;
}

void Module::ShowConn(RefTracker *trk)
{
  int  n   = 0,
       m   = 0,
       l   = trk->network[0].index;
  char line[l+20],
       frmt[8];

  for (n = 1; n <= l; n++) {
    const char *name = trk->network[n].self->Name(line,l);
    int         s    = name ? strlen(name)
                            : 1;
    if (s > m) m = s;
  }

  sprintf(frmt,"%%%ds ",m);

  for (int c = 0; c < m ; c++) {
    for (int s = m; s-- >= 0;) putchar(' ');
    for (n = 1; n <= l; n++) {
      const char *name = trk->network[n].self->Name(line,l);
      if (!name)  name = "?"; 
      int ch = *name;
      for (int i = c; i-- > 0;) if (!(ch = *++name)) { ch = ' ';
                                                       break; }
      putchar(ch);
    }
    printf("\n");
  }

  line[l] = '\0';
  for (n = 1; n <= l; n++) {
    const char *name=trk->network[n].self->Name(line,l);
    printf(frmt,name ? name 
                     : "?");
    memset(line,' ',l);
    for (PipeRef *prf = trk->network[n].pipes;
         prf ; prf = prf->next) {
      BackRef *brf = trk->findBackRef(prf->pipe);
      if (brf) {
	for (int c = brf->cnt; c-- > 0;) {
	  Connected *conn = brf->conn[c];
	  line[conn->index -1] = 'X';
	}
      }
    }
    printf("%s\n",line);
  }
}

bool void_pipe::logging(bool on)
{
  bool was = !(sts & PSTS_NOLOG);

  if (on) sts = PSTS(sts & ~PSTS_NOLOG);
  else    sts = PSTS(sts |  PSTS_NOLOG);

  return was;
}

void void_pipe::log(Module *inst,Proc *prc,eCUSG rw)
{
  if (CurrNetwork) {
    if (CurrNetwork[CurrPos].self != inst) {
      if (++CurrPos > CurrNetwork->index || CurrNetwork[CurrPos].self != inst) {
	assert(0);
      }
    }
    if (!(sts & PSTS_NOLOG)) {
      CurrNetwork[CurrPos].pipes = new PipeRef(this,CurrNetwork[CurrPos].pipes);
    }
  }
}

} // parc

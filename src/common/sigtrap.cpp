/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * sigtrap_cpp_rcsid() {return "$Id: sigtrap.cpp,v 1.14 2007/02/01 06:46:48 dkc Exp $";}

#include "system.h"
#include "v2k_mem.h"
#include "sigtrap.h"
#include "error.h"
#include "job.h"

SigTrap  *SigTrap::Traps;
int       SigTrap::Trapped;
SigState  SigTrap::Active[SIG_MAX+1];

#ifdef SA_SIGINFO
static void sg_catch(int sig, siginfo_t *info, void *data)
{
  SigTrap::Traps->Catch(sig,info,data);
}
#else
static void sg_catch(int sig)
{
  SigTrap::Traps->Catch(sig,0,0);
}
#endif

void SigTrap::Catch(unsigned int sig, siginfo_t *info, void *data)
{
  assert(sig <= SIG_MAX);

  if (Active[sig].raised || Active[sig].caught) return;

  Active[Trapped = sig].caught++;

  Active[sig].info = info;
  Active[sig].data = data;

#ifdef SA_SIGINFO

  switch (sig) {
  case SIGCLD: switch (info->si_code) {
               case CLD_EXITED: ProcStat(info->si_pid,info->si_status); break;
               }
               break;
  }

#endif

  SigMap m = 1 << (sig -1);
 
  JumpBuff tmp;

  try {
    Traps->Handle(m);
  }
  catch (JumpBuff jp) {
    tmp.jump_buff = jp.jump_buff;
    tmp.jump_val  = jp.jump_val;
  } 

  Reset(sig);

  if (tmp.jump_buff) longjmp(tmp.jump_buff,
                             tmp.jump_val);
}

void SigTrap::Reset(unsigned int sig)
{
  Active[sig].caught = Trapped = 0;
}

SigTrap::SigTrap(SigMap s_map,SigHndlr fn,SigAct s_act,void *cbd,void *own)
{
  int    s = 1;
  SigMap m = 1;

  mode  = s_act;
  data  = cbd;
  hndlr = fn;
  map   = s_map;
  next  = Traps;
  Traps = this;
  owner = own;

  for (; s <= SIG_MAX ; s++ , m <<= 1) {
    if (s_map & m) {
      if (!Active[s].traps++) {
        struct sigaction act;
        BZEROS(act);
#ifdef SA_SIGINFO
        act.sa_sigaction = sg_catch;
        act.sa_flags     = SA_SIGINFO;
#else
        act.sa_handler   = sg_catch;
#endif
        if (sigaction(s,&act,&Active[s].oact)) {
          fprintf(stderr,"sigtrap: %s\n",strerror(errno));
        }
      }
    }
  }
}

SigTrap::~SigTrap()
{
  int      s  = SIG_MAX;
  SigTrap *p0 = Traps;

  if (this == p0) {
    Traps = next;
    goto ok;
  } else for (; p0 ; p0 = p0->next) {
    if (this == p0->next) {
      p0->next = next;
      goto ok;
    }
  }

  assert("Trap wasn't linked!" == 0);

ok:
  for (; s > 0 ; s--) {
    int m = 1 << (s - 1);
    if (map & m) {
      if (!--(Active[s].traps)) {
        sigaction(s,&Active[s].oact,0);
      }
    }
  }
}

bool SigTrap::Raise(unsigned int sig, siginfo_t *info, void *data)
{
  assert(sig <= SIG_MAX);

  if (Active[sig].raised || Active[sig].caught) return false;

  Active[sig].raised++;

  SigMap m = 1 << (sig -1);

  Traps->Catch(m,info,data);

  Active[sig].raised = 0;

  return true;
}

void SigTrap::Handle(SigMap s_map)
{
  SigTrap *trap = this,
          *nt;

  for (; trap ; trap = nt) {
    nt = trap->next;
    if (trap->map & s_map) {
      SigAct    s_md = trap->mode;
      SigHndlr  fn   = trap->hndlr;
      void     *cbd  = trap->data;

      if (s_md & SA_ONCE) delete(trap);

      switch ((*fn)(s_map,cbd)) {
      case SGD_CONT: if (!(s_md & SA_RELAY)) return;
                     break;
      default:       assert("Bad Handler return" == 0);
      }
    }
  }
}

void SigTrap::UnTrap(void *own)
{
  SigTrap *trap = this,
          *nt;

  for (; trap ; trap = nt) {
    nt = trap->next;
    if (trap->owner == own) delete (trap);
  }
}

extern "C" {

void LongJmp(SysJmpBuf *jb,int rtn)
{
  longjmp(jb,rtn);
}

int SetJmp(SysJmpBuf *jb)
{
  return setjmp(jb);
}

}

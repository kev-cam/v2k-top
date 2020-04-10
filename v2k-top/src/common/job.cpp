/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * job_cpp_rcsid() {return "$Id: job.cpp,v 1.22 2007/09/26 19:10:36 dkc Exp $";}

#include "system.h"
#include "error.h"
#include "strfunc.h"
#include "strdb.h"
#include "poolmngr.h"
#include "job.h"

Job *Job::JobList;
int  Job::JobLimit;

extern "C" int SetJobLimit(int j)
{
  int r = Job::JobLimit;
  Job::JobLimit = j;
  return r;
}

Job::Job(const char *cmd)
{
  add();

  wait_sts1 = -1;
  pid       = -1;
  command   = new String(cmd);

  sts       = system(cmd);
  wait_sts1 = 0;
}

Job::Job(int id,const char *cmd)
{
  add();

  wait_sts1 = -1;
  pid       = id;
  command   = new String(cmd);
}

Job::Job(int id,int argc,const char **argv)
{
  add();

  command = new String;

  for (*command = *argv; *++argv;) {
    *command += " ";
    *command += *argv;
  }

  wait_sts1 = -1;
  pid       = id;
}

Job::~Job()
{
  DELETE(command);
  remove();
}

const char *Job::StrPid()
{
  sprintf(spid,"%d",pid);
  return spid;
}

eSTS Job::Status(int ignr_intr)
{
  int sts1,
      sts2;

  lock = 1;

  if (pid > 0) {

    do {
      sts2 = -1;
      sts1 = waitpid(0, &sts2, WCONTINUED|WUNTRACED);

      if (sts1 < 0) {
        sts2 = errno;
        if (sts2 == EINTR && !ignr_intr) goto done;
        if (sts2 == ECHILD)              goto done; // shouldn't happen
      }

      if (sts1 > 0) {
        Job *job = JobList;
        for (; job ; job = job->next) {
          if (sts1 == job->pid) job->SetStat(sts1,sts2);
          else if (job->dead)   job->remove();
        }
      }
    } while (pid);
  }

  sts = WEXITSTATUS(wait_sts2);

  if (wait_sts1 < 0) sts = errno;

  sts2 = sts;
done:
  lock = 0;
  return ERROR(sts2);
}

const char *Job::StatStr()
{
  int sts1,
      sts2;

  if (pid > 0) {

    do {
      sts2 = -1;
      sts1 = waitpid(pid, &sts2, WNOHANG|WCONTINUED|WUNTRACED);
    } while (sts1 < 0 && errno == EINTR);

    if (sts1 == pid) SetStat(sts1,sts2);

    if (-1 == sts1) {
                                   return "Zombie";
    } else if (wait_sts1 >= 0) {
      if (WIFEXITED(wait_sts2))    return "Exited";
      if (WIFSTOPPED(wait_sts2)) {
        if (WSTOPSIG(wait_sts2))   return "Suspended (signal)";
        else                       return "Suspended";
      }
    }

    return "Running";
  }

  return "Done";
}

void Job::SetStat(int sts1,int sts2)
{
  wait_sts1 = sts1;
  wait_sts2 = sts2;

  if (WIFEXITED(wait_sts2)) {
    pid = 0;
    if (waiting) *waiting = ERROR(sts2);
  }
}

bool Job::Zombie()
{
  int  sts1,
       sts2;

  if (pid > 0) {

    do {
      sts2 = -1;
      sts1 = waitpid(pid, &sts2, WNOHANG|WCONTINUED|WUNTRACED);
    } while (sts1 < 0 && errno == EINTR);

    if (sts1 == pid) SetStat(sts1,sts2);
  }

  return (pid <= 0);
}

bool Job::LogSts(eSTS *p_sts)
{
  waiting = p_sts;
}

extern "C" {

  static int process_id = -1;

  int ProcessID()
  {
    if (process_id < 0) process_id = getpid();

    return process_id;
  }

}

extern "C" int Fork(int keep)
{
  ((Stream *)0)->flushAll();

  strPoolShared(keep >= 0);

  int ppid = ProcessID(),
      pid;

  if (keep > 0) PoolMpdOwnr(ppid);

  switch (pid = fork()) {
  case 0:  process_id = -1;
           ProcessID();
           break;
  default: if (0 == keep) PoolMpdOwnr(pid);
  case -1: break;
  }

  return pid;
}

extern "C" void ProcStat(int pid,int sts)
{
  Job *pj = Job::JobList;

  for (; pj ; pj = pj->Next()) if (pid = pj->Pid()) {
    pj->SetStat(pid,sts);
    break;
  }
}

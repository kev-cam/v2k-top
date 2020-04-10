/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  job_h_rcsid
#define job_h_rcsid() {return "$Id: job.h,v 1.18 2007/03/28 07:23:27 dkc Exp $";} /* RCS ID */

#ifndef JOB_H
#define JOB_H

#ifdef __cplusplus
class String;

class Job {

  int     pid,
          sts,
          wait_sts1,
          wait_sts2,
          lock,
          dead;
  char    spid[8];
  eSTS   *waiting;
  Job    *next,
         *prev;
  String *command;

public:

  static Job *JobList;
  static int  JobLimit;

  inline Job          *Jobs()    {return JobList;};
  inline Job          *Next()    {return next;};
  inline int           Pid()     {return this ? pid : -1;};
  inline const String *Command() {return command;};

  inline void add()    {BZEROP(this);
                        if ((next = JobList)) next->prev = this;
                        JobList  = this;};
  inline Job *remove() {Job *pr = prev;
                        if (prev) prev->next = next;
                        if (next) next->prev = prev;
                        if (JobList == this) pr = JobList = next;
                        return pr;};
  inline int Busy()    {return lock;}

  Job(const char *);
  Job(int,const char *);
  Job(int,int,const char **);
  ~Job();

  inline int Pid() const {return pid;}

  bool        LogSts(eSTS *);
  bool        Zombie();
  eSTS        Status(int);
  void        SetStat(int,int);
  const char *StatStr();
  const char *StrPid();

  inline void Dead()        {dead = 1;};
  inline eSTS Status()      {return Status(1);};
  inline eSTS Kill(int sig) {int r_sts = ESRCH;
                             if (pid && kill(pid,sig)) r_sts = errno;
                             return ERROR(r_sts);}

};


extern "C" {
#endif
  int  Fork(int);

  int  SetJobLimit(int);

  int  ProcessID();

  void ProcStat(int,int);
#ifdef __cplusplus
}
#endif

#endif /* JOB_H */

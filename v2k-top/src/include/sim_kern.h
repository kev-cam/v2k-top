/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  sim_kern_h_rcsid
#define sim_kern_h_rcsid() {return "$Id: sim_kern.h,v 1.15 2012/10/16 22:30:00 cvs Exp $";} /* RCS ID */

extern const char *SimLibName;

typedef enum 
{ 
  SMST_LOADED      = 0,
  SMST_INITIALIZED = 1,
  SMST_RUNNING     = 2,
  SMST_FINISHED    = 128
} eSMST;

class Simulator {
 public:
  static Simulator *Kernel;

  virtual eSTS        Start(bool)      = 0;
  virtual void        setIrq(int)      = 0;
  virtual void        setStatus(eSMST) = 0;
  virtual void        clrStatus(eSMST) = 0;
  virtual eSMST       Status()         = 0;
  virtual const char *whoami()         = 0;
  virtual char       *simModEp(const char *,int,int,char *) = 0;
};

typedef enum {
  SIM_IRQ_USR = 1
} eIRQ;

typedef struct pond_s {
  struct pond_s *next;
} Pond;

class EventNBA : public _Event {
 public:
  void       *val;
  int         drv_n;
  _PortFlags *prt;
  void       *ip;
  PortAssFn   afn;
};

class Stream;
class v2kSim : public Simulator {
 public:
  virtual eSTS        Start(bool from_shell = 0);
  virtual eSMST       Status();
  virtual void        setStatus(eSMST);
  virtual void        clrStatus(eSMST);
  virtual void        setIrq(int);
  virtual const char *whoami();
  virtual char       *simModEp(const char *,int,int,char *);

  _Event     *Q,
            **Q_nxt;
  EventNBA   *freeNBA;
  int         Irq;
  eSMST       status;
  void       *ip;
  _SimTime    Time;
  int         pli_arg_count;
  void      **pli_args;
  Stream     *sim_log;
  jmp_buf     top;

  Stream  *SimLog();

  void  setIp(void *);
  void  StartProc(_Context *,C_entry,_SimObj *);
  void  SigInit(_PortRefBase *,int,int,int,int);
  bool  At(_Context *,int,C_entry,int);
  bool  At(_Context *,int,_Context *,int);
  int   Delay(_Context *,int,C_entry,_SimTime,_SimObj *);
  int   Delay(_Context *,int,_Context *,_SimTime,_SimObj *);
  int   Suspend(_Context *,int,C_entry,_SimObj *);

  void  Eval(int,int,void *);
  void  addSense(_Sense **,_PortFlags *);
  void  Sensitize(_Context *,_Sense *);
  void  Desensitize(_Context *);

  void  Schedule(_Event *,_SimTime);

  static void *Bucket[29];
  static Pond *Ponds[29];

  void  fillBucket(int);
  void *Allocate(unsigned int);
  void  Recycle(void *);

  v2kSim();
  void init();
};

extern v2kSim v2kSimKrnl;


eSTS  loadSimKrnl(const char *);
eSTS  initSim();

#ifndef SIM_IP
#define SIM_IP void
#endif
extern "C" {
  void v2kSimEval(SIM_IP *,int,int,void *);
  void v2kSimSigInit(SIM_IP *,_PortRefBase *,int,int,int,int);
  void v2kSimAddSense(SIM_IP *,_Sense **,_PortFlags *);
}

typedef enum {
  SD_BLDNG,
  SD_BUILT
} eSD;

class SimData {
  eSD sdt:8;
};

typedef enum {
  SI_INVALID = -1,
  SI_SIG
} eSI;
class v2kSenseCntxt;
class v2kSenseArray;
class v2kSense {
 public:
  eSNS  typ:8;

  inline v2kSenseCntxt *C() {return (v2kSenseCntxt *)this;};
  inline v2kSenseArray *A() {return (v2kSenseArray *)this;};
};

class v2kSenseCntxt : public v2kSense {
 public:
  _Context *cntxt;

  v2kSenseCntxt(_Context *cntxt = 0);

  inline void activate() { if (cntxt) cntxt->call(cntxt); };

};


class v2kSenseArray : public v2kSense {
 public:
  int       count;
  v2kSense *sense[1];
};

v2kSense *v2kAddSensEl(v2kSenseArray **,v2kSense *);

class v2kSimItem {
 public:
  eSNS      sns:8;
  eSI       typ:8;
  int       size:16,
            idx;

  void addSense(_Sense **);
};

class v2kSI_SIG : public v2kSimItem {
 public:
  int       drv;
  char      data[4];
  v2kSense *sense;
};

class V2kSimData : public SimData {
 public:
  int        used,
             max;
  int        _padding_;
  v2kSimItem data[0];
};

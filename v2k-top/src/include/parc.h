/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  parc_h_rcsid
#define parc_h_rcsid() {return "$Id: parc.h,v 1.45 2012/10/16 22:30:00 cvs Exp $";} /* RCS ID */

#define signal sys_signal

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <inttypes.h>

#include <pthread.h>

#undef signal

using namespace std;
#include <typeinfo>

namespace parc {

class Proc;
class Kernel;
class rx_base;
class void_signal;

#ifndef MIN_THREADS
#define MIN_THREADS sizeof(long)
#define MAX_THREADS MIN_THREADS
#endif

typedef pthread_t thread_id;
typedef char      cpu_id;

inline thread_id thread_self() {return pthread_self();}

typedef enum {
  SGF_NONE = 0,
  SGF_BRDGD
} eSigFlag;

typedef int       (*ResumeFn)(Proc *);
typedef rx_base  *(*UpdateFn)(void_signal *, const void *, size_t);
typedef int       (*CompareFn)(const void *, const void *, size_t);
typedef int       (*CopyFn)(const void *, void *, size_t);
typedef void     *(*SetFlagFn)(void_signal *, eSigFlag, int, void *);
typedef void      (*BridgePropFn)(void_signal *,void_signal *,int edge);

#define FN_CLASS(f)\
class f {\
 public:\
  static CopyFn      CpyFn;\
  static UpdateFn    UpdtFn;\
  static CompareFn   CompFn;\
  static SetFlagFn   StFlgFn;\
};
FN_CLASS(pod)
FN_CLASS(logic_vec_fn_slice)
FN_CLASS(logic_vec_fn_sig)
FN_CLASS(logic_vec_fn_slc2sig)

typedef int32_t  RelTime;

class Time {
 public:
  int64_t t;
# define  TIME_MAX_REL ((int64_t)((~(uint64_t)0)>>1))
  int     delta;

  inline Time()                       : t(0),   delta(0)       {}
  inline Time(uint64_t T,int d = 0)   : t(T),   delta(d)       {}
  inline Time(Time &T)                : t(T.t), delta(T.delta) {}
  inline Time(const Time &T)          : t(T.t), delta(T.delta) {}
  inline Time(volatile Time &T)       : t(T.t), delta(T.delta) {}

  inline operator Time &() {return *this;}

  inline bool operator != (Time & lhs) {
    return t != lhs.t || delta != lhs.delta;
  }

  inline bool operator == (Time & lhs) {
    return t == lhs.t && delta == lhs.delta;
  }

  inline bool operator == (volatile Time & lhs) {
    return t == lhs.t && delta == lhs.delta;
  }

  inline bool operator <= (Time & lhs) {
    int64_t T = t - lhs.t;
    return T <= 0 || (t == lhs.t && delta <= lhs.delta);
  }

  inline bool operator >= (Time & lhs) {
    int64_t T = t - lhs.t;
    return T >= 0 || (t == lhs.t && delta >= lhs.delta);
  }

  inline bool operator >= (volatile Time & lhs) {
    int64_t T = t - lhs.t;
    return T >= 0 || (t == lhs.t && delta >= lhs.delta);
  }

  inline bool operator < (Time & lhs) {
    int64_t T = t - lhs.t;
    return T < 0 || (0 == T && delta <  lhs.delta);
  }

  inline bool operator > (Time & lhs) {
    int64_t T = t - lhs.t;
    return T > 0 || (0 == T && delta >  lhs.delta);
  }

  inline bool operator > (volatile Time & lhs) {
    int64_t T = t - lhs.t;
    return T > 0 || (0 == T && delta >  lhs.delta);
  }

  inline bool operator > (RelTime lhs) {
    int64_t T = t - lhs;
    return T > 0;
  }

  inline Time &operator = (const Time & lhs) {
    t     = lhs.t;
    delta = lhs.delta;
    return *this;
  }
  
  inline Time operator + (Time & lhs) {
    Time ret(t + lhs.t);
    if (0 == lhs.t) {
      ret.delta = delta + lhs.delta;
    }
    return ret;
  }
  
  inline Time &operator += (Time & lhs) {
    t += lhs.t;
    if (0 == lhs.t) {
      delta = lhs.delta;
    } else {
      delta = 0;
    }
    return *this;
  }
  
  inline Time &operator += (volatile Time & lhs) {
    t += lhs.t;
    if (0 == lhs.t) {
      delta = lhs.delta;
    } else {
      delta = 0;
    }
    return *this;
  }
  
  inline Time &operator -= (Time & lhs) {
    t -= lhs.t;
    if (0 == lhs.t) {
      delta = lhs.delta;
    } else {
      delta = 0;
    }
    return *this;
  }
  
  inline Time &operator -= (volatile Time & lhs) {
    t -= lhs.t;
    if (0 == lhs.t) {
      delta = lhs.delta;
    } else {
      delta = 0;
    }
    return *this;
  }
  
  inline Time &operator += (RelTime & lhs) {
    t += lhs;
    if (0 != lhs) {
      delta = 0;
    }
    return *this;
  }
  
  inline Time &operator += (int64_t lhs) {
    t += lhs;
    if (0 != lhs) {
      delta = 0;
    }
    return *this;
  }
  
  inline Time operator + (RelTime & lhs) {
    Time ret(t);
    if (0 == lhs) {
      ret.delta = delta;
    } else {
      ret.t += lhs;
    }
    return ret;
  }
  
  inline Time operator - (Time & lhs) {
    Time ret(t);
    if (0 == lhs.t) {
      ret.delta = delta - lhs.delta;
    } else {
      ret.t -= lhs.t;
    }
    return ret;
  }

  inline Time &operator -- () {
    t--;
    delta = 0;
    return *this;
  }
  
  inline void incDelta() { delta++; }

  long   L() {return t;}
  double D() {return t + delta/1000.0;}

};

int nop(Proc *);
int Assert(Proc *);

class Event {
  friend class Proc;
  friend class Kernel;
  friend class void_pipe;

  Event  *next;
  Event  *prev;
  Time    time;

 public:
  Proc   *proc;
  ResumeFn   fn;

  inline Event() : fn(nop), next(0), prev(0), time(0,-1) {}

  inline Time  When()          {return time;};
  inline void  SetTime(Time t) {time = t;};
};

extern const Event NullEvent;

typedef  uintptr_t (*scan_fn)(class Module *,void *);

class Connected;
class RefTracker;

class Module {
  Module     *parent,
             *child,
             *next;
  const char *name;
  int        *index;

 public:

  static Module Root;

  Module *init(Module *up = 0,const char *nm = 0);

  Module(Module *up = 0,const char *nm = 0) {child = next = 0; index = 0; init(up,nm);}
  ~Module();

  virtual void init_prcs() {};
  virtual void post_bind() {};

  virtual const char *Name(char *buff   = 0,
                           int   length = 0) const {return name;};

  inline void bind_children() {};
  inline bool isRoot()        {return 0 == this || this == &Root;}

  void setIdx(int,...);
  void addChild(Module *);
  void detach(Module *);

  uintptr_t forSelfThenAll(scan_fn,void *);
  uintptr_t forAllThenSelf(scan_fn,void *);

  static void InitAll();
  static void RunAll();
  static void StartAll();

  RefTracker *LogUsage();
  int         Analyze(RefTracker *);
  void        ShowConn(RefTracker *);
};

 class void_pipe;
class PipeRef {
 public:
  PipeRef    *next;
  void_pipe  *pipe;

  PipeRef(void_pipe *p,PipeRef *n) : pipe(p), next(n) {} 
};

class Connected {
 public:
  Module  *self;
  int      index;
  PipeRef *pipes;

 Connected() : self(0), index(-1), pipes(0) {}
};

inline Module *root() {
  return &Module::Root;
}

typedef Module *(*create_fn)(Module *,const char *);

class Arch {
 public:
  Arch       *next;
  const char *name;
  create_fn   c_fn;

  Arch(const char *,const char *, create_fn);
};

Module *NewArch(Module *,const char *,const char *,const char *);

class Entity {
 public:
  Module *_create(Module *up,const char *nm,const char *arch = 0);
};

class EntItem {
 public:
  EntItem *next;
  const char *name;
  Arch    *list;

  EntItem(const char *);
};

#define K_PIPE_SIZE 0x100
class kern_pipe {

  friend class Kernel;

  volatile int rd_posn;
  volatile int wr_posn;

  const void *data[K_PIPE_SIZE];

  kern_pipe() : rd_posn(0),wr_posn(0) {}
};

typedef enum {
  KM_BAD      = 0,
  KM_MIGRATE  = 1,
  KM_PP_OWN_R,
  KM_PP_OWN_W,
  KM_PROC,
  KM_SIG,
  KM_RCVR,
  KM_EVENT,
  KM_PP_CLNT,
  KM_PP_CLNT_O,
  KM_PP_ACK,
  KM_WAKE_RD,
  KM_WAKE_WR,
  KM_DISABLE,
  KM_ENABLE,
  KM_DRV_OWN,
  KM_DRV_UPD,
  KM_DRV_O_U,
  KM_DONE
} eKMSG;

typedef enum {
  KS_INIT     =  0,
  KS_RUNNING  =  1,
  KS_STOPPED  = -1
} eKSTS;

typedef enum {
  ACT_NO       =    0,

  ACT_GO       =    1,
  ACT_PROC     =    2,
  ACT_PIPE     =    4,
  ACT_Q        =  010,
  ACT_SYNC_0   =  020,
  ACT_IO       =  040,
  ACT_SYNC_T   = 0100,
  ACT_OTHER    = 0200

} eACT;

typedef enum {
  SCHED_IMMED  = -1
} eSCHED;

typedef struct {
  int                     cpus;
  int                     running;
  pthread_mutex_t         count_lock;
  volatile Time           abs_time;
  volatile unsigned char  activity[MIN_THREADS];
} KernGlob;

class void_pipe;
class Kernel {
  cpu_id     cpu;
  thread_id  id;
  int        sts;
  int        outstanding;
  Time       abs_next;
  Event     *pQ;
  void_pipe *channels;

  char          flgo[MIN_THREADS];
  volatile char flgi[MIN_THREADS];
  kern_pipe     xcmd[MIN_THREADS];

  static KernGlob G;

 public:

  Kernel();

  static void *Start(void *);

  void Schedule(Event *);
  void Schedule(Event *,Time);
  void Schedule(Event *,RelTime);
  void Schedule(Event *,eSCHED);
  void ScheduleDelta(Event *);
  void Desched(Event *);
  void Continue();
  void Active(void_pipe *);
  void Sync(int *);
  int  CheckKP();
  int  anyActive(volatile unsigned char *,int act = ACT_NO);
  int  read(int);
  void write(int,const void *);
  void write(int,const void *,const void *);
  void write(int,const void *,const void *,const void *);

  inline thread_id  ThreadID()           {return id;}
  inline int        CPU()                {return cpu;}
  inline Kernel    *Master()             {return this;}
  inline void       outStanding(int c=1) {outstanding += c;}

  inline int        CPUs()     {return G.cpus;}
  inline Time       Now()      {return G.abs_time;}

  inline int GetCPU(thread_id tid)
  {
    Kernel *scan = this;
    while (tid != scan->id) scan++;
    return scan->cpu;
  }

};

extern Kernel Kern[];

inline Kernel *GetKern(thread_id tid)
{
  Kernel *scan = Kern;
  while (tid != scan->ThreadID()) scan++;
  return scan;
}

inline Kernel *MyKern() {return GetKern(thread_self());}

typedef enum {
  ERR_NONE,
  ERR_ELAB_NO_ARCH,
  ERR_ELAB_AMBG_ARCH,
  ERR_BIND_MISMATCH,
  ERR_BAD_SLICE
} ePCERR;

typedef enum {
  SIG_READY =  0,
  SIG_BLCK  = -1
} eSigSts;

typedef enum {
  SNS_RCVR   = 0,
  SNS_CHAN,
  SNS_BRIDGE
} eSense;

typedef enum {
  IMP_NONE = 0,
  IMP_BEFORE
} eImplicit;

typedef enum {
  SNF_NONE = 0
} eSnsFlg;

class sense {
public:
  Event     *cllbk;
  cpu_id     cpu;
  eSense     sns_typ:8;
  eImplicit  usage:8;
  eSnsFlg    sns_flgs:8;

  sense(eSense t = SNS_RCVR) : sns_typ(t), usage(IMP_NONE),
                               cpu(0), cllbk(NULL) {
                                  sns_flgs = SNF_NONE;
                               }

  inline void setFlag(eSnsFlg f) { sns_flgs = (eSnsFlg)(f|sns_flgs); }

  inline cpu_id CPU() {return cpu;}
};

class void_signal;
void bind(Module *,const type_info &,void_signal **,
                   const type_info &,void_signal *);

class Proc {
  friend class Kernel;
  cpu_id cpu;
 public:

  int suspend(ResumeFn,Event *);
  int suspend(ResumeFn,Event *,void_pipe &);

  int  wait(RelTime,ResumeFn,Event *);
  void unWait(Event *);
  void initWait(Event *,int,Module *m = 0);

  inline Proc() : cpu(0) {}

  inline int CPU() {return cpu;}  

  void cancel(Event *);

  int migrate(ResumeFn,Event*,int cpu = -1);

  eSigSts waitSig     (Event*,ResumeFn,sense *,...);
  eSigSts waitSigPre  (Event*,ResumeFn,sense *,...);
  void    unWaitSig   (Event*,sense *,...);
//void    unWaitSigPre(Event*,sense *,...);
#define   unWaitSigPre unWaitSig
  int     initFork(Event *,int,int,...);
  int     joinFork(Event *,int);
  virtual ~Proc();

  virtual Module *_M() { return 0; }
};

typedef enum {
   CSTS_NONE   =  0,
   CSTS_KEEP   =  1,
   CSTS_LOCK   =  2,
   CSTS_RETRY  =  4,
   CSTS_GREEDY =  8
} eCSTS;

typedef enum {
   CUSG_NONE   = 0,
   CUSG_READ   = 1,
   CUSG_WRITE  = 2
} eCUSG;

#define CSTS(x) ((eCSTS)(x))

class pipe_client : public sense {
  friend class void_pipe;

  char         notify;
  eCSTS        sts:8;
  int          ready;
  int          done;
  void_pipe   *chan;
  pipe_client *next;
 public:
  pipe_client();
  ~pipe_client();
 
  inline void Clear()              {done = 0;}
  inline int  Ready()              {return ready;}
  inline int  Notify()             {return notify;}
  inline void clrNotify()          {assert(notify >= 0);
	                            notify = -1;}
  inline void setNotify(cpu_id id) {assert(notify < 0);
	                            notify = id;}

  void init(void_pipe *,Event *evt = 0);
  int  callback();
  int  set_cllbk(ResumeFn,int);
  int  set_cllbk(void_pipe *,Event *,ResumeFn,int);
  void disable();
  void detach();
  bool Sense();

  static pipe_client *prc_init(void_pipe *,int,int,Event *,eCUSG,int mb=1);
  static inline 
         pipe_client *prc_init(void_pipe &p,int p_sz,int i,Event *wt,eCUSG u,int mb=1) {
               return prc_init(&p,p_sz,i,wt,u,mb);
         }
};

typedef enum {
  CHNS_EMPTY =  0,
  CHNS_DEAD  = -1,
  CHNS_BLOCK = -2
} eChanSts;

typedef enum {
  CHNT_CP    =  0,
  CHNT_DESC,
  CHAN_FP
} eChanType;

typedef enum {
  CHNF_XTHRD = 0x01
} eChanFlag;

typedef struct {
  int  size,
       wr_posn,
       rd_posn;
  char buff[0];
} pipe_buff;

typedef enum {
  CWM_NOWAIT = 0,
  CWM_QUEUE  = 1,
  CWM_SYNC   = 2
} eChanWM;

typedef enum {
  PSTS_NONE,
  PSTS_NOLOG = 1
} ePSTS;

#define PSTS(p) ((ePSTS)(p))

class void_pipe {
  friend class pipe_client;
  friend class Kernel;

  void_pipe   *next;

  eChanType    ctyp:8;
  ePSTS        sts:8;
  char         busy;
  char         error;
  thread_id    rd_thrd;
  thread_id    wr_thrd; // owner
  pipe_client *clients_rd;
  Event       *clients_wr;

  union {
    int        desc;
    void      *buff;
    pipe_buff *pc;
    char       byte[sizeof(char *)];
  } data;


 public:
  void_pipe(int pkt_sz = 0);
  void_pipe(FILE *);
  void_pipe(char *,int); // as open

  void         attach(pipe_client *);
  void         check_clients(pipe_client *,pipe_buff *,int cpu = -1);
  pipe_client *find_client(pipe_client *,int);
  void         add_writer(Event *);
  int          wakeup_wr();
  int          wakeup_rd();
  void_pipe   *me();
  void         log(Module *,Proc *,eCUSG);
  bool         logging(bool);

  virtual eChanSts    readV(void *,int,pipe_client *,Event *,ResumeFn,int);
  virtual eChanSts    writeV(const void *,int,int,Event *wt,ResumeFn fn,eChanWM wm = CWM_NOWAIT);
  virtual int         pktSize();
  virtual int         Ready();

  inline int size() { return 1; }

  static void_pipe NullDev;

};

template<typename P>
class pipe : public void_pipe {
 public:
  pipe() : void_pipe(sizeof(P)) {}

  inline eChanSts read(P *p,int items,pipe_client *clnt,Event *wt,int w,ResumeFn fn) {
                    return readV(p,items,&clnt[w],&wt[w],fn,sizeof(P));
                  };
  inline eChanSts write(const P *p,int items,Event *wt,ResumeFn fn,eChanWM wm = CWM_NOWAIT) {
                    return writeV(p,items,sizeof(P),wt,fn,wm);
                  };

  inline eChanSts nba(P &val,Event *wt=0,ResumeFn fn=0) {
                                return write(&val,1,wt,fn,CWM_QUEUE);}

  inline eChanSts ba(P &val,Event *wt=0,ResumeFn fn=0) {
                                return write(&val,1,wt,fn,CWM_SYNC);}
  inline eChanSts ba(P *val,Event *wt=0,ResumeFn fn=0)  {
                                return write( val,1,wt,fn,CWM_SYNC);}

  int pktSize() {return sizeof(P);};

  static inline pipe_client *prc_init(pipe  *p,int s,int n,Event *evt,eCUSG rw,int mb=1) {
    return pipe_client::prc_init((void_pipe *)p,s,n,evt,rw,mb);
  }
  static inline pipe_client *prc_init(pipe **p,int s,int n,Event *evt,eCUSG rw,int mb=1) {
    return pipe_client::prc_init((void_pipe *)p,s,1,evt,rw,mb);
  }
};

class void_signal;
class rx_base : public sense {
 public:
  rx_base *next;
  rx_base *prev;
  char     mark;

  inline rx_base(eSense sns) : sense(sns) {next = prev = 0;}
};

class receiver : public rx_base {
 public:
  char         edge;
  eImplicit    imp:8;
  void_signal *sig;
  int          max_idx;

  ~receiver();

  inline receiver(void_signal *s = 0, eSense sns = SNS_RCVR) 
   : rx_base(sns)
    {imp = IMP_NONE; max_idx =-1; sig = s;};
  inline receiver(void_signal &s, eSense sns = SNS_RCVR)
   : rx_base(sns)
    {imp = IMP_NONE; max_idx =-1; sig = &s; assert(sig);};

  void enable(Event *,int edg = 0,eImplicit imp = IMP_NONE);
  void enable(Kernel *,eImplicit);
  void disable(Event *evt = 0);
  void index(int,int bot = -1);

  inline void init(void_signal &s,int sz=1) {sig = &s; index(sz-1);};
  inline void init(void_signal *s,int sz=1) {sig =  s; index(sz-1);};

  static receiver *prc_init(void_signal &s,int sig_sz=1,int mb=1);
  static receiver *prc_init(void_signal *s,int sig_sz=1,int mb=1);
};

sense * const RCVR_NEGEDGE  = ((receiver *)-1);
sense * const RCVR_POSEDGE  = ((receiver *)1);
sense * const RCVR_ARRAY    = ((receiver *)2);
sense * const RCVR_ARRAY_SZ = ((receiver *)3);

typedef struct wave_element_s : public Event {
 public:
  struct wave_element_s *next_we;
  char                   value[];
} wave_element;

class any_driver;
class void_driver {
 public:
  virtual const  type_info &typeInfo() {return typeid(void);}
  virtual void copy(const void *src,void *dst,size_t sz) {
                                         memcpy(dst,src,sz);}
 private:
  wave_element *waveform;  
  int           size;
  thread_id     thrd;
 public:
  void_driver  *next;

  inline void_driver() : waveform(0), size(0), thrd(thread_self()), next(0) {}
  ~void_driver();

  inline any_driver   *Any()                         {return (any_driver *)this;};
  inline int           Size()                        {return size;};
  inline thread_id     Thrd()                        {return thrd;};
  inline void          setThrd(thread_id id)         {thrd = id;};
  inline void          setSize(int s)                {size = s;};
  inline wave_element *Waveform()                    {return waveform;};
  inline wave_element *SetWaveform(wave_element *we) {return waveform = we;};

  wave_element *_nba(int,RelTime t = 0);
  int           _ba(void *,int,CopyFn copy = pod::CpyFn);
};

class imp_signal;
class Slice;
class bridge;
class any_signal;
class void_signal {
  friend class void_driver;
  friend class any_driver;
  friend class any_signal;
  friend class bridge;

  char   mark;
  cpu_id cpu;
  int    i_count:8;
  int    active:2;
  int    before:1;
  int    bridged:1;

 public:

  rx_base      *receivers; // ring, bridge to master 1st, receivers fwd
  void_driver  *drivers;   // ring
  imp_signal  **implicit;

  virtual const type_info     &typeInfo()      {return typeid(void);}
  virtual       int            resolve(void *) {return -1;}
  virtual       int            dataSize()      {return  0;}
  virtual       int            Bits()          {return  1;}
  virtual       int            Sgn()           {return  0;}
  virtual       UpdateFn       UpdtFn()        {return pod::UpdtFn;}
  virtual       CompareFn      CompFn()        {return pod::CompFn;}
  virtual       UpdateFn       SlcUpdtFn()     {return pod::UpdtFn;}
  virtual       CompareFn      SlcCompFn()     {return pod::CompFn;}
  virtual       SetFlagFn      StFlgFn()       {return pod::StFlgFn;}

  virtual       void_signal   *sigIndex(int i)    = 0;
  virtual       int            Edge(const void *) = 0;

  inline void_signal() 
    : mark(0), cpu(0), i_count(0), active(0), before(0), bridged(0),
                              receivers(0), drivers(0), implicit(0)  {}

  void setFlag(eSigFlag flg,int width = 1,void *xtra = 0);


  inline bool        Bridged() { return bridged; }
  inline bool        Active()  { return active; }
  inline int         Mark()    { return mark; }
  inline cpu_id      CPU()     { return cpu; }
  inline any_signal *any()     { return (any_signal *)this; }
  inline int         size()    { return  1; }

  inline void enableBridge();
  inline void disableBridge();

  void_signal *Implicit(eImplicit);

  void         setBridged(bridge *);
  void_driver *attach(void_driver *,const type_info &,void_signal **);
  void         propagate(size_t,const void *,int edge = 0);

  void_signal *sttcSlice(Slice **,int,int,const type_info &,const type_info &);
};

#if DBGLVL > 0
const char *sigType(void_signal *sig) {
  int sts;
  return abi::__cxa_demangle(sig->typeInfo().name(), 0, 0, &sts);
}
#endif

class imp_signal : public void_signal {
 public:
   int          Edge(const void *new_val) {return  0;}
   void_signal *sigIndex(int i)           {return &this[i];}
};

class any_signal : public void_signal {
  char data[1 * sizeof(intptr_t)];
 public:
  inline void *pValue() {return data;}
};

class any_driver : public void_driver {
  any_driver();
  any_signal *sig;
  char        data[1 * sizeof(intptr_t)];
 public:
  inline void       *pValue() {return data;}
  inline any_signal *Sig()    {return sig;}
  inline any_signal *clrSig() {return sig = 0;}

  static void UpdateDriver(any_driver *);
};

inline int Edge(int new_val,int old) {if (new_val > old) return  1;
                                         if (new_val < old) return -1;
                                         return 0;}

class Slice {
 public:
  Slice       *next;
  void_signal *mstr_sig;
  void        *data;
  short int    l,
               r;
  char         sgn;

  inline void set_lr(int _l,int _r) {l = _l; r = _r;}
  inline int  L()                   {return l;}
  inline int  R()                   {return r;}

  Slice(void_signal *s,void *dp,int left,int right,int _s = 0)
   : next(0), mstr_sig(s), data(dp), l(left), r(right), sgn(_s) {}

  virtual ~Slice() {
    Slice  *s = next;
    next = 0;
    delete s;
  }
};

inline int BitsFor(char,int)
{
  return 1;
}

template<typename S,typename SLC = S,class S_FN = pod,class SLC_FN = pod>
class signal : public void_signal {
  friend class void_driver;
  S value;
 public:
  int          dataSize()         {return sizeof(value);}
  int          Bits()             {return BitsFor(value,sizeof(value));}
  void_signal *sigIndex(int i)    {return &this[i];}
  int          Edge(const void *) {return 0;}

  UpdateFn       UpdtFn()     {return S_FN::UpdtFn;}
  CompareFn      CompFn()     {return S_FN::CompFn;}
  SetFlagFn      StFlgFn()    {return S_FN::StFlgFn;}
  UpdateFn       SlcUpdtFn()  {return SLC_FN::UpdtFn;}
  CompareFn      SlcCompFn()  {return SLC_FN::CompFn;}
  SetFlagFn      SlcStFlgFn() {return SLC_FN::StFlgFn;}

  const type_info &typeInfo() {return typeid(S);}

  inline signal<SLC,SLC,SLC_FN> *sttcSlice(Slice **p_slc,int l,int r) {
    return (signal<SLC,SLC,SLC_FN> *)void_signal::sttcSlice(p_slc,l,r,
                                                        typeid(S),typeid(SLC));
  }

  inline signal()                {}
  inline signal(S &v) : value(v) {}

  inline operator const S &  ()      const {return  value;}
//inline operator       S &  ()      const {return  value;}
  inline          const S &Value()   const {return  value;}
  inline          const S *pValue()  const {return &value;}
  inline                S *pValMod()       {return &value;}
};

typedef void_signal *(*CreateSig)(void_signal *,Slice **,int,int,int,int);
typedef void_driver *(*CreateDrv)(void_driver *,void_signal **,int,int,int,int);

struct TypeData {
  const type_info  &info;
  const char       *name;
  int             (*sizes)(void_signal *,int *);
  CreateSig         crt_sig;
};
#define TYPE_DATA_P(t) TypeData t::TD = {typeid(t*),0,t::sizes,t::crt_sig};

struct TypeDataSlc {
  TypeDataSlc *next;
  const char  *name;
  CreateSig    crt_sig;
  CreateDrv    crt_drv;

  static TypeDataSlc *list;
  void reg();

  static TypeDataSlc  KnownSlc[];
};

class TypeDataSlcArray {

  int count;

  static TypeDataSlcArray KnownSlcCount;

  TypeDataSlcArray();
};

class logic_word {
 public:
  unsigned long value, // keep order
                known,
                driven;
  inline logic_word(unsigned long v=0,unsigned long k=0,unsigned long d=~0) 
                          : value(v), known(k), driven(d) {}

  enum { BITS_PW = (sizeof(long) * 8) };

  inline int Bits(int sz = sizeof(logic_word)) {
                                  return BITS_PW * sz/sizeof(logic_word);}

  inline static int words_for(int bits) {return (bits-1)+BITS_PW/BITS_PW;}

};

typedef enum {
  LGC_STRN_PWR  = 0x3F,
  LGC_STRN_NORM = 0x20
} eLGC_STRN;

class logic {
 public:
  unsigned char value:1,
                known:1,
                strength:6;

  inline logic() {known = 0; strength = LGC_STRN_NORM;};

  inline int diff(const logic *l) {
    const char *b1 = (const char *)this,
               *b2 = (const char *)l;
    return *b1 ^ *b2;
  }

  inline operator bool () {return (value && known);}

  inline int Bits(int sz = sizeof(logic)) {
                                  return sz/sizeof(logic);}

  static int          sizes(void_signal *sig = 0,int *bits = 0);
  static void_signal *crt_sig(void_signal *,Slice **,int,int,int,int);
  static TypeData     TD;
};

template<int width,int sign = 1>
class logic_vec {
 public:
  logic_word word[1 + (width-1)/logic_word::BITS_PW];

  inline int Signed() {return sign;}
  inline int Width()  {return width;}
  inline int Words()  {return 1 + (width-1)/logic_word::BITS_PW;}

  inline int Bits(int sz) {return width;}


  inline logic_vec()  { }
};

template<int width,int sign>
inline int BitsFor(logic_vec<width,sign> &v,int sz) {return v.Bits(sz);}

typedef logic_vec<logic_word::BITS_PW,1> logic_vec_1w;

#define LOGIC_VEC(...)      logic_vec<__VA_ARGS__>,logic_vec_slice,\
                            logic_vec_fn_sig,logic_vec_fn_slice
#define LOGIC_VEC_SLICE     logic_vec_slice,logic_vec_fn_slice
#define LOGIC_VEC_SLICE_SIG logic_vec_slice,logic_vec_slice,logic_vec_fn_slice

class logic_vec_slice : public Slice {
 public:

  inline logic_word *word() const {return (logic_word *)data;}

  inline int Bits(int sz) {return 1 + (r-l);}
 
  logic_vec_slice(void_signal *sig = 0,void *dp = 0,
                  int left = -1,int right = -2,int sgn = 0) 
    : Slice(sig,dp,left,right,sgn) {}

  void attach(void_driver *,const type_info &,void_signal **);

  static int          sizes(void_signal *sig = 0,int *bits = 0);
  static void_signal *crt_sig(void_signal *,Slice **,int,int,int,int);
  static void_driver *crt_drv(void_driver *,void_signal **,int,int,int,int);
  static TypeData     TD;

  static int      copy(const logic_vec_slice *,logic_vec_slice *,size_t);
  static rx_base *update(void_signal *,logic_vec_slice *,size_t);
  static int      compare(const logic_vec_slice *,logic_vec_slice *,size_t);

  static rx_base *copy2sig(void_signal *,const logic_vec_slice *,size_t);
  static int      comp2sig(const logic_vec_slice *,logic_vec_1w *,size_t);
};

inline int BitsFor(logic_vec_slice &v,int sz) {return v.Bits(sz);}

class logic_vec_slice_1 : public logic_vec_slice {
 public:
  logic_word w0[1];

  logic_vec_slice_1(int i = 0) : logic_vec_slice(0,w0,0,logic_word::BITS_PW-1)
                                 {*w0 = i;}
};

inline logic_vec_slice *val_logic_vec_slice(int i, logic_vec_slice_1 *tmp = 0)
{
  static logic_vec_slice_1 data;

  if (!tmp) {
    tmp = &data;
  }

  tmp->w0->value =  i;
  tmp->w0->known = ~0;

  return tmp;
}

class bridge {
  friend class void_signal;
  class element : public rx_base {
    friend class void_signal;
    friend class bridge;

    char          other;
    char          active;
    char          master;
    char          parent;
    BridgePropFn  propFn;
    void_signal  *sig;

    element(void_signal *s,BridgePropFn pf,int e,int m,int p,bridge *brdg)
      : rx_base(SNS_BRIDGE), other(e), active(0), master(m), parent(p),
        propFn(pf), sig(s) {
      s->setFlag(SGF_BRDGD,brdg->width,brdg);
    }

    void propagate(int);

    inline bridge *getBridge() {
      return (bridge *)&this[other > 0 ? 0 : -1];
    }
  } a,b;
  int width;
 public:
  bridge(void_signal *sa,BridgePropFn pfa,
         void_signal *sb,BridgePropFn pfb,int w = 1,int m = 0,int p = 0)
    : width(w), a(sa,pfa,1,m,p,this), b(sb,pfb,-1,-m,-p,this) {}

};

template<typename S,class FN = pod> class driver;

template<typename S,class FN> void nba(driver<S,FN> *,S *data,RelTime t);
template<typename S,class FN> int  ba (driver<S,FN> *,S *data);

template<typename S,class FN>
class driver : public void_driver {
  signal<S,S,FN> *sig;
  S               value;
 public:

  inline void       set_sig(signal<S,S,FN> *s) { sig = s; }
  inline signal<S> *pSig()                     { return sig; }
  inline S         *pValue()                   { return &value; }

  virtual const type_info &typeInfo() {return typeid(S);}

  virtual void copy(const void *src,void *dst,size_t sz) {
                                (*FN::CpyFn)(src,dst,sz);}

  int size() {return sizeof(S);};

  inline driver *init(signal<S,S,FN> *s) {
    setSize(sizeof(S));
    sig = 0;
    if (s) s->attach(this,typeid(S),(void_signal **)&sig);
    return this;
  }

  inline driver *init(signal<S,S,FN> &s) { init(&s); }

  inline driver()                  { init(0);  }
  inline driver(signal<S,S,FN> &s) { init(&s); }

  inline driver *prc_init(signal<S,S,FN> &s,size_t sz=0) { driver *pd = new driver[1];
                                                           return  pd->init(&s); }
  inline driver *prc_init(signal<S,S,FN> *s,size_t sz=0) { driver *pd = new driver[1];
                                                           return  pd->init(s); }

  inline void nba(S *val,RelTime t = 0) {parc::nba<S,FN>(this, val,t);};
  inline void nba(S  val,RelTime t = 0) {parc::nba<S,FN>(this,&val,t);};

  inline int  ba (S *val)        {return parc::ba<S,FN>(this, val);};
  inline int  ba (S  val)        {return parc::ba<S,FN>(this,&val);};

  inline int        Sgn()    {return sig->Sgn();}
  inline S          Value()  {return value;};
  inline driver<S> *Next()   {return (driver<S> *)next;};
};

#define POD_DRIVER(S)\
        template<>\
	inline void nba<S>(driver<S> *drv,S *data,RelTime t) {\
          wave_element *wes = drv->_nba(sizeof(S),t);\
          bcopy(data,wes->value,sizeof(S));\
        } \
        template<>\
        inline int  ba<S>(driver<S> *drv,S *data) {\
          return drv->_ba(data,sizeof(S)); \
        }

POD_DRIVER(bool)
POD_DRIVER(char)
POD_DRIVER(int)
POD_DRIVER(logic)

inline char val_char(int i)
{
  return i;
}

template<typename S>
class l_signal : public signal<S> {
 public:
  int Edge(const void *);
};

template<typename S>
class rl_signal : public l_signal<S> {
 public:
  int resolve(void *);

  inline driver<S> *Drivers() {return (driver<S> *)this->drivers;};
};

template<typename S>
class r_signal : public signal<S> {
 public:
  int resolve(void *);

  inline driver<S> *Drivers() {return (driver<S> *)this->drivers;};

  r_signal *sttcSlice(Slice **,int,int);
};

void init_drv(driver<LOGIC_VEC_SLICE> &,signal<LOGIC_VEC_SLICE_SIG> *);

inline int logic_edge(const logic &new_val,const logic &old)
{
  if (new_val.value == 1 && new_val.known) return  1;
  if (new_val.value == 0 && new_val.known) return -1;
  return 0;
}

template<typename S>
class c_signal : public signal<S> {
 public:
  int Edge(const void *);

  inline driver<S> *Drivers() {return (driver<S> *)this->drivers;};
};

inline logic val_logic(bool v,bool k = 1,eLGC_STRN s = LGC_STRN_NORM)
{
  logic l; 
  l.value    = v;
  l.known    = 1;
  l.strength = s;
  return l;
}

inline int       value   (const logic &l) { return l.value; }
inline int       known   (const logic &l) { return l.known; }
inline eLGC_STRN strength(const logic &l) { return (eLGC_STRN)l.strength; }

template<typename S>
class logic4 {
 public:
  S value,
    known,
    driven;

  inline logic4() {known = 0; value = 0; driven = ~0;};
};

class Converter {

  static Converter *list;

  Converter       *next;
  int              f_master;

 public:
  const TypeData  &from,
                  &to;
  BridgePropFn     bridge_fn;

  inline int fMaster() {return f_master;}

  Converter(const TypeData &,int,const TypeData &,BridgePropFn);

  static void       reg (TypeData &,int,TypeData &,BridgePropFn);

  static Converter *find(const type_info &,const type_info &);
};

class cnvSlcVc2Lgc {
 public:
  static void brdg_prp(signal<logic_vec_slice> *,signal<logic> *,int);
 private:
  cnvSlcVc2Lgc() {Converter::reg(logic_vec_slice::TD,0,logic::TD,
                                 (BridgePropFn)brdg_prp);}
  static cnvSlcVc2Lgc reg;
};

class cnvLgc2SlcVc {
 public:
  static void brdg_prp(signal<logic> *,signal<logic_vec_slice> *,int);
 private:
  cnvLgc2SlcVc() {Converter::reg(logic::TD,0,logic_vec_slice::TD,
                                 (BridgePropFn)brdg_prp);}
  static cnvLgc2SlcVc reg;
};

int mt_printf(const char *format,...);

}

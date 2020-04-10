/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  simulation_h_rcsid
#define simulation_h_rcsid() {return "$Id: simulation.h,v 1.24 2020/04/08 01:00:35 dkc Exp $";} /* RCS ID */

#include <assert.h>
#include <inttypes.h>
#include "veri_enum.h"

#define _MODULE_NAME(MODULE,PRM_SET,VERSION)\
                     MODULE ## __ ## PRM_SET ## __ ## VERSION

#define C_ENTRY_STATIC       static
#define C_ENTRY_POINT_FWD(p) extern "C" { C_ENTRY_STATIC void p 
#define C_ENTRY_POINT_DWF    }
#define C_ENTRY_POINT(p)     C_ENTRY_STATIC void p

#define _SIM_TIME_U_   1e-12
#define _SCALE_TIME(t) ((_time_u_/_SIM_TIME_U_)*(t))

class _SimTime {
 public:
  double t;

  operator uint64_t();

  inline operator double()  {return t;};

  inline uint64_t           val() {return t;}
  inline uint64_t           operator =(uint64_t u) {
                                          t = u; return u;};
  inline double             operator =(double d) {
                                          t = d; return d;};
  inline _SimTime()         {};
  inline _SimTime(double d) {t = d;};
};

template<typename I>
class Logic {
 public:
  I value,
    strength,
    certainty;
};

struct _simple_range {
  int left,right;
};

class _PortFlags {
 public:
  char flags,
       edge;
  int  index;
#ifdef PORT_FLAGS_EXTRA
  PORT_FLAGS_EXTRA
#endif

  inline _PortFlags *PF() {return this;};
};

typedef void (*PortAssFn)(_PortFlags *,void *);

class _Context;

template<typename I>
class _Logic {
  I val,
    Z,
    X;
};

template<typename V,int B,int W,int D>
class _Port;

class _VarBase {
};

template<typename V,int B,int W>
class _Var : public _VarBase {
 public:
  V value[W];

  inline int bits()      {return B;} 
  inline int words()     {return W;} 

  inline operator V()    {assert(1 == W);
                          return value[0];};

  inline const V & operator [](int i) const {return value[i];};

  inline void operator = (V     v) { assert(1 == W);
                                     value[0] = v; };
  inline void operator = (_Var &v) { int w = W;
                                     while (w--) {value[w] = v[w];} };

  inline void operator = (_Port<V,B,W,1> &);
};

class _PortRefBase;
class _RefBase {
 public:
  void       *vp;

  inline _PortRefBase *pb() {return (_PortRefBase *)&vp;};
};

class _PortRefBase : public _RefBase {
 public:
  _PortFlags  pf;

  void        SigInit(int,int,int,int);
};

template<typename V,int B,int W>
class _VarRef {
 public:
  _Var<V,B,W> *vp;

  inline _VarRef(_Var<V,B,W> *_vp) : vp(_vp) {}

  inline _Var<V,B,W> Value() { return *vp; };

  inline _RefBase *rb()  {return (_RefBase *)&vp;};
};

template<typename V,int B,int W>
class _PortRef : public _VarRef<V,B,W>, public _PortFlags {
 public:
  inline _PortRef(_Var<V,B,W> *_vp) : _VarRef<V,B,W>(_vp) {}
  inline _PortRefBase *prb() {return (_PortRefBase *)_VarRef<V,B,W>::rb();};
};

template<typename V,int B,int W,int D>
class _Port : public _PortRef<V,B,W> {
 public:
  _Var<V,B,W> value[D];

  inline _Port() : _PortRef<V,B,W>(&value[0]) {}

  inline void init(_Context *cntxt,int idx,int flags,
                   int rngs = 0,const _simple_range *sr = 0)
  {
    assert((_VarRef<V,B,W>::vp) == value);
    this->index = -1;
    _PortRef<V,B,W>::prb()->SigInit(idx,flags,D,W * sizeof(V));
  };

  inline _Var<V,B,W>  Value() {return _VarRef<V,B,W>::Value();}
  inline _Var<V,B,W> *VP()    {return _VarRef<V,B,W>::vp;}

  void assign(const _Var<V,B,W> &i)
#ifdef PORT_ASSIGN__VAR
PORT_ASSIGN__VAR
#endif
;
  void assign(const V i)
#ifdef PORT_ASSIGN_V
PORT_ASSIGN_V
#endif
;

  inline void operator = (const _Var<V,B,W> v) {assign(v);};
  inline void operator = (const V v)           {assign(v);};
  inline void operator = (_Port &p)            {assign(*p.vp);};

  static void assign_cbk(_PortFlags *pf,void *val) {_Port *prt=(_Port *)pf;
                                                    prt->assign(*(const V *)val);}

  inline int  data_size() {return W * sizeof(V);}
  inline int  drivers()   {return D;}
  inline int  width()     {return W;};

  inline operator _Var<V,8*sizeof(V),W>() {return this->Value();};
};

inline int _Bool(int b) {return b;}

template<typename V,int B,int W>
inline void _Var<V,B,W>::operator = (_Port<V,B,W,1> &prt)
{
  int w = W;

  while (w--) value[w] = prt.vp[w];
}

template<typename V,int B,int W>
class _SimTmp {
 public:
  _Var<V,B,W> value;

  _SimTmp *allocate(unsigned int sz = W * sizeof(V));

  ~_SimTmp();

  inline void set (const _Var<V,B,W> &i) {value = i;};
  inline void set (V i)                  {value = i;};
};

typedef _Port<char,1,1,1>           _RegBit;
typedef _Port<unsigned char,1,1,1>  _RegUBit;
typedef _Port<int,32,1,1>           _RegInt;
typedef _Port<unsigned int,32,1,1>  _RegUInt;
typedef _Port<char,8,1,1>           _RegChar;
typedef _Port<unsigned char,8,1,1>  _RegUChar;
typedef _Var<char,8,1>              _VarChar;
typedef _Var<unsigned char,8,1>     _VarUChar;
typedef _Var<int,32,1>              _VarInt;
typedef _Var<unsigned int,32,1>     _VarUInt;
typedef _SimTmp<char,8,1>           _TmpChar;
typedef _SimTmp<int,32,1>           _TmpInt;
typedef _SimTmp<unsigned char,8,1>  _TmpUChar;
typedef _SimTmp<unsigned int,32,1>  _TmpUInt;

inline _Var<unsigned int, 32,1>     *Eval_ASSIGN(_Var<unsigned int,32,1>     *lhs,_Var<unsigned int, 32,1> &rhs)  {*lhs = rhs; return lhs;}
inline _Var<unsigned int,  1,1>     *Eval_ASSIGN(_Var<unsigned int, 1,1>     *lhs,_Var<unsigned int, 32,1> &rhs)  {*lhs = rhs; return lhs;}
inline _Var<unsigned int,  1,1>     *Eval_ASSIGN(_Var<unsigned int, 1,1>     *lhs,_Var<unsigned int,  1,1> &rhs)  {*lhs = rhs; return lhs;}
inline _Var<unsigned char, 1,1>     *Eval_ASSIGN(_Var<unsigned char,1,1>     *lhs,_Var<unsigned int,  1,1> &rhs)  {*lhs = rhs; return lhs;}
inline _Var<unsigned int,  1,1>     *Eval_ASSIGN(_Var<unsigned int ,1,1>     *lhs,_Var<unsigned char, 1,1> &rhs)  {*lhs = rhs; return lhs;}
inline _Var<unsigned int, 32,1>     *Eval_ASSIGN(_Var<unsigned int,32,1>     *lhs,     unsigned int         rhs)  {*lhs = rhs; return lhs;}
inline _Var<unsigned int,  1,1>     *Eval_ASSIGN(_Var<unsigned int ,1,1>     *lhs,     unsigned int         rhs)  {*lhs = rhs; return lhs;}
inline _Var<unsigned char, 1,1>     *Eval_ASSIGN(_Var<unsigned char,1,1>     *lhs,     unsigned int         rhs)  {*lhs = rhs; return lhs;}

inline _Port<unsigned char,  1,1,1> *Eval_ASSIGN(_Port<unsigned char, 1,1,1> *lhs,     unsigned int         rhs)  {*lhs = rhs; return lhs;}
inline _Port<unsigned char, 32,1,1> *Eval_ASSIGN(_Port<unsigned char,32,1,1> *lhs,_Var<unsigned int, 32,1> &rhs)  {*lhs = rhs; return lhs;}
inline _Port<unsigned  int, 32,1,1> *Eval_ASSIGN(_Port<unsigned int, 32,1,1> *lhs,_Var<unsigned int, 32,1> &rhs)  {*lhs = rhs; return lhs;}
inline _Port<unsigned  int, 32,1,1> *Eval_ASSIGN(_Port<unsigned int, 32,1,1> *lhs,     unsigned int         rhs)  {*lhs = rhs; return lhs;}

class _Inst;

typedef enum {
  SCT_MOD  = 0,
  SCT_FUNC = 1,
  SCT_TASK = 2
} eSCT;

class _Sense;

typedef void (*C_entry)(void *);

typedef enum {
  SNS_INVALID = -1,
  SNS_CNTXT,
  SNS_ARRAY,
#ifdef SNS_EXTRA
  SNS_EXTRA
#endif
  SNS_LAST
} eSNS;

class _Event {
 public:
  union { struct {eSNS      sns:8;  
                  eSCT      typ:8;
                  char      waiting;
#ifdef CONTEXT_EXTRA
                  CONTEXT_EXTRA
#endif
                  } c;
        }   data;
  _Event   *next;
  C_entry   call;
  _SimTime  time;
};

class _Context : public _Event {
 public:
  _Sense   *sense;
  int       idx;
  int       lbl;
  int       dpth:16,
            dmax:16,
           *stck;

  inline _Context() {data.c.sns = SNS_CNTXT;
                     sense      = 0;};

  int Pop();
  int Push(int);
};

class _SimObj {
 public:
  void StartProc(_Context *,C_entry);

  bool At   (_Context *,int,C_entry,   int);
  bool Wait (_Context *,int,C_entry,   int);
  bool At   (_Context *,int,_Context *,int);
  bool Wait (_Context *,int,_Context *,int);

  int  Delay(_Context *,int,C_entry,   _SimTime);
  int  Delay(_Context *,int,_Context *,_SimTime);

  int  Suspend(_Context *,int,C_entry);
  bool Posedge(_PortFlags *);                   inline bool Posedge(_PortFlags f) {return Posedge(&f);} 
  bool Negedge(_PortFlags *);                   inline bool Negedge(_PortFlags f) {return Negedge(&f);} 
  bool Anyedge(_PortFlags *);                   inline bool Anyedge(_PortFlags f) {return Anyedge(&f);} 
  void PortUsage(_Sense **,_PortFlags *,int);
  void PortUsageAuto(_Sense **,_RefBase *,int);
  void Sensitize(_Context *,_Sense *);
  void Desensitize(_Context *); 
  void PushPliArgs(int,void **);
  void PopPliArgs();

  void NBA(_PortFlags *,int,void *,PortAssFn,_SimTime,int indices = 0,...);

  inline void PortUsage(_Sense **p_sens,_VarBase *vp,int usg) {};
};

typedef _SimObj *(*CodeInst)(void *);

typedef void        (*_v2k_sys_call)(int,void **);
typedef _SimTime    (*_v2k_sys_time)(int,void **);
typedef const char *(*_v2k_sys_str) (int,void **);

extern _v2k_sys_call v2k_sys_display;
extern _v2k_sys_call v2k_sys_write;
extern _v2k_sys_call v2k_sys_finish;
extern _v2k_sys_time v2k_sys_time;
extern _v2k_sys_str  v2k_sys_typeof;

#define SYS_CALL(s,i,a) (*v2k_sys_##s)(i,a)

typedef enum {
  PLI_ARG_NULL       = 0,
  PLI_ARG_SIGN        = 1,
  PLI_ARG_U_VAR       = 2,
  PLI_ARG_U_S_VAR     = PLI_ARG_U_VAR|PLI_ARG_SIGN,
  PLI_ARG_U_VAR_REF   = 4,
  PLI_ARG_U_S_VAR_REF = PLI_ARG_U_VAR_REF|PLI_ARG_SIGN,
  PLI_ARG_U_TIME      = 6,
  PLI_ARG_U_PORT      = 8,
  PLI_ARG_U_S_PORT    = PLI_ARG_U_PORT|PLI_ARG_SIGN,
  PLI_CHAR_PTR,
  PLI_INT_PTR,
  PLI_TIME_PTR
} ePLI;

class _PLI_argBase {
 public:
  ePLI  typ;
};

typedef const char *_c_string;

extern const char _NullArg[];

class _PLI_argU : public _PLI_argBase {
 public:
  union {
    void               *ptr;
    const char         *cc_ptr;
    const int          *ci_ptr;
    struct {
      void    *ptr;
      short    t,w;
    }                   var;
    uint64_t            t;
  } val;

  inline _PLI_argU(void **ap,_c_string rhs) {
    typ         = PLI_CHAR_PTR;
    val.cc_ptr  = rhs;
    if (ap) *ap = (void*)this;
  }

  inline _PLI_argU(void **ap,int *ip) {
    typ         = PLI_INT_PTR;
    val.ci_ptr  = ip;
    if (ap) *ap = (void*)this;
  }

  inline _PLI_argU(void **ap,_SimTime ta) {
    typ         = PLI_ARG_U_TIME;
    val.t       = ta;
    if (ap) *ap = (void*)this;
  }

#define SIM_TYPE(Q,S,T,I,W) inline _PLI_argU(void **ap,_Var<Q##T,W,1> &rhs) {\
                            typ = (ePLI)(PLI_ARG_U_VAR|S);\
                            val.var.w = W;\
                            val.var.t = I;\
                            val.var.ptr = (void *)rhs.value; *ap = (void*)this;}

#include "sim_types.h"

#define SIM_TYPE(Q,S,T,I,W) inline _PLI_argU(void **ap,_VarRef<Q##T,W,1> &rhs) {\
                            typ = (ePLI)(PLI_ARG_U_VAR|S);\
                            val.var.w = W;\
                            val.var.t = I;\
                            val.var.ptr = (void *)rhs.vp; *ap = (void*)this;}

#include "sim_types.h"

#define SIM_TYPE(Q,S,T,I,W) inline _PLI_argU(void **ap,_Port<Q##T,W,1,1> *rhs) {\
                            typ = (ePLI)(PLI_ARG_U_PORT|S);\
                            val.var.w = W;\
                            val.var.t = I;\
                            val.var.ptr = (void *)rhs->vp; *ap = (void*)this;}

#include "sim_types.h"
};

struct _param_range {
  
};

class _parameter {
 public:
  int value;
  _parameter(int,_param_range *,int);

  inline operator int() const {return value;};
};

inline _Var<unsigned int, 32,1> *Eval_ASSIGN(_Var<unsigned int, 32,1> *lhs,const _parameter &rhs) { *lhs = rhs;  return lhs;}

#ifndef PARAM_TYPE
#define PARAM_TYPE(t) VT_##t
#endif

extern "C" void setSimIp(void *);

#define _RESUME(SO,L) setSimIp(SO->ip); goto *_resume[L]

#include "sim_arith.h"

inline int Eval_LE      (int l,int r) {return l <= r;}
inline int Eval_PLUS    (int l,int r) {return l + r;}
inline int Eval_MULTIPLY(int l,int r) {return l * r;}
inline int Eval_UMINUS  (int oprnd)   {return -oprnd;}

#define _Trigger(e) e.assign(~e);

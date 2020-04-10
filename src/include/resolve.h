/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  resolve_h_rcsid
#define resolve_h_rcsid() {return "$Id: resolve.h,v 1.11 2007/03/28 07:23:27 dkc Exp $";} /* RCS ID */

 
typedef enum {
  VPD_VERPORT    = 1,
  VPD_VSTRENGTH  = 2,
  VPD_VERPROC_PS = 4,
  VPD_VERPROC_DF = 8,
  VPD_ANALOG     = 2, // shift
  VPD_VERDISC_PS = VPD_VERPROC_PS << VPD_ANALOG,
  VPD_VERDISC_DF = VPD_VERPROC_DF << VPD_ANALOG,
} eVPD;

#define VPD(d) ((eVPD)(d))

typedef enum {
  GTD_PLAIN      = 0,
  GTD_BIDIR      = 1
} eGTD;

typedef union {
  eVPD ver;
  eGTD gate;
  int  alien;
} DrvInfo;

typedef enum {
  DRF_DRVN =  1,
  DRF_RCVD =  2,
  DRF_OVRD =  4,
  DRF_STR  =  8,
  DRF_UTYP = 16
} eDRF;

class Inst;
class Disc;
class SigInfo {
public:
  char         drv_info;
  unsigned int nxt:3,
               xtr:3,
               ip:3,
	       pid:3,
               drv:3,
               rcv:3,
               si:3;
/*
  void        *data[];
*/
  inline DrvInfo DI() {DrvInfo di; di.alien = drv_info; return di;};

  inline int Items() { int i = 0; if (nxt) i++;
                                  if (xtr) i++;
                                  if (ip)  i++;
                                  if (pid) i++;
                                  if (drv) i++;
                                  if (rcv) i++;
                                  if (si)  i++; return i; };

  inline SigInfo **pNext() {return nxt ? (SigInfo **)(&((void **)this)[nxt])
                                       : 0;};

  inline void      set(int i,void *p) {((void **)this)[i] = p;};
  inline void      set(int i,int d)   {((void **)this)[i] = (void *)d;};
  inline void     *data(int i)        {return ((void **)this)[i];};

  inline SigInfo  *Next()  {return nxt ? (SigInfo  *)( data(nxt)) : 0;};
  inline Inst     *Ip()    {return ip  ? (Inst     *)( data( ip)) : 0;};
  inline void     *Xtr()   {return xtr ? data(xtr) : SI_NO_XTRA;};
  inline intptr_t  Dsc()   {return xtr ? (intptr_t)(data(xtr)) : -1;};
  inline intptr_t  Pid()   {return pid ? (intptr_t)(data(pid)) : -1;};
  inline intptr_t  Drv()   {return drv ? (intptr_t)(data(drv)) :  0;};
  inline intptr_t  Rcv()   {return rcv ? (intptr_t)(data(rcv)) :  0;};
  inline intptr_t  Si()    {return si  ? (intptr_t)(data(si))  : -1;};

  inline intptr_t  Drvn()  {return drv ? (intptr_t)(data(drv))
                                       : 0 != (drv_info & DRF_DRVN);};
  inline intptr_t  Rcvd()  {return rcv ? (intptr_t)(data(rcv))
                                       : 0 != (drv_info & DRF_RCVD);};

  inline void Destroy() {
    Free(this);
  }
  inline SigInfo *Create(int n) {
    SigInfo *ps  = (SigInfo *)MALLOC2_N(1+n,void *);
    *(void **)ps = 0;
    return ps;
  }
  inline SigInfo *Create(int n,SigInfo *old) {
    SigInfo *ps  = (SigInfo *)MALLOC2_N(1+n,void *);
    BCOPY(old,ps,(old->Items() + 1) * sizeof(void *));
    return ps;
  }
};

#define NO_EXTRA    ((void *)-1)
#define SI_REGISTER ((void *)-3)

class SigInfoCllctr {
public:
  virtual void      setSigInfo(void *,int,int,DrvInfo,int,int,int,void *) = 0;
  virtual SigInfo **getSigInfo(void *,int,int)                            = 0;
  virtual SigInfo  *getSigInfo(void *,int)                                = 0;

  virtual ~SigInfoCllctr() {};
};

extern SigInfoCllctr *SIcllctr;

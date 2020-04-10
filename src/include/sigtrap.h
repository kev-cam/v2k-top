/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  sigtrap_h_rcsid
#define sigtrap_h_rcsid() {return "$Id: sigtrap.h,v 1.10 2007/03/28 07:23:26 dkc Exp $";} /* RCS ID */

 
#ifndef SIGTRAP_H
#define SIGTRAP_H

typedef enum {
  SGD_CONT  = 0,
  SGD_DEF,
  SGD_EXIT,
  SGD_CORE,
  SGD_ABORT
} SigDo;

typedef enum {
  SA_NONE  = 0,
  SA_ONCE  = 1,
  SA_RELAY = 2
} SigAct;

typedef U64 SigMap;

#define SM_INT ((SigMap)(1 << (SIGINT  -1)))
#define SM_HUP ((SigMap)(1 << (SIGHUP  -1)))
#define SM_CLD ((SigMap)(1 << (SIGCLD  -1)))
#define SM_STP ((SigMap)(1 << (SIGSTOP -1)))
#define SM_CNT ((SigMap)(1 << (SIGCONT -1)))
#define SM_ABT ((SigMap)(1 << (SIGABRT -1)))

typedef SigDo (*SigHndlr)(SigMap,void *);

typedef struct {
  char              caught,
                    raised;
  int               traps;
  struct sigaction  oact;
  siginfo_t        *info;
  void             *data;

} SigState;

typedef 
#ifdef JMP_BUFF_TYPE
        JMP_BUFF_TYPE
#else
        int
#endif
        SysJmpBuf;
           
#if defined(__CPLUSPLUS__) || defined(__cplusplus)
extern "C" {
#endif

void LongJmp(SysJmpBuf *,int);

int SetJmp(SysJmpBuf *);

#if defined(__CPLUSPLUS__) || defined(__cplusplus)
}
#endif

#ifdef __cplusplus

class JumpBuff {
 public:
  SysJmpBuf *jump_buff;
  int        jump_val;

  inline JumpBuff(SysJmpBuf *jb=0,int vl=0) {jump_buff = jb;
                                             jump_val  = vl;}
};

class SigTrap {

  static int      Trapped;
#define SIG_MAX 64
  static SigState Active[SIG_MAX+1];

  SigTrap  *next;
  SigAct    mode;
  SigHndlr  hndlr;
  void     *data;
  SigMap    map;
  void     *owner;

  void Handle(SigMap);

public:

  static SigTrap *Traps;

  SigTrap(SigMap,SigHndlr,SigAct,void *,void *);
  ~SigTrap();

  void UnTrap(void *);

  bool Raise(unsigned int,siginfo_t *info, void *data);
  void Catch(unsigned int,siginfo_t *info, void *data);

  static void Reset(unsigned int);

  inline bool Raise(int s) {return Raise(s,0,0);};
  inline bool Safe()       {return !Active[Trapped].caught;};
};

#endif

#endif /* SIGTRAP_H */


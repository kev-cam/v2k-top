/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  dump_h_rcsid
#define dump_h_rcsid() {return "$Id: dump.h,v 1.33 2012/10/16 22:30:00 cvs Exp $";} /* RCS ID */

 
#ifndef DUMP_FLAGS
#define DUMP_FLAGS

typedef enum {
  DMP_ALL          =   -1,
  DMP_NONE         =    0,
  DMP_NATURE       =    1,
  DMP_DISCIPLINE   =    2,
  DMP_MODULE       =    4,
  DMP_PRIMITIVE    =    8,
  DMP_VERBOSE      =   16,
  DMP_OBFISCATE    =   32,
  DMP_SDF          =   64,
  DMP_NO_DIGITAL   =  128,
  DMP_NO_ANALOG    =  256,
  DMP_USAGE        =  512,
  DMP_LN_DRCTV     = 1024,
  DMP_AG_UNROLL    = 2048,
  DMP_PRETTY       = 4096,
  DMP_DEBUG        = 8192

} DmpFlgs;

typedef enum {
  DM_EXTERNAL,
  DM_PORT,
  DM_BIND,
  DM_INPUT,
  DM_OUTPUT,
  DM_INOUT,
  DM_DISC,
  DM_INTEGER,
  DM_REAL,
  DM_REG,
  DM_HIER,
  DM_TIME,
  DM_EVENT,
  DM_GENVAR,
  DM_WIRE,
  DM_UNKNOWN
} DpMode;

#define DMP_FLGS(f) ((DmpFlgs)(f))

#endif /* DUMP_FLAGS */

#if defined(VERILOG_OBJ) || defined(PARC_OBJ)
# ifndef DUMPER_CLASS
# define DUMPER_CLASS

typedef enum {
  DMPR_NONE    = 0,
  DMPR_VERILOG = 1,
  DMPR_CPP     = 2
} eDMPR;

typedef enum {
  DMODE_BEHAVIORAL = 1,
  DMODE_STRUCTURAL = 2,
  DMODE_DECLARE    = 4,
  DMODE_DEFINE     = 8
} eDMODE;

typedef enum {
  AM_UNTYPED =  0,
  AM_REAL_OK =  1,
  AM_TIME_OK =  2,
  AM_INT_OK  =  4,
  AM_TYPE_OK = -1
} eAttrMode;

#define DMODE(m) ((eDMODE)(m))

#  ifdef VERILOG_OBJ

typedef struct genVal_s {
  poolRef          nm;
  Expr             genvar;
  struct genVal_s *next;
} genVal;

#  else

class genVal;

#  endif

class dump_ctrl {
  friend class dump_ver;
public:

  class dump_ctrl *next;

private:
  eDMPR    dmpr;
  DmpFlgs  flags;
  char     buff[32];
  char     cls_strm;
  Stream  *strm;

  friend class vdump;
  friend class cdump;

public:
  inline dump_ctrl()   {next     = 0;
                        dmpr     = DMPR_NONE;
                        flags    = DMP_NONE;
                        strm     = Stream::Stdio(STDOUT_FILENO);
                        cls_strm = 0;};
  virtual ~dump_ctrl() {if (cls_strm) delete(strm);};

  inline eDMPR   Dmpr()               {return dmpr;};
  inline void    setDmpr(eDMPR d)     {dmpr = d;};
  inline bool    obfiscate()          {return DMP_OBFISCATE&flags;};
  inline void    Close()              {if (cls_strm) {DELETE(strm);
                                                      cls_strm = 0;}
                                       strm = strm->Stream::Stdio(STDOUT_FILENO);};
  inline int     Open(const char *f,const char *m)
                                      {if (cls_strm) {DELETE(strm);
                                                      cls_strm = 0;}
                                       strm     = new Stream;
                                       cls_strm = 1;
                                       return strm->Open(f,m);};
  inline eSTS    Status()             {return strm->Status();};
  inline void    setFlags(DmpFlgs f)  {flags = f;};
  inline int     Flags()              {return flags;};
  inline Stream *Strm()               {return strm;};
  inline Stream *setStrm(Stream *s)   {return strm = s;};
  inline Stream *setStrm(int fd)      {return strm = Stream::Stdio(fd);};
  inline const char
                *strDump(poolRef ref) {return obfiscate()
                                             && ref.pool > FIRST_FREE_POOL
                 		               ? (sprintf(buff,"R%d_%d",
                                                    ref.pool,ref.index),buff)
                                               : strDeref(ref);};
};

extern class dump_ctrl *DumpList;

typedef enum {
  PRTY_NONE = 0,
  PRTY_INST
} ePRTY;

#ifdef VERILOG_OBJ

class dumper : public dump_ctrl {
 public:
  genVal  *gen_stck;

  inline dumper *Next() {return (dumper *)next;}

  inline dumper() {gen_stck = 0;}

  inline void    genStckPop()           {gen_stck = gen_stck->next;};
  inline void    genStckPush(genVal *v) {v->next  = gen_stck;
                                         gen_stck = v;};
#undef  DUMP_VMODE
#define DUMP_VMODE = 0
#include "dump.vfn"
};

class VerilogObj;
class vdump : public dumper, public VerilogObj {
public:
#undef  DUMP_VMODE
#define DUMP_VMODE
#include "dump.vfn"

  int prtty_mode,
      prtty_indnt;

  inline void init()          {prtty_mode = 0;};

  inline int Prtty(int pm)    {int was = prtty_mode;
                               if (DMP_PRETTY & flags) prtty_mode = pm;
                               return was;}

  inline vdump()                 {init(); setDmpr(DMPR_VERILOG);};
  inline vdump(dump_ctrl **list) {init(); next = *list; *list = this;};
  inline vdump(Stream *s)        {init(); setStrm(s);};

  void const PrttyPrnt(const char *str,int);
                    
  void dumpUsage(const Stmt *,String *);
};

class cdump : public dumper,public VerilogObj {
  eDMODE mode;
public:
#undef  DUMP_VMODE
#define DUMP_VMODE
#include "dump.vfn"

  inline cdump()                 {setDmpr(DMPR_CPP);};
  inline cdump(dump_ctrl **list) {next = *list; *list = this;};
  inline cdump(Stream *s)        {setStrm(s);};
};
#  endif

# endif
#endif


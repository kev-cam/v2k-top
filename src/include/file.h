/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  file_h_rcsid
#define file_h_rcsid() {return "$Id: file.h,v 1.58 2007/11/22 23:29:04 dkc Exp $";} /* RCS ID */
 
#ifndef V2K_FILE_H
#define V2K_FILE_H

#include "system.h"
#include "v2k_misc.h"
#include "error.h"
#include "env.h"
#include "strfunc.h"
#include "fenum.h"

#define FT_DECL(e,s) e,

typedef enum {

  URL_NONE = 0,
  URL_HTTP,
  URL_FTP,
  URL_DESC,
  URL_MEM

} eURL;

eURL URLproto(const char *,const char **);

#ifndef O_RAW
# define O_RAW 0
#endif

#define FM(f) ((eFM)(f))

typedef enum {
#include "ftypes.h"
  FT_Last
} eFT;

typedef struct {
  eFT         t;
  const char *str;
} sFileType;

extern const sFileType FileTypes[];

#define  FILE_FIELDS\
  String buff;

typedef struct {
  FILE_FIELDS
} sFile;

#define MAXFILENAME   (2*MAXPATHLEN)
#define CMP_INMEMSIZE 0x4000

#ifdef __cplusplus

typedef void *(*fileCB)(const char *,void *);
typedef void *(*anyCB)(void *,...);

class Filename {
  char buff[MAXFILENAME];
public:

  U64         Date() const;
  int         Size() const;
  int         Exists(int) const;
  int         Unlink() const;
  const char *changeType(eFT ft = FT_Null);
  const char *dir();
  const char *basename(char *ret = 0) const;
  int         dirMatch(const char *,fileCB,void *) const;

  inline Filename()              {*buff = '\0';};
  inline Filename(const char *s) {strcpy(buff,s);};

  inline operator bool ()        {return 0 != *buff;};
  inline operator char *()       {return buff;};
  inline operator const char *() {return buff;};

  inline Filename *operator =(const char *str) {strcpy (buff,str);
                                                return this;}
  inline Filename *operator+=(const char *str) {strcat (buff,str);
                                                return this;}
  Filename *operator-=(const char *);

  inline const char *str()    const {return buff;};
  inline int         len()    const {return strlen(buff);}
  inline int         Exists() const {return Exists(1);};
};
#else
typedef char Filename[MAXFILENAME];
#endif

#ifdef __cplusplus
extern "C" {
#endif

int fileCmp(const char *,const char *);
int wildCmp(const char *,const char *);
int isWild(const char *);
int fileGlob(const char *,char *,int *,int *);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus

class File {
  FILE_FIELDS
public:
  const char *name();

  static const char *DirSeperator;
#ifdef OS_DIR_SEP2
  static const char *DirSeperator2;
#endif
  static const char *TypeSeperator;
  static const char *Root;
  static const File *NullFile;

  inline File()              {};
  inline File(String &s)     {buff = s.str();};
  inline File(const char *s) {buff = s;};

  
#define FILE_NO_SEP   1
#define FILE_BASENAME 2
  File(const char *,const char *,eFT,int flgs = 0);

  eFM         mode(const char *) const;
  const char *strMode(int) const;
  const char *dir() const;
  const char *basename(char *ret = 0) const;
  const char *up();
  int         down(const char *,bool trim = true);
  const char *changeType(eFT ft = FT_Null);
  int         mkDir(int) const;

  inline const Filename *FileName() const {return (Filename *)buff.str();};

  inline const char   *str()    const {return this ? buff.str()
                                                   : 0;};

  inline int         len()    const {return buff.len();};
  inline U64         Date()   const {return FileName()->Date();};
  inline int         Size()   const {return FileName()->Size();};
  inline int         Exists() const {return FileName()->Exists();};
  inline int         Unlink() const {return FileName()->Unlink();};
  inline int         Expand()       {return envExpand(&buff);};
  inline const File *operator=(const char *s)  {buff  = s; return this;};
  inline const File *operator+=(const char *s) {buff += s; return this;};
  inline const File *operator-=(const char *s) {buff -= s; return this;};

  inline operator const char *()     const {return buff.str();}
  inline operator const Filename *() const {return (Filename *)buff.str();};
  inline operator       String *()         {return &buff;}

  friend class Stream;
};

typedef enum {
  STRM_IN   = 1,
  STRM_OUT  = 2,
  STRM_BOTH = STRM_IN|STRM_OUT
} STRM_DIRN;

typedef enum {
  STRM_UNSET  = 0,
  STRM_OPEN,
  STRM_CLSD,
  STRM_RAW,
  STRM_MEM,
  STRM_BUFF,
  STRM_STRING,
  STRM_PIPE,
  STRM_PIPED
} eSTRM;

class VirtualStream {
public:
#define STRM_VIRTUAL_MODE = 0
#include "stream.vfn"
};

class Compressor;
class Job;
class Stream;

class UsrStrmData {
 public:
  virtual void *UserFn1(Stream *,void *v = 0) {return v;}
  virtual void *UserFn2(Stream *,void *v = 0) {return v;}
};

extern UsrStrmData Dummy;

class Stream : public VirtualStream {

  Stream      *self;
  FILE        *fp;
  eSTRM        strm_typ:8;
  char         master;
  eFM          mode;
  eSTS         status;
  int          fd_in,
               in_flgs,
               fd_out,
               out_flgs;
  U64          position;
  String       name;
  Compressor  *cmprssr;
  Job         *job;
  UsrStrmData *usr_data;

  static Stream **InUse;
  static long     FdSize,
                 *FdSet[3];
  static int      DesLimit,
                  FdTop;

  void rgstr(int);
  int  Open(int *,eFM);

protected:
  inline void     set_posn(U64 p)  {position = p;};
  inline int      do_flush()       {return (fp) ? fflush(fp) : 0;};

public:
  static Stream *NullStream CS_INIT(0);

  int           Open(const File *,eFM,...);
  int           Open(int,int,eFM);
  int           Pipe(eFM mode = FM_NONE);
  void          Init(const Stream *,eSTRM,Stream *slf = 0);
  Stream       *Closed();
  fwrite_fn     FwriteFn();
  ADDR          Mmap(ADDR,size_t,int,int,off_t);
  int           Munmap(ADDR,size_t);

  inline void    Init()   {Init(0,STRM_UNSET);};
  inline Stream *strm()   {return this;}       
  inline eSTS    Status() {return status;}

  inline int Open(int fd,eFM um) {
     return Open(fd,Fd(),um);
  }
  inline int Open(const File *f,const char *md) {
     return Open(f,((File *)0)->mode(md));
  };
  inline int Open(const char *f,const char *md) {
     File f2(f);
     return Open(&f2,((File *)0)->mode(md));
  };
  inline int Open(const char *f,eFM md,int extra) {
     File f2(f);
     return Open(&f2,md,extra);
  };
  inline void Open(String *s) {
     fp       = (FILE *)s;
     position = 0;
     mode     = FM(FM_WRITE);
  };

  FILE *Fp(STRM_DIRN);
  int   Fd(STRM_DIRN) const;
  int   DoClose(STRM_DIRN,int (*clsr)(FILE *));
  int   Poll();
  int   Ready(STRM_DIRN);
  int   Poll(STRM_DIRN,struct timeval *);
  Job  *setJob(Job *);

  inline int         Close()                  {return Close(STRM_BOTH);};
  inline void        CloseI()                 {Close(); Init(Closed(),STRM_CLSD);};
  inline FILE       *Fp()                     {return Fp(STRM_BOTH);};
  inline Stream     *Self()             const {return self;};
  inline Stream     *setSelf(Stream *s)       {return self = s;};
  inline int         Master()           const {return master;};
  inline int         Out()              const {return fd_out;};
  inline int         In()               const {return fd_in;};
  inline int         posn()             const {return position;};
  inline eFM         Mode()             const {return mode;};
  inline const char *Name()             const {return name.str();};
  inline int         Fd()               const {return Fd(STRM_BOTH);};

  inline fd_set     *fdset(int s)            {return (fd_set *)&FdSet[s];};
  inline const char *setName(const char *nm) {name = nm; return Name();};
  inline void        setMaster()             {master = 1;};
  inline void        clrMaster()             {master = 0;};

  inline operator FILE       *()       {return Fp();};
  inline operator const char *() const {return name.str();};

  Stream(Stream *slf = 0);
  virtual ~Stream();

  static Stream *Stdio(int);

  static void flushAll();

# define SET_STREAM(cls,str) {cls v; CopyVirtual(&v,str);\
                              str->strm_typ = v.Type();}

#include "stream.vfn"

  virtual void *UserFn1(void *v = 0) {
          void *vp = usr_data ? usr_data->UserFn1(this,v) : 0;
          return vp;}
  virtual void *UserFn2(void *v = 0) {
          void *vp = usr_data ? usr_data->UserFn2(this,v) : 0;
          return vp;}

  inline UsrStrmData *UsrData()                   {return usr_data;}
  inline UsrStrmData *setUsrData(UsrStrmData *pd) {UsrStrmData *old =  usr_data;
                                                   usr_data         = pd;
                                                   return old;}
};

void Close(Stream &);

class ClsdStream:public Stream {
  friend class Stream;
 protected:
  static ClsdStream Clsd;
 public:
  ClsdStream();

#include "stream.vfn"
};

class RawStream:public Stream {
public:
  RawStream();
#include "stream.vfn"
};

class MemStream:public Stream {
  MemStream *m_nxt;
  char      *buff;
  U64        size,
             posn,
             limit;
  char       mppd,
             shrd;
#define      MMSTRM_INCR 0x10000

  static MemStream *MmStrmLst;

  friend class Stream;

public:
#include "stream.vfn"
  MemStream(const char *);
  ~MemStream();
  MemStream *find(const char *);
  inline ADDR Addr()      {return (ADDR)buff;};
  inline void setMapped() {mppd++;};
  inline void clrMapped() {assert(--mppd >= 0);};
};

class BuffStream:public Stream {
public:
#include "stream.vfn"
  BuffStream();
  ~BuffStream() { Close(STRM_BOTH); }
};

class PipedStream:public Stream {
public:
#include "stream.vfn"
  PipedStream();
  ~PipedStream() { Close(STRM_BOTH); }
};

class StringStream:public Stream {
  inline String *str() {return (String *)Fp();};
public:
  inline StringStream(String *s) {Open(s);}
#include "stream.vfn"
  ~StringStream() { Close(STRM_BOTH); }
};

#define MAP_MINSIZE 0x100000

#ifdef WINDOWS
typedef void *ADDR;
#endif

class MappedFile {
  ADDR     addr;
  int      end,
           limit,
           prot,
           flgs;
  Stream   stream;
public:
  ~MappedFile();
  MappedFile(const char *f);
  MappedFile(const char *,const char *);
  MappedFile(const char *,const char *,long);

  void Init(long);

  inline int         Prot()  {return prot;};
  inline int         Flgs()  {return flgs;};
  inline char       *base()  {return (char *)addr;};
  inline const char *Name()  {return stream.Name();};
  inline int         Size()  {return addr ? end : 0;};
  inline eSTS        Error() {return addr ? STS_NORMAL : ERROR(end);};
  inline Stream     *Strm()  {return &stream;};

  inline operator Stream *() {return &stream;};

  int  Write(void *buf,int nbyte);
  ADDR remap(long);
};

class Compressor {

  static Compressor  *List,
                    **Last;

  Compressor *next;
public:
  const char *cmp_ext,
             *act_ext,
             *cmprssr,
             *decomp;

  Compressor(const String *);
  inline ~Compressor() {delete cmp_ext;};

  inline Compressor *Next() {return next;};

  Compressor *Init();
  Compressor *Exists(Filename *);
};

#endif /* __cplusplus */

#endif /* FILE_H */

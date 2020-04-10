/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * stream_cpp_rcsid() {return "$Id: stream.cpp,v 1.55 2009/07/09 19:07:33 dkc Exp $";}

#include "assertions.h"
#include "system.h"
#include "error.h"
#include "v2k_mem.h"
#include "job.h"
#include "env.h"
#include "file.h"
#include "v2k_misc.h"

Compressor    *Compressor::List,
             **Compressor::Last;

Stream       **Stream::InUse;
int            Stream::DesLimit,
               Stream::FdTop = -1;
Stream        *Stream::NullStream S_INIT(0);
long           Stream::FdSize,
              *Stream::FdSet[3];

UsrStrmData    Dummy;

ClsdStream::ClsdStream()
{
  Init(0,STRM_CLSD);
}

int   ClsdStream::Read   (void *buf,int nbyte)           {return 0;}
int   ClsdStream::Write  (const void *buf,int nbyte)     {return 0;}
void  ClsdStream::Flush  ()                              {return;}
int   ClsdStream::Size   ()                              {return -1;}
int   ClsdStream::Seek   (U64 off,int w)                 {return -1;}
int   ClsdStream::Unlink ()                              {return -1;}
int   ClsdStream::Close  (STRM_DIRN dirn)                {return -1;}
int   ClsdStream::printf (const char *format,...)        {return -1;}
int   ClsdStream::vprintf(const char *format,va_list ap) {return -1;}
int   ClsdStream::gets   (char *buf,int n)               {return -1;}
int   ClsdStream::gets   (String *str)                   {return -1;}
eSTRM ClsdStream::Type   () const                        {return STRM_CLSD;}

int Stream::DoClose(STRM_DIRN dirn,int (*clsr)(FILE *))
{
  int fpd = -1,
      sts =  0;

  if (mode & FM_NO_CLOSE) return -1;

  if (mode & FM_PIPE) {
      pclose(fp);
      fp = 0;
  }

  if (fp) {
    fpd = fileno(fp);

    if ((fd_in  >= 0 && fd_in  == fpd && !(STRM_IN  & dirn)) ||
        (fd_out >= 0 && fd_out == fpd && !(STRM_OUT & dirn))) goto keep;

    InUse[fpd] = 0;
    (clsr)(fp);
    fp         = 0;
    if (fd_out == fpd) fd_out = -1;
    if (fd_in  == fpd) fd_in  = -1;
  }

keep:
  if (STRM_IN & dirn) {
    if (fd_in  >= 0) {
      InUse[fd_in]  = 0;
      close(fd_in);
    }
    if (fd_out == fd_in) fd_out = -1;
    fd_in  = -1;
  }

  if (STRM_OUT & dirn) {
    if (fd_out >= 0) {
      InUse[fd_out] = 0;
      close(fd_out);
    }
    if (fd_out == fd_in) fd_in = -1;
    fd_out = -1;
  }

  if (job) {
    job->Status(0);
  }

  if (fd_out >= 0 || fd_in >= 0) sts = -1;

  if (0 == sts) {

    DELETE(job);

    assert(!fp);

    if (mode & FM_CMP) {
      Filename old(name),nw(name);
      int      sz,
	same;
      old -= OS_TMP_EXT;
      if (same = (old.Size() == (sz = nw.Size()))) {
	if (sz < CMP_INMEMSIZE) {
	  MappedFile a(old),b(nw);
	  same = (0 == BCMP(a.base(),b.base(),sz));
	} else {
	  String cmd("cmp ");
	  cmd += old;
	  cmd += " ";
	  cmd += nw;
	  cmd += " > /dev/null";
	  Job diff(cmd);
	  same = (0 == diff.Status());
	}
      }
      if (same) {
	unlink(nw);
      } else {
	rename(nw,old);
      }
    }

    Init(Closed(),STRM_CLSD);

    assert(strm_typ == self->Type() && "Sanity Check");
  }

  return sts;
}

ClsdStream ClsdStream::Clsd;

Stream *Stream::Closed()
{
  return &ClsdStream::Clsd;
}

Stream::~Stream()
{
  if (master) {
    if (strm_typ == self->Type()) {
      self->Close(STRM_BOTH);
    } else switch (strm_typ) {
      default:        assert(0);
      case STRM_CLSD: break;
    }
  }
}

int   Stream::Read (void *buf,int nbyte)       {return self->Read (buf,nbyte);}
int   Stream::Write(const void *buf,int nbyte) {return self->Write(buf,nbyte);}
int   Stream::Size ()                          {return self->Size();}
void  Stream::Flush()                          {self->Flush();}
eSTRM Stream::Type () const                    {return STRM_UNSET;}
int   Stream::Seek (U64 off,int w)             {return self->Seek(off,w);}
int   Stream::Unlink()                         {int sts = self->Unlink();
                                                self    = Closed();
                                                return sts;}

int  Stream::Close(STRM_DIRN dirn)
{
  int sts = -1;

  if (master) {
    assert(master > 0 && "Unexpected recursion");
    master = -master;
    sts = self->Close(dirn);
    master = -master;
  }

  return sts;
}

void Close(Stream &s)
{
  s.Close();
}

int Stream::printf(const char *format,...)
{
  va_list pvar;
  va_start(pvar, format);
  int p = self->vprintf(format,pvar);
  va_end(pvar);
  return p;
}

int Stream::vprintf(const char *format,va_list ap)
{
  return self->vprintf(format,ap);
}

int Stream::gets(char *s, int n)
{
  return self->gets(s,n);
}
int Stream::gets(String *str)
{
  return self->gets(str);
}

Stream::Stream(Stream *slf)
{
  Init(0,STRM_UNSET,slf);
  usr_data = &Dummy;
}

void Stream::Init(const Stream *vs,eSTRM typ,Stream *slf)
{
  if (vs) CopyVirtual(vs,this);

  self     =  slf ? slf
                  : Closed();
  name     =  "";
  mode     =  FM_NONE;
  strm_typ =  typ;
  master   =  0;
  in_flgs  =  0;
  out_flgs =  0;
  fd_in    = -1;
  fd_out   = -1;
  fp       =  0;
  position =  0;
  cmprssr  =  0;
  job      =  0;
}

Job *Stream::setJob(Job *jp)
{
  DELETE(job);
  return job = jp;
}

int Stream::Fd(STRM_DIRN dirn) const
{
  if ((dirn & STRM_IN)  && fd_in  >= 0) return fd_in;
  if ((dirn & STRM_OUT) && fd_out >= 0) return fd_out;
  if (fp)                               return fileno(fp);

  return -1;
}

extern "C" size_t StrmFwrt(const void *ptr,size_t size,size_t nitems,FILE *fp)
{
  MemStream  *ms = (MemStream *)fp;
  int         i  = 0;
  const char *cp = (char *)ptr;

  while (nitems-- > 0) {
    if (size != ms->Write(cp,size)) goto quit;
    cp += size;
    i++;
  }

quit:
  return i;
}

ADDR Stream::Mmap(ADDR addr,size_t len,int prot,int flags,off_t off)
{
  ADDR mp = 0;

  if (STRM_MEM == self->Type()) {
    MemStream *ms = (MemStream *)self;
    if (mp = ms->Addr()) ms->setMapped();
  } else {
    mp = (ADDR)mmap(addr,len,prot,flags,Fd(),off);
  }

  return mp;
}

int Stream::Munmap(ADDR addr,size_t len)
{
  if (STRM_MEM == self->Type()) {
    MemStream *ms = (MemStream *)self;
    ms->clrMapped();
  }

  return munmap(addr,len);
}

fwrite_fn Stream::FwriteFn()
{
  if (STRM_MEM == self->Type()) return StrmFwrt;

  return fwrite;
}

FILE *Stream::Fp(STRM_DIRN dirn)
{
  if (fp) {
    ASSERT(dirn == STRM_BOTH || fd_out < 0 || fd_in < 0);
    return fp;
  }

  if (STRM_MEM == self->Type()) {
    return (FILE *)self;
  }

  if (fd_in  >= 0 && fd_out == fd_in)   {fp = fdopen(fd_in, "rw"); goto done;}
  if (fd_in  >= 0 && (dirn & STRM_IN))  {fp = fdopen(fd_in, "r");  goto done;}
  if (fd_out >= 0 && (dirn & STRM_OUT)) {fp = fdopen(fd_out,"w");  goto done;}

 done:
  return fp;
}

Compressor *Compressor::Init()
{
  if (!Last) {
    Last = &List;
    Stream cmp;
    if (cmp.Open("$V2K_CMPRSSRS","r")) {
      String str;
      while (cmp.gets(&str) > 0) {
        if (*str.str() != '#') new Compressor(&str);
      }
      cmp.Close();
    }
  }

  return List;
}

Compressor::Compressor(const String *ln)
{
  const char **cpp  = &cmp_ext;
  char        *cp;
  int          flds = 4;

  strcpy(cp = new char[ln->len() +1],ln->str());

  *cpp = cp;

  while (*cp) {
    if (':' == *cp || ' ' > *cp) {
      *cp++  = 0;
      if (--flds > 0) {
        *++cpp = cp;
      } else if (flds < 0) {
        goto bad;
      }
    } else {
      cp++;
    }
  }

  if (0 == flds) {
    *Last = this;
    *(Last = &this->next) = 0;
    return;
  }

bad:
  Stream::Stdio(STDERR_FILENO)->printf("Bad compressor description: %s",ln->str());
  delete cmp_ext;
  delete this;
}

Compressor *Compressor::Exists(Filename *buff)
{
  Compressor *scan = Init();
  const char *bp   = buff->str();

  for (; scan ; scan = scan->next) {
    if (*scan->act_ext) {
      int l = buff->len();
      l -= strlen(scan->act_ext);
      if (0 == strcmp(bp + l,scan->cmp_ext)) {
        assert("NIY" == 0);
      }
    } else {
      *buff += scan->cmp_ext;
      if (buff->Exists(0)) return scan;
    }
    *buff -= scan->cmp_ext;
  }

  return 0;
}

MemStream *MemStream::MmStrmLst;

MemStream *MemStream::find(const char *vnm)
{
  MemStream *ms = MmStrmLst;

  while (ms) {
    if (0 == strcmp(vnm,ms->Name())) return ms;
    ms = ms->m_nxt;
  }

  return ms;
}

eURL URLproto(const char *url,const char **urpp)
{
  const char *urp =  url;

  while (isalpha(*urp)) urp++;

  if (':' == *urp) {

    if (urpp) *urpp = &urp[1];

    switch (urp - url) {
    case 4:
      if (0 == strncmp(url,"http",4)) {
        return URL_HTTP;
      } else if (0 == strncmp(url,"desc",4)) {
        return URL_DESC;
      }
      break;
    case 3:
      if (0 == strncmp(url,"ftp",3)) {
        return URL_FTP;
      } else if (0 == strncmp(url,"mem",3)) {
        return URL_MEM;
      }
    }
  }

  return URL_NONE;
}

int Stream::Open(const File *f,eFM um,...)
{
  int       sts,
            fd = -1;
  Filename  buff(f->str());
  va_list   pvar;

  va_start(pvar,um);

  if (um & FM_NO_CLOSE) {
    um = FM(um & ~FM_NO_CLOSE);
  } else {
    CloseI();
  }

  name = buff;
  mode = um;

  envExpand(buff,sizeof(buff));

  if (mode & FM_CMP) buff += OS_TMP_EXT;

  const char *urp;

  eURL proto = URLproto(buff,&urp);

  if (proto) {
    int        lcl,
               fdv;
    MemStream *ms = 0;

    if (lcl = ('/' == *urp)) {

    }

    switch (proto) {
    case URL_MEM:  master = 0;
                   if (ms = ms->find(urp)) self = ms;
                   if (mode & FM_WRITE) {
                     if (!ms) self = ms = new MemStream(urp);
                     if (!(mode & FM_READ)) ms->size = 0;
                   }
                   if (!ms) goto nnx;
		   if (mode & FM_APPEND) {
                     ms->posn = ms->size;
                   } else {
                     ms->posn = 0;
                   }
		   SET_STREAM(Stream,this);
                   goto ok;
    case URL_DESC: if (1 == sscanf(urp,"%d.",&fdv)) {
                     fd   = fdv;
                     mode = FM(mode|FM_RAW);
                   }
                   break;
    default:       assert(("NIY",0));
    }
  }

  if ((mode & FM_WRITE) && (mode & FM_ZPIPED)) {
    for (cmprssr = cmprssr->Init(); cmprssr ; cmprssr = cmprssr->Next()) {
      if (!*cmprssr->act_ext) {
	String cmd(cmprssr->cmprssr);
	cmd += " > ";
	cmd += buff;
	cmd += cmprssr->cmp_ext;
	if (fp = popen(cmd,"w")) {
	  SET_STREAM(PipedStream,this);
	  goto rgstr;
	}
      }
    }
  }

  if (mode & FM_RAW) {
    int m  = O_RAW,
        fm = 0666;

    if ((mode & FM_WRITE) && (mode & FM_READ)) {
       m  |= O_RDWR;
    } else {
       if (mode & FM_WRITE)  m  |= O_WRONLY;
       if (mode & FM_READ)   m  |= O_RDONLY;
    }

    if (mode & FM_CREATE) m  |= O_CREAT;
    if (mode & FM_TRUNC)  m  |= O_TRUNC;
    if (mode & FM_APPEND) m  |= O_APPEND;
    if (mode & FM_EXE)    fm |= 0111;

  retry1:
    if (fd < 0 && (fd = open(buff,m,fm)) < 0){
      switch (sts = errno) {
      case ENOENT: if ((mode & FM_MKDIR) && f->mkDir(1)) goto retry1;
                   break;
      }
    } else {
      if (mode & FM_READ)  fd_in  = fd;
      if (mode & FM_WRITE) fd_out = fd;
      SET_STREAM(RawStream,this);
      goto rgstr;
    }
  } else {
    char sm[8];
    sm[0] = '\0';

    if (mode & FM_WRITE)  strcat(sm,"w");
    if (mode & FM_READ)   strcat(sm,"r");
    if (mode & FM_APPEND) strcat(sm,"a");

    if (mode & FM_PIPE) {
      if ((fp = popen(buff,sm))) {
        SET_STREAM(BuffStream,this);
        goto rgstr;
      } else {
	sts = errno;
      }
    } else {
     retry2:
      if (!(fp = fopen(buff,sm))) {
        switch (sts = errno) {
        case ENOENT: if ((mode & FM_MKDIR) && f->mkDir(1)) goto retry2;
                     break;
        }
      } else {
        SET_STREAM(BuffStream,this);
        goto rgstr;
      }
    }
  }

  if (ENOENT == sts && (mode & FM_READ)) {
    if (cmprssr = cmprssr->Exists(&buff)) {
      int fd = open(buff,O_RDONLY);
      if (fd >= 0) {
        Pipe(FM_NO_CLOSE);
	int pid = Fork(-1);
	if (pid < 0) {
	  sts = errno;
	  DoClose(STRM_BOTH,0);
          close(fd);
	} else if (pid) {
	  DoClose(STRM_OUT,0);
	  job = new Job(pid,cmprssr->decomp);
          SET_STREAM(RawStream,this);
	  goto rgstr;
	} else {
	  DoClose(STRM_IN,0);
	  dup2(Fd(),STDOUT_FILENO);
	  dup2(fd,STDIN_FILENO);
          execlp(cmprssr->decomp,cmprssr->decomp,NULL);
          _exit(errno);
	}
      }
    }
  }

nnx:
  va_end(pvar);
  return 0;

rgstr:
  master=1;
  rgstr(Fd());
ok:
  strm_typ = self->Type();
  va_end(pvar);
  return 1;
}

int Stream::Open(int *fd,eFM um)
{
  int sts = -1;

  if (um & FM_NO_CLOSE) {
    um = FM(um & ~FM_NO_CLOSE);
  } else {
    CloseI();
  }

  SET_STREAM(RawStream,this);

  strm_typ = STRM_RAW;
  mode     = FM(FM_RAW|um);

  if (mode & FM_READ) {
    if (0 > (sts = fd_in  = *fd++)) goto done;
    rgstr(fd_in);
  }
  if (mode & FM_WRITE) {
    if (0 > (sts = fd_out = *fd))   goto done;
    rgstr(fd_out);
  }

  name.printf("Descriptors");

  assert(strm_typ == self->Type() && "Sanity Check");

done:
  return sts;
}

int Stream::Open(int fd,int as,eFM um)
{
  int sts = -1;

  if (um & FM_NO_CLOSE) {
    um = FM(um & ~FM_NO_CLOSE);
  } else {
    CloseI();
  }

  SET_STREAM(RawStream,this);

  strm_typ = STRM_RAW;
  mode     = um;

  if (mode & FM_READ) {
    if (0 > (sts = (as >= 0) ? dup2(fd,as)
                             : as = dup(fd))) goto done;
    rgstr(fd_in = sts);
    if (mode & FM_WRITE) {
      fd_out = fd_in;
    }
  } else if (mode & FM_WRITE)  {
    if (0 > (sts = (as >= 0) ? dup2(fd,as)
                             : as = dup(fd))) goto done;
    rgstr(fd_out = sts);
  }

  name.printf("Descriptor:%d",fd);

done:
  return sts;
}

int Stream::Pipe(eFM um)
{
  if (um & FM_NO_CLOSE) {
    um = FM(um & ~FM_NO_CLOSE);
  } else {
    CloseI();
  }

  int fd[2],
      sts = pipe(fd);

  if (!sts) {
    master = 2;
    Open(fd,FM(FM_WRITE|FM_READ|FM_NO_SUSPEND|FM_NO_CLOSE));
    name.printf("Pipe:%d,%d",fd[0],fd[1]);
  }

  return sts;
}

void Stream::flushAll()
{
  if (InUse) {
    int i = DesLimit;
    while (i-- > 0) if (InUse[i] && InUse[i]->fp) {
       fflush(InUse[i]->fp);
    }
  }
}

struct timeval ZeroTime;

int Stream::Poll()
{
  return Poll(STRM_IN,0);
}


int Stream::Ready(STRM_DIRN dirn)
{
  return Poll(dirn,&ZeroTime);
}

int Stream::Poll(STRM_DIRN dirn,struct timeval *wdly)
{
  fd_set *readfds  =  0,
         *writefds =  0,
         *errorfds =  fdset(2);
  int     fd1,
          fd2      = -1,
          nfds,
          s;

#ifdef POLLIN
  if (!wdly || (wdly->tv_sec < 0x7FFFFFFF/1000)) {
    int           ms = wdly ? (wdly->tv_sec * 1000) + (wdly->tv_sec / 1000)
                            : -1;
    struct pollfd fds[2];
    nfds = 0;
    switch (dirn) {
    case STRM_BOTH: fds[nfds].events = POLLOUT;
                    fds[nfds++].fd   = fd2 = Fd(STRM_OUT);
    case STRM_IN:   fds[nfds].events = POLLIN;
                    fds[nfds++].fd   = fd1 = Fd(STRM_IN);
                    break;
    case STRM_OUT:  fds[nfds].events = POLLOUT;
                    fds[nfds++].fd   = fd2 = Fd(STRM_OUT);
                    break;
    default:        assert("Bad stream direction" == 0);
    }
    poll(fds,nfds,ms);
    while (nfds--) {
      if (fds[nfds].revents & POLLERR)           goto bad;
      if (fds[nfds].revents & (POLLIN|POLLOUT))  goto rdy;
      if (fds[nfds].revents & POLLHUP) {
        if (fd2 == fds[nfds].fd)                 goto bad;
      }
    }
    goto nada;
  }
#endif

  BZERO(errorfds,FdSize);

  switch (dirn) {
  case STRM_BOTH: BZERO(writefds = fdset(1),FdSize);
                  FD_SET(fd2 = Fd(STRM_OUT),writefds);
                  FD_SET(fd2,errorfds);
  case STRM_IN:   BZERO(readfds = fdset(0),FdSize);
                  FD_SET(fd1 = Fd(STRM_IN),readfds);
                  FD_SET(fd1,errorfds);
                  break;
  case STRM_OUT:  BZERO(writefds = fdset(1),FdSize);
                  FD_SET(fd1 = Fd(STRM_OUT),writefds);
                  FD_SET(fd1,errorfds);
                  break;
  default:        assert("Bad stream direction" == 0);
  }

  if (fd1 < fd2) nfds = fd2 + 1;
  else           nfds = fd1 + 1;

  s = select(nfds,readfds,writefds,errorfds,wdly);

  if (s-- > 0) {
    switch (dirn) {
    case STRM_BOTH: if (FD_ISSET(fd2,errorfds)) goto bad;
                    if (FD_ISSET(fd2,writefds)) goto rdy;
                    if (0 == s) break;
    case STRM_IN:   if (FD_ISSET(fd1,errorfds)) goto bad;
                    if (FD_ISSET(fd1,readfds))  goto rdy;
                    break;
    case STRM_OUT:  if (FD_ISSET(fd1,errorfds)) goto bad;
                    if (FD_ISSET(fd1,writefds)) goto rdy;
                    break;
    }
  }

nada:
  return 0;
rdy:
  return 1;
bad:
  return -1;
}

void Stream::rgstr(int fd)
{
  assert((strm_typ  == self->Type() || 
          STRM_CLSD == self->Type()) && "Sanity Check");

  if (!InUse) {
    if ((DesLimit = MAX_DESC) < 0) {
      DesLimit = 99;
    }
    InUse     = CALLOC2(DesLimit+1,Stream *);
    FdSize    = FD_WORDS(DesLimit, FD_NFDBITS);
    FdSet[0]  = CALLOC2(3 * FdSize,long);
    FdSet[1]  = FdSet[0] + FdSize;
    FdSet[2]  = FdSet[0] + (2 * FdSize);
    FdSize   *= sizeof(long);
    InUse++; /* allows -1 as an index */
  }

  InUse[fd] = self = this;

  if (fd > FdTop) FdTop = fd;
}

Stream* Stream::Stdio(int fd)
{
  Stream *fds;

  if (!InUse || !(fds = InUse[fd])) {
    fds           = new RawStream;
    fds->strm_typ = STRM_RAW;
    fds->rgstr(fd);
    switch (fd) {
    case STDIN_FILENO:  fds->fd_in  = fd;
                        fds->mode   = FM_READ;
                        fds->fp     = stdin;
                        break;
    case STDERR_FILENO: fds->fd_out = fd;
                        fds->mode   = FM_WRITE;
                        fds->fp     = stderr;
                        break;
    case STDOUT_FILENO: fds->fd_out = fd;
                        fds->mode   = FM_WRITE;
                        fds->fp     = stdout;
                        break;
    case -1:            fds->fp     = fopen(DEV_NULL,"w");
                        fds->mode   = FM_WRITE;
                        fds->fd_out = fileno(fds->fp);
                        break;
    }
  }

  return fds;
}

RawStream::RawStream()
  : Stream(this)
{
}

eSTRM RawStream::Type() const
{
  return STRM_RAW;
}

int RawStream::Read(void *buf,int nbyte)
{
  return read(In(),buf,nbyte);
}

int RawStream::Write(const void *buf,int nbyte)
{
  return write(Out(),buf,nbyte);
}

int RawStream::Size()
{
  struct stat buf;
  int         sts;

  if (In() >= 0) sts = fstat(In(),  &buf);
  else           sts = fstat(Out(), &buf);

  if (sts) return -1;

  return  buf.st_size;
}

int RawStream::Unlink()
{
  Filename f = Name();
  Close(STRM_BOTH);
  return unlink(f);
}

int RawStream::Close(STRM_DIRN dirn)
{
  return DoClose(dirn,fclose);
}

void RawStream::Flush()
{
  do_flush();
}

int RawStream::Seek (U64 off,int w)
{
  return lseek(Fd(),off,w);
}

int RawStream::printf(const char *format,...)
{
  va_list pvar;

  va_start(pvar, format);

  String tmp(format,pvar);

  va_end(pvar);

  return Write((void *)tmp.str(),tmp.len());
}

int RawStream::vprintf(const char *format,va_list pvar)
{
  String tmp(format,pvar);

  return Write((void *)tmp.str(),tmp.len());
}

int RawStream::gets(char *s, int n)
{
  char *cp = fgets(s,n-1,Fp());
  int   l;

  s[n-1] = 0;

  return cp ? l = strlen(cp)
            : -1;
}

int RawStream::gets(String *s)
{
  char buf2[1024];
  int  got;

  s->cut(0);

  while ((got = gets(buf2,sizeof(buf2))) > 0) {
    *s += buf2;
    if (got < sizeof(buf2) -1 || '\n' == buf2[got -1]) {
      return s->len();
    }
  }

  return -1;
}

MemStream::MemStream(const char *nm)
{
  m_nxt     = MmStrmLst;
  MmStrmLst = this;

  size = limit = posn = 0;
  buff = 0;
  mppd = shrd  = 0;

  setName(nm);
  setMaster();
}

MemStream::~MemStream()
{
  assert(Master());

  clrMaster();

  FREE(buff);
  limit = size = posn = 0;

  MemStream **msp = &MmStrmLst,
             *ms;

  while (ms = *msp) {
    if (ms == this) {
      *msp = ms->m_nxt;
      return;
    }
    msp = &ms->m_nxt;
  }

  assert(0 == "Constructor/Destructor Mismatch");
}

eSTRM MemStream::Type() const
{
  return STRM_MEM;
}

int MemStream::Read(void *buf,int nbyte)
{
  if (size - posn < nbyte) nbyte = size - limit;

  bcopy(&buff[posn],(char *)buf,nbyte);

  posn += nbyte;

  return nbyte;
}

int MemStream::Write(const void *buf,int nbyte)
{
  if (posn + nbyte >= limit) {
    if (mppd) {
       nbyte = limit - posn;
    } else {
       limit += (1 + (nbyte/MMSTRM_INCR)) * MMSTRM_INCR;
       REALLOC(buff,limit,char);
    }
  }
  bcopy((const char *)buf,&buff[posn],nbyte);
  posn += nbyte;
  if (posn > size) size = posn;

  return nbyte;
}

int MemStream::Size()
{
  return size;
}

int MemStream::Unlink()
{
  if (Master()) {
    delete (this);
  } else {
    assert(Self() != this && "Sanity Check");
    Self()->Unlink();
  }
  return 0;
}

int MemStream::Close(STRM_DIRN dirn)
{
  size = posn;
#ifdef DEBUG
  buff  = 0;
  limit = ~0;
#endif
  return 0;
}

void MemStream::Flush()
{
  // Unnecessary
}

int MemStream::Seek (U64 off,int w)
{
  U64 p;

  switch (w) {
  case SEEK_SET: p = off;        break;
  case SEEK_END: p = size;       break;
  case SEEK_CUR: p = posn + off; break;
  default:       errno = EINVAL; return -1;
  }

  assert(p <= size);

  posn = p;

  return 0;
}

int MemStream::printf(const char *format,...)
{
  va_list pvar;

  va_start(pvar, format);

  String tmp(format,pvar);

  va_end(pvar);

  return Write((void *)tmp.str(),tmp.len());
}

int MemStream::vprintf(const char *format,va_list pvar)
{
  String tmp(format,pvar);

  return Write((void *)tmp.str(),tmp.len());
}

int MemStream::gets(char *s, int n)
{
  assert(("NIY",0));
}

int MemStream::gets(String *s)
{
  assert(("NIY",0));
}

BuffStream::BuffStream()
{
}

eSTRM BuffStream::Type() const
{
  return STRM_BUFF;
}

int BuffStream::Read(void *buf,int nbyte)
{
  int sts = fread(buf,1,nbyte,Fp());
  return sts;
}

int BuffStream::Write(const void *buf,int nbyte)
{
  int sts = fwrite(buf,1,nbyte,Fp());
  return sts;
}

void BuffStream::Flush()
{
  do_flush();
}

int BuffStream::Size()
{
  struct stat buf;

  int sts = fstat(fileno(Fp()),&buf);

  if (sts) return -1;

  return  buf.st_size;
}

int BuffStream::Seek (U64 off,int w)
{
  return fseek(Fp(),off,w);
}

int BuffStream::Unlink()
{
  Filename f = Name();
  Close(STRM_BOTH);
  return unlink(f);
}

int BuffStream::Close(STRM_DIRN dirn)
{
  return DoClose(dirn,fclose);
}

int BuffStream::printf(const char *format,...)
{
  va_list pvar;

  va_start(pvar, format);

  String tmp(format,pvar);

  va_end(pvar);

  return Write((void *)tmp.str(),tmp.len());
}

int BuffStream::vprintf(const char *format,va_list pvar)
{
  String tmp(format,pvar);

  return Write((void *)tmp.str(),tmp.len());
}

int BuffStream::gets(char *s, int n)
{
  char *cp = fgets(s,n-1,Fp());
  int   l;

  s[n-1] = 0;

  return cp ? l = strlen(cp)
            : -1;
}

int BuffStream::gets(String *s)
{
  char buf2[1024];
  int  got,
       total = -1;

  s->cut(0);

  while ((got = gets(buf2,sizeof(buf2))) > 0) {
    *s += buf2;
    if (got < sizeof(buf2) -1 || '\n' == buf2[got -1]) {
      total = s->len();
      break;
    }
  }

  return total;
}

PipedStream::PipedStream()
{
}

eSTRM PipedStream::Type() const
{
  return STRM_PIPED;
}

int PipedStream::Read(void *buf,int nbyte)
{
  return fread(buf,1,nbyte,Fp());
}

int PipedStream::Write(const void *buf,int nbyte)
{
  return fwrite(buf,1,nbyte,Fp());
}

void PipedStream::Flush()
{
  do_flush();
}

int PipedStream::Size()
{
  return -1;
}

int PipedStream::Seek (U64 off,int w)
{
  return -1;
}

int PipedStream::Unlink()
{
  Filename f = Name();
  Close(STRM_BOTH);
  return unlink(f);
}

int PipedStream::Close(STRM_DIRN dirn)
{
  return DoClose(dirn,pclose);
}

int PipedStream::printf(const char *format,...)
{
  va_list pvar;

  va_start(pvar, format);

  String tmp(format,pvar);

  va_end(pvar);

  return Write((void *)tmp.str(),tmp.len());
}

int PipedStream::vprintf(const char *format,va_list pvar)
{
  String tmp(format,pvar);

  return Write((void *)tmp.str(),tmp.len());
}

int PipedStream::gets(char *s, int n)
{
  char *cp = fgets(s,n-1,Fp());
  int   l;

  s[n-1] = 0;

  return cp ? l = strlen(cp)
            : -1;
}

int PipedStream::gets(String *s)
{
  char buf2[1024];
  int  got;

  s->cut(0);

  while ((got = gets(buf2,sizeof(buf2))) > 0) {
    *s += buf2;
    if (got < sizeof(buf2) -1 || '\n' == buf2[got -1]) {
      return s->len();
    }
  }

  return -1;
}

int StringStream::Read (void *buf,int nbyte)
{
  assert(0);
  return 0;
}

void StringStream::Flush()
{
}

eSTRM StringStream::Type() const
{
  return STRM_STRING;
}

int StringStream::Write(const void *buf,int nbyte)
{
  *str() += (char *)buf;
  return nbyte;
}

int StringStream::Size()
{
  return str()->len();
}

int StringStream::Seek(U64 off,int w)
{
  int l = str()->len();

  switch (w) {
  case SEEK_SET: set_posn(off);          break;
  case SEEK_END: set_posn(l);            break;
  case SEEK_CUR: set_posn(posn() + off); break;
  default:       errno = EINVAL;         return -1;
  }

  if (posn() > l) set_posn(l);

  return 0;
}

int StringStream::Unlink()
{
  return -1;
}

int StringStream::Close(STRM_DIRN dirn)
{
  return -1;
}

int StringStream::printf(const char *format,...)
{
  va_list pvar;

  va_start(pvar, format);

  String tmp(format,pvar);

  va_end(pvar);

  return Write((void *)tmp.str(),tmp.len());
}

int StringStream::vprintf(const char *format,va_list pvar)
{
  String tmp(format,pvar);

  return Write((void *)tmp.str(),tmp.len());
}

int StringStream::gets(char *s, int n)
{
  int         r  = -1,
              l  = str()->len() - posn();
  const char *sp = str()->str(),
             *cp = strchr(sp += posn(),'\n');

  if (cp) {
    r = (cp - sp) +1;
  } else if (l > 0) {
    r = l;
  }

  if (r >= n) r = n -1;

  if (r > 0) {
    strncpy(s,sp,r);
    s[r] = '\0';
    Seek(r,SEEK_CUR);
  } else if (n > 0) {
    *s = '\0';
  }

  return r;
}

int StringStream::gets(String *s)
{
  int         r  = -1,
              l  = str()->len() - posn();
  const char *sp = str()->str(),
             *cp = strchr(sp += posn(),'\n');

  if (cp) {
    r = (cp - sp) +1;
  } else if (l > 0) {
    r = l;
  }

  if (r > 0) {
    strncpy(s->blank(r),sp,r);
    Seek(r,SEEK_CUR);
  } else {
    s->cut(0);
  }

  return r;
}

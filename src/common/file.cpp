/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * file_cpp_rcsid() {return "$Id: file.cpp,v 1.44 2009/10/23 03:20:02 dkc Exp $";}

#include "v2k_mem.h"
#include "error.h"
#include "env.h"
#include "file.h"

const char *File::DirSeperator  = OS_DIR_SEP;
#ifdef OS_DIR_SEP2
const char *File::DirSeperator2 = OS_DIR_SEP2;
#endif
const char *File::TypeSeperator = OS_TYPE_SEP;
const char *File::Root          = OS_ROOT;

static sFile NullFile;
const File *File::NullFile = (File *)&NullFile;

const sFileType FileTypes[] ={
#define FT_DECL(e,s) {e,s},
#include "ftypes.h"
  {(eFT)-1,0}
};


Filename *Filename::operator-=(const char *str)
{
  int l = strlen(buff);

  l -= strlen(str);

  if (0 == strcmp(&buff[l],str)) {

    buff[l] = '\0';
  }

  return this;
}

int Filename::Size() const
{
  struct stat buf;
  int         l = stat(buff,&buf);

  if (0 == l) l = buf.st_size;

  return l;
}

U64 Filename::Date() const
{
  struct stat buf;
  U64         t = 0;
  String      exp(buff);

  envExpand(&exp);

  if (0 == stat(exp.str(),&buf)) t = buf.st_mtime;

  return t <<= 32;
}

int Filename::Exists(int chk_cmp) const
{
  Filename xpnd(buff);

  envExpand(xpnd.buff,sizeof(xpnd.buff));

  if (0 == access(xpnd.buff,F_OK)) return -1;

  Compressor *cmprsr = 0;

  if (chk_cmp && cmprsr->Exists(&xpnd)) return -1;

  const char *cp;
  eURL        up = URLproto(xpnd.buff,&cp);
  MemStream  *ms = 0;

  switch (up) {
  case URL_MEM: if (!ms->find(cp)) return 0;
  }

  return up;
}

int Filename::Unlink() const
{
  Filename xpnd(buff);

  envExpand(xpnd.buff,sizeof(xpnd.buff));

  return 0 == unlink(xpnd.buff);
}

eFM File::mode(const char *mode) const
{
  int fmode = 0;

  for (; *mode ; mode++) switch (*mode) {
#define FMODE(e,l,u) case l: fmode |=  e;   break;
#include "fmode.h"
#define FMODE(e,l,u) case u: fmode &= ~e;   break;
#include "fmode.h"
  }
  return (eFM)fmode;
}

const char *File::strMode(int mode) const
{
  static char str[16];

  char *cp = str;

#define FMODE(e,l,u) if (mode & e) *cp++ = l;
#include "fmode.h"

  *cp = 0;

  return str;
}

File::File(const char *dir,const char *name,eFT t,int flgs)
{
  buff  = dir;
  if (!(flgs & FILE_NO_SEP)) {
    if (*DirSeperator != buff.lastCh()
#ifdef OS_DIR_SEP2
        && *DirSeperator2 != buff.lastCh()
#endif
        ) buff += DirSeperator;
  }
  if (flgs & FILE_BASENAME) {
    const char *p = strrchr(name,*DirSeperator);
    if (p) name = ++p;
#ifdef OS_DIR_SEP2
    p = strrchr(name,*DirSeperator2);
    if (p) name = ++p;
#endif
  }
  buff += name;
  const char *f = buff.str();
  int         l = buff.len();
  while (l >= 0 && f[l] != *TypeSeperator
                && f[l] != *DirSeperator
#ifdef OS_DIR_SEP2
                && f[l] != *DirSeperator2
#endif
        ) l--;
  if (f[l] == *TypeSeperator) {
    buff.cut(l);
  }
  buff += TypeSeperator;
  buff += FileTypes[t].str;
}

const char *File::name()
{
  return buff.str();
}

const char *File::dir() const
{
  static Filename f;
  int             l = buff.len();

  f = buff.str();

  while (l >= 0 && ((char *)f)[l] != *DirSeperator
#ifdef OS_DIR_SEP2
	        && ((char *)f)[l] != *DirSeperator2
#endif
        ) l--;

  if (l < 0) return OS_CWD OS_DIR_SEP;

  ((char *)f)[l+1] = '\0';

  return f;
}

const char *Filename::dir()
{
  File f = buff;

  return f.dir();
}

const char *File::basename(char *ret) const
{
  int         l  = len();
  const char *bp = buff;

  bp += l;

  if (l && bp[-1] == *DirSeperator) bp--;

  while (l-- > 0 && bp[-1] != *DirSeperator
#ifdef OS_DIR_SEP2
	         && bp[-1] != *DirSeperator2
#endif
        ) bp--;

  if (ret) {
    char *rp = ret;
    while (*bp && *bp != *DirSeperator
#ifdef OS_DIR_SEP2
	       && *bp != *DirSeperator2
#endif
          ) {
      *rp++ = *bp++;
    }
    *rp = '\0';
    return ret;
  }

  return bp;
}

const char *Filename::basename(char *ret) const
{
  int         l  = len();
  const char *bp = buff;

  bp += l;

  if (l && (bp[-1] == *File::DirSeperator
#ifdef OS_DIR_SEP2
            || bp[-1] == *File::DirSeperator2
#endif
     )) bp--;

  while (l-- > 0 && bp[-1] != *File::DirSeperator
#ifdef OS_DIR_SEP2
                 && bp[-1] != *File::DirSeperator2
#endif
        ) bp--;

  if (ret) {
    char *rp = ret;
    while (*bp && *bp != *File::DirSeperator
#ifdef OS_DIR_SEP2
               && *bp != *File::DirSeperator2
#endif
          ) {
      *rp++ = *bp++;
    }
    *rp = '\0';
    return ret;
  }

  return bp;
}

const char *Filename::changeType(eFT typ)
{
  File f(buff);

  f.changeType(typ);

  strcpy(buff,f);

  return buff;
}

const char *File::changeType(eFT typ)
{
  int         l   = len();
  const char *bp0 = buff.str(),
             *bp  = &bp0[l];

  while (l-- > 0 && bp[-1] != *TypeSeperator) {
    if (*DirSeperator == *--bp
#ifdef OS_DIR_SEP2
	|| *DirSeperator2 == *bp
#endif
       ) {
      buff += TypeSeperator;
      goto add_typ;
    }
  }

  if (FT_Dir == typ) bp--;

  buff.cut(bp - buff.str());

 add_typ:
  if (typ) buff += FileTypes[typ].str;
  else     buff -= TypeSeperator;

  return buff;
}

const char *File::up()
{
  static char seg[32];

  const char *f  = buff.str();
  char       *sp = &seg[31];
  int         l  = buff.len();

  while (l >= 0 && f[l] != *DirSeperator
#ifdef OS_DIR_SEP2
	        && f[l] != *DirSeperator2
#endif
        ) l--;
  l--;
  for (*sp = '\0'; l >= 0 && f[l] != *DirSeperator
#ifdef OS_DIR_SEP2
	                  && f[l] != *DirSeperator2
#endif
                 ; l--) {
    if (sp > seg) *--sp = f[l];
  }

  if (l >= 0) {
    buff.cut(l+1);
    return sp;
  }

  buff = Root;
  return 0;
}

int File::down(const char *dir,bool trim)
{
  const char *f = buff.str();
  int         l = buff.len();

  if (trim) {
    while (l >= 0 && f[l] != *DirSeperator
#ifdef OS_DIR_SEP2
	          && f[l] != *DirSeperator2
#endif
           ) l--;

    if (!buff.cut(l+1)) {
      buff = Root;
    }
  }

  buff += dir;
  buff += DirSeperator;

  return 0;
}

int File::mkDir(int path) const
{
  Filename bff(dir());
  int      sts;

  envExpand(bff,sizeof(bff));

  sts = mkdir(bff,0777);

  if (sts) switch (errno) {
  case EEXIST: goto ok;
  default:     if (path) {
                 File d(bff);
                 d.up();
                 if (d.mkDir(1)) sts = mkdir(bff,0777);
               }
  }

  if (sts < 0 && EEXIST != errno) return 0;

ok:
  return 1;
}

int Filename::dirMatch(const char *mtch,fileCB fn,void *data) const
{
  int       sts = -1,
            fsts;
  DIR      *dir = opendir(str());
  eURL      proto;
  Filename  path = str();
  int       l    = len();

  if (dir) {
    struct dirent *de;
    int            f = 0;
    while (de = readdir(dir)) {
      void *rt;
      char *nm = de->d_name;
      if ('.' == *nm && (!mtch || '.' != *mtch)) goto next;
      if (mtch && !wildCmp(mtch,nm))             goto next;
      strcpy(&path.buff[l],nm);
      nm = &path.buff[l];
      struct stat buf;
      fsts = stat(path,&buf);
      if (S_ISDIR(buf.st_mode)) {
	strcat(nm,File::DirSeperator);
      } 
      f++;
      if (rt = (*fn)(nm,data))                   return f;
    next:;
    }
    closedir(dir);
    return f;
  } switch (proto = URLproto(str(),0)) {
  case URL_MEM: assert("NIY" == 0);
  }

  return sts;
}

extern "C" int fileCmp(const char *f1,const char *f2)
{
  return (0 == strcmp(f1,f2));
}

extern "C" int isWild(const char *tmpl)
{
  int ch,
      last = -1;

  for (; ch = *tmpl ; last = ch) {
    if ('\\' != last) switch(*tmpl++) {case '{':
                                       case '?':
                                       case '*': return 1;}
  }

  return 0;
}

extern "C" int wildCmp(const char *tmpl,const char *file)
{
  while (*tmpl && *file) {
    if (*tmpl == *file) {
      tmpl++; file++;
    } else if ('?' == *tmpl) {
      tmpl++; file++;
    } else if ('*' == *tmpl) {
      if (!tmpl[1])           return 1;
      if (tmpl[1] == *file++) tmpl += 2;
    } else if ('{' == *tmpl) {
      TMPARR(char,new_tmpl,strlen(tmpl)+1);
      const char *tp    = tmpl + 1,
                 *rem   = tp;
      char       *nt    = new_tmpl;
      int         depth = 0,
                  ch,
                  last  = 0;
      while (ch = *rem++) {
        if ('\\' !=  last)  switch (ch) {
        case '{': depth++; break;
        case '}': if (!depth) goto clsd;
                  depth--;
        }
        last = ch;
      }
    clsd:
      for (; ch = *nt = *tp++ ; nt++, last = ch) {
        if ('\\' !=  last) switch (ch) {
        case '{': depth++; break;
        case '}': if (depth--) break;
        case ',': strcpy(nt,rem);
                  if (wildCmp(new_tmpl,file)) return 1;
                  if (depth < 0) return 0;
                  nt = new_tmpl -1;
                  break;
        }
      }
    } else {
      return 0;
    }
  }

  return *tmpl == *file || 0 == strcmp(tmpl,"*");
}

static int subGlob(String *dir,int sgs,char **seg,char **ret,
                   int *max,int *to_do,int prfx)
{
  const char *en,
             *pre;
  int         glbd = 0,
              ol   = dir->len();

  while (sgs > 0 && !**seg) {
    sgs--;
    seg++;
    *dir += OS_DIR_SEP;
  }

  if (sgs) {
    int            s    = 0;
    DIR           *dirp = opendir(dir->str());
    struct dirent *entry;

    if (dirp) {
      *dir += OS_DIR_SEP;

      int         dl  = strlen(dir->str()) + 1,
	          mx  = *max;
      const char *pre = dir->str();

      pre += prfx;

      while (entry = readdir(dirp)) {
	en = entry->d_name;
	if (strcmp(en,OS_CWD) && strcmp(en,OS_PARENT))
	  {
	    if (wildCmp(seg[s],en)) {
	      if (1 == sgs) {
  	        glbd++;
		int l = strlen(en) + dl + 1;
		if (ret) {
		  if (l > mx || (*to_do)-- <= 0) {
		    *max = l;
		    return -2;
		  }
		  sprintf(*ret,"%s%s",pre,en);
		  (*ret) += mx;
		} else {
		  if (l > mx) *max = mx = l;
		}
	      } else {
		*dir += en;
                int g = subGlob(dir,sgs-1,seg+1,ret,max,to_do,prfx);
                if (g < 0) return g;
		glbd += g;
		mx    = *max;
		*dir -= en;
	      }
	    }
	  }
      }
      closedir(dirp);
    }
  } else {
    const char *pre = dir->str();
    int         l   = dir->len() +1;
    if (ret) {
      if (l > *max || (*to_do)-- <= 0) {
	*max = l;
        return -2;
      }
      sprintf(*ret,"%s",pre + prfx);
      (*ret) += *max;
    } else {
      if (l > *max) *max = l;
    }
    glbd++;
  }

  dir->cut(ol);
  return glbd;
}

extern "C" int fileGlob(const char *tp,char *ret,int *max,int *to_do)
{
  const char *cp   = tp;
  int         sgs  = 1,
              ch,
              last = 0,
              glbd = 0;

  if (isWild(tp)) {

    int sl = strlen(OS_DIR_SEP);

    for(; ch = *cp ; last = ch, cp++) {
      if ('\\' != last) {
        if (0 == strncmp(cp,OS_DIR_SEP,sl)) {
          sgs++;
          cp += sl -1;
        }
      }
    }

    { TMPARR(char *,seg,sgs);
      TMPARR(char,str,strlen(tp) +1);

      char **rp = ret ? &ret
	              : 0;

      strcpy(str,tp);
      seg[0] = str;
      for (cp = tp, sgs = 1; ch = *cp; last = ch, cp++) {
        if ('\\' != last) {
          if (0 == strncmp(cp,OS_DIR_SEP,sl)) {
            str[cp-tp]  = '\0';
            seg[sgs++]  = &str[1 + (cp-tp)];
            cp         += sl -1;
          }
        }
      }

      if (!*seg[0]) {
        String dir(OS_ROOT);

        glbd = subGlob(&dir,sgs,seg,rp,max,to_do,sl + strlen(OS_ROOT));
      } else {
        String dir(OS_CWD);

        glbd = subGlob(&dir,sgs,seg,rp,max,to_do,sl + strlen(OS_CWD));
      }

    }

    if (!glbd) glbd = -1;
  }

  return glbd;
}

MappedFile::~MappedFile()
{
#ifndef NO_MMAP
  if (addr) stream.Munmap((ADDR)addr,end);
  stream.Close();
#endif
}

void MappedFile::Init(long min_size)
{
  int prtf  = 0,
      flags = 0;
  eFM mode  = stream.Mode();

  end   = stream.Size();
  limit = end;

  if (mode & FM_READ) {
     prtf |= PROT_READ;
  }
  if (mode & (FM_WRITE|FM_APPEND)) {
     prtf  |= PROT_WRITE;
     flags |= MAP_SHARED;

     if (limit < min_size) limit = min_size;

  } else {
     flags |= MAP_PRIVATE;
  }
  if (mode & FM_SHARED) {
     flags &= ~MAP_PRIVATE;
     flags |=  MAP_SHARED;
  }
  if (mode & FM_EXE) {
     prtf |= PROT_EXEC;
  }

#ifdef NO_MMAP
  addr = MAP_FAILED;
#else
  addr = stream.Mmap(0,limit,prtf,flags,0);
#endif

  if (MAP_FAILED == addr) {
     end  = errno;
     addr = 0;
  }

  if (!(mode & (FM_WRITE|FM_APPEND|FM_SHARED))) {
     stream.Close();
  }

}

ADDR MappedFile::remap(long sz)
{
  if (addr) stream.Munmap(addr,limit);

  Init(sz);
  return addr;
}

MappedFile::MappedFile(const char *file,const char *smode,long sz)
{
  String s(smode);
  File   f(file);

  addr   = 0;
  s     += "d";

  if (stream.Open(f,s)) {
    Init(sz);
  } else {
    end  = errno;
  }
}

MappedFile::MappedFile(const char *file,const char *smode)
{
  String s(smode);
  File   f(file);

  addr   = 0;
  s     += "d";

  if (stream.Open(f,s)) {
    Init(MAP_MINSIZE);
  } else {
    end  = errno;
  }
}

MappedFile::MappedFile(const char *file)
{
  File f(file);

  addr = 0;

  if (stream.Open(f,"rd")) {
    Init(-1);
  } else {
    end  = errno;
  }
}

int MappedFile::Write(void *buf,int nbyte)
{
  int wr = stream.Write(buf,nbyte);

  if (wr > 0) end += wr;

  return wr;
}

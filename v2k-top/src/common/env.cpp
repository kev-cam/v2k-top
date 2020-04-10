/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * env_cpp_rcsid() {return "$Id: env.cpp,v 1.62 2010/05/01 09:54:02 dkc Exp $";}

#include "system.h"
#include "error.h"
#include "env.h"
#include "v2k_mem.h"
#include "file.h"
#include "args.h"
#include "strfunc.h"

EnvItem *EnvItem::List;

EnvItem::EnvItem()
{
  next       = 0;
  nml        = 0;
  ell        = 0;
  items      = 1;
  name_value = 0;
  start      = 0;

  used.set();
}

EnvItem::~EnvItem()
{
  FREE(name_value);
  FREE(ell);
}

EnvItem *EnvItem::def(const char *nm,const char *vl) {
  if (!List || !List->have(nm)) return add(nm,vl,ENV_LOCAL);
  return 0;
}

EnvItem *EnvItem::initEnv()
{
  static int lockout;

  if (!this && !lockout) {
    lockout = 1;
    Filename f;
    getcwd(f,sizeof(f));
    add("cwd",f);
    File dirx(Arg0());
    add(V2K_OS,         OS_TYPE);
    add(V2K_OS_BIN,     dirx.dir());
    String ot(dirx.up());
    ot += OS_DIR_SEP;
    add(V2K_OS_OBJ,     ot);
    char *ld_pth = getenv( V2K_LD_PATH );
    if (ld_pth) {
      File dirs(ld_pth);
      add(V2K_SO_LIB,   ld_pth);
      dirs.up();
      dirs.down(OS_TYPE);
      add(V2K_OS_LIB,   dirs.dir());
      dirs.up();
      add(V2K_LIB,      dirs.dir());
      dirs.up();
      add(V2K_ROOT,     dirs.dir());
      dirs.down("bin");
      add(V2K_BIN,      dirs.dir());

      def(V2K_IMPORT,   "${" V2K_ROOT "}" );
    } else {
      dirx.up();
      File dirs(dirx.dir());
      add(V2K_BIN,      dirs.dir());
      dirs.up();    
      add(V2K_ROOT,     dirs.dir());
      dirs.down("lib");
      add(V2K_LIB,      dirs.dir());
      dirs.down(OS_TYPE);
      add(V2K_OS_LIB,     dirs.dir());
      add(V2K_SO_LIB,   "${" V2K_LIB "}${" V2K_OS_OBJ "}");

      def(V2K_IMPORT,   "${" V2K_ROOT "}import" OS_DIR_SEP "v2k" OS_DIR_SEP);
    }
    add(V2K_CMPRSSRS,   "${" V2K_LIB "}compressors");
    add(V2K_LTOK_CCH,   "${" V2K_OS_LIB "}");

    def(V2K_REPOSITORY, "mem:" OS_DIR_SEP "v2k");
    def(V2K_STRINGS,    "${" V2K_REPOSITORY "}");
    def(V2K_LIBRARY,    "${" V2K_DEFLIB "}");
    def(V2K_DEFLIB,     "work");
    def(V2K_LIB_PATH,   "${" V2K_REPOSITORY "}" OS_DIR_SEP "lib");
    def(V2K_PLI_PATH,   "${" V2K_REPOSITORY "}" OS_DIR_SEP "pli");
    def(V2K_LIB_REPOS,  "${" V2K_REPOSITORY "}" OS_DIR_SEP "lib"
                                                OS_DIR_SEP "${" V2K_LIBRARY "}" OS_DIR_SEP);

    def(V2K_CSRC,       "v2k-csrc");
    def(V2K_CODE_REPOS, "${" V2K_CSRC "}lib" OS_DIR_SEP "${" V2K_LIBRARY "}" OS_DIR_SEP );

    def(V2K_INCLUDE,    OS_CWD SH_LST_SEP);
    def(V2K_SYSINCLUDE, "${" V2K_IMPORT "}include" OS_DIR_SEP);
    def(V2K_PC_INC,     "${" V2K_SYSINCLUDE "}parc" OS_DIR_SEP);
    def(V2K_SIM_INC,    "${" V2K_SYSINCLUDE "}sys" OS_DIR_SEP);
    def(V2K_CC,         "cc ");
    def(V2K_CXX,        "g++ ");
    def(V2K_OS_SO_EXT,  OS_LIB_SHRD);
#ifdef __APPLE_CC__
    def(V2K_SIM_FLAGS,  "-g -dynamiclib -single_module -undefined dynamic_lookup -fPIC -DLIBRARY");
#else
    def(V2K_SIM_FLAGS,  "-g -shared -fPIC -DLIBRARY");
#endif
    def(V2K_SIM_LIB,    "v2ksim");
    if (sizeof(long) > 4) {
      def(V2K_CC_MODE,  "-m64");
    }
    def(V2K_RC_PATH,    "${" V2K_IMPORT "}:${HOME}/:.");
    def(V2K_CC_SIM,     "${" V2K_CXX "} ${" V2K_CXX_FLAGS "} ${" V2K_CC_MODE "} -I${" V2K_SIM_INC "} "
                        "${" V2K_SIM_FLAGS "} ${" V2K_CC_FLAGS "}");
    def(V2K_CC_PC_FLAGS,"-x c++");
    def(V2K_CC_PC,      "${" V2K_CXX "} ${" V2K_CXX_FLAGS "} ${" V2K_CC_MODE "} -I${" V2K_PC_INC "} "
                        "${" V2K_SIM_FLAGS "} ${" V2K_CC_FLAGS "} ${" V2K_CC_PC_FLAGS "} ");
    def(V2K_CC_TMP,     "/tmp/$$-");
    def(V2K_CC_REPOS,   "${" V2K_CODE_REPOS "}");
    def(V2K_CC_LD_FLAGS,"${" V2K_SIM_FLAGS "}");
    def(V2K_LD_RPATH,   "-Wl,-rpath");
    def(V2K_PC_LIB,     "parc");
    def(V2K_THREAD_LIB, "-lpthread");
    def(V2K_PC_LD_FLAGS,"-L${" V2K_SO_LIB "} -l${" V2K_PC_LIB "} ${" V2K_THREAD_LIB "}");

#if (defined(DYNLOADING) && !defined(PERL_LIB))
    def(V2K_DYNLOADING,"1");
#endif

    lockout = 0;
    return List;
  }

  return this;
}

Env::Env()  {item = EnvItem::List->initEnv();
             set_val();}

void Env::destroy()
{
  if (item) {
    EnvItem **pp = &EnvItem::List;

    while (*pp != item) {pp = &(*pp)->next;}

    if (*pp) *pp = item->next;

    delete(item);

    item = 0;
    val  = "";
  }
}

const char *EnvItem::Value() const
{
  const char *vp = name_value + nml + 1;

  if (start) {
    if (start == items) vp  = "";
    else                vp += ell[start];
  }

  return vp;
}

EnvItem *EnvItem::have(const char *name)
{
  EnvItem    *env = this;
  const char *val;

  for (; env ; env = env->next) {
    const char *nvp = env->name_value,
               *np  = name;
    while (*np++ == *nvp++) {
      if (!*np && '=' == *nvp) return env;
    }
  }

  if (val = getenv(name)) {
    return EnvItem::List->add(name,val,ENV_IMPORT|ENV_EXPORT|ENV_NEW);
  }

  return 0;
}

char *EnvItem::newNV(int nl,int vl)
{
  return MALLOC_N(nl + vl + 2,char);
}

void EnvItem::delNV(char *nvp)
{
  Free(nvp);
}

bool EnvItem::change(const char *vl)
{
  if (this) {

    char *cmb = newNV(nml,strlen(vl)),
         *old = name_value;

    strncpy(cmb,old,nml);
    sprintf(&cmb[nml],"=%s",vl);

    name_value = cmb;
    if (flags & ENV_EXPORT) putenv(name_value);

    delNV(old);

    return 1;
  }

  return 0;
}

bool EnvItem::change(int val)
{
  char num[12];

  sprintf(num,"%d",val);

  return change(num);
}

const char *Env::get(const char *name)
{
  EnvItem *env = EnvItem::List->initEnv();

  if (env = env->have(name)) {
    (item = env)->read();
    return set_val();
  }

  item = 0;
  val  = "";
  return 0;
}

void EnvItem::resetUse()
{
  EnvItem *env = EnvItem::List;

  for (; env ; env = env->next) env->used.clear();
}

void *EnvItem::forAllUsed(useHandlr hndlr)
{
  EnvItem *env = EnvItem::List;
  void    *ret = 0;

  for (; env ; env = env->next) {
    if (env->used.again) {
      const char *nvp = env->name_value,
                 *val = &nvp[env->nml + 1];
      TMPARR(char,name,env->nml +1);
      const char *exp = (env->flgs() & ENV_EXPORT) ? "E"
			                           : "e";
      strncpy (name,nvp,env->nml);
      name[env->nml] = '\0';

      if (ret = (*hndlr)(exp,name,val,env->used.again)) break;
    }
  }

  return ret;
}

bool EnvItem::sub(unsigned int n,String *ret)
{
  const char *vp = nm_vl() + nm_ln() +1;

  if (!ell) {
    if (n == 1) *ret = vp;
    else        return 0;
  } else {
    if (n < 1 || n > items) {
      return 0;
    } else {
      vp += ell[n -1];
      if (n == items) {
        *ret = vp;
      } else {
        int l = (ell[n] - ell[n -1]) -1;
        strncpy(ret->blank(l),vp,l);
      }
    }
  }

  return 1;
}

EnvItem *EnvItem::add(const char *nm,const char *vl,int flgs)
{
  EnvItem *add = 0;

  if (List) {
    if (!(flgs & ENV_NEW)) {
      Env old;
      if (old.get(nm)) {
	add = (EnvItem *)old.item;
	if (add->flags & ENV_EXPORT) {
	  flgs = ENV_EXPORT;
	}
	FREE(add->ell);
	add->start = 0;
	add->items = 1;
      }
    }
  } else {
    initEnv();
  }

  if (!add) {
    add       = new EnvItem;
    add->next = List;
    List      = add;


    strcpy((char *)(add->name_value = newNV(add->nml = strlen(nm),0)),nm);
  }

  add->used.reset();

  add->flags = flgs & ~ENV_NEW;
  add->change(vl);

  return add;
}

EnvItem *EnvItem::expEnv(const char *enm)
{
  EnvItem *env = add(enm,Value(),ENV_EXPORT);
  char    *vl  = env->name_value + env->nml + 1;
  int      i   = start,
           z   = 1 + ell[i];

  while (++i < items) {
    vl[ell[i] - z] = ':';
  }

  return env;
}

int EnvItem::Shift(int by)
{
  assert(by > 0);

  start += by;

  if (start >= items) {
    by    = items - start;
    start = items;
  }

  return by;
}

int Env::Shift()
{
  int n = -1;

  if (item) {
    n   = item->Shift(1);
    val = item->Value();
  }

  return n;
}

int EnvItem::split(int delim)
{
  char *cp = name_value + nml + 1,
       *vl = cp;
  int   dc = 0,
        ch;

  while (ch = *cp++) {
    if (delim == ch)            dc++;
    else if ('\\' == ch && *cp) cp++;
  }

  if (dc) {

    items  = ++dc;
    ell    = MALLOC_N(dc,int);
    ell[0] = 0;

    for (dc = 0,cp = vl; ch = *cp ; cp++) {
      if ('\\' == ch && *cp) {
        cp++;
      } else if (delim == ch) {
        *cp       = ' ';
        ell[++dc] = 1 + cp - vl;
      }
    }

  }

  return dc;
}

EnvItem *EnvItem::add(const char *nm,int vc,const char **vla,int flgs)
{
  int  c = vc > 0 ? vc
                  : - vc,
       l  = 0,
       i  = c;

  TMPARR(int,ai,c);
  int *ap = vc > 0 ? MALLOC_N(c,int)
                   : ai;

  for (i = 0; i < c ; i++) {
    ap[i]  = l;
    l     += 1 + strlen(vla[i]);
  }

  l++;

  TMPARR(char,val,l);
  char *vp = val;

  *vp = 0;

  for (i = 0; i < c ; i++) {
    if (i) *vp++ = ' ';
    strcpy(vp,vla[i]);
    vp += strlen(vp);
  }

  EnvItem *ep = add(nm,val,flgs);

  FREE(ep->ell);
  ep->start = 0;

  if (vc > 0) {
    ep->ell     = ap;
    ep->items   = vc;
  } else {
    ep->items   = c;
  }

  return ep;
}

int envValue(char *name,String *ret)
{
  Env e(name);
  int ok = 0;

  if (true == e) {
    *ret = e;
    ok   = 1;
  }

  return ok;
}

int envExpand(String *vr,Stream *err,eSHF be,int *argc,int **argp)
{
  int  ok    = 1,
       redo  = 0;

  EnvItem::List->initEnv();

  do {
    TMPARR(char,copy,1 + vr->len());
    TMPARR(char,name,1 + vr->len());

    char        ch,
               *nm,
               *bp    = copy;
    const char *cp    = copy;
    int         s     = 0,
                br    = 0,
                quote = 0;
    String      sub;
    Env         e;

    strcpy(copy,vr->str());

    vr->cut(0);

    while (ch = *bp) switch (ch) {
    case '"':
    case '\'':
      if (ch == quote) {
        quote = 0;
        goto skip;
      } else if (!quote) {
        quote = ch;
        goto skip;
      }
      goto next;
    skip:
      *bp++ = '\0';
      *vr  += cp;
      cp    = bp;
      break;
    case '~':
      if (quote || !(be & SHF_TILDE) || !redo++) goto next;

      *bp++ = '\0';
      *vr  += cp;
      nm    = bp;
      for (; (ch = *bp) && (isalnum(ch) || '_' == ch) ; bp++);
      if (nm == bp) {
        e.get("HOME");
        sub = e;
        if (false == e) {
          err->printf("No $home variable set.\n");
          ok = 0;
        }
      } else {
        strncpy(name,nm,(bp-nm));
        name[(bp-nm)]='\0';
        struct passwd  pwd,
                      *ppw = &pwd;
        char           buff[3 * sizeof(*ppw)];
        BZEROS(pwd);
#ifdef POSIX_PW
        if (0 == getpwnam_r(name,ppw,buff,sizeof(buff),&ppw)) {
#else
        if (ppw =
# ifdef HAVE_GETPWNAM_R
                  getpwnam_r(name,ppw,buff,sizeof(buff))) {
# else
                  getpwnam(name)) {
# endif
#endif
          sub = ppw->pw_dir;
        } else {
          err->printf("Unknown user: %s.\n",name);
          ok = 0;
        }
      }
      goto noqual;

    case '$':
      if ('\'' == quote) goto next;

      *bp++  = '\0';
      *vr += cp;
      br = ('{' == *bp);
      if (br) bp++;
      nm = bp;
      ch = *bp++;
      if (isalpha(ch) || '_' == ch) {
        for (; ch = *bp ; bp++) {
          if (!isalnum(ch) && '_' != ch) break;
        }
        strncpy(name,nm,(bp-nm));
        name[(bp-nm)]='\0';
        e.get(name);
        if (':' == ch) { ch = *++bp; bp++; }
        else           { ch = 0; }
        if ('[' == *bp) {
          char *ip = ++bp;
          int   sb = 0;
          for (;;bp++) switch (*bp) {
          case ']':  if (sb--)        break;
          case 0:    goto s_clsd;
          case '[':  sb++;            break;
          case '\\': if (bp[1]) bp++; break;
          }
        s_clsd:
          if (sb >= 0) {
            err->printf("Newline in variable index.\n");
          } else {
            *bp++ = 0;
            String idx(ip);
            if (ok = envExpand(&idx,err,be)) {
              int n = 0;
              if (ok = (1 == sscanf(idx.str(),"%d",&n))) {
                if (!e.sub(n,&sub)) {
                  err->printf("%s: Subscript out of range.\n",name);
                }
              } else {
                err->printf("Bad index.\n");
              }
            }
          }
          if (!ok) goto done;
        } else {
          sub = e;
        }
        if (false == e) { ok = 0; goto noqual; }
      } else if (isdigit(ch)) {
        for (; ch = *bp ; bp++) {
          if (!isdigit(ch)) break;
        }
        strncpy(name,nm,(bp-nm));
        name[(bp-nm)]='\0';
        if (':' == ch) { ch = *++bp; bp++; }
        else           { ch = 0; }
        int a;
        if (1 != sscanf(name,"%d",&a)) { ok = 0; goto syntax; }
        e.get("argv");
        if (a > 0 && false != e) {
          e.sub(a,&sub);
        } else {
          sub = ArgN(a);
        }
      } else {
        switch(ch) {
        case '$': sub.printf("%d",getpid());
                  goto noqual;
        case '#': if (isalnum(*bp)) {
                    for (nm = bp; ch = *bp ; bp++) {
                      if (!isalnum(ch) && '_' != ch) break;
                    }
                    strncpy(name,nm,(bp-nm));
                    name[(bp-nm)]='\0';
                    e.get(name);
                  } else {
                    e.get("argv");
                  }
                  if (true == e) sub.printf("%d",e.Items());
                  else           sub.printf("%d",ArgCount());
                  goto noqual;
        case '?': if (isalnum(*bp)) {
                    for (nm = bp; ch = *bp ; bp++) {
                      if (!isalnum(ch) && '_' != ch) break;
                    }
                    strncpy(name,nm,(bp-nm));
                    name[(bp-nm)]='\0';
                    e.get(name);
                  }
                  sub.printf("%c",true == e ? '1' : '0');
                  goto noqual;
        case '*': e.get("argv");
                  if (true == e) {
                    sub = e;
                  } else {
                    int n = ArgCount(),
                        a = 1;
                    sub   = "";
                    for (; a < n ; a++) {
                      if (a > 1) sub += " ";
                      sub += ArgN(a);
                    }
                  }
                  break;
        default:
        syntax:   err->printf("Syntax Error ($%c...).\n",ch);
                  ok = 0;
                  goto invalid;
        }
        if (':' == (ch = *bp)) { ch = *++bp; bp++; }
        else                   { ch = 0; }
      }
      switch (ch) {
        default:
          err->printf("Bad : modifier in $ (%c)\n",ch);
        case 0:
          if (!envExpand(&sub,err,be)) ok = 0;
          if (argc) {
            const char *sp0  = sub.str(),
                       *sp;
            int         spc  = 1,
                        wrd  = 0,
                        wrds = 0;
  	  for (sp = sp0 ;; sp++) {
              if (!(ch = *sp) || isspace(ch)) {
                if (!spc++ && wrd) {wrd = 0; wrds++;}
                if (!ch) break;
              } else {
                if (!wrd++ && spc) {spc = 0;}
              }
  	  }
            if (wrds > 1) {
              int *ai = REALLOC2(*argp,wrds+1,int);
              *argc   = wrds;
    	    for (spc = 1,wrd = wrds = 0,sp = sp0 ;; sp++) {
                if (!(ch = *sp) || isspace(ch)) {
                  if (!spc++ && wrd) {wrd = 0; wrds++;}
                  if (!ch) break;
                } else {
                  if (!wrd++ && spc) {spc = 0; ai[wrds] = sp - sp0;}
                }
  	    }
              if (!wrd) ai[wrds] = (sp - sp0) +1;
            }
          }
        case 'q':
    noqual:
          *vr += sub.str();
      }
    invalid:
      if (br) {
         if ('}' == *bp) bp++;
         else            err->printf("Missing }.\n");
      }
      cp = bp;
      break;

    case '\\':
      if (be && bp[1]) {
        *bp++ = '\0';
        *vr  += cp;
        cp    = bp;
        switch (ch = *bp) {
        case 'n': ch = '\n'; break;
        case 'r': ch = '\r'; break;
        case 'a': ch = '\a'; break;
        case 'b': ch = '\b'; break;
        case 'f': ch = '\f'; break;
        case 't': ch = '\t'; break;
        case 'v': ch = '\v'; break;
        case 'c':
          goto done;
        case '0':
          int n = ch = 0;
          while (isdigit(cp[1]) && '7' >= cp[1] && ++n <= 3) {
            cp++;
            ch = (8 * ch) + *cp - '0';
          }
        }
        *bp = ch;
      } else if (!quote) {
        goto skip;
      }

    default:
    next:
      bp++;
    }

    *vr += cp;
  } while (1 == redo);
done:
  return ok;
}

int envExpand(String *vr,Stream *err,eSHF be)
{
  return envExpand(vr,err,be,0,0);
}

int envExpand(String *vr,Stream *err)
{
  return envExpand(vr,err,SHF_NONE);
}

int envExpand(String *vr)
{
  return envExpand(vr,Stream::Stdio(STDERR_FILENO),SHF_NONE);
}

extern "C" int envExpand(char *buff,int size)
{
  String cpy(buff);

  int sts = envExpand(&cpy);

  cpy.cut(size-1);

  strcpy(buff,cpy.str());

  return sts;
}

extern "C" void envAdd(const char *nm,const char *vl)
{
  EnvItem::List->add(nm,vl);
}

EnvItem * envAddFlg(const char *nm,const char *vl,int flgs)
{
  EnvItem::List->initEnv();

  return EnvItem::List->add(nm,vl,flgs);
}

int envPath2var(const char *env,const char *var)
{
  Env epth;

  if (epth.get(env)) {
    EnvItem *pth = EnvItem::List->add(var,epth);
    pth->split(':');
  }

  return 0;
}

int envPathFind(const char *pth_var,const char *item,int acc,String *ret)
{
  Env path;

  if (path.get(pth_var)) {
    const char *strt = path,
               *fnsh;
    int         l,
                l2   = strlen(item) + strlen(OS_DIR_SEP);
    for (; strt ; strt = fnsh) {
      if (fnsh = strchr(strt,':')) l = fnsh++ - strt;
      else                         l = strlen(strt);
      TMPARR(char,ctd,l+l2+1);
      strncpy(ctd,strt,l);
      sprintf(&ctd[l],"%s%s",OS_DIR_SEP,item);
      String tst(ctd);
      envExpand(&tst);
      if (0 == access(tst.str(),acc)) {
        *ret = tst.str();
        return 1;
      }
    }
  }

  return 0;
}

extern "C" eSTS envPathDo(envCallBack cbfn,const char *pth_var,
                                           const char *cbdata)
{
  Env path;

  if (path.get(pth_var)) {
    const char *strt = path,
               *fnsh;
    int         l,
                l2   = strlen(OS_DIR_SEP);
    String      str;
    for (; strt ; strt = fnsh) {
      if (fnsh = strchr(strt,':')) l = fnsh++ - strt;
      else                         l = strlen(strt);
      TMPARR(char,ctd,l+l2+1);
      strncpy(ctd,strt,l);
      ctd[l] = '\0';
      str    = ctd;
      envExpand(&str,Stream::Stdio(STDERR_FILENO),SHF_NONE);
      eSTS cbsts = (*cbfn)(str,cbdata);
      if (cbsts) return cbsts;
    }
  }

  return STS_NORMAL; // 0
}


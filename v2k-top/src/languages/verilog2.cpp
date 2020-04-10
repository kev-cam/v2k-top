/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * verilog2_cpp_rcsid() {return "$Id: verilog2.cpp,v 1.93 2012/10/16 22:38:45 cvs Exp $";}

#include "system.h"
#include "args.h"
#include "env.h"
#include "error.h"
#include "verilog.h"
#include "language.h"
#include "poolmngr.h"
#include "strdb.h"
#include "v2k_mem.h"
#define  NEED_VERILOG
#define  NEED_CHAR
#define  NEED_VPP
#define  NEED_PUNCTUATION
#define  NEED_QUOTES
#define  NEED_OPERATORS
#define  NEED_WHITESPACE
#define  NEED_COMMENTS
#include "tokpool.h"
#include "parser.h"

const char *const ObjTyp[OBJ_TypMax+1] = {
#define OT_DECL(e,s,l,f) s,
#include "objtyp.h"
0
};

const char *const ObjTypStr[OBJ_TypMax+1] = {
#define OT_DECL(e,s,l,f) l,
#include "objtyp.h"
0
};

const eFT ObjTypFT[OBJ_TypMax+1] = {
#define OT_DECL(e,s,l,f) f,
#include "objtyp.h"
FT_Null
};

static void *saveUse(Stream *strm,const char *typ,
                     const char *nm,const char *val,int use)
{
  if (strm) strm->printf("%s%d %s\t%s\n",typ,use,nm,val);

  return 0;
}

typedef struct sPP {
  struct sPP *next;
  poolRef     name,
              file;
  int         line;
  int         args,
              size;
  Used        used;
  Token      *value;
  poolRef     names[UNSIZED(0)];

} PpDef;

typedef struct {
  int    size;
  Token *value;
} DefArg;

typedef struct sSS {
  struct sSS  *up;
  MappedFile  *mtk;
  Stream      *deps_file;
  poolRef      src_nm;
  PpDef       *def;
  DefArg      *def_args;
  int          arg,
               arg_dpth,
               line,
               stck_dpth;
  const Token *T,
              *T0,
              *TL;
  char         cache[1];

} strmStck;

typedef const char *(*nextFileFn)(const char *,int);

int verStackLimit = 255;

class VMcontext : public PrsrCntxt {
  MappedFile  *mtk;
  Token       *pream;
  int          prm_max,
               prm_indx;
  poolRef     *arglist;
  int          arg_max,
               arg_indx;
  PpDef      **ifdefs;
  int          ifmax,
               ifdepth;
  strmStck    *inc_stck;
  int          stck_dpth;
  Stream      *object,
              *pipe;
  eObjTyp      obj_typ;
  Token        obj_end;
  poolRef      obj_file;
  int          obj_line,
               obj_level;
  DefArg      *def_args;
  int          active,
               arg,
               arg_dpth;
  PpDef       *def,
              *defs;
  Stream      *deps_file;
  eVMD         vmode;
public:

  const Token *NextTok(const Token *,int);

  ~VMcontext();
  VMcontext();

  const Token *getRef(const Token *,poolRef *);
  void         newObject(poolRef,eObjTyp,Token);
  void         endObject();
  void         resetUse();
  void        *forAllUsed(useHandlr);
  void         flushDeps(int);
  int          Done();
  const Token *popStack();
  strmStck    *pushStack(const Token *);
  void         replaceDef(PpDef *);
  const Token *addDef(const Token *);
  const Token *rmDef (const Token *);
  const Token *gotoNextFile(nextFileFn,const char *,int);
  const Token *incFile(const Token *,const Token);
  const char  *incFind(Filename &,Filename &);
  const Token *countArgs(const Token *);
  int          prsVMtok(const char *,File &,File *,eVMD);
  void         reportError(eSTS,const char *,...);
  const Token *tokGetRef(const Token *,poolRef *,int,int,
                         const Token *TL = 0);
  PpDef       *findDef(poolRef Ref);
  const Token *incDef(const Token *,PpDef *,int);
  const Token *ifDef(const Token *);
  const Token *skip2nl(const Token *);
  const Token *skip2ec(const Token *);
  const Token *skip2else(const Token *);
  const Token *skip2endif(const Token *);
  const Token *incArg(const Token *,int,int);
  int          getTimescale(const char *,int *,int *);
  const Token *setTimescale(const Token *T);
  int          setLibrary(const char *);
  int          addLibrary(const char *);
  poolRef      undefUsed(poolRef,const char *);
  void         addLevel(int);
  void         addStack(strmStck *,int,int);
  void         addFile();

  inline VerilogObj *vo()        {return 0;}
  inline Token      *prm()       {return pream;};
  inline int         prm_sz()    {return prm_indx * sizeof(Token);};
  inline void        finish()    {if (object) endObject();
                                  closeDeps();}
  inline Stream     *fdeps()     {return deps_file;};
  inline void        closeDeps() {if (deps_file) {
                                    saveUse(deps_file,"X","EOF",
                                    deps_file->Name(),0);
                                    deps_file->Close(); DELETE(deps_file);}};
  inline int         complete()  {return (0 == this) || 0 == this->object;};
  inline int         depth()     {return this ? stck_dpth : 0;};
  inline void        addLevel()  {addLevel(stck_dpth);};
  inline eVMD        Vmode()     {return vmode;};

  inline int checkObject(eObjTyp t,const char *nm,Filename *f) {
    return vo()->checkObject(t,nm,f);
  };
  inline int loadObject(eObjTyp t,const char *nm,Filename *f){
    return vo()->loadObject(t,nm,f);
  };

  inline void set_deps(const char *src,const char *tok,const char *deps) {
    EnvItem *ep = 0;
    ep->resetUse();
    resetUse();
    if (deps_file) {
      saveUse(deps_file,"C",src,src,((Filename *)src)->Size());
      closeDeps();
    }
    deps_file = new BuffStream();
    deps_file->Open(deps,"w");
  }

  inline const Token *addDef(const Token *T,const Token *TL2) {
    const Token *TL1 = file_TL; file_TL = TL2;
    T = addDef(T);     file_TL = TL1; return T;
  }

  inline const Token *expandTok(const Token *T) {
    if (T && T->pi.pool == TOK_ARG) {
      return incArg(nextTok(T,0),T->at.arg,T->at.depth);
    }
    return T;
  }

  inline const Token *nextTok(const Token *T,int needed) {
    if (T && ++T >= file_TL) {
      return NextTok(T,needed);
    }
    return T;
  };

  inline const Token *rmDef(const Token *T,const Token *TL2) {
    const Token *TL1 = file_TL; file_TL = TL2;
    T = rmDef(T);      file_TL = TL1; return T;
  }

  inline void preamAdd(const Token *T) {
    if (prm_indx + 1 >= prm_max) {
      pream = (Token *)Realloc2(pream,(prm_max += 64) * sizeof(Token));
    }
    pream[prm_indx++] = *T;
  }

  inline void preamStart(const Token *T) {
    prm_indx = 0;
    if (T) preamAdd(T);
  }

  inline poolRef *args()    {return arglist;};
  inline int      argc()    {return arg_indx;};

  inline void argAdd(const poolRef R) {
    assert(arg_indx >= 0);
    if (arg_indx + 1 >= arg_max) {
      arglist = (poolRef *)Realloc2(arglist,(arg_max += 64) * sizeof(poolRef));
    }
    arglist[arg_indx++] = R;
  }

  inline void argStart(const poolRef *R) {
    arg_indx = 0;
    if (R) argAdd(*R);
  }

  inline const Token *nextNonWhiteTok(const Token *T) {
  rpt:
    T = nextTok(T,1);
    while (T->pi.pool > 0) {
      switch (T->pi.pool) {
      case WHITESPACE_POOL:
        if (WHT_NL == T->pi.index) inc_line(T);
        T = nextTok(T,0);
        continue;
      }
      return T;
    }
    if (TOK_CHAR == T->ch.tok && isspace(T->ch.ch)) goto rpt;
    return T;
  }

  inline void addLine(int l) {
    Token T[3];
    if (l > 0x7FFF) {
       T[0].xt.tok = TOK_LINE_FULL;
       T[1].as_int = l;
       object->Write((void *)T,2 * sizeof(Token));
    } else {
       T[0].xt.tok   = TOK_LINE;
       T[0].xt.extra = l;
       object->Write((void *)T,sizeof(Token));
    }
  }

  inline void addLine() {
    if (object && line != obj_line) {
      addLine(line);
      obj_line = line;
    }
  }

  inline const Token *addToken(const Token *T,int needed) {
    if (object) {
      addLine();
      object->Write((void *)T,sizeof(Token));
    }
    return nextTok(T,needed);
  };

  inline void addFileRef(poolRef src) {
    Token T[3];
    if ((src.pool|src.index) > 0x7FFF) {
       T[0].xt.tok = TOK_FILE_FULL;
       T[1].as_int = src.pool;
       T[2].as_int = src.index;
       object->Write((void *)T,3 * sizeof(Token));
    } else {
       T[0].xt.tok   = TOK_FILE;
       T[1].pi.pool  = src.pool;
       T[1].pi.index = src.index;
       object->Write((void *)T,2 * sizeof(Token));
    }
  }

  inline void discardTok() {
    if (mtk) {
      DELETE(mtk);
    } else if (file_T0) {
      if (arg || (def && !def->args)) return;
        FREE(file_T0);
    }
    file_T0 = file_TL = 0;
  };
};

static VMcontext *currCntxt;

static int endVMload(void *data)
{
  VMcontext *cntxt = (VMcontext *)data;

  return cntxt->Done();
}

VMcontext::~VMcontext()
{
  rmExitRtn(endVMload,this);
  discardTok();
  DELETE(pools);
  DELETE(pream);
  DELETE(arglist);
  DELETE(ifdefs);
  DELETE(object);
}

poolRef VMcontext::undefUsed(poolRef name,const char *where)
{
  char nm[32];
  sprintf(nm,"%d,%d",name.pool,name.index);
  saveUse(deps_file,"U",nm,where,0);
  return name;
}


const Token *VMcontext::NextTok(const Token *T,int needed)
{
  if (T && ++T >= file_TL) {
    if (needed) {
        if ((T = popStack())) return T;
      return gotoNextFile(argNextFile,0,1);
    }
    return 0;
  }
  return T;
}

void VMcontext::endObject()
{
  File m(object->Name()),
       l(vo()->libName());

  l.Expand();
  saveUse(deps_file,"O",l.str(),m,-obj_typ);
  object->Close();
  DELETE(object);

  VerilogObj *vo = new VerilogObj(&m,vmode);
}

void VMcontext::addFile()
{
  if (object) {
    Filename tmp;
    poolRef  src = srcRef();
    if (!SAME_REF(src,obj_file)) {
      addFileRef(obj_file = src);
    }
    addLine();
  }
}

void VMcontext::addStack(strmStck *stck,int lvl,int o_lvl)
{
  poolRef src = stck->src_nm;

  if (NULL_REF(src)) {
    if (lvl > o_lvl && stck->up) {
      addStack(stck->up,lvl,o_lvl);
    }
  } else {
    if (lvl > o_lvl && stck->up) {
      addStack(stck->up,lvl-1,o_lvl);
    }
    Token T;
    T.xt.tok    = TOK_INC_LEVEL;
    T.xt.extra  = lvl;
    object->Write((void *)&T,sizeof(Token));
    addFileRef(src);
    addLine(stck->line);
  }
}

void VMcontext::addLevel(int tok_dpth)
{
  if (object) {
    int inc_lvl = tok_dpth;
    if (inc_lvl > obj_level && inc_stck) {
      addStack(inc_stck,inc_lvl -1,obj_level);
      obj_file = NullRef;
    }
    Token T;
    T.xt.tok   = TOK_INC_LEVEL;
    T.xt.extra = obj_level
               = inc_lvl;
    object->Write((void *)&T,sizeof(Token));
  }
}

void VMcontext::newObject(poolRef ref,eObjTyp typ,Token keyw)
{
  String name("${" V2K_LIBRARY "}:");
  const char *nm = strDeref(ref);
  name += nm;
  name += ObjTyp[OBJ_TypeSep];
  name += ObjTyp[typ];
  File mod(cache,name,FT_Token);

  if (object) {
    Filename tmp;
    tok2src(object->Name(),tmp);
    tmp -= ObjTyp[obj_typ];
    tmp -= ObjTyp[OBJ_TypeSep];
    reportError(S_WARNING(STS_SYNTAX),"Missing '%s' for %s",
                                      tokDeref(&obj_end),tmp.str());
    endObject();
  }

  obj_typ   = typ;
  object    = new Stream;
  if (!object->Open(mod,"pw")) Exit(Error());
  obj_file  = NullRef;
  obj_line  = 0;
  obj_level = 0;
  obj_end   = keyw;
  addLevel();
  addFile();
  object->Write(prm(),prm_sz());

  saveUse(deps_file,"O",nm,vo()->libName(),typ);
}

VMcontext::VMcontext()
{
  initCntxt(sizeof(*this));
  CmnInit();
  vmode  = VMD_IEEE;
  active = 1;
  addExitRtn(endVMload,this);
}

void *VMcontext::forAllUsed(useHandlr hndlr)
{
  PpDef *def = defs;
  void  *ret = 0;

  for (; def ; def = def->next) {
    if (def->used.again) {
      TMPARR(char,val,16 + 8 * def->size + 32 * def->args);
      char  name[32],
           *vp   = val;
      int   n    = 0;
      sprintf(vp,"%d,%d:",def->args,def->size);
      while (*vp) vp++;
      while (n < def->size) {
        unsigned char *tp = (unsigned char *)&def->value[n++];
        sprintf(vp,"%02X%02X%02X%02X",tp[0],tp[1],tp[2],tp[3]);
        vp += 8;
      }
      *vp = '\0';
      for (n = 0; n < def->args ; n++) {
         sprintf(vp,";%d,%d",def->names[n].pool,
                             def->names[n].index);
         vp += strlen(vp);
      }
      sprintf(name,"%d,%d",def->name.pool,def->name.index);
      if ((ret = (*hndlr)("D",name,val,def->used.again))) break;
    }
  }

  return ret;
}

void VMcontext::resetUse()
{
  PpDef *df = defs;

  for (; df ; df = df->next) df->used.clear();
}

static void *saveUse(const char *typ,const char *nm,const char *val,int use)
{
  return saveUse(currCntxt->fdeps(),typ,nm,val,use);
}

void VMcontext::flushDeps(int done)
{
  EnvItem *ep = 0;

  ep->forAllUsed(saveUse);
  ep->resetUse();

  forAllUsed(saveUse);
  resetUse();

  if (done) closeDeps();
}

int prsVMld(const char *,eVMD,int,File *,File *);

const Token *VMcontext::gotoNextFile(nextFileFn get_name,const char *alt,int none_ok)
{
  const Token *T  = 0;
  const char  *nf = (*get_name)(alt,none_ok);

  if (nf) {
    Filename f(nf);
    File     tok,
             deps;

    discardTok();

    if (0 == prsVMld(f,vmode,0,&tok,&deps))
    {
      mtk = new MappedFile(tok);
      file_TL = &(T = file_T0 = (Token *)mtk->base())[(mtk->Size())/sizeof(Token)];

      set_deps(f.str(),tok.str(),deps.changeType(FT_Deps));

      cache     = tok.changeType(FT_Dir);
      curr_file = strSaveStr(nf);

      addLevel();
      addFile();
    }
  }

  return T;
}

void printDef(PpDef *def)
{
  int args;

  if ((args = def->args)) {
    poolRef *nm = def->names;
    Stream::Stdio(STDERR_FILENO)->printf("`define %s(",strDeref(def->name));
    while (args--) Stream::Stdio(STDERR_FILENO)->printf("%s%s",strDeref(*nm++),args ? ","
	 						                            : ")=");
  } else {
    Stream::Stdio(STDERR_FILENO)->printf("`define %s=",strDeref(def->name));
  }
  const Token *T0 = def->value,
              *TE = T0 + def->size;
  while (T0 < TE) Stream::Stdio(STDERR_FILENO)->printf("%s",tokDeref2(&T0,def->names));
  Stream::Stdio(STDERR_FILENO)->printf("\n");
}

const Token *VMcontext::popStack()
{
  if (!stck_dpth) return 0;

  strmStck    *curr = inc_stck;
  const Token *T    = 0;

  do {

    stck_dpth--;

    discardTok();

    if (def) {
      int c = def->args;
      def   = 0;
      while (c-- > 0) FREE(def_args[c].value);
      FREE(def_args);
    }

    if (curr) {

      if (deps_file) flushDeps(1);

      T         = curr->T;
      file_T0   = curr->T0;
      file_TL   = curr->TL;
      mtk       = curr->mtk;
      def       = curr->def;
      def_args  = curr->def_args;
      line      = curr->line;
      arg       = curr->arg;
      cache     = curr->cache;
      inc_stck  = curr->up;
      deps_file = curr->deps_file;
      curr_file = curr->src_nm;

      Free(curr);

      curr = inc_stck;
    }

  } while (curr && !T);

  addLevel();

  return T;
}

static const char *nextFile(const char *name,int none_ok)
{
  return name;
}

strmStck *VMcontext::pushStack(const Token *T)
{
  strmStck *up = (strmStck *)Malloc2(sizeof(strmStck)+strlen(cache));

  if (stck_dpth >= verStackLimit) {
    ExitMsg(STS_FATAL,"Exceeded include depth limit (%d)",verStackLimit);
  }

  if (deps_file) flushDeps(0);

  strcpy(up->cache,cache);          cache     = "";

  up->T         = (Token *)T;
  up->T0        = file_T0;
  up->TL        = file_TL;          file_TL   =
                                    file_T0   = 0;
  up->mtk       = mtk;              mtk       = 0;
  up->def       = def;              def       = 0;
  up->def_args  = def_args;         def_args  = 0; 
  up->up        = inc_stck;         inc_stck  = up;
  up->deps_file = deps_file;        deps_file = 0;
  up->arg       = arg;              arg       = -1;
  up->line      = line;
  up->arg_dpth  = arg_dpth;
  up->src_nm    = curr_file;
  up->stck_dpth = stck_dpth++;

  return up;
}


const char *VMcontext::incFind(Filename &quoted,Filename &f)
{
  Filename    src;
  Env         inc_path(V2K_INCLUDE);
  const char *ip,
             *sp   = inc_path;
  source(src);
  Filename    dir(src.dir());

  for (;;) {
    dir += quoted;
    if (dir.Exists()) {f = dir ; goto ok;}
    ip = sp;
    if (sp && *sp) {
      for (; *sp && !isspace(*sp) && *sp != ':' && *sp != ':'; sp++);
      strncpy(dir,ip,sp-ip)[sp-ip] = '\0';
      sp++;
    } else {
      break;
    }
    dir += OS_DIR_SEP;
  }

  return 0;
ok:
  return f.str();
}

void v2kAddIncDir(const char *path)
{
  Env    inc_path(V2K_INCLUDE);
  String xpth(inc_path);

  if (':' != xpth.lastCh()) xpth += ":";

  xpth += path;

  inc_path.Change(xpth);
}

const Token *VMcontext::incFile(const Token *T,const Token end)
{
  Filename     f,
               quoted;
  const char  *sv = "I";
  int          gt_done  = 0,
               sq_done  = 0,
               usr_done = 0;
  const char  *error    = (TOK_OP_GT.as_int       == end.as_int ||
                           TOK_PUNC_CLS_SQ.as_int == end.as_int)
                                            ? "not a standard include file: %s"
                                            : "can't find include file: %s";
  while (T) {
    if (T->as_int == end.as_int) {
      T++;
      break;
    }
    poolRef Ref;
    T       = getRef(T,&Ref);
    quoted += strDeref(Ref);
  }

  for (;;) {
    if (TOK_PUNC_CLS_SQ.as_int == end.as_int || sq_done < 0) {
      sq_done = 0;
      f  = "${" V2K_SYSINCLUDE "}";
      f += quoted;
      if (f.Exists()) { sv = "S"; goto ok; }
      if (!gt_done) gt_done = -1;
    }
    if (TOK_OP_GT.as_int == end.as_int || gt_done < 0) {
      gt_done = 1;
      f  = "/usr/include/";
      f += quoted;
      if (f.Exists()) { sv = "S"; goto ok; }
      if (!sq_done) sq_done = -1;
    } else
    if (usr_done <= 0) {
      usr_done = 1;
      f        = quoted;
      if (quoted.Exists() || incFind(quoted,f)) { goto ok; }
      if (!sq_done) sq_done = -1;
      if (!gt_done) gt_done = -1;    
    } else {
      ExitMsg(S_ERROR(STS_ENOENT),error,quoted.str());
    }
  }
 ok:
  saveUse(deps_file,sv,quoted.str(),f.str(),f.Size());
  pushStack(T);
  line = 1;
  return gotoNextFile(nextFile,f.str(),0);
}

const Token *VMcontext::incArg(const Token *T,int def_arg,int stck)
{
  strmStck *up = pushStack(T);

  while (stck != up->stck_dpth) {
    up = up->up;
  }

  arg_dpth = stck;
  arg      = def_arg + 1;

  file_T0 = up->def_args[def_arg].value;
  file_TL = file_T0 + up->def_args[def_arg].size;

  return file_T0;
}

const Token *VMcontext::incDef(const Token *T,PpDef *inc_def,int pi)
{
  int     c        = inc_def->args;
  DefArg *new_args = 0;

  prm_indx = pi;

  if (c) {
    new_args = (DefArg *)Calloc2(c,sizeof(DefArg));
    while (T && WHITESPACE_POOL == T->pi.pool) {
      if (WHT_NL == T->pi.index) inc_line(T);
      T = nextTok(T,1);
    }
    if (T->pi.pool  == PUNCTUATION_POOL &&
        T->pi.index == PUNC_OPN_BR) {
      int i  = 0,
          br = 0;
      for (;;) {
        while ((T = nextTok(T,1))) {
          if (T->pi.pool == PUNCTUATION_POOL) switch (T->pi.index) {
          case PUNC_OPN_BR: br++; break;
          case PUNC_COMMA:  if (!br)   goto next;
                            break;
          case PUNC_CLS_BR: if (!br--) goto next; 
                            break;
          }
          preamAdd(T);
        }
       next:
        if (i < c) {
	  int    sz  = prm_indx-pi;
          Token *val = (Token *)Malloc2(sz * sizeof(Token));
          BCOPY(&pream[pi],val,sz * sizeof(Token));
	  new_args[i].size  = sz;
          new_args[i].value = val;
          prm_indx          = pi;
        } else if (i == c) {
          reportError(STS_SYNTAX,"Too many arguments to macro");
        }
        i++;
        if ((T->pi.pool  == PUNCTUATION_POOL &&
             T->pi.index == PUNC_CLS_BR)) {
          if (i < c) reportError(STS_SYNTAX,"Too few arguments to macro");
          T = nextTok(T,1);
          break;
        }
      }
    }
  }

  pushStack(T);

  def_args = new_args;
  def      = inc_def;

  if (c) {
    int    t  = def->size;
    Token *t2 = (Token *)Malloc2((t+1) * sizeof(Token)),
          *t1 = def->value;
    file_T0   = t2;
    for (; t-- > 0 ; t2++) {
      *t2 = *t1++;
      if (TOK_ARG == t2->at.tok) t2->at.depth = stck_dpth;
    }
    file_TL    = t2;
    t2->at.tok = TOK_END_MACRO;
  } else {
    file_T0 = def->value;
    file_TL = file_T0 + def->size;
  }

  return file_T0;
}

const Token *VMcontext::tokGetRef(const Token *T,poolRef *Ref,
                                  int on_line, int ignr_wht,const Token *TL)
{
  Ref->pool  = 0;
  Ref->index = 0;

  if (!TL) TL = file_TL;

  if (T && T >= TL) {
    if (!(T = nextTok(0,1)) && (def ||arg)) {
      T = popStack();
    }
  }

  while (T) {
    int nl; 
    if ((Ref->pool = T->pi.pool) > 0) switch (Ref->pool) {
    case WHITESPACE_POOL: 
        switch (Ref->index = T->pi.index) {
	case WHT_NL: nl =  1; break;
	case WHT_CR: nl = -1; break;
	default:     nl = 0;
	}
        if (!ignr_wht || (on_line && nl)) return T;
        if (nl > 0) inc_line(T);
        T = nextTok(T,1);
        break;
    case QUOTES_POOL:
        switch (T->pi.index) {
        case  QUT_BACK:
          PpDef   *def;
          poolRef  Ref2;
          int          pi = prm_indx;
          const Token *T2 = nextTok(T,1);
          T = tokGetRef(T2,&Ref2,1,0);
          if ((def = findDef(Ref2))) {
            def->used.read();
            T = incDef(T,def,pi);
          } else {
            reportError(STS_SYNTAX,"[`]%s undefined",
                                   strDeref(undefUsed(Ref2,"-")));
            T = T2;
          }
          continue;
        }
    default:
        preamAdd(T);
        Ref->index =  T->pi.index;
        return nextTok(T,1);
    } else switch (Ref->pool) {
    case TOK_ARG:
      T = expandTok(T);
      continue;
    case TOK_CHAR:
      preamAdd(T);
      char buff[2];
      buff[0] = T->ch.ch;
      buff[1] = '\0';
      *Ref = strFind(buff);
      return ++T;
    case TOK_FULL_REF:
      preamAdd(T++);
      Ref->pool  = T->as_int;
      preamAdd(T++);
      preamAdd(T);
      Ref->index = T->as_int;
      return ++T;
    case TOK_POOL_SPEC: {
      tokPoolRef *ref = (tokPoolRef *)T;
      T     = rgstrPool(ref);
      int l = PoolLoad(ref->name.buff,ref->xt.extra,fmode);
      if (l != ref->xt.extra) {
        assert(0);
      }
    } ; break;
    case TOK_END_MACRO:
    default:
      assert(0);
    }
  }

  return T;
}

const Token *VMcontext::getRef(const Token *T,poolRef *Ref)
{
  return tokGetRef(T,Ref,0,1);
}

void VMcontext::reportError(eSTS sts,const char *format,...)
{
  va_list pvar;
  va_start(pvar, format);
  String msg(format,pvar);
  ErrorMsg(sts,"%s",msg.str());
  va_end(pvar);

  Filename    tmp;
  const char *src = source(tmp);

  if (!*src) {
    source(tmp);
    src = tmp.str();
  } else {
    cch2src(src,tmp);
  }

  strmStck *up = inc_stck;
  String    indent("");
  int       ln = line_no();
  PpDef    *df = def;
  int       da = arg;

  for (;;up = up->up) {
    if (df) {
      if (arg > 0) {
        ErrorMsg(STS_NORMAL,"%s @ %s:%d, doing arg. %s of `def %s",
                            indent.str(),strDeref(df->file),df->line,
                            strDeref(df->names[arg-1]),strDeref(df->name));
      } else { 
        ErrorMsg(STS_NORMAL,"%s @ %s:%d, doing `def %s",
                            indent.str(),strDeref(df->file),df->line,
                            strDeref(df->name));
      }
    } else {
      ErrorMsg(STS_NORMAL,"%s @ %s:%d",indent.str(),tmp.str(),ln);
    }

    if (!up) break;

      indent += " ";
      ln      = up->line;
      df      = up->def;
      da      = up->arg;
      cch2src(up->cache,tmp);
  }
}

inline const Token *VMcontext::countArgs(const Token *T)
{
  poolRef Ref;

  for (;;) {
    T = getRef(T,&Ref);
    if (PUNCTUATION_POOL == Ref.pool) switch (Ref.index) {
    case PUNC_CLS_BR:  return T;
    case PUNC_COMMA:   continue;
    default:           reportError(STS_SYNTAX,"Unexpected character");
                       return T;
    } else {
      argAdd(Ref);
    }
  }

  return T;
}

PpDef *VMcontext::findDef(poolRef Ref)
{
  PpDef *scan = defs;
  for (; scan ; scan = scan->next) {
    if (SAME_REF(Ref,scan->name)) return scan;
  }
  return 0;
}

const Token *VMcontext::addDef(const Token *T0)
{
  poolRef      Name,
               Ref;
  int          args,
               pi   = 0;
  PpDef      **prev,
              *def,
              *was;
  const Token *T    = getRef(T0,&Name),
              *T2;

  argStart(0);

  if (T && WHITESPACE_POOL != T->pi.pool) {
    T = getRef(T,&Ref);
    if (Ref.pool  == PUNCTUATION_POOL &&
        Ref.index == PUNC_OPN_BR) {
      const Token *T2 = countArgs(T);
      if (argc() < 0) return T;
      T = T2;
    }
  }

  while (T && WHITESPACE_POOL == T->pi.pool && WHT_NL != T->pi.index) {
    const Token *Tn = nextTok(T,0);
    if (SAME_TOK_PI(Tn,CHAR_POOL,CHR_BACKSLASH) 
	&& SAME_TOK_PI(Tn+1,WHITESPACE_POOL,WHT_NL) ) {
      Tn = nextTok(Tn+1,0);
    }
    T = Tn;
  }

  int    cmnt = 0,
         ol   = 1,
         skp  = 0,
         esc  = 0,
         c;
  Token *TS;

  for (preamStart(0),T = tokGetRef(T2 = T,&Ref,ol,1) ;;) {
    switch (cmnt) {
    case 2:  if (SAME_REF(Ref,TOK_CMT_ENDCMNT.pi))       {cmnt = 0; ol = 1;}
             goto next_tok;
    default: if (SAME_REF(Ref,TOK_CHR_BACKSLASH.pi)) {
	       esc = 1; prm_indx--;
               goto next_tok;
	     } 
             if      (SAME_REF(Ref,TOK_CMT_EOL.pi))      {cmnt = 1; T  = T2;
                                                                    goto save_def;}
             else if (SAME_REF(Ref,TOK_CMT_STRTCMNT.pi)) {cmnt = 2; ol = 0;
                                                                    goto next_tok;}
    case 1:  if (EOL_REF(Ref))                           {          goto save_def;}
             if (cmnt)                                   {          goto next_tok;}
    }
    if (SAME_REF(Ref,NullRef)) {
        break;
    } else {
      int arg = argc();
      while (arg-- > 0) if (SAME_REF(Ref,arglist[arg])) break;
      if (arg >= 0) {
        Token a;
        a.at.tok   = TOK_ARG;
        a.at.arg   = arg;
        a.at.depth = -1;
        prm_indx   = pi;
        preamAdd(&a);
      }
    }

    pi = prm_indx;

  next_tok:
    T  = tokGetRef(T2 = T,&Ref,ol && !esc,!esc);
    if (esc) {
      if (!EOL_REF(Ref)) {
	reportError(S_ERROR(STS_SYNTAX),"'\\' not last item on line");
      }
      if (WHT_CR == Ref.index) {
	T = tokGetRef(T2 = nextTok(T,1),&Ref,0,0);
	if (!SAME_REF(Ref,NlRef)) {
	  goto escpd;
	}
      }
      T = tokGetRef(T2 = nextTok(T,1),&Ref,ol,1);
    }
   escpd:
    esc = 0;
  }

save_def:
  prm_indx = pi;
  { Token e;
    e.xt.tok = TOK_END_MACRO;
    preamAdd(&e);
  }

  prev = &defs;

  while (*prev && !SAME_REF((*prev)->name,Name)) prev = &(*prev)->next;
 
  args = argc();

  if ((was = *prev)) {
    Token *nt = pream;

    if (was->args == args && was->size == pi &&
        0 == BCMP(was->value,nt,pi * sizeof(Token))) {
      was->used.reset();
      goto done;
    }

    reportError(S_WARNING(STS_SYNTAX),"`%s: redefined",strDeref(Name));

    *prev = was->next;
  }

  def       = (PpDef *)Malloc2(sizeof(PpDef) + argc() * sizeof(poolRef));
  def->name = Name;
  def->args = args;
  def->size = pi;
  def->line = line_no();
  def->file = curr_file;

  if (was) {
    def->used = was->used;
    def->used.reset();
    FREE(was->value);
    FREE(was);
  } else {
    def->used.set();
  }

  BCOPY(pream,
	TS = def->value = (Token *)Malloc(prm_indx * sizeof(Token)),
	                                  prm_indx * sizeof(Token));

  while (args-- > 0) def->names[args] = arglist[args];

  if (Arg.verbosity & VRB_DEFN) {
    printDef(def);
  }

  def->next = defs;
  defs      = def;

done:
  return T;
}

void VMcontext::replaceDef(PpDef *nwdf)
{
  PpDef   **scan = &defs,
           *def;

  for (; (def = *scan) ; scan = &def->next) {
    if (SAME_REF(nwdf->name,def->name)) {
      *scan      = nwdf;
      nwdf->next = def->next;
      FREE(def);
      return;
    }
  }

  *scan      = nwdf;
  nwdf->next = 0;
}

const Token *VMcontext::rmDef(const Token *T)
{
  poolRef   Name;
  PpDef   **scan = &defs,
           *def;

  T = getRef(T,&Name);

  for (; (def = *scan) ; scan = &def->next) {
    if (SAME_REF(Name,def->name)) {
      *scan = def->next;
      FREE(def->value);
      FREE(def);
      break;
    }
  }

  return T;
}

extern "C" int prsDefine(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  char       *fmt = (char *)var,
             *ap  = strchr(fmt,'$');
  const char *def = *(*argv)++;
                   --(*argc);
  int         sts = STS_NORMAL,
              len = strlen(def),
              l1  = ap - fmt;

  TMPARR(char,buf,len);

  while (strncmp(def,fmt,l1)) def++;

  def += l1;
  strcpy(buf,def);
  l1   = strlen(&fmt[l1 +1]);
  len  = strlen(buf) - l1;
  strcpy(&buf[len],"\n");

  if ((ap = strchr(buf,'='))) {
    *ap++ = ' ';
  } else {
    sts   = STS_BAD_ARG;
  }

  InitLang(defPoolMode,0);

  if (!currCntxt) { currCntxt = new VMcontext; }

  Token *tok = 0;
  sts        = tokVerilogStr(buf,(void **)&tok,&len,currCntxt->Vmode());

  if (tok) {
    currCntxt->addDef(tok,&tok[len]);
    FREE(tok);
  }

  return sts;
}

extern "C" int addPliPath(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  const char *eq = argEquals(argc,argv);

  if (!eq) return STS_MSSNG_ARG;

  Env    pli_path(V2K_PLI_PATH);
  String pp(pli_path);

  pp += ":";
  pp += eq;

  pli_path.Change(pp);

  return STS_NORMAL;
}

extern "C" int prsInclude(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  char       *fmt = (char *)var,
             *ap  = strchr(fmt,'$');
  const char *def = *(*argv)++;
                   --(*argc);
  int         sts = STS_NORMAL,
              len = strlen(def),
              l1  = ap - fmt;

  TMPARR(char,buf,len);

  while (len-- > 0 && strncmp(def,fmt,l1)) def++;

  if (len <= 0) return STS_BAD_ARG;

  def += l1;
  strcpy(buf,def);
  l1       = strlen(&fmt[l1 +1]);
  len      = strlen(buf) - l1;
  buf[len] = '\0';

  v2kAddIncDir(buf);

  return sts;
}

extern "C" int prsUndef(int *argc,const char ***argv,void *var,int pls = 0,int mns = 1)
{
  const char *fmt = (char *)var,
             *ap  = strchr(fmt,'$'),
             *def = *(*argv)++;
                   --(*argc);
  int         sts = STS_NORMAL,
              len = strlen(def),
              l1  = ap - fmt;

  TMPARR(char,buf,len);

  while (len-- > 0 && strncmp(def,fmt,l1)) def++;

  if (len <= 0) return STS_BAD_ARG;

  def  += l1;
  strcpy(buf,def);
  l1    = strlen(&fmt[l1 +1]);
  len   = strlen(buf) - l1;
  strcpy(&buf[len],"\n");

  InitLang(defPoolMode,0);

  if (currCntxt) {
    Token *tok = 0;
    sts        = tokVerilogStr(buf,(void **)&tok,&len,currCntxt->Vmode());

    if (tok) {
      currCntxt->rmDef((Token *)tok,&tok[len]);
      FREE(tok);
    }
  }

  return sts;
}

const Token *VMcontext::skip2else(const Token *T)
{
  T = nextTok(T,0);

  while (T) {
    poolRef Ref;

    if (T->pi.pool == VPP_POOL) switch(T->pi.index) {
    case VPP_ENDIF:    return T;
    case VPP_ELSE:     return nextTok(T,0);
    case VPP_IFDEF:    T = skip2endif(T); continue;
    }else if (T->pi.pool == COMMENTS_POOL) switch(T->pi.index) {
    case CMT_EOL:      T = skip2nl(T);    continue;
    case CMT_STRTCMNT: T = skip2ec(T);    continue;
    } else if (T->pi.pool == WHITESPACE_POOL) switch(T->pi.index) {
    case WHT_NL:       inc_line(T);
    } else if (T->pi.pool == TOK_FULL_REF) {
      Ref.pool  = (++T)->as_int;
      Ref.index = (++T)->as_int;
    }

    T = nextTok(T,1);
  }

  return T;
}

const Token *VMcontext::skip2nl(const Token *T)
{
  T = nextTok(T,0);

  while (T) {
    if (T->pi.pool == WHITESPACE_POOL) switch(T->pi.index) {
    case WHT_NL: inc_line(T);
                 return nextTok(T,0);
    }
    T = nextTok(T,0);
  }

  return T;
}

const Token *VMcontext::skip2ec(const Token *T)
{
  T = nextTok(T,0);

  while (T) {
    if (T->pi.pool == COMMENTS_POOL) switch(T->pi.index) {
    case CMT_ENDCMNT: return nextTok(T,0);
    } else if (T->pi.pool == WHITESPACE_POOL) switch(T->pi.index) {
    case WHT_NL:      inc_line(T);
    }

    T = nextTok(T,0);
  }

  return T;
}

const Token *VMcontext::skip2endif(const Token *T)
{
  T = nextTok(T,0);

  while (T) {
    if (T->pi.pool == VPP_POOL) switch(T->pi.index) {
    case VPP_ENDIF:    return nextTok(T,0);
    case VPP_IFDEF:    T = skip2endif(T); continue;
    } else if (T->pi.pool == COMMENTS_POOL) switch(T->pi.index) {
    case CMT_EOL:      T = skip2nl(T);    continue;
    case CMT_STRTCMNT: T = skip2ec(T);    continue;
    } else if (T->pi.pool == WHITESPACE_POOL) switch(T->pi.index) {
    case WHT_NL:       inc_line(T);
    }
    T = nextTok(T,0);
  }

  return T;
}

const Token *VMcontext::ifDef(const Token *T)
{
  poolRef  Name;
  PpDef   *def;

  T = getRef(T,&Name);

  if (ifdepth >= ifmax) {
    ifmax += 64;
    ifdefs = (PpDef **)Realloc2(ifdefs,ifmax * sizeof(PpDef *));
  }

  ifdefs[ifdepth++] = def = findDef(Name);

  if (def) def->used.read();
  else     undefUsed(Name,"ifdef");
  return T;
}

int VMcontext::getTimescale(const char *cp0,int *i,int *e)
{
  char       *cp,
              exp[4];
  const char *ccp = cp0;
  int         n   = 0;

  while (isspace(*ccp)) ccp++;
  while (isdigit(*ccp)) n = (10 * n) + *ccp++ - '0';
  if (!(*i = n)) goto err;
  strncpy(exp,ccp,3);
  exp[3] = 0;
  for (cp = exp; isalpha(*cp); cp++);
  *cp = '\0';
       if (0 == strcmp(exp,"fs")) *e = -15;
  else if (0 == strcmp(exp,"ps")) *e = -12;
  else if (0 == strcmp(exp,"ns")) *e = -9;
  else if (0 == strcmp(exp,"us")) *e = -6;
  else if (0 == strcmp(exp,"ms")) *e = -3;
  else if (0 == strcmp(exp, "s")) *e = 0;
  else {
  err:
    *e = 0;
    reportError(STS_SYNTAX,"`timescale: %s - is invalid",cp0);
  }

  return n;
}

int VMcontext::addLibrary(const char *lib)
{
  return vo()->addLibrary(lib);
}


int VMcontext::setLibrary(const char *lib)
{
  return vo()->setLibrary(lib);
}

const Token *VMcontext::setTimescale(const Token *T)
{
  poolRef     Ref;
  String      str;
  const char *cp;
  int         ts1,ts2,
              pr1,pr2;
  do {
    T = tokGetRef(T,&Ref,1,1);
    str += strDeref(Ref);
  } while (WHITESPACE_POOL < Ref.pool);

  cp = str;
  getTimescale(cp,&ts1,&ts2);
  if ((cp = strchr(cp,'/'))) {
    getTimescale(++cp,&pr1,&pr2);
  }

  vo()->setTimescale(ts1,ts2,pr1,pr2);

  return T;
}

int VMcontext::prsVMtok(const char *src,File &tok,File *deps,eVMD md)
{
  poolRef      Ref,
               old_curr = curr_file;
  const Token *T;
  int          l,
               old_line = line;
  tokPoolRef  *tpr;
  Filename     tmp;

  vmode = md;
  mtk   = new MappedFile(tok);

  set_deps(src,tok.str(),deps->str());

  curr_file = strSaveStr(tok2src(tok,tmp));
  line      = 1;

  if (!mtk->base()) Exit(mtk->Error());

  file_TL = &(T = file_T0 = (Token *)mtk->base())[(mtk->Size())/sizeof(Token)];

  if (Arg.show_tok) {
    tokReconInMem(T,file_TL-file_T0,stdout);
  }

  while (T || (T = popStack())) {
    switch (T->pi.pool) {
      case WHITESPACE_POOL:
        switch (T->pi.index) {
	case WHT_NL:;
          inc_line(T);
          /* drop thru */
        default:
          T = nextTok(T,0);
          continue;
        }
        break;
      case QUOTES_POOL:
        switch (T->pi.index) {
        case  QUT_BACK:
          PpDef *def;
          int          pi = prm_indx;
          const Token *T2 = nextTok(T,1);
          T = tokGetRef(T2,&Ref,1,0);
          if ((def = findDef(Ref))) {
            def->used.read();
            T = incDef(T,def,pi);
          } else {
            reportError(STS_SYNTAX,"[`]%s undefined",
                                   strDeref(undefUsed(Ref,"-")));
            T = T2;
          }
          continue;
        }
        break;
      case VPP_POOL:
        switch (T->pi.index) {
          case VPP_DEFINE:
            T = addDef(nextTok(T,1));
            continue;
          case VPP_UNDEF:
            T = rmDef(nextTok(T,1));
            continue;
          case VPP_IFDEF:
            T = ifDef(nextTok(T,1));
            if (!(active = 0 != ifdefs[ifdepth-1])) T = skip2else(T);
            continue;
          case VPP_ENDIF:
            if (ifdepth > 0) ifdepth--;
            else             reportError(STS_SYNTAX,"Extraneous `endif");
            T = nextTok(T,0);
            continue;
          case VPP_IFNDEF:
            T = ifDef(nextTok(T,1));
            if (!(active = 0 == ifdefs[ifdepth-1])) T = skip2else(T);
            continue;
          case VPP_ELSE:
            if (ifdepth > 0) ifdepth--;
            else             reportError(STS_SYNTAX,"Dangling `else");
            T = skip2endif(T);
   	    continue;
          case VPP_INCLUDE:
            T = nextNonWhiteTok(T);
            if (T->as_int == TOK_QUT_DOUBLE.as_int) {
              T = incFile(nextTok(T,1),TOK_QUT_DOUBLE);
            } else if (T->as_int == TOK_PUNC_OPN_SQ.as_int) {
              T = incFile(nextTok(T,1),TOK_PUNC_CLS_SQ);
            } else if (T->as_int == TOK_OP_LT.as_int) {
              T = incFile(nextTok(T,1),TOK_OP_GT);
            }
  	    continue;
	  case VPP_DEFAULT_DISCIPLINE:
            T = tokGetRef(T,&Ref,1,1);
            vo()->setDefaultDisc(Ref);
            if (NULL_REF(Ref)) {
              reportError(STS_SYNTAX,
                          "Missing discipline for `default_discipline");
            }
            continue;
	  case VPP_DEFAULT_NETTYPE:
            T = tokGetRef(T,&Ref,1,1);
            vo()->setDefaultNetType(Ref);
            if (NULL_REF(Ref)) {
              reportError(STS_SYNTAX,
                          "Missing net-type for `default_nettype");
            }
            continue;
	  case VPP_NOUNCONNECTED_DRIVE:
            vo()->setUnconnected(NullRef);
            T = nextTok(T,0);
            continue;
	  case VPP_UNCONNECTED_DRIVE:
            T = tokGetRef(T,&Ref,1,1);
            vo()->setUnconnected(Ref);
            if (NULL_REF(Ref)) {
              reportError(STS_SYNTAX,
                          "Missing pull for `unconnected_drive");
            }
            continue;
	  case VPP_CELLDEFINE:
            vo()->setCellDefine(1);
            T = nextTok(T,0);
            continue;
	  case VPP_ENDCELLDEFINE:
            vo()->setCellDefine(0);
            T = nextTok(T,0);
            continue;
	  case VPP_PROTECT:
            vo()->setProtect(1);
            T = nextTok(T,0);
            continue;
	  case VPP_ENDPROTECT:
            vo()->setProtect(0);
            T = nextTok(T,0);
            continue;
          case VPP_PROTECTED:
            reportError(STS_NIY,"`Protected not implemented yet");
            do {
              T = nextTok(T,1);
              T = expandTok(T);
            } while (T && (T->pi.index != VPP_ENDPROTECTED ||
                           T->pi.pool  != VPP_POOL));
	    T = nextTok(T,0);
            continue;
	  case VPP_TIMESCALE:
            T = setTimescale(nextTok(T,0));
            continue;
  	}
        break;
      case VERILOG_POOL:
        switch (T->pi.index) {
          Token   end_tok;
          eObjTyp typ;
          case VER_PRIMITIVE:   typ     = OBJ_Primitive;
                                end_tok = TOK_VER_ENDPRIMITIVE;
                                goto start_obj;
          case VER_FUNCTION:    if (!object) {
                                  typ     = OBJ_Function;
                                  end_tok = TOK_VER_ENDFUNCTION;
                                  goto start_obj;
	                        }
                                break;
          case VER_TASK:        if (!object) {
                                  typ     = OBJ_Task;
                                  end_tok = TOK_VER_ENDTASK;
                                  goto start_obj;
	                        }
                                break;
          case VER_MODULE:      typ     = OBJ_Module;
                                end_tok = TOK_VER_ENDMODULE;
                                goto start_obj;
          case VER_MACROMODULE: typ     = OBJ_Module;
                                end_tok = TOK_VER_ENDMODULE;
                                goto start_obj;
          case VER_DISCIPLINE:  typ     = OBJ_Discipline;
                                end_tok = TOK_VER_ENDDISCIPLINE;
                                goto start_obj;
          case VER_NATURE:      typ     = OBJ_Nature;
                                end_tok = TOK_VER_ENDNATURE;
                                goto start_obj;
	start_obj:
            preamStart(T);
  	    T = getRef(nextTok(T,1),&Ref);
  	    newObject(Ref,typ,end_tok);
  	    continue;
          case VER_ENDTASK:
          case VER_ENDFUNCTION:
            if (object && OBJ_Module == obj_typ) goto add;
          case VER_ENDDISCIPLINE:
          case VER_ENDMODULE:
          case VER_ENDNATURE:
          case VER_ENDPRIMITIVE:
            if (object) {
              T = addToken(T,0);
              endObject();
              continue;
            } else {
              reportError(S_ERROR(STS_SYNTAX),
                          "Unexpected keyword (%s)",tokDeref(T));
              T = nextTok(T,0);
            }
  	    continue;
  	}
        break;
      case COMMENTS_POOL:
        switch (T->pi.index) {
 	  case CMT_EOL:      T = skip2nl(T); continue;
	  case CMT_STRTCMNT: T = skip2ec(T); continue;
  	}
        break;
      case TOK_CHAR:
        if (isspace(T->ch.ch)) {
          T = nextTok(T,0);
        } else {
          assert(T->ch.count);
          T = addToken(T,0);
        }
        continue;
      case TOK_FULL_REF:
        T = addToken(T,1);
        T = addToken(T,1);
        T = addToken(T,0);
        continue;
      case TOK_POOL_SPEC:
        T = rgstrPool(tpr = (tokPoolRef *)T);
        l = PoolLoad (tpr->name.buff,tpr->xt.extra,fmode);
        assert(l == tpr->xt.extra);
        continue;
      case TOK_ARG:
        T = expandTok(T);
        continue;
    }
  add:
    T = addToken(T,0);
  }

  curr_file = old_curr;
  line      = old_line;

  if (deps && !object) {
    flushDeps(1);
  }

  return STS_NORMAL;
}

void prsVMdone()
{
  if (currCntxt) currCntxt->finish();
}

int VMcontext::Done()
{
  while(popStack());
  if (!object) {
    assert(currCntxt == this);
    DELETE(currCntxt);
  }
  return 0;
}

int prsVMloadTok(const char *src,File &tok,File *deps,eVMD mode)
{
  if (!currCntxt) { currCntxt = new VMcontext(); }

  currCntxt->setCache(tok.str());

  return currCntxt->prsVMtok(src,tok,deps,mode);
}

typedef struct sTmpEnv {
  struct sTmpEnv *next;
  int             flgs;
  char           *val,
                  nmv[UNSIZED(0)];
} TmpEnv;

typedef struct sObjRef {
  struct sObjRef *next;
  eObjTyp         typ;
  char           *lib,
                 *name;
  Filename        file;
} ObjRef;

static struct {
  ObjRef *Stack,
         *Defs,
        **DefNext;
  int     StackDepth,
          StackSize;
}
  Obj = {0,0,&Obj.Defs};

static PpDef   *testDefs;
static int      skipArgs;
static TmpEnv  *testEnv;

int resetTest(int load)
{
  int     ok    = load,
          done  = 0;
  PpDef  *def,
         *next  = testDefs;
  ObjRef *obj,
         *nobj  = Obj.Defs;
  int     n     = Obj.StackSize;
  TmpEnv *te,
         *e_nxt = testEnv;

  while (n-- > 0) {
    if (Obj.Stack[n].typ) {
      if (Arg.verbosity & VRB_MAKE) {
        Stream::Stdio(STDERR_FILENO)->printf("Partial object: %s\n",Obj.Stack[n].name);
      }
      Obj.Stack[n].typ = OBJ_Null;
      ok               = 0;
      FREE(Obj.Stack[n].name);
    }
  }
  Obj.StackSize = n;

  while ((obj = nobj)) {
    nobj = obj->next;
    if (load && ok) {
      if ((ok = currCntxt->loadObject(obj->typ,obj->name,&obj->file))) {
        if (Arg.verbosity & VRB_MAKE) {
          Stream::Stdio(STDERR_FILENO)->printf("Loaded %s %s\n",
                                       ObjTypStr[obj->typ],obj->name);
        }
      } else {
        if (done) {
          ExitMsg(S_FATAL(STS_OUTOFDATE),
                  "Database corrupt. Rerun with -make=-A (%s %s)",
                  ObjTypStr[obj->typ],obj->name);
        } else if (Arg.verbosity & VRB_MAKE) {
          Stream::Stdio(STDERR_FILENO)->printf("Database corrupt (%s %s)\n",
                                       ObjTypStr[obj->typ],obj->name);
        }
      }
      done++;
    }
    FREE(obj->name);
    free(obj);
  }
  *(Obj.DefNext = &Obj.Defs) = 0;

  while ((def = next)) {
    next = def->next;
    if (load && ok) {
      currCntxt->replaceDef(def);
    } else {
      FREE(def->value);
      free(def);
    }
  }
  testDefs = 0;

  while ((te = e_nxt)) {
    e_nxt = te->next;
    if (load && ok) {
      EnvItem *ep = 0;
      ep->add(te->nmv,te->val,te->flgs);
    }
    free(te);
  }
  testDefs = 0;

  if (skipArgs) {
    if (ok) {
      prsVerSkipArg = skipArgs;
    }
    skipArgs = 0;
  }

  return ok;
}

int loadFromVDeps(const char *src_cache,const char *deps)
{
  if (!currCntxt) { currCntxt = new VMcontext(); }

  currCntxt->setCache(src_cache);

  return resetTest(1);
}

PpDef *findAnyDef(poolRef Ref)
{
  PpDef *scan = testDefs;

  for (; scan ; scan = scan->next) {
    if (SAME_REF(Ref,scan->name)) return scan;
  }

  if (currCntxt) return currCntxt->findDef(Ref);

  return 0;
}

const char *findAnyEnv(const char *name)
{
  TmpEnv *te = testEnv;

  for (; te ; te = te->next) {
    if (0 == strcmp(name,te->nmv)) return te->val;
  }

  Env tmp;

  if (tmp.get(name)) {
    return tmp;
  }

  return 0;
}

int vDepsOK(Stream *deps,int make,int depth)
{
  String      ds,
              obj;
  int         more,
              ch,
              use,
              s,
              sts = STS_NORMAL;
  PpDef       def,
             *pdf;

  do {
    more = deps->gets(&ds);
    const char *lp = ds.str();

    s = ds.len();
    if ('\n' == lp[s-1]) ds.cut(s-1);

    switch (ch = *lp) {
    case 'C':
    case 'S':
    case 'I':  s  = 0;
               while(isdigit(*++lp)) s = s * 10 + *lp - '0';
               ds.shift(++lp - ds.str());
               for (lp = ds.str(); *lp && *lp++ != '\t' ;);
               if (((Filename *)lp)->Exists()) {
                 Filename src(lp);
                 ds.cut((lp - ds.str()) -1);
                 if ('C' == ch) {
                   int a = checkNextArg(ds.str(),src.str());
                   if (a) {
                     skipArgs++;
                     goto next;
                   }
                   if (Arg.verbosity & VRB_MAKE) {
                     Stream::Stdio(STDERR_FILENO)->printf(
                            "Continuing file mismatch: %s\n",ds.str());
                   }
                   goto bad;
                 }
                 PrsrCntxt::encode(src);
                 File tok("${V2K_REPOSITORY}/cache/",src,FT_Token);
                 if (tok.Date() <= ((Filename *)lp)->Date()) {
                   if (Arg.verbosity & VRB_MAKE) {
                     Stream::Stdio(STDERR_FILENO)->printf("New file: %s\n",lp);
                   }
                 } else {
                   int      nd = depth + (ch != 'C');
                   Filename deps_name;
                   sprintf(deps_name,"${V2K_LIBRARY}:%s-%d",src.str(),nd);
                   File deps("${V2K_REPOSITORY}/cache/",deps_name,FT_Deps);
                   if (deps.Exists()) {
                     Stream chk;
                     if (chk.Open(deps,"r")) {
                       if ((sts = vDepsOK(&chk,make,nd)) || ch == 'C') {
                         goto done;
                       }
                       goto next;
                     } else {
                       ErrorMsg(S_WARNING(Error()),"can't open %s",deps.str());
                     }
                   } else if (Arg.verbosity & VRB_MAKE) {
                     Stream::Stdio(STDERR_FILENO)->printf("No info for: %s\n",lp);
                   }
                 }
               } else {
                 if (Arg.verbosity & VRB_MAKE) {
                   Stream::Stdio(STDERR_FILENO)->printf("Can't find Include: %s\n",lp);
                 }
               }
               goto bad;
    case 'e':
    case 'E':  if (1 ==  sscanf(lp+1,"%d",&use)) {
                 int flgs = (ch == 'e') ? ENV_LOCAL
		                        : ENV_EXPORT;
                 for (s = 2; (ch = lp[s++]) && !isspace(ch););
                 ds.shift(s);
                 for (s = 0; lp[s] && !isspace(lp[s]); s++);
                 String val(&lp[s+1]);
                 ds.cut(s);
                 const char *vp = findAnyEnv(ds.str());
                 if (vp) {
                   if (0 == strcmp(val.str(),vp)) goto next;
                 }

                 if (!(use & USE_ST_1ST)) {
                   if (Arg.verbosity & VRB_MAKE) {
                     Stream::Stdio(STDERR_FILENO)->printf(
                                 "Environment changed: %s\n",ds.str());
                   }
                   goto bad;
                 }

                 TmpEnv *te = MALLOC2_N(sizeof(TmpEnv)
                                        + SIZED(char,2 + ds.len() + val.len()),
                                        TmpEnv);
                 strcpy(te->nmv,ds.str());
                 strcpy(te->val = &te->nmv[1 + ds.len()],val.str());
                 te->flgs = flgs;
                 te->next = testEnv;
                 testEnv  = te;
                 goto next;
               }
               assert(("Unrecognized environment data in dependencies file."));
               break;
    case 'U':  if (3 == sscanf(lp,"U%d %d,%d",
                                  &use,&def.name.pool,&def.name.index)) {
                 if (findAnyDef(def.name)) {
                   if (Arg.verbosity & VRB_MAKE) {
                       Stream::Stdio(STDERR_FILENO)->printf(
                               "New Definition: %s\n",strDeref(def.name));
                   }
                   goto bad;
                 }
                 goto next;
               }
               assert(("Unrecognized define data in dependencies file."));
               goto bad;
    case 'D':  if (5 == sscanf(lp,"D%d %d,%d %d,%d:",
                                  &use,&def.name.pool,&def.name.index,
			               &def.args,&def.size)) {
                 while (*lp && *lp++ != ':');
                 if (*lp) {
                   TMPARR(Token,tkns,def.size);
                   unsigned char *dp = (unsigned char *)tkns;
                   int            ch1,
                                  ch2;
                   for (s = sizeof(tkns); s-- > 0 && (ch1 = *lp++)
                                                  && (ch2 = *lp++);) {
                     ch1 = isalpha(ch1) ? (ch1 - 'A') + 10 : ch1 - '0';
                     ch2 = isalpha(ch2) ? (ch2 - 'A') + 10 : ch2 - '0';
                     *dp++ = (ch1 << 4) + ch2;
                   }
                   assert(("Corrupt Definition in Deps",s < 0));
                   if ((pdf = findAnyDef(def.name))) {
                     if (pdf->args == def.args &&
                         pdf->size == def.size &&
                         0 == BCMP(tkns,pdf->value,sizeof(tkns))) {
                       goto next;
                     }
                   }

                   if (!(use & USE_ST_1ST)) {
                     if (Arg.verbosity & VRB_MAKE) {
                       Stream::Stdio(STDERR_FILENO)->printf(
                                "Definition changed: %s\n",strDeref(def.name));
                     }
                     goto bad;
                   }

                   pdf       = (PpDef *)Malloc2(sizeof(def) +
                                                def.args * sizeof(poolRef));
                   *pdf      = def;
                   pdf->next = testDefs;
                   testDefs  = pdf;
                   BCOPY(tkns,
                         pdf->value = (Token *)Malloc2(sizeof(tkns)),
                         sizeof(tkns));
                   for (s = 0; s < def.args ; s++) {
                      assert (';' == *lp++ &&
                                2 == sscanf(lp,"%d,%d",&pdf->names[s].pool,
                                                       &pdf->names[s].index));
                      while (*++lp && ';' != *lp);
                   }
                   goto next;
		 }
               }
               assert(("Unrecognized define data in dependencies file."));
               break;
    case 'O':  if ('-' == lp[1]) {
                 assert(Obj.StackDepth > 0);
                 ObjRef *por       = MALLOC2(ObjRef);
                 *por              = Obj.Stack[s = --Obj.StackDepth];
                 Obj.Stack[s].typ  = OBJ_Null;
                 Obj.Stack[s].lib  = 0;
                 Obj.Stack[s].name = 0;
                 *Obj.DefNext      = por;
                 por->next         = 0;
                 *(Obj.DefNext     = &por->next) = 0;
               } else {
                 ObjRef *por;
                 s = Obj.StackDepth++;
                 if (Obj.StackDepth > Obj.StackSize) {
                   REALLOC2(Obj.Stack,Obj.StackDepth,ObjRef);
                   BZERO(por = &Obj.Stack[s],sizeof(*por));
                 } else {
                   por = &Obj.Stack[s];
                   FREE(por->name);
                   por->file = "";
                 }
                 assert(1 == sscanf(lp,"O%d",&s));
                 if ((lp = strchr(lp,'\t'))) {
                   strcpy(por->file,lp);
                 }
                 ds.shift(3);
                 ds.cut(strchr(ds.str(),'\t') - ds.str());
                 switch(por->typ = (eObjTyp)s) {
                 case OBJ_Module:
                 case OBJ_Primitive:
                   if (!currCntxt->checkObject(por->typ,ds.str(),&por->file)) {
                     if (Arg.verbosity & VRB_MAKE) {
                       Stream::Stdio(STDERR_FILENO)->printf(
                                   "%s not valid: %s\n",ObjTypStr[s],ds.str());
                     }
                     goto bad;
                   }
                   strcpy(por->name = MALLOC2_N(ds.len()+1,char),ds.str());
                   break;
                 case OBJ_Function:
                 case OBJ_Task:
                 case OBJ_Nature:
                 case OBJ_Discipline:
                   if (Arg.verbosity & VRB_MAKE) {
                     Stream::Stdio(STDERR_FILENO)->printf(
                                   "%s not saved: %s\n",ObjTypStr[s],ds.str());
                   }
                   goto bad;

                 default: assert("Unkrecognized object type" == 0);
                 }
               }
               break;
    default:   assert("Unrecognized data in dependencies file." == 0);
    case 'X':  goto done;
    next:
    case '\0':;
    }
  } while(more >= 0);

  if (Arg.verbosity & VRB_MAKE) {
    Stream::Stdio(STDERR_FILENO)->printf("Premature end: %s\n",deps->Name());
  }

  sts = STS_TRUNC;

done:
  deps->Close();
  return sts;

bad:
  resetTest(0);
  sts = STS_OUTOFDATE;
  goto done;
}

int prsVMld(const char *file,eVMD mode,int load, File *rtok,File *rdeps)
{
  Filename  src(file);
  File      f(file);
  char     *sp   = src;
  int       make = V2kMake;

  envExpand(src,sizeof(src));

  if (!src.Exists()) {
    return ErrorMsg(S_ERROR(STS_BAD_ARG),"File missing - %s",file);
  }

  U64 mtime = (make & MAKE_FILE) ? src.Date()
                                 : 0;
  currCntxt->encode(src);

  int      depth = currCntxt->depth();
  Filename deps_name;
  sprintf(deps_name,"${V2K_LIBRARY}:%s-%d",src.str(),depth);

  File tok("${V2K_REPOSITORY}/cache/",src,FT_Token);
  File deps("${V2K_REPOSITORY}/cache/",deps_name,FT_Deps);

  if (!mtime || (tok.Date() <= mtime)) {
    Stream  in,out;
    File   *fp;

    if (Arg.verbosity & VRB_MAKE) {
      Stream::Stdio(STDERR_FILENO)->printf("Tokenizing: %s\n",sp);
    }

    if (in.Open (fp = &f,"r") && out.Open(fp = &tok,"pw")) {
      deps.Unlink();
      tokVerilog(&in,&out,mode);
      in.Close();
      out.Close();
    } else {
      ExitMsg(S_ERROR(Error()),"can't open %s",fp->str());
    }

    make = MAKE_NONE;
  }

  if (rtok)  *rtok  = tok.str();
  if (rdeps) *rdeps = deps.str();

  if ((make & MAKE_DEPS) && deps.Exists()) {
    Stream chk;
    if (chk.Open(deps,"r")) {
      if (0 == vDepsOK(&chk,make,depth) && loadFromVDeps(tok.str(),deps.str())) {
        return STS_ALREADY;
      }
      chk.Close();
    } else {
      ErrorMsg(S_WARNING(Error()),"can't open %s",deps.str());
    }
  }

  if (load) return prsVMloadTok(file,tok,&deps,mode);

  return 0;
}

extern "C" int prsVM(const char *file,eVMD mode)
{
  return prsVMld(file,mode,1,0,0);
}

int tokVerilog (Stream *in,Stream *out,eVMD mode)
{
  return tokVerilog(in->Fp(),out->FwriteFn(),out->Fp(),mode);
}

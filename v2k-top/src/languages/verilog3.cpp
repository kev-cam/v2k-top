/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * verilog3_cpp_rcsid() {return "$Id: verilog3.cpp,v 1.143 2012/10/16 22:38:45 cvs Exp $";}

#define VERILOG3

#include "system.h"
#include "args.h"
#include "env.h"
#include "error.h"
#include "language.h"
#include "poolmngr.h"
#include "strdb.h"
#include "v2k_mem.h"
#include "v2k.h"
#define  NEED_FUNC
#define  NEED_VPP
#define  NEED_CPP
#define  NEED_TASKS
#define  NEED_SYSFUNC
#define  NEED_MATH
#define  NEED_CHAR
#define  NEED_QUOTES
#define  NEED_VERILOG
#define  NEED_OPERATORS
#define  NEED_WHITESPACE
#define  NEED_PUNCTUATION
#define  NEED_SPECIAL
#define  NEED_GLOBAL
#define  NEED_SIGNAL
#define  NEED_ANALYSIS
#define  NEED_TIMING
#define  NEED_LABELS
#include "tokpool.h"
#include "verilog.h"
#include "dump.h"

template class sortedList<Usage, true>;

VDataC VerilogObj::VD;

int V2kMinimize = 1;

const poolRef InitVal  = {SPECIAL_POOL,SPC_INITIAL},  //!< Sys. Attribute
              Specify  = {SPECIAL_POOL,SPC_SPECIFY},
              Global   = {SPECIAL_POOL,SPC_GLOBAL},
              Generate = {SPECIAL_POOL,SPC_GENERATE};

VerilogObj::~VerilogObj()
{
}

deAliaser *Expr::deAliasers;

static eDIFF cmpUsage(void *v1,void *v2)
{
  Usage *u1   = (Usage *)v1,
        *u2   = (Usage *)v2;

  return u1->xpr->diff(u2->xpr);
}

bool fixUsageXpr(Expr *alsd)
{
  Stmt *stmt = NullVobj->vd()->process;
  bool  ret  = 0;

  if (stmt) {
    Usage *pd,
           tmp;

    tmp.xpr = alsd;

    if ((pd = stmt->usage()->findord(&tmp,cmpUsage))) {
      stmt->usage()->dirty();
      pd->ignr = 1;
      ret      = 1;
      DELETE(pd->xpr);
    }

    tmp.xpr = 0;
  }

  return ret;
}

void VerilogObj::Init() //!< Initialize Verilog Parser
{
  if (vd()->init) return;

  vd()->init = 1;

  InitLists(vd()->lists,LST_MAX);

  Root()->pushScope(0,Global);

  logGlobals(REF_SIGNAL,   SIG_tokens);
  logGlobals(REF_SYS_TASK, TSK_tokens);
  logGlobals(REF_SYS_FUNC, SFN_tokens);
  logGlobals(REF_FUNC,     MTH_tokens);
  logGlobals(REF_FUNC,     FNC_tokens);

  memset(vd()->hshdInUse,-1,sizeof(vd()->hshdInUse));

  setLibrary(0);

  ((Expr *)0)->addDeAlias(fixUsageXpr);
}

VerilogObj::VerilogObj(const File *f,eVMD vmode)
{
  MappedFile   mtk(f->str());
  const Token *T0  = (Token *)mtk.base(),
              *TL  = T0 + mtk.Size()/sizeof(Token);
  Task        *tsk;
  Func        *fn;

  vd()->vmode = vmode;

  Init();

  if (T0) {
    InfoRef  Ref;
    dumper  *dmp = (dumper *)DumpList; // ???

    BZEROS(vd()->posn);
    T0 = getRef(T0,TL,&Ref);
    assert(VERILOG_POOL == Ref.pool);

    Expr::CheckInUse = true;

    switch (Ref.index) {
    case VER_DISCIPLINE: {
      Disc *disc = new Disc(T0,TL,0);
      for (; dmp ; dmp = dmp->Next()) if (dmp->Flags() & DMP_DISCIPLINE) {
        dmp->dump(disc);
      }
      break;
    }
    case VER_NATURE: {
      Nature *nat = ReadNature(T0,TL,-1);
      for (; dmp ; dmp = dmp->Next()) if (dmp->Flags() & DMP_NATURE) {
        dmp->dump(nat);
      }
      break;
    }
    case VER_PRIMITIVE: {
      Prim *prm = new Prim(T0,TL);
      for (; dmp ; dmp = dmp->Next()) if (dmp->Flags() & DMP_PRIMITIVE) {
        dmp->dump(prm);
      }
      if (vd()->save & DMP_PRIMITIVE) {
        prm->createPool(1);
      }
      if (V2kMinimize) prm->minimize();
      break;
    }
    case VER_MODULE: {
      Module *mod = new Module(T0,TL,false);
      for (; dmp ; dmp = dmp->Next()) if (dmp->Flags() & DMP_MODULE) {
        dmp->dump(mod);
      }
      if (vd()->save & DMP_MODULE) {
        mod->createPool();
      }
      if (V2kMinimize) mod->minimize();
      break;
    }
    case VER_MACROMODULE: {
      Module *mod = new Module(T0,TL,true);
      for (; dmp ; dmp = dmp->Next()) if (dmp->Flags() & DMP_MODULE) {
        dmp->dump(mod);
      }
      if (vd()->save & DMP_MODULE) {
        mod->createPool();
      }
      if (V2kMinimize) mod->minimize();
      break;
    }
    case VER_FUNCTION:
      fn  = new Func(0,&T0,TL,(StmtFunc *)0);
      break;
    case VER_TASK:
      tsk = new Task(0,&T0,TL,(StmtTask *)0);
      break;
    }
    clearRefs();
    vd()->mod_indx = -1;
  } else {
    ExitMsg(Error(),"Object file missing: %s",f->str());
  }
}

void ContextObj::deleteLists()
{
#define LST(t,b,n,e) n()->clear();
#define LST_LOCAL
#include "lists.inc"
}

Attr  *ContextObj::Saved,
     **ContextObj::SavedNext = &ContextObj::Saved;

ContextObj::~ContextObj()
{
  assert(!Saved);
  deleteLists();
}

VerCntxtObj::VerCntxtObj()
{
  stmt_count = 0;
  saved      = 0;
  io         = PRT_NC;
  next       = 0;
}

void ContextObj::popScope(Stmt *stmt,poolRef ref)
{
  if (!NULL_REF(ref)) vd()->scp_stk[vd()->cur_scp--] = -1;

  if (stmt == vd()->process) vd()->process = 0;
}

void ContextObj::pushScope(Stmt *stmt,poolRef ref)
{
  if (!NULL_REF(ref)) {

    int upi = scp_id(),
        scp = -1,
        lvl = scp_dpth();

    if (++lvl >= vd()->max_scp) {
      REALLOC2(vd()->scp_stk,vd()->max_scp += 16,int);
    }

    switch (lvl) {
    case GLOBAL_SCP: break;
    case ROOT_SCP:   vd()->root  = this;
                     break;
    default:         Scope      *ps = new Scope(stmt);
                     String      nm;
                     ContextObj *vp;
                     switch (typ) {
		       default:      vp = this;
                                     break;
                       case AO_FUNC:
                       case AO_TASK:
                       case AO_STMT: vp = Root();
                     }
                     ASSERT(vp == Root());
                     if ((ps->up = upi) >= 0) {
                       Scope *parent = vp->scope_map(upi);
                       assert(parent);
                       nm  = strDeref(parent->name);
                       nm += ".";
                     }
                     nm             += strDeref(ref);
                     ps->name        = strSaveStr(nm);
                     scp = vp->scope()->add(ps);
                     vp->logLocal(ref,REF_SCOPE,scp);
                     break;
    }

    vd()->scp_stk[vd()->cur_scp = lvl] = scp;
  }

  if (!vd()->process) {
    vd()->process = stmt;
  }
}

INLINE hshdRef *VerCntxtObj::log_obj(poolRef nm,eREF ref_typ,int indx)
{
  return saveRef(nm,ref_typ,scp_id(),scp_dpth(),indx);
}

hshdRef *VerCntxtObj::logLocal(poolRef nm,eREF ref,int indx)
{
  switch (typ) {
  case AO_STMT:    assert(REF_COUNTER == ref);
  case AO_SDF:
  case AO_LOCALS:
  case AO_MACRO:
  case AO_MODULE:
  case AO_PRIM:
  case AO_TASK:
  case AO_FUNC:    return log_obj(nm,ref,indx);
  case AO_NATURE:
  case AO_DISC:    return 0;
  default:         break;
  }
  assert("Bad Local Ref" == 0);
  return 0;
}

void VerCntxtObj::dumpPortRef(Stream *s,int i,int p)
{
  assert(("Inappropriate call",0));
}

hshdRef *VerilogObj::saveRef(poolRef nm,eREF ref_typ,int scp,int dpth,int indx)
{
  hshdRef      *ref = vd()->hshdFree;
  unsigned int  hsh = hash(nm);

  if (ref) {
    vd()->hshdFree = ref->next;
  } else {
    ref = MALLOC(hshdRef);
  }

  if (!(ref->next = vd()->hshdTbl[hsh])) {
    vd()->hshdInUse[vd()->nxtInUse++] = hsh;
    vd()->hshdStart[hsh]              = ref;
  }

  ref->next          = vd()->hshdTbl[hsh];
  vd()->hshdTbl[hsh] = ref;
  ref->name          = nm;
  ref->scp           = scp;
  ref->dpth          = dpth;
  ref->indx          = indx;
  ref->ref           = ref_typ;

  return ref;
}

hshdRef *VerilogObj::saveGlobRef(poolRef nm,eREF ref_typ,int indx)
{
  hshdRef *ref = vd()->hshdFree;
  U16      hsh = hash(nm);

  if (ref) {
    vd()->hshdFree = ref->next;
  } else {
    ref = CALLOC2(1,hshdRef);
  }

  ref->next           = vd()->hshdTblG[hsh];
  vd()->hshdTblG[hsh] = ref;
  ref->name           = nm;
  ref->scp            = -1;
  ref->indx           = indx;
  ref->ref            = ref_typ;

  return ref;
}

void VerilogObj::clearRefs()
{
  int n = vd()->nxtInUse,
      i;

  while (n-- > 0) {
    int i                = vd()->hshdInUse[n];
    vd()->hshdInUse[n]   = -1;
    hshdRef *ref0        = vd()->hshdStart[i],
            *ref         = vd()->hshdTbl[i];
    vd()->hshdTbl[i]     = 0;
    ref0->next           = vd()->hshdFree;
    vd()->hshdFree       = ref;
  }

  vd()->nxtInUse = 0;
}

void VerilogObj::logGlobals(eREF ref_typ,const tokTable *table)
{
  for (;table->name;table++) {
     poolRef nm;
     nm.pool  = table->id.pi.pool;
     nm.index = table->id.pi.index;
     saveGlobRef(nm,ref_typ,-REF_AS_INT(nm));
  }
}

hshdRef *VerilogObj::logGlobal(poolRef nm,eREF ref_typ,int indx)
{
  return saveGlobRef(nm,ref_typ,indx);
}

Attr *AttrObj::addAttr(poolRef name,poolRef value)
{
  Attr *ap;

  if (name.pool < FIRST_FREE_POOL) {
    ap = newAttr(new AttrAny(name,value));
  } else {
    ap = newAttr(new AttrAnyU(name,value));
  }

  return ap;
}

Attr *AttrObj::addAttr(poolRef name,poolRef value,AttrType at)
{
  Attr *ap;

  if (name.pool < FIRST_FREE_POOL) {
    ap = newAttr(new AttrAny(name,value,at));
  } else {
    ap = newAttr(new AttrAnyU(name,value,at));
  }

  return ap;
}

Attr *AttrObj::addAttr(poolRef name,double value)
{
  Attr *ap;

  if (name.pool < FIRST_FREE_POOL) {
    ap = newAttr(new AttrAny(name,value));
  } else {
    ap = newAttr(new AttrAnyU(name,value));
  }

  return ap;
}

Attr *AttrObj::addAttr(poolRef name,Expr *expr)
{
  Attr *ap = 0;

  if (expr)
  {
    if (name.pool < FIRST_FREE_POOL) {
      ap = newAttr(new AttrAny(name,expr));
    } else {      
      ap = newAttr(new AttrAnyU(name,expr));
    } 
  }

  return ap;
}

Attr *AttrObj::addAttr(poolRef name,Nature *nature,NatureIdx idx)
{
  Attr *ap = 0;

  if (nature)
  {
    assert (name.pool < FIRST_FREE_POOL);

    ap = newAttr(new AttrAny(name,nature,idx));
  }

  return ap;
}

Attr *AttrObj::addAttr(poolRef name,Disc *disc,DiscIdx idx)
{
  Attr *ap = 0;

  if (disc)
  {
    assert (name.pool < FIRST_FREE_POOL);

    ap = newAttr(new AttrAny(name,disc,idx));
  }

  return ap;
}

Attr *AttrObj::addAttrFront(poolRef name,Expr *expr)
{
  Attr *ap = 0;

  if (expr)
  {
    assert (name.pool < FIRST_FREE_POOL);
    assert (!pending);

    ap        = new AttrAny(name,expr);
    ap->next  = attr_list;
    attr_list = ap;

    if (attr_next == &attr_list) attr_next = &ap->next;
  }

  return ap;
}

void AttrObj::addPendAttr(AttrObj *to_obj)
{
  if (pending) {
    Attr *ap = *pending;

    assert(to_obj);

    *to_obj->attr_next     = ap;
    to_obj->attr_next      = attr_next; 

    *(attr_next = pending) = 0;
    pending                = 0;
  }
}

void AttrObj::addPendAttr(Attr *to_obj)
{
  if (pending) {
    Attr **ap = &to_obj->attr;

    while (*ap) {
      ap = &((*ap)->next);
    }

    *ap = *pending;

    *(attr_next = pending) = 0;
    pending                = 0;
  }
}

poolRef AttrObj::attrVal(poolRef nm)
{
  Attr *scan = attr_list;

  for (; scan ; scan = scan->next) {
    if (SAME_REF(nm,scan->name())) {
      if (scan->valType() == VT_REF) return scan->vf()->piValue();
      break;
    }
  }

  return NullRef;
}

void VerilogObj::reportError(eSTS sts,const char *format,...)
{
  va_list pvar;
  va_start(pvar, format);
  String msg(format,pvar);
  ErrorMsg(sts,"%s",msg.str());
  ErrorMsg(STS_NORMAL," @ %s:%d",source(),line_no());
  int l = vd()->posn.level;
  while (l-- > 0) {
    if (!NULL_REF(vd()->incStack[l].file)) {
      ErrorMsg(STS_NORMAL," > %s:%d",
                          vd()->incStack[l].source(),
                          vd()->incStack[l].line_no());
    }
  }
}

void VerilogObj::reportError(eSTS sts,InfoRef &Ref,const char *format,...)
{
  va_list pvar;
  va_start(pvar, format);
  String msg(format,pvar);
  ErrorMsg(sts,"%s",msg.str());
  InfoRef *pRef = NULL_REF(Ref) ? &vd()->posn
                                : &Ref;
  int      l    = pRef->level;

  ErrorMsg(STS_NORMAL," @ %s:%d",pRef->source(),pRef->line_no());

  if ((pRef = vd()->incStack)) {
    while (l-- > 0) {
      if (!NULL_REF(pRef[l].file)) {
        ErrorMsg(STS_NORMAL," > %s:%d",pRef[l].source(),pRef[l].line_no());
      }
    }
  }
}

const char *strDerefArr(InfoRef *pRef,int len,String *str,char *pad)
{
  TMPARR(poolRef,ref,len);
  int     l = 0;

  while (l < len) ref[l++] = *pRef++;

  return strDerefArr(ref,len,str,pad);
}

INLINE const Token *VerilogObj::rescanning(const Token *T)
{
  return T;
}

INLINE const Token *VerilogObj::getRefOpt(const Token *T,const Token *TL,InfoRef *Ref)
{
  InfoRef *pp = &vd()->posn;

  while (T < TL) {
    poolRef ref;
    int     l;
    eTOK    t = T->ch.tok;

    switch (t) {
    case WHITESPACE_POOL:
    case TOK_END_MACRO:
      break;
    case TOK_FULL_REF:
      if (Ref) {
        pp->pool  = (++T)->as_int;
        pp->index = (++T)->as_int;
        T++;
      }
      goto done;
    case TOK_LINE:
      pp->line = T->xt.extra;
      break;
    case TOK_LINE_FULL:
      pp->line = (++T)->as_int;
      break;
    case TOK_FILE:
      ref.pool  = (++T)->pi.pool;
      ref.index =     T->pi.index; T++;
      setSource(ref);
      continue;
    case TOK_FILE_FULL:
      ref.pool  = (++T)->as_int;
      ref.index = (++T)->as_int;   T++;
      setSource(ref);
      continue;
    case TOK_INC_LEVEL:
      l = pp->level;
      if (T->xt.extra < l) {
        vd()->posn = vd()->incStack[T->xt.extra];
      } else if (T->xt.extra > l) {
        if (T->xt.extra > vd()->max_level) {
          vd()->max_level = T->xt.extra + 10;
          REALLOC2(vd()->incStack,vd()->max_level,InfoRef);
        }
        vd()->incStack[l] = vd()->posn;
        while (++l < T->xt.extra) {
          BZEROS(vd()->incStack[l]);
        }
        pp->line  = 0;
        pp->file  = NullRef;
        pp->level = T->xt.extra;
      }
      break;
    case TOK_CHAR:
      assert(T->ch.count == 1);
      if (Ref) {
	char buff[2];
	switch (T->ch.ch) {
	case '$': pp->pool      = CHAR_POOL;
	          pp->index     = CHR_DOLLAR;
	          break;
	default:  buff[0]       = T->ch.ch;
	          buff[1]       = '\0';
	          *(poolRef*)pp = strSaveStr(buff);
	}
        T++;
      }
      goto done;
    default:
      assert(T->pi.pool > 0);
      if (Ref) {
        pp->pool  = T->pi.pool;
        pp->index = T++->pi.index;
      }
      goto done;
    }
    T++;
  }

  if (Ref) BZERO(Ref,sizeof(*Ref));

  return T;

done:
  if (Ref) {
    *Ref = *pp;
  }
  return T;
}

const Token *VerilogObj::getRef(const Token *T,const Token *TL,InfoRef *Ref)
{
  return getRefOpt(T,TL,Ref);
}

InfoRef VerilogObj::getRef(const Token *T,const Token *TL)
{
  InfoRef Ref;
  getRefOpt(T,TL,&Ref);
  return Ref;
}

const Token *VerilogObj::skipWhite(const Token *T,const Token *TL)
{
  return getRefOpt(T,TL,0);
}

INLINE const Token *VerilogObj::skipToken(const Token *T,const Token *TL,const Token kw,const char *where)
{
  T = skipWhite(T,TL);
  if (T >= TL || T->as_int != kw.as_int) {
    reportError(S_ERROR(STS_SYNTAX),where ? "Expected %s not %s %s"
		                          : "Expected %s not %s",
                                    tokDeref(&kw),strDeref(getRef(T,TL)),
                                    where);
    return T;
  }

  return ++T;
}

const Token *VerCntxtObj::getDelayExpr(const Token *T,const Token *TL)
{
  const Token *T_1;
  InfoRef      Ref;

  T = getRef(T_1 = T,TL,&Ref);

  if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_BR)) {
    T = getExpr(T,TL,STMT_CNTX(SC_DELAY|SC_TOP|SC_LIST));
    T = skipToken(T,TL,TOK_PUNC_CLS_BR);
  } else {
    T = getExpr(T_1,TL,STMT_CNTX(SC_REFERENCE|SC_NUMBER));
  }

  return T;
}

const Token *VerCntxtObj::getEventExpr(const Token *T,const Token *TL)
{
  const Token *T_1;
  InfoRef      Ref;

  T = getRef(T_1 = T,TL,&Ref);

  if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_BR)) {
    T = getExpr(T,TL,STMT_CNTX(SC_SENSE|SC_TOP));
    T = skipToken(T,TL,TOK_PUNC_CLS_BR);
  } else if (SAME_REF_PI(Ref,OPERATORS_POOL,OP_MULTIPLY)) {
    resetExpr();
    Ref.pool  = SPECIAL_POOL;
    Ref.index = SPC_ALL;
    extendExpr(Ref);
  } else {
    T = getExpr(T_1,TL,SC_REFERENCE);
  }

  return T;
}

const Token *ContextObj::getExpr(const Token *T0,const Token *TL,StmtCntx cntx,
				 InfoRef **ppRf,int *pLen)
{
  const Token *T          = T0,
              *T_1        = 0;
  InfoRef      Ref;
  int          expect_dot = 0,
               br         = 0,
               br1,
               pstrt      = -1,
               sq         = 0,
               cr         = 0,
               q0         = 0;

  resetExpr();

  vd()->cntxt = cntx;
  if (vd()->expr_attr) {
    String str;
    char   spc[2] = {' ',0};
    reportError(S_ERROR(STS_INTERNAL),"Expression attributes discarded - %s",
		                      strDerefArr(&vd()->expr[vd()->expr_len],
                                          vd()->expr_attr - vd()->expr_len,&str,spc));
    vd()->expr_attr = 0;
  }

# define NEXT_REF(r,brk) if (ppRf) { if (*pLen <= 0) brk;		\
                                     r = *(*ppRf)++; (*pLen)--;}	\
                        else T = getRef(T_1 = T,TL,&r);

  while (ppRf || T < TL) {
    NEXT_REF(Ref,goto done);
    switch (Ref.pool) {
    case NULL_POOL:
      goto done;
    case OPERATORS_POOL:
      switch (Ref.index) {
      case OP_MULTIPLY: if (br1+1 == vd()->expr_len && pstrt < 0) {
	                  assert(!ppRf);
			  pstrt = br1;
	                }
	                break; 
      case OP_MINUS:
      case OP_PLUS:     if (cntx & SC_NUMBER) {
			  cntx = STMT_CNTX(cntx & ~SC_REFERENCE); goto ext;
			}
      }
      goto def;
    case VERILOG_POOL:
      switch (Ref.index) {
      case VER_CLS_ATTR: if (pstrt >= 0) { // ???
	                   extendExpr(Ref,T_1);
	                   vd()->expr_attr = vd()->expr_len;
	                   T_1             = rescanning(vd()->expr[
                                                  vd()->expr_len = pstrt].tok);
			   goto done;
	                 }
      case VER_FROM:  
      case VER_EXCLUDE:
      case VER_OPN_ATTR: if (cntx & (SC_PARAMETER|SC_ATTR)) {
 	                   T = T_1; goto done;
                         }
      }
      goto def;
    case QUOTES_POOL:
      switch (Ref.index) {
      case QUT_DOUBLE: if (IS_ATTR(cntx) && q0) { // ???
	                 extendExpr(Ref,T_1);
	                 goto done;
	               }
	               if (T_1 == T0) q0 = 1;
      }
      goto def;
    case PUNCTUATION_POOL:
      switch (Ref.index) {

      case PUNC_OPN_CR: cr++; break;
      case PUNC_OPN_BR: if (1 == ++br) br1 = vd()->expr_len; 
	                break;
      case PUNC_OPN_SQ: sq++; if (cntx & SC_REFERENCE) {
                                cntx = STMT_CNTX((cntx|SC_INDXD)
                                                      & ~SC_REFERENCE);
                              }
                              break;

      case PUNC_CLS_BR: if (--br < 0) {T = T_1; goto done;} else break;
      case PUNC_CLS_SQ: if (--sq < 0) {T = T_1; goto done;} else break;
      case PUNC_CLS_CR: if (--cr < 0) {T = T_1; goto done;} else break;

      case PUNC_PERIOD: if (cntx & SC_REFERENCE) {
                          if (!expect_dot) goto done;
                          expect_dot = 0;
                          goto ext;
			}
                        break;
      case PUNC_COMMA:  if (br || sq || cr 
                                  || (cntx & (SC_LIST|SC_SENSE))) break;
      case PUNC_SCOLON: T = T_1;
                        goto done;
      }
      goto def;

    default:
    def:
      if (cntx & (SC_REFERENCE|SC_NUMBER)) {
        if (identifier(Ref) && (cntx & SC_REFERENCE)) {
          cntx = STMT_CNTX(cntx & ~SC_NUMBER);
          if (!expect_dot++) goto ext;
          T = T_1;
        }
        if (number(Ref) && (cntx & SC_NUMBER)) {
          extendExpr(Ref,T_1);
        }
        goto done;
      }
    }
  ext:
    extendExpr(Ref,T_1);
  }

  if (!(cntx & SC_COMPLETE))
                   reportError(S_ERROR(STS_SYNTAX),"Expression not terminated");
  else if (br > 0) reportError(S_ERROR(STS_SYNTAX),"Missing ')'");
  else if (sq > 0) reportError(S_ERROR(STS_SYNTAX),"Missing ']'");
  else if (cr > 0) reportError(S_ERROR(STS_SYNTAX),"Missing '}'");

  goto end_expr;

done:
  if ((cntx & SC_COMPLETE) && T != TL) {
    reportError(S_ERROR(STS_SYNTAX),"Expression terminated unexpectedly");
  }

# undef NEXT_REF

end_expr:
  endExpr();

  return T;
}

const Token *ContextObj::endExpr(const Token *T,const Token *TL)
{
  int          i = 0;
  const Token *T_1;

  for (; T < TL ; i++) {
    T = getRef(T_1 = T,TL,&vd()->expr[i]);
    switch (vd()->expr[i].pool) {
    case VERILOG_POOL:
      switch (vd()->expr[i].index) {
      case VER_FROM:    return T_1;
      case VER_EXCLUDE: return T_1;
      }
      break;
    case PUNCTUATION_POOL:
      switch (vd()->expr[i].index) {
      case PUNC_SCOLON: return T;
      }
    }
  }

  reportError(S_ERROR(STS_SYNTAX),"Expression not terminated");

  return T;
}

bool VerilogObj::findGlobObj(eRFF rff,poolRef nm,refTup *ret)
{
  U16      hsh  = hash(nm);
  hshdRef *scn  = vd()->hshdTblG[hsh];

  for (; scn ; scn = scn->next) {
    if ((RFF(scn->ref) & rff) && SAME_REF(scn->name,nm)) {
      ret->typ   = scn->ref;
      ret->index = scn->indx;
      return 1;
    }
  }

  ret->index = -1;
  return 0;
}

bool VerilogObj::findLibObj(eRFF rff,int lib_indx,poolRef nm,refTup *ret)
{
  U16      hsh  = hash(nm);
  hshdRef *scn  = vd()->hshdTblG[hsh];

  for (; scn ; scn = scn->next) {
    if ((RFF(scn->ref) & rff) && SAME_REF(scn->name,nm)) {
      int l;
      switch (rff) {
      case RFF_MODULE: l = module_map(scn->indx)->lib_indx; break;
      case RFF_PRIM:   l = prim_map(scn->indx)->lib_indx;   break;
      default: assert(0);
      }
      if (l == lib_indx) {
        ret->typ   = scn->ref;
        ret->index = scn->indx;
        return 1;
      }
    }
  }

  ret->index = -1;
  return 0;
}

NatureIdx VerilogObj::findNature(poolRef name)
{
  refTup rt;
  findGlobObj(RFF_NATURE,name,&rt);
  return rt.index;
}

DiscIdx VerilogObj::findDisc(poolRef name)
{
  refTup rt;
  findGlobObj(RFF_DISC,name,&rt);
  return rt.index;
}

//! Test if token is a valid Identifier
bool VerilogObj::identifier(poolRef name)
{
  switch (name.pool) {
  case PUNCTUATION_POOL:
  case WHITESPACE_POOL:
  case OPERATORS_POOL:
  case COMMENTS_POOL:
  case QUOTES_POOL:
                         return false;
  case LABELS_POOL:
  case BUILTIN_POOL:
  case GLOBAL_POOL:      return true;
  default:               break;
  }

  const char *cp = strDeref(name);
  int         ch = *cp++;

  if ('\\' == ch) {
    while ((ch = *cp++)) if (!isprint(ch)) goto bad;
  } else {
    if (!(isalpha(ch) || '_' == ch)) goto bad;
    while ((ch = *cp++)) if (!(isalnum(ch) || '_' == ch || '$' == ch)) goto bad;
  }
  return true;

bad:
  return false;
}

bool VerilogObj::number(poolRef name)
{
  return isdigit(*strDeref(name));
}

bool VerilogObj::checkName(poolRef name)
{
  if (identifier(name)) return true;

  reportError(S_ERROR(STS_SYNTAX),"Invalid name `%s`",strDeref(name));
  return false;
}

CnstBaseExpr BaseExpr::CBE[] = {
#define BASE_EXPR(e,n,t,x,r,c,s) {t,x,r,c},
#include "base_expr.inc"
  {VT_VOID,VTX_NONE,REF_UNKNOWN,CT_NONE}
};

const char *BaseExpr::typStr[VT_LAST +1] = {
#define AV_TYPE(e,p,s) s,
#include "av_types.h"
  0
};

ePrcsn BaseExpr::Precision[VT_LAST +1] = {
#define AV_TYPE(e,p,s) p,
#include "av_types.h"
  (ePrcsn)-1
};

Operator Operator::oprs[] = {
#define  OPERATOR(o,p,i,prcd,R,l,r,alt,sc,lv) {o,alt,lv,sc,{p,i},prcd,R,l,r}
#include "operator.inc"
};

Stmt::~Stmt() {
  switch (typ) {
#define STATEMENT(e,t,m) case e: ((t *)this)->~t(); break;
#include "statement.inc"
  default: assert("Bad Statement Type" == 0);
  }
}

StmtExpr::~StmtExpr() {
  DELETE(expr);
}

StmtDefparam::~StmtDefparam() {
  DELETE(expr);
}

StmtAt::~StmtAt() {
  DELETE(expr);
  DELETE(child);
}

StmtRepeat::~StmtRepeat() {
  DELETE(expr);
  DELETE(child);
}

StmtWhile::~StmtWhile() {
  DELETE(expr);
  DELETE(child);
}

StmtWait::~StmtWait() {
  DELETE(expr);
  DELETE(child);
}

StmtDelay::~StmtDelay() {
  DELETE(expr);
  DELETE(child);
}

StmtAnalog::~StmtAnalog() {
  DELETE(child);
}

StmtInit::~StmtInit() {
  DELETE(child);
}

StmtQC::~StmtQC() {
  DELETE(str);
}

StmtTask::~StmtTask() {
}

StmtFunc::~StmtFunc() {
}

StmtFuncA::~StmtFuncA() {
}

StmtDecl::~StmtDecl() {
  DELETE(child);
  DELETE(lcls);
}

StmtSpec::~StmtSpec() {
}

StmtBlock::~StmtBlock() {
}

StmtFork::~StmtFork() {
}

StmtIf::~StmtIf() {
  DELETE(expr);
  DELETE(child_t);
  DELETE(child_f);
}

StmtIfNone::~StmtIfNone() {
  DELETE(expr);
  DELETE(child_t);
  DELETE(child_f);
}

StmtFor::~StmtFor() {
  DELETE(ass1);
  DELETE(expr);
  DELETE(ass2);
  DELETE(child);
}

StmtGen::~StmtGen() {
  DELETE(expr);
  DELETE(child);
}

StmtDisable::~StmtDisable() {
  DELETE(expr);
}

StmtAssign::~StmtAssign() {
  DELETE(expr);
  DELETE(delay);
}

StmtEvent::~StmtEvent() {
  DELETE(expr);
}

StmtDeassign::~StmtDeassign() {
  DELETE(expr);
}

StmtForce::~StmtForce() {
  DELETE(expr);
}

StmtRelease::~StmtRelease() {
  DELETE(expr);
}

StmtCase::~StmtCase() {
  case_s *l  = list,
         *ln;
  DELETE(expr);
  for (;l;l = ln) {
    ln = l->next;
    DELETE(l->expr);
    DELETE(l->child);
    Free(l);
  }
}

StmtInst::~StmtInst() {
  DELETE(param);
  DELETE(inst);
}

StmtForever::~StmtForever() {
  DELETE(child);
}

StmtAlways::~StmtAlways() {
  DELETE(child);
}

eDIFF Expr::diff(Expr *as)
{
  eDIFF df = DIFF_HSHD;
  Expr *self;
  int   d;

  if (this && as)
  {
    self = unalias();
    as   = as->unalias();

    if ((d = self->typ - as->typ)) {

      df = DIFF_HSHD;

    } else switch (self->typ) {
    case VT_OPERATOR: if (self->op() == as->op()     &&
                          self->left->same(as->left) &&
                          self->right->same(as->right)) {
                        df =  DIFF_SAME;
                      }
                      break;
    case VT_I64:
    case VT_U64:      if (self->value.i64 == as->value.i64) df = DIFF_SAME;
                      break;

    case VT_INT:
    case VT_UINT:     if (self->value.i == as->value.i) df = DIFF_SAME;
                      break;
    case VT_LUINT:    if (self->value.lgc.i == as->value.lgc.i &&
                          self->value.lgc.b == as->value.lgc.b &&
                          self->value.lgc.w == as->value.lgc.w)   df = DIFF_SAME;
                      break;
    case VT_REFL:     if ((d = self->rtyp - as->rtyp) ||
                          (d = self->ref().index - as->ref().index) ||
                          (d = self->rrf().scp - as->rrf().scp)) {
                        df = (d > 0) ? DIFF_GRTR
			             : DIFF_LESS;
                      } else {
                        df = DIFF_SAME;
                      }
                      break;
    case VT_VOID:     if ((VTX_HIER & as->xtra) && (VTX_HIER & self->xtra)) {
                        if (as->right == self->right) {
                          intptr_t  n  = (intptr_t)self->right;
                          poolRef  *ps = self->value.hrf,
  			           *pa = as->value.hrf;
                          for (; n-- ; pa++,ps++) if (!SAME_REF(*pa,*ps)) {
                            goto done;
                          }
                          df = DIFF_SAME;
                        }
                      }
                      break;
    default:          assert("Case not handled (yet)" == 0);
    }
  } else if (!(this || as)) {
    df = DIFF_SAME;
  }
done:
  return df;
}

int printQzx(Qzx *pq,int width,Stream *strm,const char *frmt,
             int base,int digits,int lead,int cb)
{
  int mask0 = base -1,
      shft  = 1,
      b     = 2;

  for (; b & mask0; b <<= 1) shft++;

  int r = QZX_WDTH % shft;

  if (pq->next) {
    printQzx(pq->next,width - QZX_WDTH,
             strm,frmt,base,-1,lead,(cb + r) % shft);
    lead   = 1;
    frmt   = "_";
    width  = QZX_WDTH;
  } else {
    if (width > QZX_WDTH) width = QZX_WDTH;
  }

  if (digits < 0) {
    digits = (width-1)/shft;
  }

  char buff[72];
  int  d = 0,
       c = width - digits * shft;

  U64 x = pq->x,
      q = pq->q,
      z = pq->z,
      n = pq->n;

  if (8 == base) {
    if ((pq = pq->next) && !cb) {
      width += 2;
    } else {
      digits--;
    }
    if (cb) {
      x >>= shft - cb;
      q >>= shft - cb;
      z >>= shft - cb;
      n >>= shft - cb;
    }
  }

  do {
    int i,
        ds   = (digits * shft);
    if (ds < width) {
      int x2 = x >> ds,
          q2 = q >> ds,
          z2 = z >> ds,
          n2 = n >> ds;
      if (8 == base) {
        if (digits == 21) {
          n2 |= (pq->n << 1) & 07;
        } else if (digits == 20 && 1 == cb && pq) {
          n2 |= (pq->n << 2) & 07;
        }
      }
      if      (x2 & mask0) lead = buff[d++] = 'x';
      else if (z2 & mask0) lead = buff[d++] = 'z';
      else if (q2 & mask0) lead = buff[d++] = '?';
      else switch (i = (n2 & mask0)) {
      case 15: buff[d++] = 'f';     break;
      case 14: buff[d++] = 'e';     break;
      case 13: buff[d++] = 'd';     break;
      case 12: buff[d++] = 'c';     break;
      case 11: buff[d++] = 'b';     break;
      case 10: buff[d++] = 'a';     break;
      case  0: if (0 == lead && ds) break;
      default: lead = buff[d++] = '0' + i; break;
      }
    }
  } while (digits-- > 0);
  buff[d] = '\0';
  return strm->printf("%s%s",frmt,buff);
}

inline Qzx *saveQzxn(U64 *q,U64 *z,U64 *x,U64 *n,Qzx *qzxn)
{
  Qzx *qzx  = MALLOC2_N(1,Qzx);

  qzx->q    = *q; *q = 0;
  qzx->z    = *z; *z = 0;
  qzx->x    = *x; *x = 0;
  qzx->n    = *n; *n = 0;
  qzx->next =  qzxn;

  return qzx;
}

Expr *Expr::alias()
{
  Expr *ret = 0;

  if (this) {
    Expr *start    = unalias();
    start->xtra    = VL_TYP_XTRA(start->xtra|VTX_ALIASSED);
    ret            = new Expr(BxPointer,0,0),
    ret->value.ptr = start;
    ret->typ       = VT_ALIAS;
  }

  return ret;
}

void AdjustQzx(Qzx *pq,int width,int bits)
{
  if (bits) {
    int shft;

    shft = QZX_WDTH - bits;

    Qzx *pqn = pq->next;

    if (pqn) {
      pq->n |= (pqn->n << bits);  pqn->n >>= shft;
      pq->q |= (pqn->q << bits);  pqn->q >>= shft;
      pq->z |= (pqn->z << bits);  pqn->z >>= shft;
      pq->x |= (pqn->x << bits);  pqn->x >>= shft;

      AdjustQzx(pqn,width,bits);
    } else {

    }
  }
}

double multiplier(InfoRef *pRef,int len)
{
  if (1 == len) {
    const char *str = strDeref(*pRef);
    if (0 == str[1]) switch (*str) {
#define MULTIPLIER(c,d)\
      case c: return d;
#include "multipliers.h"
    }
  }
  return 0.0;
}

Expr *VerilogObj::evalNumber(InfoRef *pRef,int len,int strts_tick)
{
  Expr       *ret  = 0;
  const char *str  = strDeref(pRef[0]);
  double      mult = 0;

  if (len > 1 && !strts_tick
              && !(3 == len && SAME_REF(pRef[1],TOK_QUT_SINGLE.pi))
              && !(mult = multiplier(pRef+1,len-1))) {
    String str;
    char   spc[2] = {' ',0};
    reportError(S_ERROR(STS_SYNTAX),"Confused @ %s",strDerefArr(pRef,len,&str,spc));
  } else if (isdigit(*str) || '\'' == *str) {
    int    base = 10,
           tick = -1,
           ch,
           bf,
           s    =  0,
           i    =  0,
           bits =  0,
           dig;
    U64    l,
           n    =  0,
           z    =  0,
           x    =  0,
           q    =  0,
          *bm   =  0;
    double m    =  0,
           div  = 10.0;
    Qzx   *qzxn =  0;

#define SHIFT_QZX(B) {z <<= B; x <<= B; q <<= B;}

    while ((ch = *str++)) {
      switch (ch) {
      case '\0':
        if (len-- > 1) {
          str = strDeref(*++pRef);
          continue;
        }
        goto out;
      case '\'': if (tick >= 0) goto bad;
        assert(tick <= 0xFFFF);
        tick     = n;
        n = bits = 0;
        if (!*str && len-- > 1) str = strDeref(*++pRef);
        switch (ch = *str++) {
        case 'H': case 'h': base = 16; break;
        case 'D': case 'd': base = 10; break;
        case 'O': case 'o': base =  8; break;
        case 'B': case 'b': base =  2; break;
        default:
          reportError(S_ERROR(STS_SYNTAX),
                      "Bad base '%c' (in %s)",ch,strDeref(pRef[0]));
          goto done;
        }
#ifndef NO_SPACE_FROM_BASE
        while ((ch = *str) && isspace(ch)) str++;
#endif
        continue;
      case 'E': case 'e': if (tick < 0) goto exp;
                          i = 14; break;
      case 'D': case 'd': i = 13; break;
      case 'C': case 'c': i = 12; break;
      case 'B': case 'b': i = 11; break;

      case '_': continue;
      case '.': if (tick >= 0) goto bad;
                else           goto point;
      case '9':
      case '8':
      case '7':
      case '6':
      case '5':
      case '4':
      case '3':
      case '2':
      case '1':
      case '0': i  = ch - '0';
                break;

      case '?': bm = &q; goto logic;
      case 'x':
      case 'X': bm = &x; goto logic;
      case 'z':
      case 'Z': bm = &z; goto logic;
	logic:
                i = 0;
                break;
#define MULTIPLIER(c,d)\
      case c: if (base == 10) goto exp;\
	      if ('a' == c || 'a' == c) {i = 10; break;}\
	      if ('f' == c || 'F' == c) {i = 15; break;}
#include "multipliers.h"
	}
      if (i >= base) {
        reportError(S_ERROR(STS_SYNTAX),"Digit out of range '%c' (in %s)",
                                        ch,strDeref(pRef[0]));
      }

      switch(base) {
      case  2: bf =  1; s = 1; break;
      case  8: bf =  7; s = 3; break;
      case 10: bf =  0; s = 0;
               l  =  0;
	       l  = ~l;
               if (bm) {
                 reportError(S_ERROR(STS_SYNTAX),
                             "Inappropriate digit '%c' (in %s)",
                             ch,strDeref(pRef[0]));
               } else if (n >= l - (base * i)) {
                 reportError(S_ERROR(STS_SYNTAX),
                             "Number out of supported range: %s",
                              strDeref(pRef[0]));
               }
      case 16: bf = 15; s = 4; break;
      }
      if (bits == 64) {
        qzxn = saveQzxn(&q,&z,&x,&n,qzxn);
        bits = s;
        n    = i;
        q = z = x = 0;
        if (bm) *bm = bf;
      } else if (bits + s > 64) {
        int r = 64 - bits,
            d;
        U64 m = 0;
        n  <<= r;
	bits = s - r;
        m    = ~m << bits;
        n   |=  i >> bits;
        SHIFT_QZX(r)
        if (bm) *bm |= bf >> bits;
        qzxn = saveQzxn(&q,&z,&x,&n,qzxn);
        n    = i & ~m;
        q = z = x = 0;
        if (bm) *bm |= bf & ~m;
      } else {
        bits += s;
        n = (base * n) + i;
        SHIFT_QZX(s)
        if (bm) *bm |= bf;
      }
      bm = 0;
    next:;
    }
  out:
    if (!bits && !qzxn) goto bad;
    if (q || x || z || qzxn) {
      Qzx *qzx  = MALLOC2_N(1,Qzx);
      qzx->n    = n;
      qzx->q    = q;
      qzx->z    = z;
      qzx->x    = x;
      qzx->next = qzxn;
      if (tick <= 0) tick = bits;
      AdjustQzx(qzx,tick,bits);
      ret = new Expr(n,BASE_N_WIDTH(1,base,tick),qzx);
    } else if (n >> 32) {
      if (10 == base) {
        ret = new Expr(n);
      } else {
        if (tick <= 0) tick = bits;
        ret = new Expr(n,BASE_N_WIDTH(0,base,tick),0);
      }
    } else if (tick > 0) {
      szdInt li;
      li.i = n;
      li.w = tick;
      li.b = base;
      ret = new Expr(li);
    } else {
      unsigned int u = n;
      ret = new Expr(u);
    }
    goto done;
  point:
    dig = 0;
    while ((ch = *str++)) {
	switch (ch) {
      default:  goto exp_next;
	case '_': continue;
	case '9':
	case '8':
	case '7':
	case '6':
	case '5':
	case '4':
	case '3':
	case '2':
	case '1':
	case '0': m += (ch - '0')/div; dig++; div *= 10;
	}
    }
  exp_next:
    if (!dig) goto bad;
  exp:
    m += n;
    double dbl = 0.0;
    int    e   = 0;
    switch (ch) {
    case 'E': case 'e':
      dig = 0;
      s   = 0;
      while ((ch = *str++)) {
        switch (ch) {
        case '_': continue;
        case '-': if (s) goto bad; s = -1; continue;
        case '+': if (s) goto bad; s =  1; continue;
	  case '9':
	  case '8':
	  case '7':
	  case '6':
	  case '5':
	  case '4':
	  case '3':
	  case '2':
	  case '1':
	  case '0': e = (e * 10) + (ch - '0'); dig++;
        }
      }
      if (!dig) {
	  reportError(S_ERROR(STS_SYNTAX),
                      "Bad exponent in '%s'",ch,strDeref(pRef[0]));
	  goto done;
      }
      dbl = pow(10,s > 0 ? e : -e);
      break;
#define MULTIPLIER(c,d)\
    case c: dbl = d;  break;
#include "multipliers.h"

    case '\0': ret = new Expr(m); goto done;

    default: goto bad;
    }
    ret = new Expr(m * dbl);
    goto done;
  } else {
    if (checkName(pRef[0])) ret = new Expr(pRef[0]);
    goto done;
  }
bad:
  reportError(S_ERROR(STS_SYNTAX),"Bad number: \"%s\"",strDeref(pRef[0]));
done:
  return ret;
}

int VerCntxtObj::evalRef(Expr *rx,eRFF rff,eREF cr)
{
  poolRef      nm    = rx->ref();
  U16          hsh   = hash(nm);
  int          dpth  = scp_dpth(),
               ret   = -1,
               umtch = 0;
  hshdRef     *scn   = vd()->hshdTbl[hsh],
              *unk   = 0;
  VlType       lg    = VT_REFL;
  eRFF         rfu   = (eRFF)(rff | RFF_UNKNOWN);
  Unknown     *punk;
  ContextObj  *rp    = Root();

  for (; dpth > 0 ; dpth--) {
    for (; scn ; scn = scn->next) {
      if ((RFF(scn->ref) & rfu) && SAME_REF(scn->name,nm)) {
        if (scn->ref) {
          if (vd()->scp_stk[scn->dpth] == scn->scp) goto fix_ref;
        } else {
          punk = rp->unknown_map(scn->indx);
          if (punk->rff == rff && punk->scp == scp_id()) unk = scn;
        }
      }
    }
  }

  if (!(rff & ~(RFF_SCOPE|RFF_INST))) {
    goto done;
  }

  scn = vd()->hshdTblG[hsh];
  lg  = VT_REFG;
  for (; scn ; scn = scn->next) {
    if (SAME_REF(scn->name,nm)) {
      if (!(RFF(scn->ref) & rfu)) {
        umtch |= RFF(scn->ref);
      } else {
        if (scn->ref) goto fix_ref;
        punk = rp->unknown_map(scn->indx);
        if (punk->rff == rff && punk->scp == scp_id()) unk = scn;
      }
    }
  }

  if (!(scn = unk)) {
    switch (cr) {
    default:           assert("Can't create REF type" == 0);
    case REF_NONE:     goto done;
    case REF_PORT:   { ContextObj *rp  = Root();
                       Port       *prt = new Port(PRT_ALIASSED,
                                                  PT_NONE,ST_VEC,DS_NONE,0,-1);

                       scn = rp->logLocal(prt->name = nm,
                                          REF_PORT,rp->port()->add(prt));
                     } break;
    case REF_UNKNOWN:if (!(umtch & RFF_UNKNOWN)) {
                       ContextObj *rp  = Root();
                       scn = rp->logLocal(nm,REF_UNKNOWN,
			              rp->unknown()->add(new Unknown(nm,rff)));
                     } break;
    }
  }

  lg  = VT_REFL;

fix_ref:
  rx->typ           = lg;
  rx->rtyp          = scn->ref;
  rx->prrf()->scp   = scn->scp;
  rx->prrf()->index = ret = scn->indx;

done:
  return ret;
}

int Expr::countLeaves()
{
  if (!this) return 0;

  if (typ != VT_OPERATOR) return 1;

  return lhs()->countLeaves() + rhs()->countLeaves();
}

bool Expr::collectNames(Expr *expr)
{
  VlType  t  = vt();

  switch (t) {
  case VT_VOID:     if (VTX_REF & xtra) {
                      int *pi = (int *)&(expr->right);
                      expr->val()->hrf[(*pi)++] = ref();
                      return 1;
                    }
                    goto failed;
  case VT_OPERATOR: if (OPR_HIER == opr()->self) break;
  default:          goto failed;
  }

  if (!lhs() || !lhs()->collectNames(expr)) return 0;
  if (!rhs() || !rhs()->collectNames(expr)) return 0;

  return 1;
failed:
  return 0;
}

Expr *VerCntxtObj::collapseHier(Expr *xpr,VlTypXtra in_use)
{
  int      n    = xpr->countLeaves();
  poolRef *pRef = MALLOC2_N(n,poolRef);
  Expr    *expr = new Expr(pRef,(XData)(XD_FLAG|XD_HREF),0);

  if (xpr->collectNames(expr)) {
    ContextObj *rp  = Root();
    if (rp) {
      Unknown    *unk = rp->unknown()->first();
      for (n = 0; unk ; unk = unk->next,n++) if (NULL_REF(unk->ref)) {
	Expr *ux = unk->expr;
	if (expr->same(ux)) {
	  delete(expr);
	  goto have_it;
	}
     }
     unk                 = new Unknown(NullRef,RFF_ALL,expr);
     n                   = rp->unknown()->add(unk);
    have_it:
     expr                = new Expr();
     expr->typ           = VT_REFL;
     expr->rtyp          = REF_UNKNOWN;
     expr->prrf()->scp   = 0;
     expr->prrf()->index = n;
     return expr;
    }
  }

  delete(expr);

  xpr->xtra = VL_TYP_XTRA(in_use|xpr->xtra);
  return xpr;
}

Expr *Expr::collapseHier(ContextObj *vp,VlTypXtra in_use)
{
  return vp->collapseHier(this,in_use);
}

bool VerilogObj::baseNature(NatureIdx ni1,NatureIdx ni2)
{
  Nature *n1,*n2;

  do {
    if ((n1 = nature_map(ni1))) ni1 = n1->parent;
  } while (n1 && ni1 >= 0);

  do {
    if ((n2 = nature_map(ni2))) ni2 = n2->parent;
  } while (n2 && ni2 >= 0);

  return n1 == n2;
}

Branch *ContextObj::findBranch(Expr *n1,Expr *n2)
{
  ContextObj *rp = Root();
  Branch     *br = rp->branch_map(0);

  for (; br ; br = br->next) {
    if (!n1->diff(br->p1)) {
      if (n2 || br->p2) {
        if (!n2 || !br->p2 || n2->diff(br->p2)) continue;
      }
      return br;
    }
  }

  return 0;
}

bool Expr::fixContrib(ContextObj *vp)
{
  Expr     *xpr,
           *nds[2],
           *brx[2];
  Disc     *dsc;
  int       n;
  OPR       op     = OPR_ACCESS;
  VlTypXtra xt;
  int       implct = 1;

  if (!rhs()) goto no_access;

  if (OPR_LIST1 == rhs()->opr()->self) {
    nds[0] = brx[0] = rhs()->lhs();
    nds[1] = brx[1] = rhs()->rhs();
  } else {
    nds[0] = brx[0] = rhs();
    nds[1] = brx[1] = 0;
  }

  for (n = 0; n < 2 && nds[n] ; n++) {
    xpr = nds[n];
    if ((VT_OPERATOR == xpr->vt())) switch (xpr->opr()->self) {
    case OPR_INDEX:   xpr = xpr->lhs(); break;
    case OPR_ANYEDGE: xpr = xpr->rhs(); break;
    }
  retry:
    xt = xpr->vtx();
    if (VTX_REF & xt) {
      switch (xpr->rt()) {
      case REF_BRANCH:  implct = 0;
      case REF_PORT:    break;
      case REF_UNKNOWN: if (vp->evalRef(xpr,RFF_DRV,REF_UNKNOWN) >= 0) goto retry;
      default:          goto no_node;
      }
    } else {
    no_node:
      String str;
      vp->reportError(S_ERROR(STS_SYNTAX),
                      "Not a node: %s",xpr->strValue(&str));
      xpr = 0;
    }
    nds[n] = xpr;
  }

  switch(lhs()->vt()) {
  case VT_VOID: if (VTX_REF & lhs()->xtra) {
                  if (SAME_REF_PI(ref(),VERILOG_POOL,VER_FLOW)) {
                    op = OPR_FLOW;
                    break;
                  } else if (SAME_REF_PI(ref(),VERILOG_POOL,VER_POTENTIAL)) {
                    op = OPR_POTENTIAL;
                    break;
                  } else {
                    vp->evalRef(lhs(),RFF(REF_ACCESS),REF_UNKNOWN);
                    if (VT_REFG != lhs()->vt()) goto no_access;
                    NatureIdx nat = lhs()->ref().index;
                    if ((dsc = vp->findBrDisc(nds[0],nds[1]))) {
                      if (vp->baseNature(dsc->potential,nat)) op = OPR_POTENTIAL;
                      else if (vp->baseNature(dsc->flow,nat)) op = OPR_FLOW;
                      else {
                        vp->reportError(S_ERROR(STS_SYNTAX),
                                        "Access function inappropriate");
                        goto done;
                      }
                      break;
                    } else {
                      vp->reportError(S_ERROR(STS_SYNTAX),
                              "Can't determine discipline of access function");
                      goto done;
                    }
                  }
                }
  no_access:
  default:      vp->reportError(S_ERROR(STS_SYNTAX),"Bad access function");
                goto done;
  }

  if (implct) {
    Branch *br = vp->findBranch(brx[0],brx[1]);

    if (!br) {
      br = new Branch(brx[0]->alias(),brx[1]->alias(),-1);
      vp->Root()->branch()->add(br);
    }
  }

  value.op = op;

  return 1;
done:
  return 0;
}

eREF Expr::unambigous(VerilogObj *vp,eRFF rff)
{
  poolRef  nm   = ref();
  U16      hsh  = vp->hash(nm);
  int      mtch = 0;
  hshdRef *scn  = vp->vd()->hshdTbl[hsh];
  VlType   lg   = VT_REFL;
  eREF     ret;

  for (; scn ; scn = scn->next) {
    if ((RFF(scn->ref) & rff) && SAME_REF(scn->name,nm)) {
      mtch++;
      ret = scn->ref;
    }
  }
  scn = vp->vd()->hshdTblG[hsh];
  lg  = VT_REFG;
  for (; scn ; scn = scn->next) {
    if ((RFF(scn->ref) & rff) && SAME_REF(scn->name,nm)){
      mtch++;
      ret = scn->ref;
    }
  }

  if (mtch != 1) ret = REF_UNKNOWN;

  return ret;
}

InstRef::InstRef(poolRef nm)
{
  next     = 0;
  name     = nm;
  mod_indx = 1;
  refs     = 0;
  ref_max  = 0;
  referer  = 0;

  logGlobal(nm,REF_INST,instLst()->add(this));
}

InstRef *InstRef::findRef(poolRef nm)
{
  U16      hsh  = hash(nm);
  hshdRef *scn  = vd()->hshdTblG[hsh];

  for (; scn ; scn = scn->next) {
    if (REF_INST == scn->ref && SAME_REF(scn->name,nm)) {
      return instLst_map(scn->indx);
    }
  }

  return 0;
}

void PrtdObj::addInstRef(poolRef nm)
{
  InstRef *ri = 0;

  if (!(ri = ri->findRef(nm))) {
    ri                      = new InstRef(nm);
    ri->referer             = MALLOC2_N(ri->ref_max = 16,int);
    ri->referer[ri->refs]   = vd()->mod_indx;
  } else if (ri->referer[ri->refs] != vd()->mod_indx) {
    if (ri->ref_max <= ++ri->refs) {
      REALLOC2(ri->referer,ri->ref_max += 16,int);
    }
    ri->referer[ri->refs] = vd()->mod_indx;
  }
}

Expr *Expr::eval(VlTypXtra in_use,ContextObj *vp,eRFF rff,eREF cr)
{
  Operator  *op;
  VlType     t      = vt();
  Expr      *ret    = 0,
            *LHS    = 0,
            *RHS    = 0;
  VlTypXtra  x;
  eREF       rf;

  switch (t) {
  case VT_ALIAS:    unalias()->eval(in_use,vp,rff,cr);
  case VT_DOUBLE:
  case VT_STRING:
  case VT_ULOGIC:
  case VT_LOGIC:
  case VT_I64:
  case VT_U64:
  case VT_LUINT:
  case VT_LINT:
  case VT_REFL:
  case VT_REFG:
  case VT_UINT:
  case VT_INT:      goto use_me;
  case VT_VOID:     if (VTX_REF & xtra) {
                      switch (ref().pool) {
		      case MATH_POOL:
                        switch (ref().index) {
		        case MTH_INF: ret = new Expr(HUGE); goto done;
                        }
                        goto use_me;
		      case VERILOG_POOL:
                        switch (ref().index) {
#   define TASKS(n)     case VER_##n: goto eval;
#   include                           "tasks.inc"
#   undef TASKS
			}
                       
                        goto use_me;
                      }
		     eval:
                      vp->evalRef(this,rff,cr);
                      goto use_me;
                    } else if (VTX_HIER & xtra) {
                      goto use_me;
                    }
  default:          assert (0);

  case VT_CAT:
  case VT_SUB:      assert(!(LHS = lhs()));
                    RHS = rhs()->eval(in_use,vp,rff,cr);
                    if (RHS != rhs()) {
                      ret = new Expr(this,LHS,RHS,VTX_NONE);
                      goto done;
                    }
                    goto use_me;
  case VT_OPERATOR: x = VTX_CONST;
                    switch ((op = opr())->self) {
                    case OPR_INST:    if ((LHS = lhs())) {
                                        vp->logLocal(
                                               lhs()->ref(),REF_INST,
                                               vp->scope()->add(new Scope(0)));
		                      }
		    case OPR_BIND:    if ((LHS = lhs())) {
                                         LHS->xtra
                                               = VL_TYP_XTRA(in_use|LHS->xtra);
		                      }
		                      rff = RFF_BIND;
                                      goto do_rhs;
                    case OPR_CALL:    if (lhs() &&
                                          (rf = lhs()->unambigous(vp,RFF_CALL))) {
	                          	if (REF_ACCESS == rf) {
                                          fixContrib(vp);
                                        }
		                      }
                                      break;
                    case OPR_CONTRIB: if (lhs() &&
                                          OPR_CALL == lhs()->opr()->self) {
                                        lhs()->fixContrib(vp);
                                        break;
	                              } else {
                                        vp->reportError(S_ERROR(STS_SYNTAX),
                                                        "Bad contribution");
	                              }
                                      goto use_me;
                    case OPR_HIER:    ret = collapseHier(vp,in_use);
                                      goto done;
                    default:;
                    }
                    if ((LHS = lhs())) {
                      LHS = lhs()->eval(in_use,vp,rff,cr);
                      if (!LHS->constant()) x = VTX_NONE;
                    }
                  do_rhs:
                    if ((RHS = rhs())) {
                      RHS = rhs()->eval(in_use,vp,rff,cr);
                      if (!RHS->constant()) x = VTX_NONE;
                    }
                    xtra = VL_TYP_XTRA(x|xtra);
                    if (lhs() != LHS || rhs() != RHS) {
                      ret = new Expr(this,LHS,RHS,VTX_NONE);
                      goto done;
                    }
                    goto use_me;
  }

  ret = new Expr(this);
done:
  return ret;

use_me:
  xtra = VL_TYP_XTRA(in_use|xtra);
  return this;
}

Expr *ContextObj::evalExpr(eREF cr)
{
  vd()->expr_val->destroy();

  return vd()->expr_val = vd()->cmpld_expr->eval(VTX_INUSE,this,RFF_ALL_LCL,cr);
}

Expr *ContextObj::saveExpr()
{
  Expr *ret = vd()->expr_val;

  DISPOSE(vd()->cmpld_expr);

  vd()->expr_val  = 0;
  ret->xtra = VL_TYP_XTRA(ret->xtra|VTX_INUSE);
  return ret;
}

Usage::~Usage()
{
  DELETE(xpr);
}

Usage::Usage(eUSG usg,Expr *x)
{
  next = 0;
  ignr = 0;
  use  = usg;
  xpr  = x->alias();
}

Expr *ContextObj::logUsage(eUSG use,Expr *xpr)
{
  Stmt *stmt = vd()->process;

  if (stmt) {
    Usage *pd,
           tmp;
    OPR    op;

  retry:
    switch (xpr->vt()) {
    case VT_OPERATOR: op = xpr->op();
                      switch (op) {
                      case OPR_LIST1:
                        logUsage(use,xpr->rhs());
                        xpr = xpr->lhs();
                        goto retry;
                      }
                      break;
    case VT_REFL:     switch (xpr->rt()) {
  		      case REF_PORT:
		        ContextObj *co = getScp(xpr->rrf().scp);
                        Port       *pp = co->port_map(xpr->rrf().index);
                        int         io = pp->io;
		        if (use & USG_DRIVEN)  io |= PRT_DRIVEN;
		        if (use & USG_SENSE)   io |= PRT_SENSED;
		        if (use & USG_CONTRIB) io |= PRT_CONTRIB;
		        if (use & USG_A_READ)  io |= PRT_PROBE;
                        pp->io = PORT_DIR(io);
                      }
                      break;
    case VT_VOID:     switch (xpr->rt()) {
                      case REF_UNKNOWN:
                        Unknown *pu = Root()->unknown_map(xpr->rrf().index);
                        int      io = pu->io;
	  	        if (use & USG_DRIVEN)  io |= PRT_DRIVEN;
		        if (use & USG_SENSE)   io |= PRT_SENSED;
		        if (use & USG_CONTRIB) io |= PRT_CONTRIB;
		        if (use & USG_A_READ)  io |= PRT_PROBE;
                        pu->io = PORT_DIR(io);
                      }
                      break;
    }

    tmp.xpr = xpr;

    if ((pd = stmt->usage()->findord(&tmp,cmpUsage))) {
      pd->use = USG(pd->use|use);
      goto done;
    }

    pd = new Usage(use,xpr->alias());

    stmt->usage()->addord(pd,cmpUsage);

    // dumpUsage(stmt);

  done:
    tmp.xpr = 0;
  }

  return xpr;
}

int fixSysRef(InfoRef *pRef)
{
  switch (pRef->pool) {
    case VERILOG_POOL: switch (pRef->index) {
#   define TASKS(n)    case VER_##n: pRef->index = TSK_##n; goto tsk;
#   include                          "tasks.inc"
                       tsk: pRef->pool  = TASKS_POOL;
                       goto done;
#   define SYSFUNC(n)  case VER_##n: pRef->index = SFN_##n; goto sfn;
#   include                          "sysfunc.inc"
                       sfn: pRef->pool  = SYSFUNC_POOL;
                       goto done;
                       }
  }
 done:
  return pRef->pool;
}

Expr *ContextObj::cmplExpr(InfoRef *pRef,int len,VlType result,StmtCntx cntxt)
{
  int         l,
              u,
              qm,
              lpi   = -1,
              data  =  0,
              top   =  cntxt & SC_TOP,
              chce  =  cntxt & SC_CHOICE;
  OprPrec     prec  =  PRC_MAX;
  poolRef     clsr;
  Operator   *op    =  0,
             *lop   =  0;
  Expr       *left  =  0,
             *right =  0,
             *ret;
  const char *eform,
             *eitem;
  StmtCntx    lhs_cntxt;
  Module     *mod   = (Module *)this; // maybe
  eUSG        lv    =  USG_NONE;
  VlType      typ   = VT_VOID;

  if (top) {
    cntxt = STMT_CNTX(cntxt&~SC_TOP);
  }
  if (chce) {
    cntxt = STMT_CNTX(cntxt&~SC_CHOICE);
    qm    = 0;
  }
  if (cntxt & SC_LVALUE) {
    lv = (cntxt & SC_ANALOG) ? USG_CONTRIB
                             : USG_DRIVEN;
  }
  for (l=0,u=1; l < len ; l++) {
    int opnr  = -1,
        depth;

    switch (pRef[opnr = l].pool) {
    case TIMING_POOL:
      switch(pRef[l].index) {
      case TIM_POSEDGE: op = op->get(OPR_POSEDGE);      goto tst_prc;
      case TIM_NEGEDGE: op = op->get(OPR_NEGEDGE);      goto tst_prc;
      }
      goto keyword;
    case SPECIAL_POOL:
      switch(pRef[l].index) {
      case SPC_ALL: op = op->get(OPR_ANY);              goto tst_prc;
      }
      goto keyword;
    case VERILOG_POOL:
      switch(pRef[l].index) {
      case VER_OR:      op = op->get(OPR_OR);           goto tst_prc;
      case VER_REPEAT:  op = op->get(OPR_REPEAT);       goto tst_prc;
      }
      goto keyword;
    case OPERATORS_POOL:
      switch (pRef[l].index) {
      case OP_ASSIGN:   op = op->get(OPR_ASSIGN);       break;
      case OP_PLUS:     op = u ? op->get(OPR_UPLUS)
                               : op->get(OPR_PLUS);    break;
      case OP_MINUS:    op = u ? op->get(OPR_UMINUS)
                               : op->get(OPR_MINUS);   break;
      case OP_MULTIPLY: op = op->get(OPR_MULTIPLY);     break;
      case OP_DIVIDE:   op = op->get(OPR_DIVIDE);       break;
      case OP_MOD:      op = op->get(OPR_MOD);          break;
      case OP_GT:       op = op->get(OPR_GT);           break;
      case OP_GE:       op = op->get(OPR_GE);           break;
      case OP_LT:       op = op->get(OPR_LT);           break;
      case OP_LE:       op = (top && !(cntxt & SC_BOOL))
                             ? op = op->get(OPR_NBA)
                             : op = op->get(OPR_LE);
                        break;
      case OP_L_NEG:    op = op->get(OPR_L_NEG);        break;
      case OP_L_AND:    op = op->get(OPR_L_AND);        break;
      case OP_L_OR:     op = op->get(OPR_L_OR);         break;
      case OP_L_EQ:     op = op->get(OPR_L_EQ);         break;
      case OP_L_NE:     op = op->get(OPR_L_NE);         break;
      case OP_CS_EQ:    op = op->get(OPR_CS_EQ);        break;
      case OP_CS_NE:    op = op->get(OPR_CS_NE);        break;
      case OP_B_NEG:    op = op->get(OPR_B_NEG);        break;
      case OP_B_AND:    op = op->get(OPR_B_AND);        break;
      case OP_B_OR:     op = u ? op->get(OPR_UB_OR)
                               : op->get(OPR_B_OR);     break;
      case OP_B_XOR:    op = u ? op->get(OPR_UB_XOR)
                               : op->get(OPR_B_XOR);    break;
      case OP_B_EQ1:    op = u ? op->get(OPR_UB_EQ1)
                               : op->get(OPR_B_EQ1);    break;
      case OP_B_EQ2:    op = u ? op->get(OPR_UB_EQ2)
                               : op->get(OPR_B_EQ2);    break;
      case OP_B_RD_AND: op = u ? op->get(OPR_UB_RD_AND)
                               : op->get(OPR_B_RD_AND); break;
      case OP_B_RD_OR:  op = u ? op->get(OPR_UB_RD_OR)
                               : op->get(OPR_B_RD_OR);  break;
      case OP_LFT_SH:   op = op->get(OPR_LFT_SH);       break;
      case OP_RHT_SH:   op = op->get(OPR_RHT_SH);       break;
      case OP_SLFT_SH:  op = op->get(OPR_SLFT_SH);      break;
      case OP_SRHT_SH:  op = op->get(OPR_SRHT_SH);      break;
      case OP_QMARK:    if (chce) {
	                  qm++;
                          continue;
                        }
                        op = op->get(OPR_QMARK);
                        break;
      case OP_CONTRIB:  op = op->get(OPR_CONTRIB);      break;
      case OP_TRIGGER:  op = op->get(OPR_EVENT);        break;
      case OP_AT:       op = op->get(OPR_AT);           break;
      case OP_DELAY:    op = op->get(OPR_DELAY);        break;
      case OP_PATH:     op = op->get(OPR_PATH);         break;
      case OP_MERGE:    op = op->get(OPR_MERGE);        break;
      case OP_POL_POS:  op = op->get(OPR_POL_POS);      break;
      case OP_POL_NEG:  op = op->get(OPR_POL_NEG);      break;
      default:          goto unknown_op;
      }
      goto tst_prc;
    case PUNCTUATION_POOL:
      switch (pRef[l].index) {
      case PUNC_COMMA:  if (top && (cntxt&SC_LIST)
                                && (cntxt&SC_ASSIGN)) {
                          op = op->get(OPR_LIST0);
                        } else if (top && (cntxt&SC_SENSE)) {
                          op = op->get(OPR_OR);
                        } else {
                          op = op->get(OPR_LIST1);
                        }
                        break;
      case PUNC_COLON:  if (chce) {
	                  if (qm) {qm--; continue;}
                          op = op->get(OPR_CHOICE);
                        } else {
                          op = op->get(OPR_RANGE);
                        }
                        break;
      case PUNC_PERIOD: op = l ? op->get(OPR_HIER)
                               : op->get(OPR_BIND);   break;
      case PUNC_OPN_BR: op =(top && (cntxt & SC_INSTANCE)) ? op->get(OPR_INST)
			                                   : data
                                                            ? op->get(OPR_CALL)
                                                            : op->get(OPR_SUB);
                        clsr.pool  = PUNCTUATION_POOL;
                        clsr.index = PUNC_CLS_BR;
                        goto skip2cls;
      case PUNC_OPN_SQ: op = data ? op->get(OPR_INDEX)
                                  : op->get(OPR_ARRAY);
                        clsr.pool  = PUNCTUATION_POOL;
                        clsr.index = PUNC_CLS_SQ;
                        goto skip2cls;
      case PUNC_OPN_CR: op = data ? op->get(OPR_NCAT)
                                  : op->get(OPR_CAT);
                        clsr.pool  = PUNCTUATION_POOL;
                        clsr.index = PUNC_CLS_CR;
                        goto skip2cls;
      }
      goto tst_prc;
    case QUOTES_POOL:
      switch (pRef[l].index) {
      case QUT_SINGLE:  op = op->get(OPR_TICK);     goto tst_prc;
      case QUT_DOUBLE:  op = op->get(OPR_STRING);
                        clsr.pool  = QUOTES_POOL;
                        clsr.index = QUT_DOUBLE;    goto skip2cls;
      }
      break;
    }
  keyword:
    data = 1;
    u    = 0;
    continue;
  skip2cls:
    opnr  = l,
    u     = 0;
    depth = 0;
    while (++l < len ) if      (SAME_REF(pRef[l],clsr) && --depth < 0) break;
                       else if (SAME_REF(pRef[l],pRef[opnr])) depth++;
    if (l >= len) {
      String xpr;
      char   spc[2] = {' ',0};
      reportError(S_ERROR(STS_SYNTAX),"Couldn't find closing '%s' for '%s'",
                             strDeref(clsr),
                             strDerefArr(&pRef[opnr],len - opnr,&xpr,spc));
      goto nll;
    } else {
      int last = l+1 == len;
      int all  = (0 == opnr && last);
      switch (op->self) {
      default:         goto confused;
      case OPR_STRING: if (all) {
                         BaseExpr *sub;
                         if (3 == len) {
                           sub  = new Expr(pRef[1]);
                         } else {
                           assert (2 == len);
                           sub  = new Expr(NullStrRef);
                         }
                         *sub = *BxString;
                         ret  = (Expr*)sub;
                         goto done;
                       }
                       break;
      case OPR_CAT:
      case OPR_ARRAY:
      case OPR_SUB:    if (!all) {
                         data = 1;
                         continue;
                       }
                       break;
      case OPR_INDEX:
      case OPR_INST:
      case OPR_CALL:
      case OPR_NCAT:   if (!last) continue;
      }
    }
  tst_prc:
    if (!op) goto confused;

    if (op->cntxt & cntxt) {
      if (op->preced < prec) {
        prec = op->preced;
        lpi  = opnr;
        lop  = op;
        if (OPR_INDEX == op->self) goto keep_data;
      } else if (op == lop) switch (op->self) {
        case OPR_CHOICE: lpi = opnr; // take last
      }
    }
  unknown_op:
    data = 0;
  keep_data:
    u    = 1;
  }

  switch (prec) {
  case PRC_REPEAT:
    if (lpi != 0) goto confused;
    if (lpi + 1 >= len) {
      eform = "RHS missing for '%s' in: %s";
      eitem = strDeref(pRef[lpi]);
      goto error;
    }
    len--;
    pRef++;
    eitem = "repeat";
    if (!SAME_REF_PI(pRef[lpi],PUNCTUATION_POOL,PUNC_OPN_BR)) {
      eform = "Expected (<expression>) after '%s' in: %s";
    } else {
      int depth  = 0;
      while (++lpi < len) {
        if  (SAME_REF_PI(pRef[lpi],PUNCTUATION_POOL,PUNC_CLS_BR) &&
           --depth < 0) {
          break;
        } else if (SAME_REF_PI(pRef[lpi],PUNCTUATION_POOL,PUNC_OPN_BR)) {
          depth++;
        }
      }
      if (lpi >= len) {
        eform = "Couldn't find ')' for '%s(' in: %s";
        goto error;
      }
      len--;
      pRef++;
      lpi--;
      left  = cmplExpr(pRef,lpi,VT_EXPR,cntxt);
      right = cmplExpr(&pRef[lpi+1],len - (lpi+1),lop->right->vt(),cntxt);
      if (left && right) {
        ret = new Expr(lop->self,left,right);
        goto done;
      }
    }
  case PRC_DE:
    if (lpi != 0) goto confused;
    if (lpi + 1 >= len) {
      eform = "RHS missing for '%s' in: %s";
      eitem = strDeref(pRef[lpi]);
      goto error;
    }
    len--;
    pRef++;
    l = 1;
    if (SAME_REF_PI(pRef[lpi],PUNCTUATION_POOL,PUNC_OPN_BR)) {
      int depth  = 0;
      while (++lpi < len) {
        if  (SAME_REF_PI(pRef[lpi],PUNCTUATION_POOL,PUNC_CLS_BR) &&
           --depth < 0) {
          break;
        } else if (SAME_REF_PI(pRef[lpi],PUNCTUATION_POOL,PUNC_OPN_BR)) {
          depth++;
        }
      }
      if (lpi >= len) {
        eform = "Couldn't find ')' for '%s(' in: %s";
        eitem = strDeref(pRef[-1]);
        goto error;
      }
      len--;
      pRef++;
      lpi--;
    } else if (identifier(pRef[lpi])) {
      lpi++;
      l = 0;
      while (SAME_REF_PI(pRef[lpi+1],PUNCTUATION_POOL,PUNC_PERIOD)) {
        if (!identifier(pRef[lpi+2])) break;
        lpi += 2;
      }
    } else if (number(pRef[0])) {
      lpi++;
      l = 0;
    } else {
      eform = "Bad delay expression after '%s' in: %s";
      eitem = strDeref(pRef[lpi]);
      goto error;
    }
    left   = cmplExpr(pRef,lpi,VT_EXPR,cntxt);
    right  = cmplExpr(&pRef[lpi+l],len - (lpi+l),lop->right->vt(),cntxt);
    if (left && right) {
      ret = new Expr(lop->self,left,right);
      goto done;
    }
    goto abort;
  case PRC_ANY:
    ret = new Expr(lop->self,0,0);
    goto done;

  default:
    lhs_cntxt = cntxt;
    switch (lop->self) {
    case OPR_CONTRIB:
    case OPR_CALL:
      lhs_cntxt = STMT_CNTX(lhs_cntxt|SC_CALL);
      break;
    case OPR_HIER:
      lhs_cntxt = STMT_CNTX(lhs_cntxt|SC_HIER);
      break;
    case OPR_INDEX:
      lhs_cntxt = STMT_CNTX(lhs_cntxt|SC_INDXD);
      break;
    case OPR_LIST0:
    case OPR_LIST1:
      if (!(cntxt & SC_LIST)) {
        String xpr;
        char   spc[2] = {' ',0};
        reportError(S_ERROR(STS_SYNTAX),
                    "List unexpected: ",strDerefArr(pRef,len,&xpr,spc));
      } else if (top) {
        cntxt     = STMT_CNTX(cntxt | SC_TOP);
        lhs_cntxt = STMT_CNTX(cntxt & ~SC_LIST);
      }
    default:;
    }
    typ = VT_VOID;
    if (0 == lpi) {
      while (lop->left && lop->alt != OPR_NULL) lop = lop->get(lop->alt);
      if (lop->left) {
        eform = "'%s' is not a unary operator, in: %s";
        eitem = strDeref(lop->name);
        goto error;
      } else if (OPR_BIND == lop->self) {
        pRef++; len -= 2; lpi =1; typ = VT_REF;
        lhs_cntxt = STMT_CNTX(cntxt | SC_HIER);
        goto do_lhs;
      }
    } else if (!lop->left) {
      eform = "'%s' unary operator, in binary context: %s";
      eitem = strDeref(lop->name);
      goto error;
    } else {
      typ = lop->left->vt();
    do_lhs:
      if (lv) {
	lhs_cntxt = STMT_CNTX(lhs_cntxt & ~SC_LVALUE);
	cntxt     = STMT_CNTX(cntxt     & ~SC_LVALUE);

      } else if (lop->lval) {
	lhs_cntxt = STMT_CNTX(lhs_cntxt | SC_LVALUE);
      }
      if (!(left = cmplExpr(pRef,lpi,typ,lhs_cntxt))) goto abort;
    }
    l = len - (lpi + 1);
    if (!l) {
      switch (lop->self) {
      case OPR_LIST1:
      case OPR_LIST2:
        if (!(cntxt & SC_INSTANCE)) {
          String xpr;
          char   spc[2] = {' ',0};
          reportError(S_WARNING(STS_SYNTAX),
                      "Missing final element in: ... %s",
                      strDerefArr(pRef,len,&xpr,spc));
        }
        break;
      default:
        eform = "RHS missing for '%s' in: %s";
        eitem = strDeref(lop->name);
        goto error;
      case OPR_INST:  mod->inst_count++;
      case OPR_CALL:
      case OPR_BIND:;
      }
    } else {
      StmtCntx rhs_cntxt = cntxt;
      switch (lop->self) {
      case OPR_HIER:    if (!lv && !(rhs_cntxt & SC_HIER)) {
                          lv = (cntxt & SC_ANALOG) ? USG_A_READ
                                                   : USG_READ;
                        }
                        rhs_cntxt = STMT_CNTX(rhs_cntxt | SC_HIER);
	                break;
      case OPR_QMARK:   rhs_cntxt = STMT_CNTX(rhs_cntxt | SC_CHOICE);
                        break;
      case OPR_EVENT:   rhs_cntxt = STMT_CNTX(rhs_cntxt | SC_LVALUE);
			break;
      case OPR_POSEDGE:
      case OPR_NEGEDGE:
                        rhs_cntxt = STMT_CNTX(rhs_cntxt|SC_LEVEL);
			break;
      case OPR_UN_INST:
      case OPR_INST:    mod->inst_count++;
                        goto as_call;
      case OPR_CAT:     if (lv) {
                          lv = USG_NONE;
                        }
      case OPR_SUB:
      case OPR_ARRAY:   if (l != len-1) {
                          clsr.pool  = lop->name.pool;
                          clsr.index = lop->name.index;
                          eform      = "Confused at '%s' in: %s";
                          eitem      = strDeref(clsr);
                          goto error;
                        }
      case OPR_NCAT:
    as_call:
      case OPR_CALL:    if (1 == l) goto null_ok;
                	goto rlist;
      case OPR_INDEX:   if (!lv) {
                          lv = (cntxt & SC_ANALOG) ? USG_A_READ
                                                   : USG_READ;
                        }
      rlist:            l--;
                        rhs_cntxt = STMT_CNTX(rhs_cntxt|SC_LIST);
      default:;
      }

      if (!lop->right) {
	goto confused;
      }

      if (!(right = cmplExpr(&pRef[lpi + 1],l,lop->right->vt(),rhs_cntxt))) {
        goto abort;
      }
    null_ok:;
    }
    switch (lop->self) {
    case OPR_QMARK: if (right->opr()->self != OPR_CHOICE) {
                        eform = "Missing %s for '?' in: %s";
                        eitem = "':'";
                        goto error;
		    }
                    break;
    case OPR_CALL:  if (SAME_REF_PI(*pRef,CHAR_POOL,CHR_DOLLAR)) {
                      switch (fixSysRef(&pRef[1])) {
                      default:           lop = &lop->oprs[OPR_PLI_CALL]; break;
                      case SYSFUNC_POOL:
                      case TASKS_POOL:   lop = &lop->oprs[OPR_SYS_CALL]; break;
                      }
                    } else switch (pRef[0].pool) {
		      case FUNC_POOL:
                      switch (pRef[0].index) {
                        case FNC_IDT:
                        case FNC_DDT: lop = &lop->oprs[OPR_DT_CALL];
                      }
                    }
    default:;
    }
    ret = new Expr(lop->self,left,right);
    if (lv && !(cntxt & (SC_HIER|SC_INSTANCE))) {
      Expr *rfx = ret;
      if (OPR_HIER == lop->self) {
        rfx = collapseHier(ret,VTX_NONE);
        if (rfx != ret) { delete ret;
                          ret = rfx; }
        if (cntxt & SC_SENSE) {
          if (!(cntxt & SC_LEVEL)) ret = new Expr(OPR_ANYEDGE,0,rfx);
          lv = USG(lv|USG_SENSE);
        }
      }
      logUsage(lv,rfx);
    }
    goto done;

  case PRC_MAX:
  case PRC_TICK: if (!len) return 0;
                 ret = evalData(pRef,len);
                 if (ret) {
                   Expr *rfx = ret;
                   if (cntxt & (SC_HIER|SC_CALL)) {
                     // not a valid ref.
		   } else {
                     if (rfx->vt() == VT_VOID && rfx->rt() == REF_UNKNOWN) {
                       if (evalRef(rfx,RFF_DATA,REF_NONE) < 0) goto done;
                     }
                     if (cntxt & SC_INSTANCE) {
                       Port *pp;
                       switch (rfx->rt()) {
  		       case REF_PORT: pp     = port_map(rfx->rrf().index);
                                      pp->io = PORT_DIR(pp->io|PRT_XPRTD);
                       }
                     } else if (!(cntxt & SC_INDXD)) {
                       if ((cntxt & SC_SENSE) && !(cntxt & SC_LEVEL)
                                              && !rfx->constant()) {
                         ret = new Expr(OPR_ANYEDGE,0,rfx);
                       }
                       switch (rfx->vt()) {
                       case VT_REFL:
                       case VT_REFG: switch (rfx->rt()) {
  		                     default:
                                       if (rfx && (cntxt & SC_LVALUE)) {
                                         if (cntxt & SC_ANALOG) {
                                           logUsage(USG_CONTRIB,rfx);
                                         } else {
                                           logUsage(USG_DRIVEN,rfx);
                                         }
                                       } else if (!rfx->constant()) {
                                         if (cntxt & SC_ANALOG) {
                                           logUsage(USG_A_READ,rfx);
                                         } else {
                                           if (cntxt & SC_SENSE) {
                                             logUsage(USG_SENSE,rfx);
                                           } else {
                                             logUsage(USG_READ,rfx);
                                           }
                                         }
                                       }
                  		   case REF_PARM:;
                                     }
                       }
                     }
  		   }
                 }
                 goto done;
  }

confused:
  eform = "Confused @ %s%s";
  eitem = "";
error: {
     String xpr;
     char   spc[2] = {' ',0};
     reportError(S_ERROR(STS_SYNTAX),eform,eitem,strDerefArr(pRef,len,&xpr,spc));
  }
abort:
  DELETE(left);
  DELETE(right);
nll:
  ret = 0;
done:
  return ret;
}

Expr *ContextObj::cmplExpr(VlType typ)
{
  DISPOSE(vd()->cmpld_expr);

  return vd()->cmpld_expr = cmplExpr(vd()->expr,vd()->expr_len,typ,vd()->cntxt);
}

Expr *ContextObj::cmplExpr()
{
  return cmplExpr(VT_VOID);
}

void ShowExpr(ContextObj *co)
{
  String xpr;
  char   spc[2] = {' ',0};
  fprintf(stderr,"%s\n",strDerefArr(co->vd()->expr,co->vd()->expr_len,&xpr,spc));
}

void ContextObj::addExprAttrSub()
{
  if (vd()->expr_attr) {
    int     l = vd()->expr_attr-vd()->expr_len;
    InfoRef *IR = new InfoRef[l];
    BCOPY(&vd()->expr[vd()->expr_len],IR,l * sizeof(InfoRef));
    assert(NULL_REF(IR[0]));
    IR[0]           = vd()->expr_term;
    vd()->expr_attr = 0;
    if (SAME_REF_PI(IR[0],PUNCTUATION_POOL,PUNC_OPN_BR)) {
      assert(SAME_REF_PI(IR[1],OPERATORS_POOL,OP_MULTIPLY));
      SaveAttr(0,0,1,IR+2,l-2);
    } else {
      SaveAttr(0,0,1,IR,l);
    }
    delete [] IR;
  }
}

const Token *ContextObj::addExprAttr(const Token *T,AttrObj *to_obj)
{
  addExprAttrSub();

  addPendAttr(to_obj);

  return T;
}

const Token *ContextObj::addExprAttr(const Token *T,Attr *to_obj)
{
  addExprAttrSub();

  addPendAttr(to_obj);

  return T;
}

const Token *ContextObj::SaveAttr(const Token *T0,const Token *TL,int bail_none,
				  InfoRef *pRef,int len)
{
  const Token *T   = T0;
  int          a   = 0,
               bad = 0;
  InfoRef      Ref1;
  poolRef      Name,
               Sub = NullRef;

# define NEXT_REF(r,brk) if (pRef) { if (len <= 0) brk;  \
 		  	             r = *pRef++; len--;}\
                         else T = getRef(T,TL,&r);

  for (; pRef || T < TL; a++) {
    NEXT_REF(Ref1,break);
   next:
    switch (Ref1.pool) {
    case VERILOG_POOL: switch(Ref1.index) {
                       case VER_CLS_ATTR: goto done;
                       }
    }
    if (bad) continue;
  check:
    if (!checkName(Ref1)) {
      if (0 == a && bail_none) return 0;
      bad++;
      reportError(S_ERROR(STS_SYNTAX),"Expected a name not '%'",strDeref(Ref1));
      continue;
    } 
    Name = Ref1;
    NEXT_REF(Ref1,Ref1 = NullRef);
    if (!SAME_REF_PI(Ref1,OPERATORS_POOL,OP_ASSIGN)) {
      switch (Ref1.pool) {
      case PUNCTUATION_POOL: 
	switch(Ref1.index) { case PUNC_COMMA: goto no_value; 
	                     case PUNC_COLON: Sub = Name; 
                                              NEXT_REF(Ref1,Ref1 = NullRef);
					      goto check; }
	break;
      case VERILOG_POOL: 
	switch(Ref1.index) { case VER_CLS_ATTR:
	                     no_value:
			          addAttr(Name); goto next;
	}
      }
      reportError(S_ERROR(STS_SYNTAX),"Expected '=',',' or '*)'");
      bad++; continue;
    }
    T = getExpr(T0 = T,TL,SC_ATTR,pRef ? &pRef 
                                       : 0,    &len);
    if (cmplExpr() && evalExpr()) {
      Attr *attr = addAttr(Name,saveExpr());
      if (!NULL_REF(Sub)) {
	poolRef cls = {CPP_POOL,CPP_CLASS};
	attr->attr  = new AttrAny(cls,Sub);
      }
    }
  }

# undef NEXT_REF

  reportError(S_ERROR(STS_SYNTAX),"Didn't find closing '*)'",strDeref(Ref1));
 done:
  return T;
}

void Nature::getAttr(const Token *T0,const Token *TL,int to_end,int id)
{
  const Token *T  = T0,
              *TN;
  InfoRef      Ref1,
               Ref2;
  Nature      *prev;
  NatureIdx    pi;

  for (; T < TL ; T++) {
    T = getRef(T,TL,&Ref1);
    switch (Ref1.pool) {
    case WHITESPACE_POOL:
      continue;
    case PUNCTUATION_POOL:
      switch (Ref1.index) {
      case PUNC_SCOLON: reportError(S_WARNING(STS_SYNTAX),"Extraneous ';'"); break;
      default:          reportError(S_ERROR(STS_SYNTAX),  "Didn't expect '%s'",
                                                            strDeref(Ref1)); break;
      }
      continue;
    case VERILOG_POOL:
      switch (Ref1.index) {
      Attr   *attr;
      case VER_ABSTOL:
	T = skipToken(T,TL,TOK_OP_ASSIGN,"after abstol");
        T = getExpr(T,TL,SC_NATURE);
	T = skipToken(T,TL,TOK_PUNC_SCOLON,"after abstol expression");
        if (cmplExpr() && evalExpr()) {
          if (!vd()->expr_val->constant()) {
            reportError(S_ERROR(STS_SYNTAX),"Abstol should be a constant");
          } else {
            addAttr(Ref1,vd()->expr_val->dbl());
          }
        }
	break;
      case VER_UNITS:
	T = skipToken(T,TL,TOK_OP_ASSIGN,"after units");
	T = skipToken(T,TL,TOK_QUT_DOUBLE,"units string");
        T = getRef(T,TL,&Ref2);
        attr      = addAttr(Ref1,Ref2,ATTR_STRING);
	T = skipToken(T,TL,TOK_QUT_DOUBLE,"after units string value");
	T = skipToken(T,TL,TOK_PUNC_SCOLON,"after units string");
	break;
      case VER_ACCESS:
	T = skipToken(T,TL,TOK_OP_ASSIGN,"after access");
        T = getRef(T,TL,&Ref2);
	T = skipToken(T,TL,TOK_PUNC_SCOLON,"ending access statement");
        if (id < 0) {
          reportError(S_ERROR(STS_SYNTAX),Ref1,"Access function inappropriate");
        } else {
          addAttr(Ref1,Ref2);
          logGlobal(Ref2,REF_ACCESS,id);
        }
	break;
      case VER_DDT_NATURE:
      case VER_IDT_NATURE:
	T    = skipToken(T,TL,TOK_OP_ASSIGN);
        T    = getRef(TN = T,TL,&Ref2);
        while (!(prev = nature_map(pi = findNature(Ref2)))) {
          prev = ReadNature(TN,T,-1);
        }
        addAttr(Ref1,prev,pi);
	break;
      case VER_ENDNATURE:
	assert(T == TL);
	return;
      default:
        reportError(S_ERROR(STS_SYNTAX),"Invalid attribute name `%s`",strDeref(Ref1));
      }
      break;
    default:
      checkName(Ref1);
      T = skipToken(T,TL,TOK_OP_ASSIGN);
      T = getExpr(T,TL,SC_NATURE);
      T = skipToken(T,TL,TOK_PUNC_SCOLON);
      if (cmplExpr() && evalExpr()) {
        addAttr(Ref1,saveExpr());
      }
    }
  }

  if (to_end) reportError(S_ERROR(STS_SYNTAX),"keyword 'endnature' missing");
}

Nature::Nature(NatureIdx up)
{
  flags   = DF_NONE;
  name    = NullRef;
  parent  = up;
  next    = 0;
  typ     = AO_NATURE;
}

void Nature::init(const Token *T0,const Token *TL,int id)
{
  const Token *T  = T0;
  InfoRef      Ref1,
               Ref2;

  T = getRef(T,TL,&Ref1);
  if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_COLON)) {
    T = getRef(T,TL,&Ref1);
    if (SAME_REF(Ref1,NullRef)) {
      reportError(S_ERROR(STS_SYNTAX),"Parent nature name missing");
    } else {
      T = getRef(T0 = T,TL,&Ref2);
      switch (Ref2.pool) {
      case PUNCTUATION_POOL:
        switch (Ref2.index) {
          case PUNC_PERIOD:
	    DiscIdx  di   = findDisc(Ref1);
            Disc    *disc = 0;
	    if (di < 0) reportError(S_ERROR(STS_SYNTAX),"Discipline not found");
            else        disc = disc_map(di);
	    T = getRef(T,TL,&Ref2);
	    if (SAME_REF_PI(Ref2,VERILOG_POOL,VER_POTENTIAL)) {
	      if (disc) parent =  disc->potential;
	      T0 = T;
	    } else if (SAME_REF_PI(Ref2,VERILOG_POOL,VER_POTENTIAL)) {
	      if (disc) parent =  disc->flow;
	      T0 = T;
	    } else if (disc) {
	      reportError(S_ERROR(STS_SYNTAX),"Invalid nature: %s.%s",
		                              strDeref(Ref1),strDeref(Ref2));
	    }
	}
	break;
      default:
	int id = findNature(Ref1);
        T = T0;
        if (id >= 0) {
	  parent = id;
        } else {
          reportError(S_ERROR(STS_SYNTAX),"Parent nature (%s) for '%s' not found",
                                           strDeref(Ref1),strDeref(name));
	}
      }
    }
  } else {
    T = rescanning(T0);
  }

  getAttr(T,TL,1,id);
}

Nature *VerilogObj::ReadNature(const Token *T0,const Token *TL,NatureIdx up)
{
  const Token *T  = T0;
  int          id;
  Nature      *prev;
  InfoRef      nm;

  T = getRef(T,TL,&nm);

  if (SAME_REF(nm,NullRef)) reportError(S_ERROR(STS_SYNTAX),"Nature name missing");

  if ((id = findNature(nm)) >= 0) {
    prev = nature_map(id);
    if (prev->attr_list) {
      reportError(S_ERROR(STS_SYNTAX),"Nature '%s' already declared",strDeref(nm));
    }
  } else {
    logGlobal(nm,REF_NATURE,id = nature()->add(prev = new Nature(up)));
    prev->name = nm;
  }

  if (T < TL) prev->init(T,TL,id);

  return prev;
}

Disc::Disc(const Token *T0,const Token *TL,Disc *up)
{
  const Token *T = T0,
              *TE;
  InfoRef      Ref1,
               Ref2;
  Nature      *own;

  typ       = AO_DISC;
  next      = 0;
  parent    = -1;
  flow      = -1;
  potential = -1;

  flags = DF_VOID;

  T = getRef(T,TL,&name);

  if (SAME_REF(name,NullRef)) {
    reportError(S_ERROR(STS_SYNTAX),"Discipline name missing");
  }

  if (findDisc(name) >= 0) {
    reportError(S_ERROR(STS_SYNTAX),"Discipline '%s' already declared",strDeref(name));
  }

  T = getRef(T,TL,&Ref1);
  if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_COLON)) {
    for (;;) {
      T = getRef(T,TL,&Ref1);
      if ((parent = findDisc(Ref1)) < 0) switch (Ref1.pool) {
      case VERILOG_POOL:
	switch (Ref1.index) {
        case VER_DIGITAL:   flags = DSC_FLGS(flags & ~DF_ANALOG);    continue;
        case VER_ANALOG:    flags = DSC_FLGS(flags & ~DF_DIGITAL);   continue;
        case VER_POTENTIAL: flags = DSC_FLGS(flags & ~DF_FLOW);      continue;
        case VER_FLOW:      flags = DSC_FLGS(flags & ~DF_POTENTIAL); continue;
	}
      }
      break;
    }
    if (parent < 0) {
      if (SAME_REF(Ref1,NullRef))
        reportError(S_ERROR(STS_SYNTAX),"Parent discipline name missing");
      else
        reportError(S_ERROR(STS_SYNTAX),"Parent discipline not found");
    }
  } else if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_SCOLON)){
    reportError(S_WARNING(STS_SYNTAX),"Extraneous ';'");
  } else {
    goto have_ref;
  }

  do {
    T = getRef(T,TL,&Ref1);
   have_ref:
    switch (Ref1.pool) {
    case WHITESPACE_POOL:
      continue;
    case VERILOG_POOL:
      switch (Ref1.index) {
      case VER_FLOW:
        T = getRef(T,TL,&Ref2);
        if (SAME_REF_PI(Ref2,PUNCTUATION_POOL,PUNC_PERIOD)) {
          if (!(flags & DF_OWN_FLOW)) {
            flow = nature()->add(own = new Nature(flow));
          } else {
            own = nature_map(flow);
          }
          goto get_attr;
        } else if (flow >= 0) {
          reportError(S_ERROR(STS_SYNTAX),"Flow already specified (%s)",strDeref(Ref2));
        } else if ((flow = findNature(Ref2)) < 0) {
          reportError(S_ERROR(STS_SYNTAX),"Invalid flow name `%s`",strDeref(Ref2));
        }
	T = skipToken(T,TL,TOK_PUNC_SCOLON);
	break;
      case VER_POTENTIAL:
        T = getRef(T,TL,&Ref2);
        if (SAME_REF_PI(Ref2,PUNCTUATION_POOL,PUNC_PERIOD)) {
          if (!(flags & DF_OWN_POTENTIAL)) {
            potential = nature()->add(own = new Nature(potential));
          } else {
            own = nature_map(potential);
          }
          goto get_attr;
        } else if (potential >= 0) {
          reportError(S_ERROR(STS_SYNTAX),"Potential already specified (%s)",
                                 strDeref(Ref2));
        } else if ((potential = findNature(Ref2)) < 0) {
          reportError(S_ERROR(STS_SYNTAX),"Invalid potential name `%s`",strDeref(Ref2));
        }
	T = skipToken(T,TL,TOK_PUNC_SCOLON);
	break;
      case VER_DOMAIN:
        T = getRef(T,TL,&Ref2);
        if (SAME_REF_PI(Ref2,VERILOG_POOL,VER_CONTINUOUS)) {
          flags = DSC_FLGS(flags & ~DF_DIGITAL);
        } else if (SAME_REF_PI(Ref2,VERILOG_POOL,VER_DISCRETE)) {
          flags = DSC_FLGS(flags & ~DF_ANALOG);
        } else {
          reportError(S_ERROR(STS_SYNTAX),"Invalid domain name `%s`",strDeref(Ref2));
        }
	T = skipToken(T,TL,TOK_PUNC_SCOLON);
	break;
      case VER_ENDDISCIPLINE:
        logGlobal(name,REF_DISC,disc()->add(this));
	assert(T == TL);
	return;
      get_attr:
        TE = endExpr(T,TL);
        own->getAttr(T,TE,0,-1);
        T  = TE;
        break;
      default:
        reportError(S_ERROR(STS_SYNTAX),"Invalid attribute name `%s`",strDeref(Ref1));
      }
      break;
    default:
      checkName(Ref1);
      T = skipToken(T,TL,TOK_OP_ASSIGN);
      T = getExpr(T,TL,SC_DISCIPLINE);
      T = skipToken(T,TL,TOK_PUNC_SCOLON);
      if (cmplExpr() && evalExpr()) {
        addAttr(Ref1,saveExpr());
      }
    }
  } while (T < TL);

  reportError(S_ERROR(STS_SYNTAX),"keyword 'enddiscipline' missing");
}

int getDisc(poolRef nm)
{
  Expr        drf(nm);
  VerilogObj *po = 0;

  return po->Root()->evalRef(&drf,RFF_DISC,REF_NONE);
}

Port *PrtdObj::findPort(poolRef nm)
{
  Port *scan = port()->first();

  for (; scan ; scan = scan->next) {
    if (SAME_REF(nm,scan->name)) break;
  }
  return scan;
}

void VerilogObj::setTimescale(int t1,int t2,int p1,int p2)
{
  vd()->ts1    = t1;
  vd()->ts2    = t2;
  vd()->prcsn1 = p1;
  vd()->prcsn2 = p2;
}

int VerilogObj::addLibrary(const char *pth0)
{
  Filename pth(pth0);
  String   libx(pth.basename());
  int      l = 0;

  return libLst()->add(new Library(libx.str(),pth));
}

LibraryIdx Library::current = -1;

int Library::setLibrary(int l)
{
  int      was  = current;
  Library *plib = 0;

  if ((plib = libLst_map(l))) {
    current = l;
    envAdd(V2K_LIBRARY,strDeref(plib->name));
  }

  return was;
}

int VerilogObj::setLibrary(const char *lib_name)
{
  int      l    = 0;
  Library *plib = 0;

  if (!lib_name || !*lib_name) lib_name = "${" V2K_DEFLIB "}";

  String libx(lib_name);
  envExpand(&libx);

  if ((l = findLib(libx.str())) > 0) goto finish;

  plib = new Library(libx.str());
  l    = libLst()->add(plib);

finish:
  plib->setLibrary(l);
  return vd()->lib_indx = l;
}

LibraryIdx VerilogObj::findLib(const char *lib_name)
{
  Library *plib;
  poolRef  ref = strFind(lib_name);
  int      l   = 0;

  for (; (plib = libLst_map(l)) ; l++) {
    if (0 == strcmp(strDeref(plib->name),lib_name)) return l;
  }

  return -1;
}

Library::Library(const char *lib_name)
{
  Init();

  name    = strSaveStr(lib_name);
  pth_typ = AT_NONE;
}

Library::Library(const char *lib_name,const char *pth)
{
  Init();

  name    = strSaveStr(lib_name);
  path    = pth;
  pth_typ = AT_PATH;
}

extern "C" int setLibrary(const char *lib)
{
  VerilogObj *vo = 0;

  return vo->setLibrary(lib);
}

extern "C" int addLibrary(const char *lib)
{
  VerilogObj *vo = 0;

  return vo->addLibrary(lib);
}

const char *VerilogObj::libName()
{
  Env lib(V2K_LIBRARY);

  return lib;
}

void VerilogObj::setDefaultDisc(poolRef Ref)
{
  vd()->def_dsc_nm  = Ref;
  vd()->def_disc    = 0;
  vd()->def_disc_id = -1;
}

void VerilogObj::setDefaultNetType(poolRef Ref)
{
  vd()->def_net_type  = Ref;
}

void VerilogObj::setUnconnected(poolRef Ref)
{
  vd()->unconnected = Ref;
}

int VerilogObj::setCellDefine(int t)
{
  int r = vd()->vmode;

  vd()->vmode = t ? VMD(r |  VMD_CELL)
                  : VMD(r & ~VMD_CELL);

  return r & VMD_CELL;
}

int VerilogObj::setProtect(int t)
{
  int r = vd()->protect;

  vd()->protect = t;

  return r;
}

void VerilogObj::resetAll()
{
  setDefaultDisc(NullRef);
  setDefaultNetType(NullRef);
  setUnconnected(NullRef);
  setCellDefine(0);
}

int VerilogObj::reposPath(File *ret,
			  const char *root,const char *nm,const char *stem,eObjTyp typ)
{
  *ret  = root;
  ret->down(nm,false);
  *ret += stem;
  ret->changeType(ObjTypFT[typ]);

  return 0;
}

int VerilogObj::checkObject(eObjTyp typ,const char *nm,Filename *fnm)
{
  File f;

  reposPath(&f,"${" V2K_LIB_REPOS "}/",nm,"pool",typ);

  return f.Exists();
}

int VerilogObj::loadObject(eObjTyp typ,const char *nm,Filename *fnm)
{
  const char *lib      = "${" V2K_LIB_REPOS "}/",
             *pool     = "pool";
  String      nm2;
  int         l,
              lib_indx = vd()->lib_indx;
  poolRef     ref;
  eRFF        rff;
  refTup      rtp;
  Filename    base;
  eObjTyp     typ2;

  Init();

  if (!nm) {
    nm = fnm->basename(base);
  }

  ref = strFind(nm);

  switch (typ2 = typ) {
  default:            return 0;
  case OBJ_Primitive: rff  = RFF_PRIM;   break;
  case OBJ_Null:      typ2 = OBJ_Module;
  case OBJ_Module:    rff  = RFF_MODULE; break;
  }

  if (findLibObj(rff,lib_indx,ref,&rtp) > 0) return -1;

  File f;

  reposPath(&f,lib,nm,"pool",typ2);

  Init();

  switch (typ2) {
  case OBJ_Module:    return loadModule(&f,lib_indx);
  case OBJ_Primitive: return loadPrim  (&f,lib_indx);
  default:;
  }

  return 0;
}

extern "C" int loadObject(int typ,const char *nm,char *pf)
{
  VerilogObj *vo = 0;

  return vo->loadObject((eObjTyp)typ,nm,(Filename *)pf);
}

typedef struct {
  anyCB  call;
  void  *dp;
} CBOD;

void *ContextObjCB(ContextObj *obj,CBOD *dp)
{
  void       *ret  = 0;
  const char *xtra = 0;

  switch (obj->typ) {
  case AO_MACRO:  xtra = " (macro)";
  case AO_MODULE: { Module *mod = (Module *)obj;
                    int     lid = mod->lib_indx;
                    ret = (*dp->call)(dp->dp,
                               lid >= 0 ? strDeref(mod->libLst_map(lid)->name)
                                        : "?",
                               ":",strDeref(mod->name),".mod",xtra,0);
                  } break;
  case AO_PRIM:   { Prim *prm = (Prim *)obj;
                    int     lid = prm->lib_indx;
                    ret = (*dp->call)(dp->dp,
                               lid >= 0 ? strDeref(prm->libLst_map(lid)->name)
                                        : "?",
                                ":",strDeref(prm->name),".udp",xtra,0);
                  } break;
  default:;
  }
  return ret;
}

extern "C" int prntAllObj(int rrf,anyCB call,void *dp)
{
  VerilogObj *vo = 0;
  CBOD        data;

  data.call = call;
  data.dp   = dp;

  if (rrf & RFF_MODULE) vo->module()->for_all((listCB)ContextObjCB,&data);
  if (rrf & RFF_PRIM)   vo->prim()->for_all((listCB)ContextObjCB,&data);

  return 0;
}

ContextObj *ContextObj::getScp(int i)
{
  ContextObj *co = Root();

  if (i >= 0) {
    Scope *scp = co->scope_map(i);
    Stmt  *ss  = scp->stmt;
    switch (ss->stmt_typ()) {
    case STMT_FUNC_A:
    case STMT_FUNC:    co = funcObj(ss); break;
    case STMT_TASK:    co = taskObj(ss); break;
    default:           co = ((StmtDecl *)ss)->lcls;
    }
  }

  return co;
}

Disc *ContextObj::findBrDisc(Expr *n1,Expr *n2)
{
  Port   *prt;
  Disc   *dsc[2];
  int     n;
  Expr   *xpr[2];
  Branch *br;

  xpr[0] = n1;
  xpr[1] = n2;

  dsc[0] = 0;

  for (n = 0; n < 2 && xpr[n] ; n++) {

    if (VT_OPERATOR == xpr[n]->vt() && OPR_INDEX == xpr[n]->opr()->self) {
      xpr[n] = xpr[n]->lhs();
    }

    if (VT_REFL != xpr[n]->vt()) return 0;

    switch (xpr[n]->rtyp) {
    default:
      return 0;
    case REF_BRANCH:
      br = getScp(xpr[n]->rrf().scp)->branch_map(xpr[n]->rrf().index);
      return findBrDisc(br->p1,br->p2);
    case REF_PORT:
      prt     = getScp(xpr[n]->rrf().scp)->port_map(xpr[n]->rrf().index);
      prt->io = PORT_DIR(prt->io|PRT_BRANCH);
      if (!(dsc[n] = disc_map(prt->dsc_indx))) {
        if (vd()->def_disc) {
          dsc[n] = vd()->def_disc;
        } else {
          if (NULL_REF(vd()->def_dsc_nm)) {
            dsc[n] = vd()->def_disc = disc_map(0);
          } else {
            dsc[n] = vd()->def_disc
                   = disc_map(vd()->def_disc_id = findDisc(vd()->def_dsc_nm));
          }
        }
      }
    }

    if (!dsc[n]) return 0;

    while (dsc[n]->parent >= 0) dsc[n] = disc_map(dsc[n]->parent);

    if (dsc[n] != dsc[0]) {
      String s;
      xpr[0]->strValue(&s);
      s += ",";
      xpr[n]->strValue(&s);
      reportError(S_ERROR(STS_SYNTAX),"Nodes are incompatible: %s",s.str());
      return 0;
    }
  }

  return dsc[0];
}

Port::Port(PortDir pd, PortTyp pt, SigType st, Strength s, int v,DiscIdx dsc)
{
  typ      = AO_PORT;
  next     = 0;
  io       = pd;
  ptyp     = pt;
  styp     = st;
  dsc_indx = dsc;
  chrg     = CHG_NONE;
  value    = v;
  ds0      = s;
  ds1      = s;
  pckd     = 0;
  unpckd   = 0;
  dly      = 0;
  rng      = 0;

}

DrvStrnth drvStrength(poolRef Ref)
{
  DrvStrnth ds;

  if (VERILOG_POOL == Ref.pool) switch(Ref.index) {
  case VER_SUPPLY0: ds.lvl = 0; ds.strnth = DS_SUPPLY; goto done;
  case VER_STRONG0: ds.lvl = 0; ds.strnth = DS_STRONG; goto done;
  case VER_PULL0:   ds.lvl = 0; ds.strnth = DS_PULL;   goto done;
  case VER_WEAK0:   ds.lvl = 0; ds.strnth = DS_WEAK;   goto done;
  case VER_HIGHZ0:  ds.lvl = 0; ds.strnth = DS_HIGHZ;  goto done;
  case VER_SUPPLY1: ds.lvl = 1; ds.strnth = DS_SUPPLY; goto done;
  case VER_STRONG1: ds.lvl = 1; ds.strnth = DS_STRONG; goto done;
  case VER_PULL1:   ds.lvl = 1; ds.strnth = DS_PULL;   goto done;
  case VER_WEAK1:   ds.lvl = 1; ds.strnth = DS_WEAK;   goto done;
  case VER_HIGHZ1:  ds.lvl = 1; ds.strnth = DS_HIGHZ;  goto done;
  }
  ds.strnth = DS_NONE;
done:
  return ds;
}

const Token *PrtdObj::getBranches(const Token *T0,const Token *TL)
{
  const Token *T = T0;
  Expr        *expr1,
              *expr2;
  InfoRef      Ref1;
  int          count;

  T = getRef(T,TL,&Ref1);
  for (;;) {
    if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_OPN_BR)) {
      expr1 = expr2 = 0;
      T = getExpr(T,TL,SC_REFERENCE);
      if (cmplExpr() && evalExpr()) {
        expr1 = saveExpr();
      }
      T = getRef(T0 = T,TL,&Ref1);
      if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_COMMA)) {
	T = getExpr(T,TL,SC_REFERENCE);
	if (cmplExpr() && evalExpr()) {
	  expr2 = saveExpr();
	}
	T = skipToken(T,TL,TOK_PUNC_CLS_BR);
      } else if (!SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_CLS_BR)) {
	 reportError(S_ERROR(STS_SYNTAX),"Expected `)` or `,` after first node in branch");
      }
      T = getRef(T0 = T,TL,&Ref1);
      count = 0;
      while (!SAME_REF(NullRef,Ref1) &&
             !SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_OPN_BR) &&
             !SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_SCOLON)) {
        if (checkName(Ref1)) count++;
        T = getRef(T,TL,&Ref1);
        if (!SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_COMMA)) break;
        T = getRef(T,TL,&Ref1);
      }
      if (count) {
        Branch *brnch = new Branch(expr1,expr2,count);
        int     n     = branch()->add(brnch);
        count  = 0;
        T      = getRef(T0,TL,&Ref1);;
        while (!SAME_REF(NullRef,Ref1) &&
               !SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_OPN_BR) &&
               !SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_SCOLON)) {
          brnch->name[count++] = Ref1;
          logLocal(Ref1,REF_BRANCH,n);
          T = getRef(T,TL,&Ref1);
          if (!SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_COMMA)) break;
          T = getRef(T,TL,&Ref1);
        }
      } else {
        expr1->dispose();
        expr2->dispose();
      }
    } else {
      reportError(S_ERROR(STS_SYNTAX),"Expected `(` to start terminal list");
    }
    if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_SCOLON)) break;
  }

  return T;
}

#define PP_VAR \
  PortDir      p_io = PRT_INVALID;\
  PortTyp      pt   = PT_NONE;\
  SigType      st   = ST_BIT;\
  Strength     drv  = DS_NORM;\
  DiscIdx      disc = -1;\
  int          val  = 0,\
               dpth = vd()->depth;\
  const Token *TS;\
  AttrType     a_t;\
  Range       *rng;\
  Expr        *xpr;\
  Attr        *atr;\
  InfoRef      Ref1,\
               Ref2;

const Token *PrtdObj::getParam(const Token *T0,const Token *TL)
{
  PP_VAR

  const Token *T  = T0;
  PrtdObj     *po = this;

 new_param:
#include "param.inc"

  return T;
}

const Token *PrtdObj::getPorts(const Token *T0,const Token *TL,
                               PortDir decl_io, PortTyp decl_t, SigType st,
                               Strength drv, int value,
                               DiscIdx disc, int mode)
{
  const Token *T        = T0;
  int          opnr     = 0,
               rgx_used = 0,
               rgx_cnt  = 0,
               dlx_used = 0,
               uc       = 0,
               last     = 0,
               pckd     = 0,
               pck_1    = 0,
               r;
  InfoRef      Ref1;
  Port        *prt      = 0;
  Expr        *rgx[12],
              *dlx      = 0;
  DrvStrnth    drv2[2],
              *pd       = 0;
  PortDir      p_io     = decl_io;
  PortTyp      pt       = decl_t;

  T = getRef(T,TL,&Ref1);
  if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_SCOLON)) return T0;

  if (0 == mode) {

    if (SAME_REF_PI(Ref1,OPERATORS_POOL,OP_DELAY)) {
       T = getRef(T,TL,&Ref1);
       if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_OPN_BR)) {
          T = getParam(T,TL);
          T = getRef(T,TL,&Ref1);
       }
    }

    if ((opnr = SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_OPN_BR))) {
      T = getRef(T0=T,TL,&Ref1);
      if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_CLS_BR)) {
        reportError(S_WARNING(STS_SYNTAX),"Empty port list");
        goto done;
      }
    } else {
      reportError(S_ERROR(STS_SYNTAX),"Expected `(` to start port list");
    }

    if (!(p_io & PRT_INOUT)) {
      if (SAME_REF_PI(Ref1,VERILOG_POOL,VER_INPUT)) {
        p_io  = PORT_DIR(PRT_IN|p_io);
        T     = getRef(T0=T,TL,&Ref1);
	pck_1 = 2;
      } else if (SAME_REF_PI(Ref1,VERILOG_POOL,VER_OUTPUT)) {
        p_io  = PORT_DIR(PRT_OUT|p_io);
        T     = getRef(T0=T,TL,&Ref1);
	pck_1 = 2;
      } else if (SAME_REF_PI(Ref1,VERILOG_POOL,VER_INOUT)) {
        p_io  = PORT_DIR(PRT_INOUT|p_io);
        T     = getRef(T0=T,TL,&Ref1);
	pck_1 = 2;
      }
    }

    if (PT_NONE == pt) {
      if (SAME_REF_PI(Ref1,VERILOG_POOL,VER_REG)) {
        pt    = PT_REG;
        T     = getRef(T0=T,TL,&Ref1);
	pck_1 = 2;
      } else if (SAME_REF_PI(Ref1,VERILOG_POOL,VER_WIRE)) {
        pt    = PT_WIRE;
        T     = getRef(T0=T,TL,&Ref1);
        pck_1 = 2;
      }
    }
  }

  if (SAME_REF(Ref1,TOK_VER_SIGNED.pi)) {
    pt = PORT_TYP(pt|PT_SIGNED);
    T  = getRef(T,TL,&Ref1);
    if (1 == mode) decl_t = PORT_TYP(decl_t|PT_SIGNED); 
  }

  if (SAME_REF_PI(Ref1,VERILOG_POOL,VER_SCALARED)) {
    pt = PORT_TYP(pt|PRT_SCALARED);
    T  = getRef(T,TL,&Ref1);
  } else if (SAME_REF_PI(Ref1,VERILOG_POOL,VER_VECTORED)) {
    pt = PORT_TYP(pt|PRT_VECTORED);
    T  = getRef(T,TL,&Ref1);
  }

  while (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_OPN_SQ)) {
    T = getExpr(T,TL,SC_MODULE);
    if (cmplExpr() && evalExpr()) {
      rgx[pckd++] = saveExpr();
    }
    T = skipToken(T,TL,TOK_PUNC_CLS_SQ);
    T = getRef(T,TL,&Ref1);
  }

  if (SAME_REF_PI(Ref1,OPERATORS_POOL,OP_DELAY)) {
    T = getDelayExpr(T,TL);
    if (cmplExpr() && evalExpr()) {
      dlx = saveExpr();
    }
    T = getRef(T,TL,&Ref1);
  }

  if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_OPN_BR)) {
    pd = drv2;
    T = getRef(T,TL,&Ref1);
    pd[0] = drvStrength(Ref1);
    T = skipToken(T,TL,TOK_PUNC_COMMA,"while parsing port");
    T = getRef(T,TL,&Ref1);
    pd[1] = drvStrength(Ref1);
    T = skipToken(T,TL,TOK_PUNC_CLS_BR);
    T = getRef(T,TL,&Ref1);
  }

  for (;;) {

    poolRef ref_nm = NullRef;

    if (--pck_1 == 0) {
      r = rgx_cnt;
      if (rgx_used) while (--r >= 0) rgx[r] = 0;
      else          while (--r >= 0) DESTROY(rgx[r]);
      pckd = 0;
    }

    rgx_cnt = pckd;

    if (SAME_REF(Ref1,NullRef)) {
      reportError(S_ERROR(STS_SYNTAX),"Invalid port specification");
      goto done;
    }

    if (pck_1 <= 0) {
      if (!(p_io & PRT_INOUT)) {
        if (SAME_REF_PI(Ref1,VERILOG_POOL,VER_INPUT)) {
          p_io  = PORT_DIR(PRT_IN|p_io);
          T     = getRef(T0=T,TL,&Ref1);
	  pck_1 = 1;
        } else if (SAME_REF_PI(Ref1,VERILOG_POOL,VER_OUTPUT)) {
          p_io  = PORT_DIR(PRT_OUT|p_io);
          T     = getRef(T0=T,TL,&Ref1);
	  pck_1 = 1;
        } else if (SAME_REF_PI(Ref1,VERILOG_POOL,VER_INOUT)) {
          p_io  = PORT_DIR(PRT_INOUT|p_io);
          T     = getRef(T0=T,TL,&Ref1);
	  pck_1 = 1;
        }
      }

      if (PT_NONE == pt) {
        if (SAME_REF_PI(Ref1,VERILOG_POOL,VER_REG)) {
          pt    = PT_REG;
          T     = getRef(T0=T,TL,&Ref1);
	  pck_1 = 1;
        } else if (SAME_REF_PI(Ref1,VERILOG_POOL,VER_WIRE)) {
          pt    = PT_WIRE;
          T     = getRef(T0=T,TL,&Ref1);
	  pck_1 = 1;
        }
      }

      if (SAME_REF(Ref1,TOK_VER_SIGNED.pi)) {
         pt = PORT_TYP(pt|PT_SIGNED);
         T  = getRef(T,TL,&Ref1);
      }

      while (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_OPN_SQ)) {
        T = getExpr(T,TL,SC_MODULE);
        if (cmplExpr() && evalExpr()) {
          rgx[pckd++] = saveExpr();
        }
        T = skipToken(T,TL,TOK_PUNC_CLS_SQ);
        T = getRef(T,TL,&Ref1);
      }
    }

    Expr *dtx  = 0;
    int   nmd  = 1;
    if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_PERIOD) ||
        SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_OPN_CR)) {
      poolRef ref = Ref1;
      if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_OPN_CR)) {
        Ref1 = NullRef;
        nmd  = 0;
      } else {
        T    = getRef(T,TL,&Ref1);
      }
      if (PT_NONE != pt || dlx || rgx_cnt) {
        reportError(S_ERROR(STS_SYNTAX),"`%s%s` Unexpected",
                                        strDeref(ref),
                                        NULL_REF(Ref1) ? ""
                                                       : strDeref(Ref1));
      }
      T = getExpr(T0,TL,STMT_CNTX(SC_MODULE|SC_PORTS));
      if (cmplExpr() && evalExpr(REF_PORT)) {
        dtx = saveExpr();
      }
    }
    if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_COMMA) ||
        (last = SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_CLS_BR))) {
      reportError(S_WARNING(STS_SYNTAX),"missing port name ?");
      String uc_name;
      uc_name.printf("/*%d*/",++uc);
      Ref1 = strSaveStr(uc_name);
      nmd  = -1;
    }
    ref_nm = *(poolRef *)&Ref1;
    if (nmd > 0 && !checkName(Ref1)) {
      reportError(S_ERROR(STS_SYNTAX),"Invalid port name `%s`",
                                      strDeref(Ref1));
      T = getRef(T,TL,&Ref1);
      continue;
    } else {
      if (nmd >= 0 && (prt = findPort(Ref1))) {
        if (p_io) {
          if (!(prt->io & PRT_PORT)) {
              reportError(S_WARNING(STS_SYNTAX),
                          "Can't set direction on %s - not a port ",
                          strDeref(Ref1));
          } else if (prt->io & PRT_INOUT){
              reportError(((p_io ^ prt->io) & PRT_INOUT) ? S_ERROR(STS_SYNTAX)
                                                         : S_WARNING(STS_SYNTAX),
                          "Port (%s) direction already set",strDeref(Ref1));
          } else {
              prt->io = PORT_DIR(p_io|prt->io);
              io      = PORT_DIR(io|p_io);
          }
        }
        if (disc >= 0) {
          if (prt->io & PRT_DSC_DFLTD) {
            prt->io       = PORT_DIR(prt->io & ~PRT_DSC_DFLTD);
            prt->dsc_indx = disc;
          } else if (disc != prt->dsc_indx) {
            reportError(S_ERROR(STS_SYNTAX),
                        "Port (%s) discipline already set",strDeref(Ref1));
          }
        }
        if (PT_NONE != pt) {
          if (prt->ptyp & ~PT_SIGNED) {
            eSTS s = prt->ptyp == pt ? S_WARNING(STS_SYNTAX)
	                             : S_ERROR(STS_SYNTAX);
            reportError(s,"Port (%s) already typed",strDeref(Ref1));
          } else {
            prt->ptyp = pt;
          }
        }
        if (prt->pckd != pckd) {
          if (prt->pckd && !disc) reportError(S_ERROR(STS_SYNTAX),
                                     "Port (%s) already typed",strDeref(Ref1));
	  prt->pckd = pckd;
        }
        if (st) {
          if (prt->styp) {
            reportError(S_ERROR(STS_SYNTAX),
                        "Signal (%s) already typed",strDeref(Ref1));
          } else {
            prt->styp = st;
          }
        }
      } else {
        prt       = new Port(p_io,pt,st,drv,value,disc);
        prt->name = Ref1;
        if (dtx) {
          prt->ptyp   = PT_ALIAS;
          prt->io     = PRT_NC;
	  assert(!prt->rng && !pckd);
	  REALLOC(prt->rng,prt->pckd = 1,Expr *);
	  prt->rng[0] = dtx;
          dtx         = 0;
          io          = PORT_DIR(io|PRT_ALIASSED);
        } else {
          prt->pckd   = pckd;
          io          = PORT_DIR(io|p_io);
        }
        if (pd) {
          prt->ds0 = (Strength)pd[0].strnth;
          prt->ds1 = (Strength)pd[1].strnth;
        }
        logLocal(Ref1,REF_PORT,port()->add(prt));
      }
      if (!last) {
	T = getRef(T,TL,&Ref1);
	if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_OPN_BR)) {
	  T = getRef(T,TL,&Ref1);
	  if (SAME_REF_PI(Ref1,OPERATORS_POOL,OP_MULTIPLY)) goto get_attr;
	} else if  (SAME_REF_PI(Ref1,VERILOG_POOL,VER_OPN_ATTR)) {
	get_attr:
	  T = SaveAttr(T,TL,1);
	  T = getRef(T,TL,&Ref1);
	}
      }
      addPendAttr(prt);
      if (last) goto done;
      for (r = 0;
           SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_OPN_SQ);
           r++) {
	T = getExpr(T,TL,SC_MODULE);
	if (cmplExpr() && evalExpr()) {
          int same = 0;
	  if (prt->io & PRT_IO_LSTD) {
            if (r < prt->Rngs() && prt->rng && prt->rng[r] &&
                !(same = vd()->expr_val->same(prt->rng[prt->pckd + r]))) {
              reportError(S_WARNING(STS_SYNTAX),
                          "Port (%s) range already set",strDeref(ref_nm));
            }
	  } else {
            st = SIG_TYPE(st|ST_VEC);
	  }
          if (!same) rgx[prt->pckd + r] = saveExpr();
	}
        T = skipToken(T,TL,TOK_PUNC_CLS_SQ);
	T = getRef(T,TL,&Ref1);
      }
      if ((rgx_cnt = (prt->pckd + (prt->unpckd = r)))
   	                    && prt->ptyp != PT_ALIAS) {
        r = 0;
        if (prt->rng) {
          for (; r < rgx_cnt ; r++) {
            if (!prt->rng[r] || !rgx[r]->same(prt->rng[r])) {
              reportError(S_WARNING(STS_SYNTAX),
                          "Port (%s) range already set",strDeref(ref_nm));
              break;
            }
          }
        }
        if (r < rgx_cnt) {
          REALLOC(prt->rng,rgx_cnt,Expr *);
          for (; r < rgx_cnt ; r++) {
	    if (r < pckd && rgx_used) {
	      prt->rng[r] = rgx[r]->alias();
	    } else {
	      rgx_used = 1;
	      prt->rng[r] = rgx[r];
	    }
	  }
        }
      }
      if (dlx) {
        if (prt->dly) {
          if (!dlx->same(prt->dly)) {
            reportError(S_WARNING(STS_SYNTAX),
                        "Port (%s) delay already set",strDeref(ref_nm));
          }
        } else if (dlx_used++) {
          prt->dly = dlx->alias();
        } else {
          prt->dly = dlx;
        }
      }
    }
    if (SAME_REF_PI(Ref1,OPERATORS_POOL,OP_ASSIGN)) {
      T = getExpr(T,TL,SC_INITIAL);
      T = getRef(T,TL,&Ref1);
      if (cmplExpr() && evalExpr()) {
        prt->addAttrFront(InitVal,saveExpr());
      }
    }
    if (nmd < 0) {
      // save token
    } else if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_COMMA)) {
      T    = getRef(T0=T,TL,&Ref1);
      p_io = decl_io;
      pt   = decl_t;
    } else {
      if (opnr &&
          SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_CLS_BR)) goto done;
      if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_SCOLON)) break;
      reportError(S_ERROR(STS_SYNTAX),"Unexpected token '%s'",strDeref(Ref1));
    }
  }

  if (opnr) reportError(S_ERROR(STS_SYNTAX),"Missing ')'");

  if (!dlx_used) DESTROY(dlx);
  if (!rgx_used) {
    r = rgx_cnt;
    while (--r >= 0) DESTROY(rgx[r]);
  }
done:
  return T;
}

const Token *PrtdObj::getPorts(const Token *T0,const Token *TL)
{
  return getPorts(T0,TL,
                  PORT_DIR(PRT_IO_LSTD|PRT_DSC_DFLTD),PT_NONE,
                  ST_BIT,DS_NORM,0,vd()->def_disc_id,0);
}

Attr::~Attr()
{
  if (typ & ATTR_RNGD) {
    RngdAttr *ra = (RngdAttr *)this;
    Range    *rp = ra->rng_lst;
    switch (typ & ~(ATTR_USR|ATTR_RNGD)) {
    case ATTR_EXPR:  DELETE(ra->value.expr);
    }
    while (rp) {
      Range *nxt = rp->next;
      delete(rp);
      rp = nxt;
    }
  } else if (typ & ATTR_USR) {
    AttrAnyU2 *au = (AttrAnyU2 *)this;
    switch (typ & ~ATTR_USR) {
    case ATTR_LOGIC:
    case ATTR_ULOGIC: DELETE(au->value2.expr);
    case ATTR_EXPR:   DELETE(au->value.expr);
    }
  } else {
    AttrAny *at = (AttrAny *)this;
    switch (typ) {
    case ATTR_EXPR:  DELETE(at->value.expr); break;
    default:;
    }
  }
}

Attr *PrtdObj::getParm(poolRef nm,AttrType at,poolRef end,Expr *rgstr)
{
  Attr *attr = 0;
  bool  rngd = (VERILOG_POOL == end.pool && (VER_FROM    == end.index ||
                                             VER_EXCLUDE == end.index)),
        usr  = nm.pool >= FIRST_FREE_POOL;
  Expr *xpr;

  if (cmplExpr(at) && evalExpr()) {
    xpr = saveExpr();
    if (xpr->constant()) switch (at) {
    case ATTR_DOUBLE:
      if (rngd)     attr = new RngdAttr(nm,xpr->dbl());
      else if (usr) attr = new AttrAnyU(nm,xpr->dbl());
      else          attr = new AttrAny (nm,xpr->dbl());
      goto done;
    case ATTR_U64:
      if (rngd)     attr = new RngdAttr(nm,xpr->u64());
      else if (usr) attr = new AttrAnyU(nm,xpr->u64());
      else          attr = new AttrAny (nm,xpr->u64());
      goto done;
    case ATTR_INT:
      if (rngd)     attr = new RngdAttr(nm,xpr->i64());
      else if (usr) attr = new AttrAnyU(nm,xpr->i64());
      else          attr = new AttrAny (nm,xpr->i64());
      goto done;
    case ATTR_ULOGIC:
    case ATTR_LOGIC:
                    attr = new AttrAnyU2(nm,xpr,rgstr);
      goto done;
    default:;
    }
    if (rngd)     attr =  new RngdAttr(nm,xpr);
    else if (usr) attr =  new AttrAnyU(nm,xpr);
    else          attr =  new AttrAny (nm,xpr);
  }

done:
  if (this && attr) {
    int n = parm()->add(attr);
    logLocal(nm,REF_PARM,n);
  }
  return attr;
}

Module::~Module()
{
  DELETE(stmts);
}

const Token *VerCntxtObj::getCase(const Token *T,const Token *TL,
                                 StmtCase *case_stmt,StmtCntx cntxt)
{
  case_s **next_c = &case_stmt->list,
          *cs;

  case_stmt->list = 0;
  cntxt           = STMT_CNTX(cntxt & ~SC_BLOCK);

  T = skipToken(T,TL,TOK_PUNC_OPN_BR);
  T = getExpr(T,TL,cntxt);

  if (cmplExpr() && evalExpr()) {
    case_stmt->expr = saveExpr();
  } else {
    case_stmt->expr = 0;
  }

  T = skipToken(T,TL,TOK_PUNC_CLS_BR);

  while (T < TL && !SAME_REF_PI(T->pi,VERILOG_POOL,VER_ENDCASE)) {
    const Token *T0 = T,
                *TC;
    InfoRef      Ref;

    do {
      T = getRef(TC = T,TL,&Ref);
      if (T >= TL) {
        reportError(S_ERROR(STS_SYNTAX),"Missing 'endcase'");
        return TL;
      }
    } while (!SAME_REF_PI(Ref,VERILOG_POOL,VER_ENDCASE) &&
             !SAME_REF_PI(Ref,VERILOG_POOL,VER_DEFAULT) &&
             !SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_COLON));

    if (VER_ENDCASE == Ref.index) return T;

    *next_c = cs = CALLOC2(1,case_s);
    next_c  = &(cs->next);

    if (PUNC_COLON == Ref.index) {
      getExpr(T0,TC,STMT_CNTX(cntxt|SC_SENSE|SC_COMPLETE|SC_LIST));
      if (cmplExpr() && evalExpr()) {
        cs->expr = saveExpr();
      }
    } else {
      T = getRef(TC = T,TL,&Ref);
      if (!SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_COLON)) T = TC;
    }
    long check = 0;
    T          = getStmts(T,TL,&cs->child,
                          STMT_CNTX(cntxt & ~SC_BLOCK),&check);
    assert (check <= 1);
  }

  assert(0); // shouldn't get here

  return T;
}

void Stmt::setLocation()
{
  poolRef     ref = vd()->posn.file;
  refList    *prf;
  ContextObj *mod = Root();

  for (file_id = 0; (prf = mod->file_map(file_id)) ; file_id++) {
    if (SAME_REF(ref,prf->ref)) goto ln;
  }

  prf = new refList(ref);

  mod->file()->add(prf);

ln:
  level = vd()->posn.level;
  line  = vd()->posn.line;
}

const char *Stmt::File(const ContextObj *mod) const
{
  if (!mod) mod = Root();

  poolRef ref = mod->file_map(file_id)->ref;

  return strDeref(ref);
}

const Token *VerCntxtObj::getForeign(const Token *T,const Token *TL,
                               InfoRef *pRef,String *C)
{
  int         ea = 0,
              ll = pRef->line;
  const char *cp;
  InfoRef     Ref1;

  T = getRef(T,TL,pRef);
  if (QUOTES_POOL == pRef->pool) {
    T = getRef(T,TL,pRef);
    T = getRef(T,TL,&Ref1);
  }
  while (T < TL) {
    T = getRef(T,TL,&Ref1);
    if (SAME_REF_PI(Ref1,VPP_POOL,VPP_ENDLANGUAGE)) {
      break;
    }
    for (; ll < Ref1.line; ea=0,ll++) *C += "\n";
    cp  = strDeref(Ref1);
    if (ea && isalpha(*cp)) *C += " ";
    *C += cp;
    ea  = isalpha(C->lastCh());
  }

  return T;
}

const Token *VerCntxtObj::getStmts(const Token *T,const Token *TL,
                                   Stmt **next_stmt,StmtCntx cntxt,long *count)
{
  PP_VAR

  Stmt        *sa;
  const Token *T0;
  Expr        *x1,*x2,*x3;
  PrtdObj     *po = 0;
  long         check;

  T = getRef(T0 = T,TL,&Ref1);

  for (;;) {
    switch (Ref1.pool) {
    case WHITESPACE_POOL:
      break;
    case PUNCTUATION_POOL:
      switch (Ref1.index) {
      case PUNC_SCOLON: if (!(cntxt&SC_BLOCK)) goto done;
      case PUNC_OPN_CR:
      case PUNC_OPN_BR: goto try_expr;
      }
    unx:
      reportError(S_ERROR(STS_SYNTAX),"Unexpected `%s`",strDeref(Ref1));
      goto next_tok;
    case OPERATORS_POOL:
      switch (Ref1.index) {
      case OP_AT:       T = getEventExpr(T,TL);
                        if (cmplExpr() && evalExpr()) {
                          *next_stmt = sa = new StmtAt(saveExpr(),count);
                          next_stmt  = &(*next_stmt)->next;
                          check      = 0;
                          T          = sa->getStmts(T,TL,&((StmtAt *)sa)->child,
                                           STMT_CNTX(cntxt & ~SC_BLOCK),
                                           &check);
                          assert(check <= 1);
                        }
                        goto next_or_done;
      case OP_DELAY:    T = getDelayExpr(T,TL);
                        if (cmplExpr() && evalExpr()) {
                          *next_stmt = sa = new StmtDelay(saveExpr(),count);
                          next_stmt  = &(*next_stmt)->next;
                          check      = 0;
                          T          = sa->getStmts(T,TL,&((StmtDelay *)sa)->child,
                                           STMT_CNTX(cntxt & ~SC_BLOCK),
                                           &check);
                          assert(check <= 1);
                        }
                        goto next_or_done;
      }
      break;
    case VPP_POOL:
      switch (Ref1.index) {
      case VPP_LANGUAGE:{ String *C  = new String;
                          T          = getForeign(T,TL,&Ref1,C);
                          *next_stmt = sa = new StmtQC(Ref1,C,count);
                          next_stmt  = &(*next_stmt)->next;
                          goto next_tok;
                        }
      }
      break;
    case VERILOG_POOL:
      Ref2 = NullRef; /* for <name> : */
      switch (Ref1.index) {
#define BPORTS
#include "ports-b.inc"

      case VER_ENDSPECIFY:
                        if (!(cntxt&SC_SPECIFY)) goto unx;
                        goto done;
      case VER_END:     if (!(cntxt&SC_BLOCK)) {
                          reportError(S_ERROR(STS_SYNTAX),"'end' unexpected.");
                        }
                        goto done;

      case VER_MODULE:  T = T0; goto done;

      case VER_ELSE:    if (!(cntxt&SC_IF)) {
                          reportError(S_ERROR(STS_SYNTAX),"Dangling 'else'");
                        } else {
                          T = T0;
                        }
                        goto done;
      case VER_REPEAT:  T = skipToken(T,TL,TOK_PUNC_OPN_BR);
                        T = getExpr(T,TL,
                                STMT_CNTX((cntxt|SC_BOOL) & ~SC_BLOCK));
                        T = skipToken(T,TL,TOK_PUNC_CLS_BR);
                        if (cmplExpr() && evalExpr()) {
                          *next_stmt = sa = new StmtRepeat(saveExpr(),count);
                          next_stmt  = &(*next_stmt)->next;
                          check      = 0;
                          T = sa->getStmts(T,TL,&((StmtRepeat *)sa)->child,
                                           STMT_CNTX(cntxt & ~SC_BLOCK),
                                           &check);
                          assert(check <= 1);
                          goto next_or_done;
                        }
                        goto next_tok;
      case VER_WHILE:   T = skipToken(T,TL,TOK_PUNC_OPN_BR);
                        T = getExpr(T,TL,
                                STMT_CNTX((cntxt|SC_BOOL) & ~SC_BLOCK));
                        T = skipToken(T,TL,TOK_PUNC_CLS_BR);
                        if (cmplExpr() && evalExpr()) {
                          *next_stmt = sa = new StmtWhile(saveExpr(),count);
                          next_stmt  = &(*next_stmt)->next;
                          check      = 0;
                          T = sa->getStmts(T,TL,&((StmtWhile *)sa)->child,
                                           STMT_CNTX(cntxt & ~SC_BLOCK),
                                           &check);
                          assert(check <= 1);
                          goto next_or_done;
                        }
                        goto next_tok;
      case VER_WAIT:    T = skipToken(T,TL,TOK_PUNC_OPN_BR);
                        T = getExpr(T,TL,
                                STMT_CNTX((cntxt|SC_SENSE|SC_LEVEL) & ~SC_BLOCK));
                        T = skipToken(T,TL,TOK_PUNC_CLS_BR);
                        if (cmplExpr() && evalExpr()) {
                          *next_stmt = sa = new StmtWait(saveExpr(),count);
                          next_stmt  = &(*next_stmt)->next;
                          check      = 0;
                          T = sa->getStmts(T,TL,&((StmtWait *)sa)->child,
                                           STMT_CNTX(cntxt & ~SC_BLOCK),
                                           &check);
                          assert(check <= 1);
                          goto next_or_done;
                        }
                        goto next_tok;
      case VER_CASEZ:   *next_stmt = sa = new StmtCaseZ(count);
                        next_stmt  = &(*next_stmt)->next;
                        goto gc;
      case VER_CASEX:   *next_stmt = sa = new StmtCaseX(count);
                        next_stmt  = &(*next_stmt)->next;
                        goto gc;
      case VER_CASE:    *next_stmt = sa = new StmtCase(count);
                        next_stmt  = &(*next_stmt)->next;
                     gc:T = getCase(T,TL,(StmtCase *)sa,
                                    STMT_CNTX(cntxt|SC_CASE));
                        goto next_or_done;
      case VER_ENDCASE: if (!(cntxt&SC_CASE)) {
                          reportError(S_ERROR(STS_SYNTAX),"Dangling 'endcase'");
                        } else {
                          T = T0;
                        }
                        goto done;
      case VER_FORK:    T = getRef(T0 = T,TL,&Ref1);
	                if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_COLON)) {
                          T = getRef(T0 = T,TL,&Ref2);
                        } else {
                          T = T0;
                        }
                        *next_stmt = sa = new StmtFork(Ref2,count);
                        next_stmt  = &(*next_stmt)->next;
                        pushScope(sa,Ref2);
                        T = sa->getStmts(T,TL,&((StmtFork *)sa)->child,
                                         STMT_CNTX(cntxt|SC_BLOCK),
                                         &((StmtFork *)sa)->stmt_count);
                        popScope(sa,Ref2);
                        goto next_or_done;
      case VER_JOIN:    if (!(cntxt&SC_BLOCK)) {
                          reportError(S_ERROR(STS_SYNTAX),"'join' unexpected.");
                        }
                        goto done;
      case VER_FOREVER: *next_stmt = sa = new StmtForever(count);
                        next_stmt  = &(*next_stmt)->next;
                        check      = 0;
                        T = sa->getStmts(T,TL,&((StmtForever *)sa)->child,
                                         STMT_CNTX(cntxt & ~SC_BLOCK),&check);
                        assert(check <= 1);
                        goto next_or_done;
      case VER_BEGIN:   T = getRef(T0 = T,TL,&Ref1);
	                if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_COLON)) {
                          T = getRef(T0 = T,TL,&Ref2);
                        } else {
                          T = T0;
                        }
                        *next_stmt = sa = new StmtBlock(Ref2,count);
                        next_stmt  = &(*next_stmt)->next;
                        pushScope(sa,Ref2);
                        T = sa->getStmts(T,TL,&((StmtBlock *)sa)->child,
                                         STMT_CNTX(cntxt|SC_BLOCK),
                                         &((StmtBlock *)sa)->stmt_count);
                        popScope(sa,Ref2);
                        goto next_or_done;
      case VER_ASSIGN:  T = getExpr(T,TL,STMT_CNTX(SC_ASSIGN|SC_TOP));
                        *next_stmt = sa
                                   = (StmtBlock *)new StmtAssign(0,count);
                        next_stmt  = &(*next_stmt)->next;
                	pushScope(sa,NullRef);
                        if (cmplExpr() && evalExpr()) {
                          ((StmtAssign *)sa)->expr = saveExpr();
                        }
                	popScope(sa,NullRef);
                        T = skipToken(T,TL,TOK_PUNC_SCOLON);
                        goto next_or_done;
      case VER_DEASSIGN:T = getExpr(T,TL,STMT_CNTX(SC_LVALUE|SC_LIST));
                        if (cmplExpr() && evalExpr()) {
                          *next_stmt = sa
                                     = (StmtBlock *)new StmtDeassign(saveExpr(),
                                                                        count);
                          next_stmt  = &(*next_stmt)->next;
                        }
                        T = skipToken(T,TL,TOK_PUNC_SCOLON);
                        goto next_or_done;
      case VER_DISABLE: T = getExpr(T,TL,STMT_CNTX(SC_TASK|SC_LVALUE));
                        if (cmplExpr() && evalExpr()) {
                          *next_stmt = sa
                                     = (StmtBlock *)new StmtDisable(saveExpr(),
                                                                        count);
                          next_stmt  = &(*next_stmt)->next;
                        }
                        T = skipToken(T,TL,TOK_PUNC_SCOLON);
                        goto next_or_done;
      case VER_FORCE:   T = getExpr(T,TL,STMT_CNTX(SC_ASSIGN));
                        if (cmplExpr() && evalExpr()) {
                          *next_stmt = sa
                                     = (StmtBlock *)new StmtForce(saveExpr(),
                                                                        count);
                          next_stmt  = &(*next_stmt)->next;
                        }
                        T = skipToken(T,TL,TOK_PUNC_SCOLON);
                        goto next_or_done;
      case VER_RELEASE: T = getExpr(T,TL,STMT_CNTX(SC_LVALUE));
                        if (cmplExpr() && evalExpr()) {
                          *next_stmt = sa
                                     = (StmtBlock *)new StmtRelease(saveExpr(),
                                                                        count);
                          next_stmt  = &(*next_stmt)->next;
                        }
                        T = skipToken(T,TL,TOK_PUNC_SCOLON);
                        goto next_or_done;
      case VER_IFNONE:
      case VER_IF:      T = skipToken(T,TL,TOK_PUNC_OPN_BR);
                        T = getExpr(T,TL,STMT_CNTX((cntxt|SC_BOOL) & ~SC_BLOCK));
                        T = skipToken(T,TL,TOK_PUNC_CLS_BR);
                        if (cmplExpr() && evalExpr()) {
                          *next_stmt = sa =
                           (VER_IF == Ref1.index
                                  ? (StmtBlock *)(new StmtIf(saveExpr(),count))
                                  : (StmtBlock *)(new StmtIfNone(saveExpr(),
                                                                      count)));
                          next_stmt  = &(*next_stmt)->next;
                          check      = 0;
                          T = sa->getStmts(T,TL,&((StmtIf *)sa)->child_t,
                                          STMT_CNTX((cntxt|SC_IF) & ~SC_BLOCK),
                                          &check);
                          assert(check <= 1);
                          T = getRef(T0 = T,TL,&Ref1);
                          if (SAME_REF_PI(Ref1,VERILOG_POOL,VER_ELSE)) {
                            check = 0;
                            T     = sa->getStmts(T,TL,&((StmtIf *)sa)->child_f,
                                          STMT_CNTX(cntxt & ~SC_BLOCK),&check);
                            assert(check <= 1);
                          } else {
                            T = T0;
                          }
                          goto next_or_done;
                        }
                        goto next_tok;
      case VER_FOR:     x1 = x2 = x3 = 0;
                        T = skipToken(T,TL,TOK_PUNC_OPN_BR);
                        T = getExpr(T,TL,
                                STMT_CNTX((cntxt|SC_ASSIGNMENT) & ~SC_BLOCK));
                        if (cmplExpr() && evalExpr()) x1 = saveExpr();
                        T = skipToken(T,TL,TOK_PUNC_SCOLON);
                        T = getExpr(T,TL,
                                STMT_CNTX((cntxt|SC_BOOL) & ~SC_BLOCK));
                        if (cmplExpr() && evalExpr()) x2 = saveExpr();
                        T = skipToken(T,TL,TOK_PUNC_SCOLON);
                        T = getExpr(T,TL,
                                STMT_CNTX((cntxt|SC_ASSIGNMENT) & ~SC_BLOCK));
                        if (cmplExpr() && evalExpr()) x3 = saveExpr();
                        T = skipToken(T,TL,TOK_PUNC_CLS_BR);
                        *next_stmt = sa = new StmtFor(x1,x2,x3,count);
                        next_stmt  = &(*next_stmt)->next;
                        check      = 0;
                        T = sa->getStmts(T,TL,&((StmtFor *)sa)->child,
                                         STMT_CNTX((cntxt|SC_IF) & ~SC_BLOCK),
                                         &check);
                        assert(check <= 1);
                        goto next_or_done;
      case VER_GENERATE:T = getRef(T0 = T,TL,&Ref1);
                        T = skipToken(T,TL,TOK_PUNC_OPN_BR);
                        T = getExpr(T,TL,
                                    STMT_CNTX((cntxt|SC_LIST) & ~SC_BLOCK));
                        T = skipToken(T,TL,TOK_PUNC_CLS_BR);
                        if (cmplExpr() && evalExpr()) {
                          *next_stmt = sa = new StmtGen(Ref1,saveExpr(),count);
                          next_stmt  = &(*next_stmt)->next;
                          pushScope(sa,Generate);
                          logLocal(Ref1,REF_COUNTER,-1);
                          check = 0;
                          T     = sa->getStmts(T,TL,&((StmtGen *)sa)->child,
                                          STMT_CNTX((cntxt|SC_IF) & ~SC_BLOCK),
                                          &check);
                          assert(check <= 1);
                          popScope(sa,Generate);
                          goto next_or_done;
                        }
                        goto next_tok;
      default:          goto try_expr;
      }
      goto next_tok;
    port_dir:
      disc = -1;
      pt   = PT_NONE;
      st   = ST_BIT;
      drv  = DS_NORM;
    ports:
      if ((cntxt&SC_BLOCK) && !NULL_REF(((StmtBlock *)this)->name)) {
        T = ((StmtDecl *)this)->addLcls()->getPorts(T,TL,p_io,pt,st,drv,val,disc,1);
      } else {
        reportError(S_ERROR(STS_SYNTAX),"Declaration disallowed here");
      }
      goto next_tok;
    }
  try_expr:
    T = getExpr(T0,TL,STMT_CNTX(cntxt|SC_TOP));
    if (cmplExpr() && evalExpr()) {
      StmtExpr *xp = new StmtExpr(saveExpr(),count);
      *next_stmt   = xp;
      next_stmt    = &(*next_stmt)->next;
      T            = addExprAttr(T,xp);
    }
    T = skipToken(T,TL,TOK_PUNC_SCOLON);
  next_or_done:
    if (!(cntxt&SC_BLOCK)) goto done;
  next_tok:
    if (T >= TL) break;
    T = getRef(T0 = T,TL,&Ref1);
  }

done:
  return T;
}

bool PrtdObj::AnalogFunc(const Token *T, const Token *TL)
{
  InfoRef tok;

  T = getRef(T,TL,&tok);

  if (VERILOG_POOL == tok.pool && VER_FUNCTION == tok.index) return true;

  return false;
}

void PrtdObj::fixUnknowns()
{
  Unknown *unk = unknown()->first();

  found = 0;

  for (; unk ; unk = unk->next) {
    int    dpth,
           d,
           scp  = unk->scp;
    Scope *ps   = scope_map(scp);
    Expr  *expr;

    dpth = ps ? ps->dpth
              : ROOT_SCP;

    vd()->cur_scp = d = dpth;

    while (d > ROOT_SCP) {
      vd()->scp_stk[d--] = scp;
      scp                = ps->up;
      ps                 = scope_map(scp);
    }

    Expr xrf(unk->ref);
    if ((expr = unk->expr)) {
      Port       *prt;
      intptr_t    n   = (intptr_t)expr->rhs();
      PortDir     dir = PRT_HIER;
      String      nm;
      const char *dot = "";
      int         xtra;

      for (dpth = 0; dpth < n ;) {
        *xrf.pref() = expr->val()->hrf[dpth++];
        eREF rt     = REF_UNKNOWN;
        nm         += dot; dot = ".";
        nm         += strDeref(xrf.val()->ref);
        if (0 <= evalRef(&xrf,n == dpth ? RFF_HIER
	                        	: (eRFF)(RFF_SCOPE|RFF_INST),
                              REF_UNKNOWN)) {
          rt = xrf.rt();
        }
        switch (rt) {
	case REF_INST:
          dir = PORT_DIR(dir|PRT_XPRTD);
        case REF_UNKNOWN:
          while (dpth < n) {
            nm       += dot;
            nm       += strDeref(expr->val()->hrf[dpth++]);
          }
          unk->ref  = strSaveStr(nm);
          prt       = findPort(unk->ref);
          if (!prt) {
            prt       = new Port(PORT_DIR(dir|unk->io),
                                 PT_HIER,ST_BIT,DS_NONE,0,-1);
            prt->name = unk->ref;
            io        = PORT_DIR(io|PRT_HIER);
            logLocal(unk->ref,REF_PORT,port()->add(prt));
          }
          xtra = (expr->xtra & VTX_ALIASSED);
          expr->reset(BxReference,unk->ref);
          evalRef(expr,unk->rff,REF_UNKNOWN);
          expr->xtra = VL_TYP_XTRA(xtra|expr->xtra);
          goto tst;
        case REF_SCOPE:
          ps            = scope_map(scp = xrf.rrf().index);
          d             = ps->dpth;
          vd()->cur_scp = dpth;
          while (d > ROOT_SCP) {
            vd()->scp_stk[d--] = scp;
            scp                = ps->up;
            ps                 = scope_map(scp);
          }
        default:;
        }
      }
    } else {
      expr = &xrf;
      evalRef(expr,unk->rff,REF_UNKNOWN);
    }
  tst:
    if (REF_UNKNOWN == unk->rtyp && unk->fnd.index < 0) {
      switch (unk->rtyp = expr->rt()) {
      case REF_UNKNOWN: if (PORT_WRNUNKWN & V2kWireMode) {
                          reportError(S_WARNING(STS_SYNTAX),unk->ref,
                                      "Could not find: %s",strDeref(unk->ref));
                        }
                        break;
      default:          found++;
      }
      unk->fnd  = expr->rrf();
    }
  }
}

StmtInst::StmtInst(poolRef nm,Expr *pm,PrtdObj *po,long *count) {
  typ2  = STMT_INST;
  name  = nm;
  inst  = 0;
  udp   = 0;
  prim  = 0;
  drv   = NullRef;
  param = pm;
  (*count)++;
  po->addInstRef(nm);
}

Module::Module(const Token *T0,const Token *TL,bool mac)
{
  PP_VAR

  const Token *T         = T0;
  Stmt        *sa,
             **next_stmt = &stmts;
  PrtdObj     *po        = this;
  long         check;
  Task        *tsk;
  Func        *fn;

  macro      = mac;
  typ        = macro ? AO_MACRO
                     : AO_MODULE;
  vmode      = vd()->vmode;
  if (!(tsi  = vd()->ts1))    tsi = 1;
  tse        = vd()->ts2;
  if (!(pri  = vd()->prcsn1)) pri = 1;
  pre        = vd()->prcsn2;
  lib_indx   = vd()->lib_indx;
  stmts      = 0;
  ref_indx   = -1;
  inst_count = 0;
  def_prm    = 0;

  T = getRef(T,TL,&name);
  if (identifier(name)) {
    logGlobal(name,REF_MODULE,vd()->mod_indx = module()->add(this));
    pushScope(0,name);
  } else {
    name = NullRef;
  }

  T = getPorts(T,TL);
  T = getRef(T,TL,&Ref1);
  if (SAME_REF_PI(Ref1,VERILOG_POOL,VER_OPN_ATTR)) {
    T       = SaveAttr(T,TL);
    pending = 0;
    T       = getRef(T,TL,&Ref1);
  }
  if (!SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_SCOLON)) {
    reportError(S_WARNING(STS_SYNTAX),"Expected ';' at of module declaration");
  }

  goto next_tok;

  for (;;) {
    switch (Ref1.pool) {
    case WHITESPACE_POOL:
      break;
    case VPP_POOL:
      switch (Ref1.index) {
      case VPP_LANGUAGE:{ // copied above
                          String *C  = new String;
                          T          = getForeign(T,TL,&Ref1,C);
                          *next_stmt = sa = new StmtQC(Ref1,C,&stmt_count);
                          next_stmt  = &(*next_stmt)->next;
                          break;
                        }
      }
      break;
    case VERILOG_POOL:
      switch (Ref1.index) {
#define MPORTS
#include "ports-m.inc"

      case VER_OPN_ATTR:T = SaveAttr(T,TL);
	                break;
      case VER_ANALOG:  if (AnalogFunc(T,TL)) {
	                  T = getRef(T,TL,&Ref1);
	                  *next_stmt = sa = new StmtFuncA(&stmt_count);
			  goto do_func;
                        }
                        *next_stmt = sa = new StmtAnalog(&stmt_count);
                        next_stmt  = &(*next_stmt)->next;
                        check      = 0;
                	pushScope(sa,NullRef);
                        T = sa->getStmts(T,TL,&((StmtAnalog *)sa)->child,
                                         STMT_CNTX(SC_MODULE|SC_ANALOG),
                                         &check);
                	popScope(sa,NullRef);
                        assert(check <= 1);
                        break;
      case VER_INITIAL: *next_stmt = sa = new StmtInit(&stmt_count);
                        next_stmt  = &(*next_stmt)->next;
                        check      = 0;
                	pushScope(sa,NullRef);
                        T = sa->getStmts(T,TL,&((StmtInit *)sa)->child,
                                         STMT_CNTX(SC_MODULE|SC_DIGITAL),
                                         &check);
                	popScope(sa,NullRef);
                        assert(check <= 1);
                        break;
      case VER_TASK:    *next_stmt = sa = new StmtTask(&stmt_count);
                        next_stmt  = &(*next_stmt)->next;
                	pushScope(sa,NullRef);
                        tsk = new Task(Root(),&T,TL,(StmtTask*)sa);
                	popScope(sa,NullRef);
                        goto next_tok;
      case VER_FUNCTION:*next_stmt = sa = new StmtFunc(&stmt_count);
      do_func:
                        next_stmt  = &(*next_stmt)->next;
                	pushScope(sa,NullRef);
                        fn = new Func(Root(),&T,TL,(StmtFunc*)sa);
                	popScope(sa,NullRef);
                        goto next_tok;
      case VER_ASSIGN:  *next_stmt = sa = new StmtAssign(&stmt_count);
                        next_stmt  = &(*next_stmt)->next;
                	pushScope(sa,NullRef);
                        T = getRef(TS = T,TL,&Ref1);
                        if (SAME_REF_PI(Ref1,OPERATORS_POOL,OP_DELAY)) {
                          T = getDelayExpr(T,TL);
                          if (cmplExpr() && evalExpr()) {
                            ((StmtAssign *)sa)->delay = saveExpr();
                          }
                          T = getRef(TS = T,TL,&Ref1);
                        }
                        if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_OPN_BR)) {
                          T = getRef(TS = T,TL,&Ref1);
                          ((StmtAssign *)sa)->ds[0] = drvStrength(Ref1);
                          T = skipToken(T,TL,TOK_PUNC_COMMA);
                          T = getRef(TS = T,TL,&Ref1);
                          ((StmtAssign *)sa)->ds[1] = drvStrength(Ref1);
                          T = skipToken(T,TL,TOK_PUNC_CLS_BR);
                        } else {
                          T = TS;
                        }
                        T = getExpr(T,TL,SC_ASGN_LST);
                        if (cmplExpr() && evalExpr()) {
                          ((StmtAssign *)sa)->expr = saveExpr();
                        }
                	popScope(sa,NullRef);
                        T = skipToken(T,TL,TOK_PUNC_SCOLON);
                        goto next_tok;
      case VER_SPECIFY: specs = 1;
                        *next_stmt = sa = new StmtSpec(&stmt_count);
                        next_stmt  = &(*next_stmt)->next;
                        pushScope(sa,Specify);
                        T = sa->getStmts(T,TL,&((StmtSpec *)sa)->child,
                                      STMT_CNTX(SC_MODULE|SC_SPECIFY|SC_BLOCK),
                                      &((StmtSpec *)sa)->stmt_count);
                        popScope(sa,Specify);
                        goto next_tok;
      case VER_DEFPARAM:def_prm++;
                        T = getExpr(T,TL,
                              STMT_CNTX(SC_MODULE|SC_DFPRM|SC_ASGN_LST));
                        T = skipToken(T,TL,TOK_PUNC_SCOLON);
                        if (cmplExpr() && evalExpr()) {
                          StmtDefparam *dp = new StmtDefparam(saveExpr(),
                                                              &stmt_count);
                          *next_stmt       = dp;
                          next_stmt        = &(*next_stmt)->next;
                        }
                        goto next_tok;
      case VER_ALWAYS:  *next_stmt = sa = new StmtAlways(&stmt_count);
	                addPendAttr(sa);
                        next_stmt  = &(*next_stmt)->next;
                	pushScope(sa,NullRef);
                        check = 0;
                        T     = sa->getStmts(T,TL,&((StmtAlways *)sa)->child,
                                             STMT_CNTX(SC_MODULE|SC_DIGITAL),
                                             &check);
                	popScope(sa,NullRef);
                        assert(check <= 1);
                        break;
      case VER_BRANCH:  T = getBranches(T,TL);
                        break;

      case VER_ENDMODULE:
	assert(T == TL);
	goto done;
#define PRIM(g,l) case VER_##g: goto prim;
#include "vprim.inc"
      prim:
        *next_stmt = sa = new StmtInst(Ref1,0,this,&stmt_count);
        ((StmtInst *)sa)->prim = 1;
        goto prim2;
      default:
        goto outer_def;
      }
      break;
    case PUNCTUATION_POOL:
      switch (Ref1.index) {
      case PUNC_OPN_BR:
	T = getRef(T,TL,&Ref2);
	if (SAME_REF_PI(Ref2,OPERATORS_POOL,OP_MULTIPLY)) {
	  if ((T = SaveAttr(T,TL,1))) {
	    goto next_tok;
	  }
	}
	T = rescanning(TS);
      }
    outer_def:
    default:
      if ((disc = findDisc(Ref1)) >= 0) {
        p_io = PRT_NC;
        drv  = DS_NORM;
        pt   = PT_NONE;
        goto ports;
      }
      *next_stmt = sa = new StmtInst(Ref1,0,this,&stmt_count);
    prim2: {
        int ic = inst_count;
        next_stmt  = &(*next_stmt)->next;
        T = getRef(TS = T,TL,&Ref1);
        if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_OPN_BR)) {
          const Token *TB = T;
          T = getRef(T,TL,&Ref2);
          ((StmtInst *)sa)->ds[0] = drvStrength(Ref2);
          if (((StmtInst *)sa)->ds[0].strnth == DS_NONE)       {T = TB;
                                                                goto not_drv;}
          T = getRef(T,TL,&Ref2);
          if (!SAME_REF_PI(Ref2,PUNCTUATION_POOL,PUNC_COMMA))  {T = TB;
                                                                goto not_drv;}
          T = getRef(T,TL,&Ref2);
          ((StmtInst *)sa)->ds[1] = drvStrength(Ref2);
          if (((StmtInst *)sa)->ds[1].strnth == DS_NONE)       {T = TB;
                                                                goto not_drv;}
          T = getRef(T,TL,&Ref2);
          if (!SAME_REF_PI(Ref2,PUNCTUATION_POOL,PUNC_CLS_BR)) {T = TB;
                                                                goto not_drv;}
          ((StmtInst *)sa)->udp |= IUDP_DRV;
          T  = getRef(TS = T,TL,&Ref1);
        }
      not_drv:
        if (SAME_REF_PI(Ref1,OPERATORS_POOL,OP_DELAY)) {
          T = getRef(TS = T,TL,&Ref1);
          if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_OPN_BR)) {
            T = getExpr(T,TL,STMT_CNTX(SC_INSTANCE|SC_PARAMETER|SC_LIST0));
            T = skipToken(T,TL,TOK_PUNC_CLS_BR);
          } else {
            ((StmtInst *)sa)->udp |= IUDP_DELAY;
            T = getExpr(TS,TL,STMT_CNTX(SC_REFERENCE|SC_NUMBER));
          }
          if (cmplExpr() && evalExpr()) {
            ((StmtInst *)sa)->param = saveExpr();
          }
        } else {
          T  = rescanning(TS);
        }
        T = getExpr(T,TL,STMT_CNTX(SC_INSTANCE|SC_PORTS|SC_LIST0));
        if (cmplExpr() && evalExpr()) {
          ((StmtInst *)sa)->inst = saveExpr();
        }
        T = skipToken(T,TL,TOK_PUNC_SCOLON);
        if (ic == inst_count) inst_count++;
      }
      break;
    port_dir:
      disc = -1;
      pt   = PT_NONE;
      st   = ST_BIT;
      drv  = DS_NORM;
    ports:
      T  = getPorts(T,TL,p_io,pt,st,drv,val,disc,1);
      break;
    }
  next_tok:
    if (T >= TL) break;
    T = getRef(TS = T,TL,&Ref1);
  }

  reportError(S_ERROR(STS_SYNTAX),"keyword 'endmodule' missing");

done:
  fixUnknowns();
  popScope(0,name);
}

Range *Attr::addRange(Range *rng)
{
  assert(typ&ATTR_RNGD);
  return ((RngdAttr *)this)->addRange(rng);
}

const char *ValueFn::strValue(String *ret,VlType typ) const
{
  StringStream strm(ret);
  vdump        vdmp(&strm);
  *ret = "";
  vdmp.dump(this,typ,0,0);
  return ret->str();
}

const char *Expr::strValue(String *ret)
{
  StringStream strm(ret);
  vdump        vdmp(&strm);
  *ret = "";
  vdmp.dump(this);
  return ret->str();
}

Expr *Attr::expr() const
{
  if (typ & ATTR_USR) return ((AttrAnyU *)this)->value.expr;

  return ((AttrAny *)this)->value.expr;
}

ValueFn *Attr::vf() const
{
  if (typ & ATTR_USR) return (ValueFn *)&((AttrAnyU *)this)->value;

  return (ValueFn *)&((AttrAny *)this)->value;
}

const char *Attr::strValue(String *ret) const
{
  return vf()->strValue(ret,valType());
}

int Attr::hasValue() const
{
  switch (valType()) {
  case VT_REF: return pool || index;
  }
  return 1;
}

poolRef Attr::name() const
{
  poolRef nm;
  if (typ & ATTR_USR) {
    nm = ((AttrU *)this)->name();
  } else {
    nm.pool  = pool;
    nm.index = index;
  }
  return nm;
}

eTable Prim::tblConvert(int ch,poolRef Ref)
{
  switch (ch) {
  case '0': return TBL_ZERO;
  case '1': return TBL_ONE;
  case 'x':
  case 'X': return TBL_X;
  case '?': return TBL_UNKNWN;
  case 'b':
  case 'B': return TBL_B;
  case '*': return TBL_STAR;
  case 'r':
  case 'R': return TBL_RISE;
  case 'f':
  case 'F': return TBL_FALL;
  case '-': return TBL_DASH;
  case 'p':
  case 'P': return TBL_PTN_R;
  case 'n':
  case 'N': return TBL_PTN_F;
  }
  reportError(S_ERROR(STS_SYNTAX),"Illegal entry '%s'",strDeref(Ref));
  return TBL_NONE;
}

const Token *Prim::readTable(const Token *T,const Token *TL)
{
  InfoRef      Ref;
  Table      **next = &table;
  const char  *vp;
  int          colon = 0,
               index = 0,
               ch;
  Port        *scan  = port()->first();

  for (; scan ; scan = scan->next) {
    if (scan->io & PRT_IN)  in++;
    if (scan->io & PRT_OUT) out++;
  }

  TMPARR(char,tmp,in+seq+out);
  do {
    T = getRef(T,TL,&Ref);
    if (SAME_REF_PI(Ref,VERILOG_POOL,VER_ENDTABLE)) goto done;
    if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_COLON)) {
      colon++;
      if (colon > 1+seq || (1 == colon && index != in)
                        || (2 == colon && (!seq || index != in+1)))
      {
        reportError(S_ERROR(STS_SYNTAX),"':' Unexpected");
      }
    } else if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_SCOLON)) {
      if (index != in+seq+out) {
        reportError(S_ERROR(STS_SYNTAX),"Insufficient entries",strDeref(Ref));
      }
      if (colon != 1 + seq) {
        reportError(S_ERROR(STS_SYNTAX),"Seperators (':') missing");
      }
      *next = (Table *)Malloc2(sizeof(Table)+SIZED(char,in+seq+out));
      tbl_sz++;
      BCOPY(tmp,(*next)->entry,in+seq+out);
      *(next = &((*next)->next)) = 0;
      colon = index = 0;
    } else if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_BR)) {
      int    i = 0;
      eTable t[2];
      do {
        T  = getRef(T,TL,&Ref);
        if (PUNCTUATION_POOL == Ref.pool) {
          reportError(S_ERROR(STS_SYNTAX),"Bad entry (%d)",index);
          tmp[index++] = TBL_NONE;
          goto skip;
        }
        vp = strDeref(Ref);
        while ((ch = *vp++)) t[i++] = tblConvert(ch,Ref);
      } while (i < 2);
      T = skipToken(T,TL,TOK_PUNC_CLS_BR);
      if (index > in+seq+out)
        reportError(S_ERROR(STS_SYNTAX),"Too many entries.");
      else
        tmp[index++] =  TABLE_VW(t[0],t[1]);
    } else {
      vp = strDeref(Ref);
      while ((ch = *vp++)) if (index > in+seq+out)
                             reportError(S_ERROR(STS_SYNTAX),"Too many entries.");
                           else
                             tmp[index++] = tblConvert(ch,Ref);
    }
  skip:;
  }  while (T < TL);

  reportError(S_ERROR(STS_SYNTAX),"keyword 'endtable' missing");
done:
  return T;
}

Prim::Prim(const Token *T0,const Token *TL)
{
  PP_VAR

  const Token *T = T0;

  lib_indx = vd()->lib_indx;

  typ    = AO_PRIM;
  next   = 0;
  stmt   = 0;
  table  = 0;
  tbl_sz = 0;

  in = out = seq = 0;

  T = getRef(T,TL,&name);
  if (identifier(name)) {
    logGlobal(name,REF_PRIM,prim()->add(this));
    pushScope(0,name);
  } else {
    name = NullRef;
  }
  T = getPorts(T,TL);
  T = skipToken(T,TL,TOK_PUNC_SCOLON);

  do {
    T = getRef(T,TL,&Ref1);
    switch (Ref1.pool) {
    case WHITESPACE_POOL:
      continue;
    case VERILOG_POOL:
      switch (Ref1.index) {
#define PPORTS
#include "ports-p.inc"
      case VER_ENDPRIMITIVE:
	assert(T == TL);
	goto done;
      case VER_TABLE:
        if (table) goto unx;
        T = readTable(T,TL);
        break;
      case VER_INITIAL:
        if (!stmt) {
          long check = 0;
          T          = getStmts(T,TL,&stmt,STMT_CNTX(SC_DIGITAL),&check);
          assert(check <= 1);
          break;
        }
      default:
      unx:
        reportError(S_ERROR(STS_SYNTAX),"Unexpected keyword `%s`",strDeref(Ref1));
      }
      break;
    default:
      reportError(S_ERROR(STS_SYNTAX),"Unexpected item `%s`",strDeref(Ref1));
      break;
    port_dir:
      disc = -1;
      pt   = PT_NONE;
      st   = ST_BIT;
      drv  = DS_NORM;
    ports:
      T  = getPorts(T,TL,p_io,pt,st,drv,val,disc,1);
    }
  } while (T < TL);

  reportError(S_ERROR(STS_SYNTAX),"keyword 'endprimitive' missing");
done:
  popScope(0,name);
}

const char *Prim::tblString(eTable t) const
{
  static char str[8] = "(~~)";

  switch (t) {
  case TBL_ZERO:   return "0";
  case TBL_ONE:    return "1";
  case TBL_X:      return "x";
  case TBL_UNKNWN: return "?";
  case TBL_B:      return "b";
  case TBL_STAR:   return "*";
  case TBL_RISE:   return "r";
  case TBL_FALL:   return "f";
  case TBL_DASH:   return "-";
  case TBL_PTN_R:  return "p";
  case TBL_PTN_F:  return "n";
  default: if (t & TBL_VW) {
             str[1] = *tblString((eTable)(TBL_MAP & (t >> 3)));
             str[2] = *tblString((eTable)(TBL_MAP & t));
             return str;
           }
  case TBL_NONE:;
  }
  return "~";
}

Func::~Func() {
  DELETE(range);
  DELETE(stmts);
}

Func *VerilogObj::funcObj(const Stmt *stmt)
{
  StmtFunc *s2 = (StmtFunc *)stmt;

  return (Func *)(s2->glob ? glbFunc_map(s2->func_indx)
                           : Root()->func_map(s2->func_indx));
}

Func::Func(ContextObj *prnt,const Token **T0,const Token *TL,StmtFunc *up)
{
  PP_VAR

  const Token *T         = *T0;
  Stmt        *sa;
  PrtdObj     *po        = this;
  int          id        = prnt ? prnt->func()->add(this)
                                : glbFunc()->add(this);
  typ        = AO_FUNC;
  parent     = prnt;
  p_stmt     = up;
  stmts      = 0;
  parent     = 0;
  range      = 0;
  name       = NullRef;
  ret        = FUNC_VOID;

  T = getRef(TS = T,TL,&Ref1);
  if (SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_OPN_SQ)) {
    T = getExpr(T,TL,STMT_CNTX(SC_RANGE|SC_FUNCTION));
    if (cmplExpr() && evalExpr()) {
      range = saveExpr();
      ret   = FUNC_RNG;
    }
    T = skipToken(T,TL,TOK_PUNC_CLS_SQ);
  } else if (SAME_REF_PI(Ref1,VERILOG_POOL,VER_INTEGER)) {
    ret  = FUNC_INT;
  } else if (SAME_REF_PI(Ref1,VERILOG_POOL,VER_REAL)) {
    ret  = FUNC_REAL;
  } else {
    name = Ref1;
  }
  if (NULL_REF(name)) {
    T = getRef(T,TL,&name);
  }
  if (!(glob = !up)) {
    up->glob      = glob;
    up->func_indx = id;
    pushScope(up,name);
  }
  logLocal(name,REF_RETURN,-1);
  T = skipToken(T,TL,TOK_PUNC_SCOLON);
  do {
    T = getRef(TS = T,TL,&Ref1);
    switch (Ref1.pool) {
    case VERILOG_POOL:
      switch (Ref1.index) {
#define FPORTS
#include "ports-f.inc"
      case VER_ENDFUNCTION:
	assert(up || T == TL);
	goto done;
      default:
        if (!stmts) {
          T = getStmts(TS,TL,&stmts,
                       STMT_CNTX(SC_FUNCTION|SC_DIGITAL),&stmt_count);
          assert(stmt_count <= 1);
        } else {
          reportError(S_ERROR(STS_SYNTAX),"Unexpected keyword `%s`",strDeref(Ref1));
        }
      }
      break;
    default:
      if (!stmts) {
        T = getStmts(TS,TL,&stmts,
                     STMT_CNTX(SC_FUNCTION|SC_DIGITAL),&stmt_count);
        assert(stmt_count <= 1);
      } else {
        reportError(S_ERROR(STS_SYNTAX),"Unexpected item `%s`",strDeref(Ref1));
      }
      break;
    port_dir:
      disc = -1;
      pt   = PT_NONE;
      st   = ST_BIT;
      drv  = DS_NORM;
    ports:
      T  = getPorts(T,TL,p_io,pt,st,drv,val,disc,1);
    }
  } while (T < TL);

  reportError(S_ERROR(STS_SYNTAX),"keyword 'endfunction' missing");
done:
  if (up) {
    int scp = scp_id();
    popScope(up,name);
    logLocal(name,REF_FUNC,scp);
  }
  *T0 = T;
}

Task::~Task() {
  DELETE(stmts);
}

Task *VerilogObj::taskObj(const Stmt *stmt)
{
  StmtTask *s2 = (StmtTask *)stmt;

  return (Task *)(s2->glob ? glbTask_map(s2->task_indx)
                           : Root()->task_map(s2->task_indx));
}

Task::Task(ContextObj *prnt,const Token **T0,const Token *TL,StmtTask *up)
{
  PP_VAR

  const Token *T         = *T0;
  Stmt        *sa,
             **next_stmt = &stmts;
  PrtdObj     *po        = this;
  int          id        = prnt ? prnt->task()->add(this)
                                : glbTask()->add(this);

  typ        = AO_TASK;
  parent     = prnt;
  p_stmt     = up;
  stmt_count = 0;
  stmts      = 0;
  parent     = 0;

  T   = getRef(T,TL,&name);
  if (!(glob = !up)) {
    up->glob      = glob;
    up->task_indx = id;
    pushScope(up,name);
  }
  T = skipToken(T,TL,TOK_PUNC_SCOLON);
  do {
    T = getRef(TS = T,TL,&Ref1);
    switch (Ref1.pool) {
    case VERILOG_POOL:
      switch (Ref1.index) {
#define TPORTS
#include "ports-t.inc"

      case VER_ENDTASK:
	assert(up || T == TL);
	goto done;
      default:
        if (!stmts) {
          T = getStmts(TS,TL,&stmts,STMT_CNTX(SC_TASK|SC_DIGITAL),&stmt_count);
          if (!stmts) reportError(S_WARNING(STS_SYNTAX),"Null statement ignored");
        } else {
          reportError(S_ERROR(STS_SYNTAX),"Unexpected keyword `%s`",strDeref(Ref1));
        }
      }
      break;
    default:
      if (!stmts) {
        T = getStmts(TS,TL,&stmts,
                     STMT_CNTX(SC_TASK|SC_DIGITAL),&stmt_count);
        if (!stmts) reportError(S_WARNING(STS_SYNTAX),"Null statement ignored");
      } else {
        reportError(S_ERROR(STS_SYNTAX),"Unexpected item `%s`",strDeref(Ref1));
      }
      break;
    port_dir:
      disc = -1;
      pt   = PT_NONE;
      st   = ST_BIT;
      drv  = DS_NORM;
    ports:
      T  = getPorts(T,TL,p_io,pt,st,drv,val,disc,1);
    }
  } while (T < TL);

  reportError(S_ERROR(STS_SYNTAX),"keyword 'endtask' missing");
done:
  if (up) {
    int scp = scp_id();
    popScope(up,name);
    logLocal(name,REF_TASK,scp);
  }
  *T0 = T;
}

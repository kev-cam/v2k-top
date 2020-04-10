/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * pc2_cpp_rcsid() {return "$Id: pc2.cpp,v 1.79 2020/04/06 00:28:44 dkc Exp $";}

#include "system.h"
#include "args.h"
#include "env.h"
#include "error.h"
#include "language.h"
#include "poolmngr.h"
#include "strdb.h"
#include "v2k_mem.h"
#include "v2k.h"
#define  NEED_PUNCTUATION
#define  NEED_OPERATORS
#define  NEED_VERILOG
#define  NEED_QUOTES
#define  NEED_WHITESPACE
#define  NEED_INTEGER
#define  NEED_COMMENTS
#define  NEED_CHAR
#define  NEED_PP
#define  NEED_PRP
#define  NEED_CPP
#define  NEED_PRC
#define  NEED_CPPOPS
#define  NEED_PRCOPS
#define  NEED_GNU
#include "tokpool.h"
#define PC_POOL_DESC
#include "parser.h"
#include "pc.h"

#define SKIP_CLS_CR(T,TL) { if (T < TL) switch (T->pi.pool) {\
                              case PUNCTUATION_POOL: switch (T->pi.index) {\
			        case PUNC_CLS_CR: T0 = T = nextTok(T,0); break;}}}

const char *const PcObjTyp[PCO_TypMax+1] = {
#define PC_OT_DECL(e,s,l,f) s,
#include "pc_o_typ.h"
0
};

const char *const PcObjTypStr[PCO_TypMax+1] = {
#define PC_OT_DECL(e,s,l,f) l,
#include "pc_o_typ.h"
0
};

const eFT PcObjTypFT[PCO_TypMax+1] = {
#define PC_OT_DECL(e,s,l,f) f,
#include "pc_o_typ.h"
FT_Null
};

class CpObj {
  ePcObjTyp typ;
};
 
CppType    *CppType::RListType;
CppTypeCls *CppTypeCls::RListTypeCls;
CppTypeRef *CppTypeRef::RListTypeRef;

eAZR (*CppTypeRef::AnalysisFn)(CppTypeRef *,CppType *,CppScope *,const Location *,int,eAZC); 
eAZR (*CppExpr::   AnalysisFn)(CppExpr *,   CppType *,CppScope *,const Location *,int,eAZC);

void CppTypeV::recycle()
{
  if (-2 != source.file_num[0]) {
    source.file_num[0] = FNM_RCYCLD;     
    CppTypeV *nxt = next,
             *ovr = ovrld;
    next = ovrld = 0;
    if (nxt) nxt->recycle();
    if (ovr) ovr->recycle();
    CppTypeV **free_list = recycleList();
    if (free_list) {
      next       = *free_list;
      *free_list = this;
    }
  }
}

CppTypeCls::~CppTypeCls()
{
  FREE(frnds);
}

static String Description;

#define DUMP Write(*(strm ? strm : Stream::Stdio(2)) ,0,WFLG_DEBUG);

const void CppExpr::dump(Stream *strm)    const { DUMP }
const void Location::dump(Stream *strm)   const { DUMP }
const void CppType::dump(Stream *strm)    const { DUMP }
const void CppTypeRef::dump(Stream *strm) const { DUMP }
const void CppDecl::dump(Stream *strm)    const { DUMP }
const void CppScope::dump(Stream *strm)   const { DUMP }

#define DESCRIBE Description.cut(0);\
                 StringStream ss(&Description);\
                 Write(ss,0,WFLG_DEBUG);\
                 return bf ? (strcpy(bf,Description.str()))\
                           : Description.str();  

const char *CppExpr::describe(char *bf)    const { DESCRIBE }
const char *Location::describe(char *bf)   const { DESCRIBE }
const char *CppType::describe(char *bf)    const { DESCRIBE }
const char *CppTypeRef::describe(char *bf) const { DESCRIBE }
const char *CppDecl::describe(char *bf)    const { DESCRIBE }
const char *CppScope::describe(char *bf)   const { DESCRIBE }

static const char *stringCT(CppTypeV *typ)
{
  static char buff[64];

  sprintf(buff,"%4d %4d %s",typ->name.pool,typ->name.index,
	                    strDeref(typ->name));

  return buff;
}

void CppTypeV::addOverload(CppTypeV *typ) {
  if (ovrld) {
    typ->next   = ovrld->next;
    ovrld->next = typ;
  } else {
    ovrld       = typ; 
    typ->next   = typ;
  }
  typ->set_rgstrd(2);
}

CppTypeV *CppScope::addType(CppTypeV *typ)
{
  CppScope       *add_to = this;
  CppTypeV       *base;
  const CppTypeV *sub;
  CppTypeRef     *ref    = typ->cRef(),
                 *bsr;

  while (ref && (sub = ref->sub)) {
    const CppScope *scp = typ->Scope(1);
    if (scp) {
      add_to = const_cast<CppScope *>(scp);
    }
    typ = const_cast<CppTypeV *>(sub);
    ref = typ->cRef();
  }

  if (NULL_REF(typ->name)) {
    return addTypeAnon(typ);
  }

  if (SCP_C == sc_typ) add_to = up;

  if (typ->isStrctr()) {
    const CppTypeCls *cls = typ->cTypCls(1);
    base = const_cast<CppTypeCls *>(cls);
    assert(SAME_REF(typ->Name(),base->Name()) ||
           0 == strcmp(strDeref(base->Name()),strDeref(typ->Name())+1));
    goto strctr;
  } else {
    base = add_to->findType(typ->name,0,1);
  }

  if (base) {
    ref  = typ->cRef();
   strctr:
    CppTypeRef *bsr = base->cRef();
    CppType    *tt  = ref ? 0 
                          : typ->cTyp(),
               *bst = bsr ? 0
                          : base->cTyp();
    if (ref && (ref->x.f.cnstrctr || ref->x.f.destrctr)) {
      if (bst) {
        ref->typ = bst;
        base->addOverload(typ);
      } else {
        assert(bsr->typ->isTmplt() || bsr->typ->cTypCls(1));
        goto as_ref;
      }
    } else if (ref && ref->x.f.func) {
     as_ref:
      if (bsr) {
        if (bsr->x.f.anon > 0 && !bsr->x.f.func) {
          bsr->set_func();
          bsr->set_anon(-1);
        }
        assert(bsr->x.f.func || ref->x.f.cnstrctr || ref->x.f.destrctr);
      }
      base->addOverload(typ);
    } else {
      const CppTypeRef *ref = typ->cRef();
      if (ref) {
        assert(ref->x.f.typnm || ref->x.f.typdf || ref->x.f.unres);
      } else {
        const CppType *t = typ->cTyp();
        assert(t->x.f.tmplt);
        base->addOverload(typ);
      }
    }
  } else {
    add_to->types.addord(typ,(listCmp)strRefCmpV);
    assert(!typ->isRgstrd());
    typ->set_rgstrd();

    if (typ->isDclrd()) {
      // ???
    } else if (typ->Where()->Tok()) {
      addStmt(new CppStmtType(typ,typ->Where()));
    }
  }

  return typ;
}

CppTypeV *CppScope::addTypeAnon(CppTypeV *typ)
{
  anon_types.add(typ);

  assert(!typ->isRgstrd());
  typ->set_rgstrd();

  return typ;
}

CppType *CppType::TypeName;
CppType *CppType::Void;
CppType *CppType::Bool;
CppType *CppType::Char;
CppType *CppType::Short;
CppType *CppType::Int;
CppType *CppType::Long;
CppType *CppType::Float;
CppType *CppType::Double;

Location Nowhere;
Location Permanent;

CppContext::CppContext()
  : blk_dpth(0),
    root(SCP_GLOBAL),
    current(&root),
    max_line(0),
    line_map(0),
    max_drctv(0),
    last_drctv(-1),
    line_drctv(0)   
{
  initCntxt(0);
  CmnInit();
  dbg       = new String;
  cmode     = PCMD_NONE;
  root.next = &root;

  if (0 == Permanent.file_num[0]) {

    Permanent.file_num[0] = FNM_PERM;

    current->addType(CppType::TypeName =
                     new CppType(&Permanent,PCT_SCALAR,   strFind("typename")));
    current->addType(CppType::Bool = 
		     new CppType(&Permanent,PCT_SCALAR,   strFind("bool")));
    current->addType(CppType::Char = 
		     new CppType(&Permanent,PCT_QUAL_SCLR,strFind("char")));
    current->addType(CppType::Short = 
		     new CppType(&Permanent,PCT_QUAL_SCLR,strFind("short")));
    current->addType(CppType::Int = 
                     new CppType(&Permanent,PCT_QUAL_SCLR,strFind("int")));
    current->addType(CppType::Long = 
                     new CppType(&Permanent,PCT_QUAL_SCLR,strFind("long")));
    current->addType(CppType::Void = 
		     new CppType(&Permanent,PCT_SCALAR,   strFind("void")));
    current->addType(CppType::Float = 
		     new CppType(&Permanent,PCT_SCALAR,   strFind("float")));
    current->addType(CppType::Double = 
		     new CppType(&Permanent,PCT_SCALAR,   strFind("double")));

    CppTypeRef::Ellipsis.typ = CppType::Void;
    CppTypeRef::VaList.typ   = CppType::Void;

    poolRef  Ref;
    Ref.pool     = CPP_POOL;
    Ref.index    = CPP_FALSE;
    current->addDecl(new CppDecl(CppType::Bool,Ref,new CppExpr(CXPR_U32,0)));
    Ref.index    = CPP_TRUE;
    current->addDecl(new CppDecl(CppType::Bool,Ref,new CppExpr(CXPR_U32,1)));

    CppTypeRef *cp = new CppTypeRef(&Nowhere,0,CppType::Char);
    cp->x.f.indrct++;     
    CppType::CharP = cp;      

    CppTypeRef *ccp = new CppTypeRef(&Nowhere,0,CppType::Char);
    ccp->x.f.indrct++;     
    ccp->x.f.cnst_src   = 1;     
    CppType::ConstCharP = ccp;      

    CppExpr::InitCommon();

    CppTypeRef *bp = new CppTypeRef(&Nowhere,0,CppType::Int);
    bp->set_width(CppExpr::Int1);
    CppType::BitField = bp;
  }
}

void CppScope::showTypes()
{
  CppTypeV *typ = types.first();
  fprintf(stderr,"%s\n",strDeref(Name()));
  for (; typ ; typ = typ->next) {
    fprintf(stderr,"\t((class CppTypeV *) 0x%llX) %s\n",(U64)typ,strDeref(typ->Name()));
  }
}

void CppScope::showDecls()
{
  CppDecl *dcl = decls.first();
  fprintf(stderr,"%s\n",strDeref(Name()));
  for (; dcl ; dcl = dcl->next) {
    fprintf(stderr,"\t((class CppDecl *) 0x%llX) %s\n",(U64)dcl,strDeref(dcl->name));
  }
}

const char *CppScope::strScope[] = {"~",
                                    "-C-",
                                    "C++",
                                    "?",
                                    "<>",
                                    "()",
                                    "-t-",
                                    "{}",
                                    ""};

const char *CppScope::str() const
{
  return strScope[sc_typ];
}

void CppScope::showScope(int dpth)
{
  if (up) { up->showScope(dpth+1);
            fprintf(stderr,"/");}

  poolRef nm = Name();
  if (NULL_REF(nm)) {
    CppTypeV *typ = Type();
    if (typ || (typ = Func())) {
      nm = typ->Name();
    }
  }
  fprintf(stderr,dpth ? "%s"
	              : "%s\n",nm.pool ? strDeref(nm)
	                               : str());
}

void CppContext::showScope()
{
  static int count;

  fprintf(stderr,"%d ",++count);
  current->showScope();
  fprintf(stderr,"\n");
}

CppType *CppType::cppType(poolRef Ref) 
{
  assert(CPP_POOL == Ref.pool);
  switch (Ref.index) {
  case CPP_BOOL:   return CppType::Bool;
  case CPP_CHAR:   return CppType::Char;
  case CPP_SHORT:  return CppType::Short;
  case CPP_INT:    return CppType::Int;
  case CPP_LONG:   return CppType::Long;
  case CPP_VOID:   return CppType::Void;
  case CPP_FLOAT:  return CppType::Float;
  case CPP_DOUBLE: return CppType::Double;
  }
  return 0;
}

CppTypeRef CppTypeRef::Ellipsis;
CppTypeRef CppTypeRef::VaList;

void CppContext::reportError(eSTS sts,const char *format,...)
{
  va_list pvar;
  va_start(pvar, format);
  String msg(format,pvar);
  LnDrctv &ld(line_drctv[last_drctv]);
  int ln = ld.file_num[0]+(line-ld.offset);
  ErrorMsg(sts,"%s:%d - %s",strDeref(ld.file_name),ln,msg.str());
  va_end(pvar);
}

int CppContext::dumpLnMap(FILE *out,const Token *TE,int flgs)
{
  int l = 0;

  if (line_map) {
    const Token *T;

    while (++l <= max_line) {
      if (T = line_map[l-1]) {
        if (l > 2) T++;
        if (flgs & 1) {
          fprintf(stderr,"%-5d ",l);
	}
        if (line_map[l]) {
          tokReconInMem(T,-1,out);
	} else {
          if (flgs & 2) break;
          tokReconInMem(T,(flgs & 4) ? 100 
                                     : TE-T,out);
	}
      }
    }
  }

  return l;
}

const Token *CppContext::rescanning(const Token *T0,int back_only)
{
  rescan = 1;

  if (T0) {
    int l = line,
        d = back_only;
    while (line > 0) {
      if (line_map) {
	const Token *T = line_map[line];
	if (T) {
	  if (T <= T0) {
	    if (line != l) {
	      prsLnDrctv(T,line);
	    }
	    if (!d) {
	      while ((T = line_map[line+1]) && T <= T0) {
                ++line;
		prsLnDrctv(T,line);
	      }
	    }
	    goto done;
	  }
	}
      } else {
	if (line <= 1) goto done;
      }
      d = 1; line--;
    }
    assert(0);
  } else {
  }
 done:
  return T0;
}

const Token *CppContext::advanceTo(const Token *T0,const Token *T)
{
  while (T0 < T) {
    T0 = nextTok(T0,0);
  }
  return T;
}

const Token *CppContext::nextPostNL(const Token *T0,int l)
{
  if (l > 0) {
    if (l >= max_line) {
      if (line_map) {
        const Token **new_map = new const Token * [max_line +1000];
        delete [] line_map;
        BCOPY(line_map,new_map,max_line * sizeof(*line_map));
        BZERO(&new_map[max_line],1000 * sizeof(*line_map));
        line_map = new_map;
      } else {
        max_line = mtk->Size()/20 + 1000;
        line_map = new const Token * [max_line];
        BZERO(line_map,max_line * sizeof(*line_map));
      }
    }

    if (line_map[l]) { // tokReconInMem(line_map[l]+1,-1,0)
      assert (line_map[l] == T0);
    } else {
      line_map[l] = T0;
      assert (l == 0 || line_map[l-1] < T0);
    }
  }

  const Token *T = nextTok(T0,1,-1);
  
  switch (T->pi.pool) {
  case OPERATORS_POOL:
    switch (T->pi.index) { case OP_DELAY: return prsLnDrctv(T0); } break;
  case PRP_POOL:
    switch (T->pi.index) { case PRP_HASH: return prsLnDrctv(T0); } break;
  }
  return T0;
}

const Token *CppContext::skip2nl(const Token *T)
{
  T = nextTok(T,0);

  while (T) {
    if (T->pi.pool == WHITESPACE_POOL) switch(T->pi.index) {
    case WHT_NL: return nextTok(T,0);
    }
    T = nextTok(T,0);
  }

  return T;
}

const Token *CppContext::skip2ec(const Token *T)
{
  T = nextTok(T,0);

  while (T) {
    if (T->pi.pool == COMMENTS_POOL) switch(T->pi.index) {
    case CMT_ENDCMNT: return nextTok(T,0);
    }

    T = nextTok(T,0);
  }

  return T;
}

INLINE const Token *CppContext::skipToken(const Token *T,const Token *TL,const Token kw,const char *where)
{
  T = skipWht(T,TL,1);
  if (T >= TL || T->as_int != kw.as_int) {
    poolRef Ref;
    tokGetRef(T,&Ref,1,1);
    reportError(S_ERROR(STS_SYNTAX),where ? "Expected %s not %s %s"
		                          : "Expected %s not %s",
                                    tokDeref(&kw),strDeref(Ref),
                                    where);
    return rescanning(T);
  }

  return ++T;
}

const Token *CppContext::findClose(const Token *T,const Token *TL,const Token &end,int opnr)
{
  Token  T0;
  int    dpth =  0;

  if (opnr) {
    T0.pi.pool  = PUNCTUATION_POOL;
    T0.pi.index = opnr;
  } else {
    T0          = *T;
    T           =  T +1;
  }

  while (T < TL) {
    if (T->pi.pool == TOK_FULL_REF) {
      T += 2;
    } else if (T->as_int  == T0.as_int) {
      dpth++;
    } else if (T->as_int  == end.as_int) {
      if (--dpth < 0) {
	return T;
      }
    }
    T++;
  }

  return 0;
}

const Token *CppContext::prsLnDrctv(const Token *T,int rscn)
{
  if (source.lckd) {
    if (rscn >= 0) {
      source.rescan  = T;
    } else if (!source.pending) {
      source.pending = T;
    }
    return T;
  }

  source.lckd = T;

  poolRef      Ref;
  const char  *file;
  const Token *TP   = 0;
  int         *np;
  const int    max  = (sizeof(source.file_num)
                       /sizeof(source.file_num[0])) -1;

  do {
    int l = line;

    if (source.pending) {
      TP             = source.pending;
      source.pending = 0;
    }

    if (rscn >= 0) {
      rscn = -1;
      int          bot = 0,
	           mid = 0,
	           top = last_drctv,
                   ti;
      const Token *Ti;

      if (top >= 0)  {
	while (bot < top) {
	  mid = (top + bot)/2;
	  ti  = line_drctv[mid].line;	             
	  Ti  = line_map[ti];
	  if (T > Ti) {        bot = mid +1;
	  } else if (T < Ti) { top = mid -1;
	  } else             { goto match; }
	}
	mid = bot;
      match:
	if (mid != last_drctv) {
	  source = line_drctv[mid];
	}
      }

      assert(!source.pending && !source.rescan);

    } else {

      do {
        T = tokGetRef(T,&Ref,0,1);
      } while (WHITESPACE_POOL == Ref.pool ||
               SAME_REF_PI(Ref,PRP_POOL,PRP_HASH));

      if (SAME_REF_PI(Ref,PRP_POOL,PRP_PRAGMA)) {
        const Token *T0 = T;
        T = skip2nl(T0);
        CppStmtPragma *prg = new CppStmtPragma(Where(),T0,T);
        addStmt(prg);
        goto chk_pend;
      }

      last_drctv++;

      if (last_drctv >= max_drctv) {
        LnDrctv *new_drctv = new LnDrctv [max_drctv += 200];
        int i = last_drctv;
        while (--i >= 0) new_drctv[i] = line_drctv[i];
        delete [] line_drctv;
        line_drctv = new_drctv;
      }

      LnDrctv *ld = &line_drctv[last_drctv];

      ld->line = line;

      np    = ld->file_num;
      np[0] = atoi(strDeref(Ref));
      assert(np[0] > 0);
      ld->offset    = line - np[0];
      T             = tokGetRef(T,&Ref,1,1); // skip quote
      T             = tokGetRef(T,&Ref,1,1);
      ld->file_name = Ref;
      T             = tokGetRef(T,&Ref,1,0); // skip quote
      for (;;) {
        T = tokGetRef(T,&Ref,1,1);
        if (WHITESPACE_POOL == Ref.pool) {
          switch (Ref.index) { case WHT_NL: goto done;
                               default:     continue; }
        }
        *++np = atoi(strDeref(Ref));
        assert(*np > 0);
      }
      assert(np <= &ld->file_num[max]);
      while (np <  &ld->file_num[max]) *++np = -1;

      source = *ld;

     done:
      if (T == source.rescan) {
        source.rescan = 0;
      }
      source.tok = T;
    }

   chk_pend:
    if (!source.pending && source.rescan) {
      source.pending = source.rescan;
      source.rescan  = 0;
      rscn           = 0;
    }

  } while (source.pending);

 unlock:
  source.lckd = 0;

  return T;
}

int CppContext::closeStmt(const Token *T0,const Token *TL,const Token **ret,int alt_stop,int tmplt)
{
  poolRef Ref;
  int     br = 0,
          cr = 0;

  const Token *T  = skipWht(T0,TL,-1),
              *Tc,
              *Ts = 0;

  while (T < TL) {

    switch (T->pi.pool) {
    case PUNCTUATION_POOL: switch(T->pi.index) {
      case PUNC_COMMA:  if (0 == br) cr = 0;
      default:          if (alt_stop != T->pi.index) {
                            break;
			}
      case PUNC_SCOLON: if (0 >= br) goto done;
	                break;
      case PUNC_CLS_CR: if (1 == br) {
                          if (cr++) {
                            T = Tc;
                            goto done;
			  }
                          Tc = T;
                        }
      case PUNC_CLS_BR: 
      case PUNC_CLS_SQ: br--;
                        if (0 > br) goto done;
                        break;
      case PUNC_OPN_CR: 
      case PUNC_OPN_SQ:
      case PUNC_OPN_BR: if (0 == br && alt_stop == T->pi.index) {
                          goto done;
			}
                        br++; break;
      }
      break;
    case CPPOPS_POOL: switch(T->pi.index) {
      case COP_ASSIGN:  if (!br) {
	                  tmplt = 0; cr = 0;
	                }
                        break;
      case COP_LT:      if (tmplt) br++;      break;
      case COP_GT:      if (tmplt) {
                          if (0 >= br && tmplt > 1) goto done;
                          br--;
			}
                        break;
      case COP_RHT_SH:  if (!Ts) {
                          Ts = T;
			}
      }
      break;
    case TOK_FULL_REF:
      Ref.pool  = (++T)->as_int;
      Ref.index = (++T)->as_int;
    }

    T = nextTok(T,1,-1);
  }  

  if (br && Ts) {
    rescanning(Ts);
    reportError(S_WARNING(STS_SYNTAX),"'>>' instead of '> >' ?");
  }
  *ret = TL;
  return 0 == br;

 done:
  *ret = T;
  return T->pi.index;
}

void CppScope::Show(Stream *out,int top,int verbose)
{
  if (!this) return;

  if (!out) out = Stream::Stdio(2);

  if (top) out->printf("\nScope %x: ",this);

  if (up) {
    up->Show(out,0);
     out->printf(".");
  }
  switch (sc_typ) {
  case SCP_TEMPLATE: out->printf("<>");
                     break;
  case SCP_TYPE:     out->printf("T:%s",strDeref(Type()->name));
                     break;
  case SCP_FUNCTION: out->printf("F:%s",strDeref(Func()->name));
                     break;
  case SCP_C:        out->printf("C"); break;
  case SCP_GLOBAL:   out->printf("~"); break;
  default:           out->printf("?"); break;
  }

  if (verbose) {
    CppDecl *dcl = decls.first();

    for (; dcl ; dcl = dcl->next) {
      out->printf("\n\t%s",strDeref(dcl->name));
    }
  }

  if (top) {
    out->printf("\n");}
}

CppScope *CppContext::pushScope(eSCPT st,CppTypeRef *f,CppScope *use_scp,
                                const CppTypeV *r)
{
  CppScope *ret = current,
           *scp;

  if (use_scp) {
    current = use_scp;
  }

  switch(st) {
  case SCP_FUNCTION: scp = f->Scope();
                     if (scp) {
                       assert(!f->hasBod());
                     } else {
                       if (!r) {
                         r = f->typ;
		       }
		       scp = new CppFuncScp(f,r);
                     }
                     pushDown(scp);
                     goto done;
  }

  pushDown(new CppScope(st));
 done:
  return ret;
}

CppScope *CppContext::addScope(CppScope *scp)
{
  CppScope *ret = current;

  assert(scp->up != ret);
  
  scp->up   = ret;
  scp->next = ret->next;
  ret->next = scp;
  current   = scp;

  return ret;
}

inline int CppScope::findObj(poolRef Ref,CppTypeV **rtp,CppDecl **rdp,CppScope **nmp,
                             CppTypeRef::flags *flags,int lcl) const
{
  CppScope *scp   = const_cast<CppScope *>(this),
           *scan;
  CppTypeV *ret   = 0;
  CppDecl  *dcl   = 0,
            find(Ref);
  int       sts   = 0;
  CppTypeX  fndx(Ref);

  while (scp) {
    if (rtp) {
      CppTypeV *st = scp->Type();
      if (st && SAME_REF(Ref,st->Name())) {
        ret = st;
        goto have_typ;
      }
    }
    CppScope    *scan0   = scp,
                *scan    = 0,
                *stop;
    CppScopeRef *nms     = scp->nm_spcs;
    CppScope    *scan_up = scp->up;
    for (;;) {
      if (!scan) {
	if (nms) {
	  scan  = nms->Scope();
	  nms   = nms->next;
	} else if (scan0) {
	  scan  = scan0;
	  scan0 = 0;
	} else {
	  break;
	}
	stop = scan;
      }

      if (nmp && SCP_NAMESPACE == scan->sc_typ) {
        if (SAME_REF(Ref,scan->Name())) {
	  *nmp = scan;
	  sts  = 3;
          goto done;
        }
      }
      if (rdp) {
        List<CppDecl,true> &dcls = scan->decls;
        if (dcl = dcls.findord(&find,(listCmp)strRefCmp)) {
          if (flags) {
            assert(0);
          }
          goto have_dcl;
        }
      }
      if (rtp) {
        List<CppTypeV,LIST_RECYCLE> &types = scan->types;
        ret = types.findord(&fndx,(listCmp)strRefCmpV);
        while (ret) {
          const CppTypeRef *trf;
          const CppType    *typ;
          if (typ = ret->cTyp()) {
            trf = typ->refs;
	  } else {
            trf   = ret->cRef();
            int i = 0;
          }
          if (flags) {
            for (; trf ; trf = trf->next ? trf->next->cRef()
	                                 : 0) {
              if (0 == memcmp(flags,trf->x.as_chrs,sizeof(*flags))) {
                goto have_typ;
	      }
	    }
          } else {
            if (trf) {
              if (0 == trf->x.f.func && !trf->x.f.bod && trf->ovrld) {
                ret = trf->ovrld; 
                continue;
	      }
	    } else {
              if (!typ->x.f.bod && typ->ovrld) {
                ret = typ->ovrld; 
                continue;
	      }
	    }
  	  }
          goto have_typ;
        }
        if (SCP_FUNCTION == scan->sc_typ) {
          const CppTypeV   *ft = scan->Ret();
          if (ft) {
  	    const CppTypeRef *to = ft->cRef(),
	                     *ti;

            if (to && to != (ti = to->Sub())) {
	      if (SAME_REF(Ref,ti->Name())) {
                ret = const_cast<CppTypeRef *>(ti);
                goto have_typ;
	      }
	    }
	  }
	}
      }
      CppTypeV *typ = scan->Type(),
               *inh = typ ? const_cast<CppTypeV *>(typ->Inherits())
                          : 0;
      if (lcl && !nms) {
	break;
      }
      for (; inh ; inh = inh->next) {
        const CppTypeRef *rfi = inh->cRef(); 
        const CppScope   *sci = rfi ? rfi->Sub()->Scope(1)
	                            : inh->Scope(1);
        if (sci) {
          if (rtp) {
            CppTypeV *st = const_cast<CppScope *>(sci)->Type();
	    if (st && SAME_REF(Ref,st->Name())) {
              ret = st;
              goto have_typ;
	    }
	  }
          if (sts = sci->findObj(Ref,rtp,rdp,nmp,flags,lcl)) {
            goto done;
	  }
        }
      }
      scan = scan->next;
      if (scan == stop) break;
    }
    if (lcl) break;
      scp = scan_up;
  }

 done:
  return sts;

 have_typ:
  assert(ret); *rtp = ret;  return 1;

 have_dcl:
  assert(dcl); *rdp = dcl;  return 2;
}

CppScope *CppScope::findNameSpace(poolRef Ref)
{
  CppScope *ret = 0;

  findObj(Ref,0,0,&ret);

  return ret;
}

CppTypeV *CppScope::findType(poolRef Ref,CppTypeRef::flags *flags,int lcl) const
{
  CppTypeV *ret = 0;

  findObj(Ref,&ret,0,0,flags,lcl);

  return ret;
}

int CppTypeRef::closeFunc(eTYPX *xtra) {
  int t = isFunc();

  assert(XF_FN_OPEN & isFunc());

  if (xtra && (*xtra & TYPX_NOEXCEPT)) {
      set_noexcept(); *xtra = TYPX((*xtra) & ~TYPX_NOEXCEPT);
  }
  
  if (XF_FN_OPEN == t) {
    t = setFunc(XF_FN_CLSD);
  }
  return t;
}

int CppTypeRef::isFunc() const 
{
  int t = x.f.func;

  if (1 == t && !x.f.bod) t |= XF_FN_FWD;

  if (0 == t && typ) t = typ->isFunc();
  if (0 == t && sub) t = sub->isFunc();

  return t;
}

int CppTypeRef::isAddr() const
{
  if (x.f.indrct) return -1;
  if (index)      return 1;

  return typ->isAddr();
}

int CppType::isA(int pool,int index) const
{
  if (pool == name.pool && index == name.index) return name.index;

  return 0;
}

CppTypeV **CppType::recycleList() 
{
  return (CppTypeV **)&RListType;
}

CppTypeV **CppTypeRef::recycleList() 
{
  return (CppTypeV **)&RListTypeRef;
}

CppTypeV **CppTypeCls::recycleList() 
{
  return (CppTypeV **)&RListTypeCls;
}

int CppTypeCls::isA(int pool,int index) const
{
  if (pool == name.pool && index == name.index) return name.index;

  CppTypeV *ip = inherits;
  while (ip) {
    int idx = ip->isA(pool,index);
    if (idx) return idx;
      ip = ip->next;
  }

  return 0;
}

int CppTypeRef::isA(int pool,int index) const
{
  if (pool == name.pool && index == name.index) return name.index;

  return typ->isA(pool,index);
}

void CppTypeCls::addCastFn(CppTypeRef *cfn)
{
  cfn->next = castFn;
  castFn    = cfn;
}

const CppTypeV *CppTypeCls::findEntity() const
{
  const CppTypeV *scan = inherits;
  const CppTypeV *scan2;

  for (; scan ; scan = scan->next) {
    if (PCT_ENTITY == scan->getCT()) return scan;
    if (scan2=scan->findEntity()) return scan2;
  }
}

int CppTypeCls::isBuiltin() const
{
  switch (name.pool) {
  case BUILTIN_POOL: return name.index;
  }

  CppTypeV *it = inherits;

  for (; it ; it = it->next) {
    int idx = it->isBuiltin();
    if (idx) return idx;
  }

  return 0;
}

int CppTypeRef::isBuiltin() const
{
  if (BUILTIN_POOL == name.pool) {
    return name.index;
  }

  return typ->isBuiltin();
}

void dumpScope(CppScope *scope)
{
  CppDecl *scan = scope->decls.first();
  fprintf(stderr,"Decls:\n");
  while (scan) {
    fprintf(stderr,"  %s\n",strDeref(scan->name));
    scan = scan->next;
  }
  fprintf(stderr,"Types:\n");
  CppTypeV *scnt = scope->types.first();
  while (scnt) {
    fprintf(stderr,"  %s\n",strDeref(scnt->Name()));
    scnt = scnt->next;
  }
}

CppScope *CppTypeRef::Scope(int fllw)
{
  CppScope         *ret = scp;
  const CppTypeRef *rfit;
  const CppTypeV   *ttv;

  if (typ && (rfit = typ->cRef())
          && rfit->x.f.inst) {
    if (ttv = rfit->typ) {
      const CppTypeRef *ttr = ttv->cRef();
      if (ttr && ttr->sub) {
	ret = (const_cast<CppTypeRef *>(ttr->sub))->Scope(1);
      }
    }
  }

  if (!ret) {
    if (fllw && typ) {
      ret = (const_cast<CppTypeV *>(typ))->Scope(1);
      switch (fllw) {
      case 2:
	if (ret && ret->up && SCP_TEMPLATE == ret->up->sc_typ) {
	  do {
	    ret = ret->up;
	  } while (ret->up && SCP_TEMPLATE == ret->sc_typ);
	}
      }
    }
  }

  return ret;
}

void CppTypeRef::regType(CppScope *prnt,CppScope *use_scp) 
{
  if (x.f.rgstrd) {
    return;
  }

  if (!scp) {
    scp = new CppScope();
  }
  if (use_scp) {
    prnt = use_scp;
  }
  if (SCP_TEMPLATE == prnt->sc_typ) {
    prnt =  prnt->up;
  }  

  prnt->addType(this);
}

void CppTypeRef::set_bod(CppScope *prnt,CppScope *use_scp,CppStmtType *dcl_stmt) 
{
  if (dcl_stmt) dcl_stmt->fwrd = 0;

  if (!x.f.rgstrd) {
    if (!(x.f.func || x.f.inst)) {
      CppTypeCls *cls = const_cast<CppTypeV *>(typ)->cTypCls();
      assert(cls);
      cls->set_bod();
      assert(0 == prnt || prnt == cls->Scope()->up); 
      return;
    }
    if (!scp) {
      scp = new CppScope();
    }
    if (use_scp) {
      prnt = use_scp;
    }
    if (SCP_TEMPLATE == prnt->sc_typ) {
      prnt =  prnt->up;
    }
    prnt->addType(this);
  }

  if (x.f.func) {
    CppDecl *arg = args;
    for (; arg ; arg = arg->next) {
      scp->addDecl(arg);
    }
  }

  x.f.bod = 1;
}

const Token *CppContext::getFnArg(Token const *T, Token const *TL, eCPRS parsing,ePcMod mods,ePCMD mode, CppDecl **pRet,CppTypeV *prnt)
{
  assert(!*pRet);

  parsing = CPRS((parsing|CPRS_Args|CPRS_TypeExprOK|CPRS_FuncArg)
		                         & ~(CPRS_Func|CPRS_Type));

  T = prsDeclItem(T,TL,parsing,mods,mode,prnt,0,pRet);

  return T;  
}

const Token *CppContext::getFnArgs(Token const *T0, Token const *TL, eCPRS parsing,ePcMod mods,ePCMD mode, CppDecl** pRet, CppTypeV *prnt,int rgstr)
{
  const Token *T    = T0,
              *TE,
              *clsr = &TOK_PUNC_CLS_BR;
  int          idx;
  CppDecl     *dcl,
             **dcl_p = pRet;
  
  if (mods & PCX_TMPLT_ARG) {
    clsr = &TOK_COP_GT;
  }

  assert(!*pRet);

  mods = PCX((mods & ~PCX_TYPEDEF)|PCX_AUTODECL);

  for (; T <= TL ; T = nextTok(T,0)) {
    idx = closeStmt(T,TL,&TE,PUNC_COMMA,1);
    assert(idx);
    if (T < TE) {
      T = getFnArg(T,TE,parsing,mods,mode,dcl_p,prnt);
      if (dcl = *dcl_p) {
        if (!NULL_REF(dcl->name)) {
          dcl->setIsArg();
	  if (rgstr) {
            current->addDecl(dcl);
	  }
	}
        dcl_p = &dcl->next;
      }
      assert(T >= TE);
    }
    if (SAME_TOK(clsr,TE)) {
      if (T <= TE) {
	T = skip1(TE,TL);
      }
      break;
    }
  }

  return T;
}
const Token *CppContext::getTmpltInstArgs(Token const *T0, Token const *TL, eCPRS parsing,ePcMod mods,ePCMD mode, CppTypeRef *ref)
{
  ref->set_inst();
  Token const *T = getFnArgs(T0,TL,parsing,mods,mode,&ref->args,
                             const_cast<CppTypeV *>(ref->typ));
  fixTmpltTyp(ref);
  return T;
}

CppInit *CppInitList::Add(CppDecl *dcl,CppTypeV *spr,CppExpr *xpr,int idx)
{
  if (idx < 0) idx = count;

  CppInit *old = list;
  list         = new CppInit [1 + ++count];

  int i = -1;
  while (++i < idx) {
    list[i] = old[i];
  }

  list[i].decl  = dcl;
  list[i].super = dcl ? dcl->typ
                      : spr;
  list[i].expr  = xpr;

  while (++i < count) {
    list[i] = old[i-1];
  }

  return &list[idx];
}

const Token *CppContext::prsCnstrctrs(const Token *T0,const Token *TL,eCPRS parsing,
                                      ePcMod mods,ePCMD mode,
                                      CppTypeRef *pFnc,int depth)
{
  poolRef           Ref;
  const Token      *T;
  CppInit           i;
  const CppScope   *scp;
  const CppTypeV   *prf = pFnc->typ;
  const CppTypeRef *tst;

  while (prf && (tst = prf->cRef())) {
    prf = tst->typ;
  }
  if (0 == prf || !(scp = prf->Scope())) {
    scp = current;
  }

  T = tokGetRef(T0,&Ref,0,1);
  for (;;) {
    switch (Ref.pool) {
    case PUNCTUATION_POOL: switch (Ref.index) {
      case PUNC_COMMA:
        T = tokGetRef(T0 = T,&Ref,0,1);
        continue;
      case PUNC_OPN_CR:
        assert(depth);
        pFnc->inits = new CppInitList(depth);
        return rescanning(T0);
      } break;

    default:
      const Token *TE;
      CppDecl     *dcl = 0;
      CppTypeV    *typ[2];
      typ[1] = typ[0] = 0;
      T = rescanning(T0);
      if (closeStmt(T,TL,&TE,PUNC_OPN_BR,1)) {
        T = tokGetRef(T0 = T,&Ref,0,1);
	T = skipWht(T);
        if (T >= TE) {
          int sts = scp->findObj(Ref,&typ[0],&dcl,0);
          switch (sts) {
	  case 1: case 2: goto found;
	  }
	} 
        if (!dcl) {
          T = prsType(rescanning(T0),TE,parsing,mods,mode,
                      const_cast<CppTypeV *>(pFnc->typ),typ);
	}
       found:
	if (!(dcl || typ[0])) {
          const CppTypeCls  *cls = pFnc->typ->cTypCls(1);
	  CppTypeV         **pi = 0;
          if (cls) {
            CppTypeV *inh;
	    pi = &(const_cast<CppTypeCls *>(cls))->inherits;
            for (; inh = *pi; pi = &inh->next) {
              const CppTypeRef *irf = inh->cRef();
	      const CppTypeV   *it  = irf->typ;
              if (it && (irf = it->cRef())) {
                poolRef nm = irf->Sub()->Name();
                if (SAME_REF(Ref,nm)) {
                  typ[0] = const_cast<CppTypeCls *>(irf->Sub()->cTypCls());
                  goto retest;
	        }
	      } else if (irf->x.f.stub) {
		poolRef nm = irf->Name();
		if (SAME_REF(Ref,nm)) {
		  typ[0] = const_cast<CppTypeRef *>(irf);
                  goto retest;
		}
	      } 
	    }
	  }
          CppTypeRef *stb = new CppTypeRef(Where(),Ref);
          stb->set_stub();
	  typ[0] = stb;
	  if (pi) *pi = stb; 
#         if DBGLVL > 0
          reportError(S_WARNING(STS_SYNTAX),
                      "Construction object not recognized - %s",
                      strDeref(Ref));
#         endif	  
	}
      }
     retest:
      T = tokGetRef(T,&Ref,0,1);
      switch (Ref.pool) {
      case CPPOPS_POOL: switch (Ref.index) {
	case COP_SCOPE: assert(!dcl);
	                T = getSub(T,TE,parsing,mods,mode,typ[0]);
                        goto retest;                       
	}
        assert(0);
      case PUNCTUATION_POOL: assert(PUNC_OPN_BR == Ref.index);
      }
      i.decl  = dcl;
      i.super = dcl ? dcl->typ
	            : typ[0];
      if (closeStmt(T,TL,&TE)) {
        assert(T <= TL);
        i.expr = prsExpr(T,TE,parsing,mods,mode);
        T      = prsCnstrctrs(nextTok(TE,0),TL,parsing,mods,mode,pFnc,depth+1);
        pFnc->inits->list[depth] = i;
        goto done;
      }
      assert(0);
    }
  }

 done:
  return T;
}

const Token *CppContext::prsClass(const Token *T,const Token *TL,eCPRS parsing,
                                  ePcMod mods,ePCMD mode,CppTypeV **pTyp)
{
  poolRef      Ref;
  int          named = 0;
  int          anon;
  const Token *T2,
              *TE,
              *Ts    = T;
  eCT          ct    = PCT_CLASS;

  switch (parsing & (CPRS_Class|CPRS_Union|CPRS_Struct|CPRS_Module)) {
  case CPRS_Union:        //fprintf(stderr," [union] ");
                          ct = PCT_UNION  ;      break;
  case CPRS_Struct:       //fprintf(stderr," [struct] ");
                          ct = PCT_STRUCT ;      break;
  case CPRS_Module:       //fprintf(stderr," [module] ");
                          ct = PCT_MODULE ;      break;
  case CPRS_Entity:       //fprintf(stderr," [entity] ");
                          ct = PCT_ENTITY ;      break;
  case CPRS_Architecture: //fprintf(stderr," [arch] ");
                          ct = PCT_ARCHITECTURE; break;
  case CPRS_Class:        //fprintf(stderr," [class] ");
                                                break;
  default:                assert(0);
  }

  CppTypeV   **inherits = 0,
              *t_curr   = 0,
              *prior    = 0,
              *sub;
  CppTypeRef  *inherit  = 0,
              *ref      = 0;
  int          in_tmplt;
  CppScope    *saved    = 0,
              *prnt     = current;

  if (in_tmplt = (mods & (PCX_TEMPLATE|PCX_TMPLT_ARG))) {
    mods = PCX(mods & ~(PCX_TEMPLATE|PCX_TMPLT_ARG));
  }

  while (T < TL) {
    const Token *T0 = T;
    T = tokGetRef(T0,&Ref,0,1);
    switch (Ref.pool) {
      case PUNCTUATION_POOL:
	switch (Ref.index) {
	case PUNC_SCOLON: TL = T;
	                  goto body;
        case PUNC_COMMA:
	case PUNC_COLON:  if (!inherits) {
                            assert(ref && ref->x.f.inst && !ref->scp);
                            CppTypeScp *cti = new CppTypeScp(Where(Ts),ct,SCP_TYPE);
                            ref->scp = cti;
			    inherits = &cti->inherits;
			  }
             	          *inherits = 
                            inherit = new CppTypeRef(Where(),T0,(CppTypeV *)0);
                          inherits  = &inherit->next;
                          continue;
	case PUNC_OPN_CR: goto body;
	}
      case CPP_POOL:
        switch (Ref.index) {
	case CPP_PUBLIC:    inherit->set_pblc(); continue;
	case CPP_PRIVATE:   inherit->set_prvt(); continue;
	case CPP_VIRTUAL:   inherit->set_vrtl(); continue;
        case CPP_PROTECTED: inherit->set_prot(); continue;
	}
      case GNU_POOL:
        assert(0);
      case CPPOPS_POOL: 
        switch (Ref.index) {
        case COP_GT: if (T > TL) {
     	               TE = skipWht(rescanning(T0),TL);
                       if (T > TL) {
			 T = TE; goto done;
		       }
	             }
	             assert(0);
		     continue;
	case COP_LT: assert((in_tmplt & PCX_TEMPLATE) 
			    && t_curr);
	             ref= t_curr->cRef();
                     if (!ref) {
                       ref = new CppTypeRef(Where(),rescanning(T0),t_curr);
		     }
                     assert(!ref->args);
                     TE = closeTmpltArg(&T,TL,1);
                     T  = getTmpltInstArgs(T,TE,parsing,mods,mode,ref);
                     continue;
        case COP_SCOPE:
	             assert(ref);
                     T = getSub(T,TL,parsing,mods,mode,ref);          
	             continue;
        case COP_ASSIGN:
	             if (in_tmplt & PCX_TMPLT_ARG) {
                       T2 = T = rescanning(T0);
                       goto decl;
	             }
		     assert(0);
	}
      default:
        if (inherit) {
          assert(checkName(Ref));
          CppScope    *scp = current;
          CppTypeV    *ti  = scp->findType(Ref);
	  CppTypeRef  *trf;
          const Token *TE;
          if (!ti) {
            CppDecl *dcl = scp->findRef(Ref);
            if (dcl) {
              assert(0);
            } else if (scp = scp->findNameSpace(Ref)) {
              trf = new CppTypeRef(Where(T0),Ref);
	      trf->set_nmspc(scp);
              ti  = trf;
	    } else {
              assert(0);
	    }
	  }
          for (;;) {
            poolRef nxt;
            T = tokGetRef(T0 = T,&nxt,0,1);
	    switch (nxt.pool) {
            case CPPOPS_POOL: switch (nxt.index) {
              case COP_SCOPE:
                if (!(trf = ti->cRef())) {
                  trf = new CppTypeRef(Where(T0),T,ti);
		}
                T  = getSub(T,TL,parsing,mods,mode,trf);
                ti = trf;
                continue;
              case COP_LT:
                rescanning(T0);
                TE  = closeTmpltArg(&T0,TL);
		trf = new CppTypeRef(Where(T0),T,ti);
                T   = getTmpltInstArgs(T,TE,parsing,mods,mode,trf);
                ti  = trf;
                continue;
	      }
            default:
              T  = rescanning(T0); goto end_inh;
	    }
	  }
	 end_inh:
          inherit->set_typ(current,ti);
	} else if (named++) {
          T2 = T0; 
          goto decl;
	} else {
          CppDecl  *p_dcl = 0;
          CppScope *p_nms = 0;
          CppTypeV *p_typ = 0;
          CppDecl  *args  = 0;
          poolRef   cls   = Ref;
          CppScope *scp   = current;
	 retry:
          int       sts   = scp->findObj(cls,&p_typ,&p_dcl,&p_nms);
          if (p_nms) {
            T2    = tokGetRef(T,&Ref,0,1);
            assert(SAME_REF_PI(Ref,CPPOPS_POOL,COP_SCOPE));
            T     = tokGetRef(T2,&cls,0,1);
            scp   = p_nms;
            p_nms = 0;
            goto retry;
	  }
          if (p_typ) {
            t_curr = prior = p_typ;
	    const Token *Tc = T; 
	    for (;;) {
	      T2 = tokGetRef(T,&Ref,0,1);
	      if (SAME_REF_PI(Ref,CPPOPS_POOL,COP_LT)) {
 	        const Token *TE = closeTmpltArg(&T2,TL,1);
  	        T = getFnArgs(T2,TE,parsing,PCX(mods|PCX_T_INST),mode,&args);
                if (in_tmplt & PCX_TEMPLATE) {
                  T2 = tokGetRef(T,&Ref,0,1);
                  if (SAME_REF_PI(Ref,CPPOPS_POOL,COP_SCOPE)) {
    		    assert(prior->isTmplt());
                    ref = new CppTypeRef(Where(T0),T,prior);
                    fixTmpltTyp(ref);
		  } else {
                    rescanning(T);
                    prior = 0; goto as_new;
		  }
	        } else {
  		  assert(prior->isTmplt());
  	  	  ref = new CppTypeRef(Where(T0),T,prior);
 	          ref->set_inst(args);
		  prior = ref;
		}
	      } else if (SAME_REF_PI(Ref,CPPOPS_POOL,COP_SCOPE)) {
		ref   = new CppTypeRef(Where(T0),T,prior);
		T     = getSub(T2,TL,parsing,mods,mode,ref);
		prior = ref;
	      } else {
		rescanning(T);
                if ((in_tmplt & PCX_TEMPLATE) && !ref) {
                  prior = 0; goto as_new;
		}
		break;
	      }
	    }
            if (!(parsing & CPRS_Friends)) {
              if (prior == t_curr) {
                CppTypeCls *cls = prior->cTypCls();
                if (!cls) {
                  rescanning(T);
                  goto as_new;
		}
                CppTypeScpR *obj = new CppTypeScpR(Where(T0),cls);
                t_curr           = obj->Type();
	      } else {
                t_curr = prior;
                prior  = 0;
	      }
	    }
	  } else {
 	   as_new:
            assert(!inherits);
            if (parsing & CPRS_Friends) {
              assert(!args);
              CppType *obj = new CppType(Where(T0),ct,cls);
              obj->set_fwd();
              t_curr = obj;
              continue;
	    } else {
              CppTypeScp *obj = new CppTypeScp(Where(T0),ct,SCP_TYPE,cls);
              t_curr = obj;
	      if (in_tmplt & PCX_TMPLT_ARG) {
                obj->set_tmplt_arg();
                assert(SCP_TEMPLATE == current->sc_typ);
                current->addType(t_curr);
              } else if (in_tmplt) {
                obj->set_tmplt();
                assert(SCP_TEMPLATE == current->sc_typ);
                current->up->addType(t_curr);
              } else {
                current->addType(t_curr);
                obj->up = current;
  	      }
              if (args) {
                assert(!ref);
                ref = new CppTypeRef(Where(),T0,t_curr);
                ref->set_inst(args);
                fixTmpltTyp(ref);
	      }
	    }
	  } 
          CppType   *pCls = t_curr->cTyp();
          CppTypeV **in0;
          if (pCls && !*(in0 = pCls->Inherits0())) {
	    inherits = in0;
	  }
	}
    }
  }

  goto done;

 body: 
  parsing = CPRS(parsing & ~CPRS_Top);

  if (anon = !t_curr) {
    t_curr = new CppTypeScp(Where(Ts),ct,SCP_TYPE);
    current->addTypeAnon(t_curr);
  }
  if (T < TL) {
    assert(!prior || !prior->hasBod());
    sub = t_curr;
    while ((ref = sub->cRef()) && ref->sub) {
      sub  = const_cast<CppTypeRef *>(ref->sub);
      prnt = ref->Scope(); 
    }
    sub->set_bod(prnt);
    T2 = findClose(T,TL,TOK_PUNC_CLS_CR,PUNC_OPN_CR);
    if (!T2) {
      reportError(S_ERROR(STS_SYNTAX),"Couldn't find end of construct %s",
		                                strDeref(t_curr->Name()));
      T2 = TL;
    }
    saved = pushDown(sub->Scope(1));
    T     = prsPCtok(T,T2,parsing,PCX((mods|PCX_FIELD) & ~PCX_TYPEDEF),
                     mode,sub);
    if (T < T2) {
      assert(0);
    }
    T2    = nextTok(T2,0);
    T     = T2;
    popScope(saved);
   decl:
    if (in_tmplt) {
      assert(T == T2);
    } else {
      CppTypeV *typ[2];
      typ[0] = t_curr;
      typ[1] = 0;
      T = prsDecl(T2,TL,parsing,mods,mode,0,typ);
    }
  } else {
    assert(!anon);
    if (in_tmplt && t_curr) {
      CppScope *ts = t_curr->Scope(); 
      if (ts) {
        ts->up = current;
      }
    }
  }

 done:
  if (pTyp) {
    assert(!*pTyp);
    *pTyp = t_curr;
  }

  return T;  
}

const Token *CppContext::prsASM(const Token *T,const Token *TL,
				CppExpr **pRet,eXPRF flgs)
{
  const Token *T0,
              *T2;
  poolRef      Ref;

  T = tokGetRef(T0 = T,&Ref,0,1);
  if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_BR)) {
    T2 = findClose(T0 = T,TL,TOK_PUNC_CLS_BR,PUNC_OPN_BR);
    assert(T2);
    T  = nextTok(T2,0);
  } else {
    T2 = T;
  }

  CppExpr *ax = new CppExpr(T0,T2);
  ax->flgs = flgs;
  CppExpr::ListAdd(pRet,ax);

  return T;
}

const Token *CppContext::prsAttr(const Token *T,const Token *TL,
                                 CppExpr **pRet)
{
  const Token *T0 = T;
  poolRef      Ref;

  T = tokGetRef(T0,&Ref,0,1);

  const Token *T2 = findClose(T,TL,TOK_PUNC_CLS_BR,PUNC_OPN_BR);
  assert(T2);
  
  CppExpr::ListAdd(pRet,new CppExpr(T,T2));

  T = nextTok(T2,0);

  return SAME_TOK_PI(T,PUNCTUATION_POOL,PUNC_SCOLON) ? T
                                                     : skip1(T,TL);
}

const Token *CppContext::closeTmpltArg(const Token **pT,const Token *TL,int strtd)
{
  poolRef      Ref;
  const Token *T = *pT;
  int          depth = 0;

  if (!strtd) {
    const Token *T2 = tokGetRef(T,&Ref,0,1);
    if (!SAME_REF_PI(Ref,CPPOPS_POOL,COP_LT)) {
      rescanning(T);
      return 0;
    }

    *pT = T = T2;
  }

  for (; T < TL ; T = nextTok(T,0,-1)) {
    switch (T->pi.pool) {
      case CPPOPS_POOL:  switch (T->pi.index) {
        case COP_LT:     depth++ ; break;
        case COP_GT:     if (0 >= depth--) goto end_parm;
                         break;
        }
        break;
      case TOK_FULL_REF:
        T += 2;
    }
  }

 end_parm:
  return T;
}

int doClsFunc(CppTypeV **pTyp) 
{ 
  if (pTyp) {
    if (pTyp[1] && pTyp[1]->isFunc()) {
      if (!pTyp[1]->funcClsd(1)) {
        assert(pTyp[1]->funcClsd());
        return 1;
      }
    }
    if (pTyp[0] && pTyp[0]->isFunc()) {
      if (!pTyp[0]->funcClsd(1)) {
        assert(pTyp[0]->funcClsd());
        return 1;
      }
    }
  }

  return 0;
}

int CppContext::tryClsFunc(const Token **T,const Token *TL,CppTypeV **typ)
{
  if (doClsFunc(typ)) {
    poolRef Ref;
    const Token *T2 = tokGetRef(*T,&Ref,0,1);
    assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_CLS_CR));
    *T = T2;
    return 1;
  }

  return 0;
}

const Token *CppContext::prsTemplate(const Token *T,const Token *TL,
                                     eCPRS parsing,ePcMod mods,ePCMD mode,
                                     CppTypeV *prnt)
{
  CppTypeV    *typ[2];
  const Token *TE = closeTmpltArg(&T,TL);
  CppTypeV    *tt;

  typ[1] = typ[0] = 0;

  if (TE) { 
    CppScope *ts = new CppTypedScp(SCP_TEMPLATE);
    pushDown(ts);      
    T = getFnArgs(T,TE,parsing,PCX(mods|PCX_TMPLT_ARG),mode,&ts->args);
    if (ts->args) {
      mods = PCX(mods|PCX_TEMPLATE);
    } else {
      popScope();
      delete ts; ts = 0;
    }
    poolRef Ref;
    T = rescanning(TE);
    T = tokGetRef(T,&Ref,0,1);
    assert(SAME_REF_PI(Ref,CPPOPS_POOL,COP_GT));
    if (TE < TL) {
      T = prsType(T,TL,parsing,mods,mode,prnt,typ);
      tryClsFunc(&T,TL,typ);
    }

    if (ts) {
      popScope();
    } else {
      addStmt(new CppStmtTmplRef(typ,Where(),PCX_T_INST));
    }
  } else if (closeStmt(T,TL,&TE)) {
    T      = prsType(T,TL,parsing,mods,mode,prnt,typ);
    addStmt(new CppStmtTmplRef(typ,Where(),PCX_EXTERN));
  } else {
    assert(0);
  }

  return T;
}

const Token *CppContext::prsProcess(const Token *T,const Token *TL,
                                    eCPRS parsing,ePcMod mods,ePCMD mode,
                                    CppTypeV **pTyp)
{
  poolRef      Ref;
  poolRef      nm  = NullRef;
  const Token *T2;
  CppTypeScp  *prc = 0;

  T = tokGetRef(T,&Ref,0,1);
  if (checkName(Ref)) {
    nm = Ref;
    T  = tokGetRef(T,&Ref,0,1);
  }
  if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_CR)) {
    const Token *TE = findClose(T,TL,TOK_PUNC_CLS_CR,PUNC_OPN_CR);
    if (TE) {
      prc = new CppTypeScp(Where(),PCT_PROCESS,SCP_TYPE,nm);
      current->addType(prc);
      prc->incrRsm();
      CppScope *saved = pushDown(prc);
      T = prsStmts(T,TE,CPRS(parsing|CPRS_Block),mods,mode);
      popScope();
      assert(saved == current);
      assert(T >= TE);
      T = nextTok(T,0);
    }
  }

  if (prc) {
    CppTypeV *typ[2] = {prc,0};
    if (pTyp) *pTyp = typ[0];
    T = prsDecl(T,TL,parsing,mods,mode,0,typ);
  }

  return T;
}

const Token *CppContext::prsTypeDef(const Token *T,const Token *TL,
                                    eCPRS parsing,ePcMod mods,ePCMD mode)
{
  mods = PCX(mods|PCX_TYPEDEF);
   
  T = prsType(T,TL,parsing,mods,mode);

  return T;
}

CppStmt *CppContext::addStmt(CppStmt *stmt,int posn)
{
  return current->addStmt(stmt,posn);
}

CppStmt *CppScope::addStmt(CppStmt *stmt,int posn)
{
  switch (posn) {
  default: assert(0);
  case -1: if (p_stmt == &stmt0) {
             p_stmt = &(stmt->next);
           }
           stmt->next = stmt0;
           stmt0      = stmt;
           break;
  case  1: assert(!*p_stmt);
           *p_stmt    = stmt;
           p_stmt     = &(stmt->next);
  }

  return stmt;
}

CppStmt *CppStmt::addStmt(CppStmt *stmt,CppScope *scp,int posn)
{
  CppStmt **scan,
           *insrt;

  switch (posn) {
  default: assert(0);
  case -1: scan = &scp->stmt0;
           while ((insrt = *scan) != this) {
             assert(insrt);
             scan = &insrt->next;
           }
           stmt->next = insrt;
           *scan      = stmt;
           break;
  case  1: if (!(stmt->next = next)) {
             assert(scp->p_stmt = &next);
             scp->p_stmt = &stmt->next;
	   }
           next = stmt;
  }

  return stmt;  
}

int CppContext::isPntr(const Token **pT,const Token *TL,poolRef *ret)
{
  const Token *T = *pT;
  poolRef      Ref,
               func;

  T = tokGetRef(T,&Ref,0,1);
  if (SAME_REF_PI(Ref,CPPOPS_POOL,COP_MULTIPLY)) {
    T = tokGetRef(T,&func,0,1);
    if (checkName(func)) {
      T = tokGetRef(T,&Ref,0,1);
      if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_CLS_BR)) {
        *pT  = T;
        *ret = func;
        return 1; 
      }
    }
  } else if (checkName(Ref)) {
    T = tokGetRef(T,&Ref,0,1);
    if (SAME_REF_PI(Ref,CPPOPS_POOL,COP_SCOPE)) {
      *pT = T;
      return isPntr(pT,TL,ret);
    } else {
      assert(!(SAME_REF_PI(Ref,CPPOPS_POOL,COP_LT)));
    }
  }

  return 0;
}

//> expr = 1, decl = -1, 2 = null
int CppContext::stmtTyp(const Token *T0,const Token *TL,
		       eCPRS parsing,ePcMod mods,ePCMD mode,CppTypeV *typ)
{
  const Token *T   = T0,
              *TE,
              *T2;
  poolRef      Ref;
  int          ret = 2;
  CppTypeV    *t1  = 0;
  CppScope    *scp = current;

  if (typ) {
    if (typ->isFunc()) {
      goto done;
    }
    t1 = typ;
  }

  while (T < TL) {
    T = tokGetRef(T2 = T,&Ref,0,1,TL);
   got_ref:
    if (NULL_REF(Ref)) goto done;
    ret = 1;
    switch (Ref.pool) {
    case PUNCTUATION_POOL: switch (Ref.index) {
      case PUNC_COMMA:
      case PUNC_PERIOD:
      case PUNC_OPN_BR: ret = 1; goto done;
      }
      assert(0);
      continue;
    case CPP_POOL:
      switch (Ref.index) {
      case CPP_TYPENAME:
      case CPP_INT:
      case CPP_UNION:    ret = -1; goto done;
      case CPP_CONST:
      case CPP_STATIC:
      case CPP_VOLATILE: ret =  0; goto done;
      case CPP_THIS:
      case CPP_DELETE:   ret =  1; goto done;
      }
      assert(0);
      continue;
    case CPPOPS_POOL:  switch (Ref.index) {
      case COP_SCOPE:  T = tokGetRef(T,&Ref,0,1,TL);
	               if (checkName(Ref)) {
                         if (t1) {
			   scp = t1->Scope(1);
			 }
		         if (!scp) {
                           ret = 1; goto done;
		         }
                         goto test_typ;
		       } else if (CPP_POOL == Ref.pool) {
                         ret = 1; goto done;
		       }
	               assert(0);
      case COP_LT:     if (t1 && t1->isTmplt()) {
                         if (TE = closeTmpltArg(&T,TL,1)) {
                           T = tokGetRef(advanceTo(T,TE),&Ref,0,1,TL);
                           continue;
			 }                         
		       }
		       ret = 1;
      default:         goto done;
	}
    default:  if (checkName(Ref)) {
	        if (t1) {
                   ret = -1; goto done;
	        }
    test_typ:  
                CppTypeV *t2  = 0;
                CppDecl  *dcl = 0;
                CppScope *nms = 0;
                switch (scp->findObj(Ref,&t2,&dcl,&nms)) {
                case 1:  ret = 0;
                         t1  = t2;
                         if (t2->isFunc()) {
                           T = tokGetRef(T2 = T,&Ref,0,1,TL);
			   if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_BR)) {
                             ret = 1; goto done;
			   }
                           goto got_ref;
		         }
	                 continue;
                case 2:  if ((t2 = dcl->typ)->isTypNm()) {
 	                   t1 = t2;
                           continue;
	        	 }
		         if (t2->isFunc() && t2->isTypdf()) {
			   ret = -1;
			   goto done;
			 }
                         ret = 1;
                         goto done;
                case 3:  scp = nms;
                         continue;
                }
                ret = 1; goto done;               
              }
    }
  }

  if (ret < 2) {
    if (T < TL && SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_BR)) {
      ret = 1;
    } else {
      ret = 0;
    }
  }
 done:
  rescanning(T0);
  return ret;
}

const Token *CppContext::prsStmts(const Token *T,const Token *TL,
				  eCPRS parsing,ePcMod mods,ePCMD mode)
{
  poolRef      Ref,
               tmp;
  const Token *Ts       = T;
  const Token *T2;
  eCPRS        pars_svd = parsing;

  assert(parsing & (CPRS_Stmt|CPRS_Block|CPRS_Type|CPRS_Fork));

  parsing = CPRS(pars_svd & ~(CPRS_Stmt|CPRS_Block|CPRS_Type));

#define ADD_STMT(s,t,g) {addStmt(s);\
                         Ts   = (t) ? T = (t) < TL ? nextTok(t,0) : TL	\
                                    : T;\
                         goto g;}

#define SKIP_CR          T = tokGetRef(T,&Ref,0,1);\
                         assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_CR));

  while (T < TL) {
    CppTypeV    *typ = 0;
    const Token *T0  = T;
    int          idx,
                 dcl = 0,
                 x;
    eCSTMT       st;

    T = tokGetRef(T,&Ref,0,1,TL);
    switch (Ref.pool) {
      case 0:
        assert(T >= TL);
        T = rescanning(TL);
        goto done;
      case PUNCTUATION_POOL: switch (Ref.index) {
        case PUNC_OPN_CR: { CppStmtBlock *blk = new CppStmtBlock(Where());
                            CppScope *saved = pushDown(blk);
                            T = prsStmts(T,TL,CPRS(parsing|CPRS_Block),
                                                   mods,mode);
			    popScope();
                            assert(saved == current);
	                    ADD_STMT(blk,T,stmt)
 	                  } break;
        case PUNC_CLS_CR: return skipWht(rescanning(T0));
        case PUNC_SCOLON: if (parsing & CPRS_Stmt) {
                            return skipWht(rescanning(T0));
	                  }
	                  if (T0 == Ts) {
                            Ts = T;
                            continue;
			  }
        }
        goto stmts;
      case CPP_POOL: switch (idx = Ref.index) {
        case CPP_UNION:
        case CPP_STRUCT:
        case CPP_CLASS:
        case CPP_UNSIGNED: goto try_typ;
        case CPP_FOR:
          T = tokGetRef(T,&Ref,0,1);
          assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_BR));
          if (closeStmt(T,TL,&T2)) {
            CppScope *cntrs = 0; 
            CppExpr  *init  = 0;
	    switch (stmtTyp(T,T2,parsing,mods,mode)) {
            case 2: break;
            case 1:
              init = prsExpr(T,T2,parsing,mods,mode);
	      break;
            default:
              cntrs = new CppScope(SCP_FOR);
              pushDown(cntrs);
              T = prsStmts(T,T2,CPRS(parsing|CPRS_Stmt),mods,mode);
	    }
            if (closeStmt(T = nextTok(T2,0),TL,&T2)) {
              CppExpr *cnd = prsExpr(T,T2,parsing,mods,mode);
              if (closeStmt(T = nextTok(T2,0),TL,&T2)) {
		CppExpr    *tail = prsExpr(T,T2,parsing,mods,mode);
		CppStmtFor *fr   = new CppStmtFor(Where(),cntrs,init,cnd,tail);
		T2 = nextTok(T2,0);
		T  = tokGetRef(T2,&Ref,0,1);
		if (!SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_SCOLON)) {
		  CppScope *saved = pushDown(fr);
		  T = prsStmts(rescanning(T2),TL,CPRS(parsing|CPRS_Stmt),mods,mode);
		  popScope();
		  assert(saved == current);
		}
		if (cntrs) {
		  popScope();
		}
		ADD_STMT(fr,(const Token *)0,stmt)
	      }
	    } else if (cntrs) {
              popScope();
	    }
	  }
          assert(0);
        case CPP_TRY: {
	    SKIP_CR
 	    CppStmtBlock *blk   = new CppStmtBlock(Where(),CSTMT_try);
	    CppScope     *saved = pushDown(blk);
	    T = prsStmts(T,TL,CPRS(parsing|CPRS_Block),mods,mode);
	    popScope();
	    assert(saved == current);
	    ADD_STMT(blk,T,stmt)
	} assert(0);
        case CPP_CATCH: {
            T = tokGetRef(T,&Ref,0,1);
            assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_BR));
            CppDecl *ctch = 0;
            T = getFnArgs(T,TL,parsing,mods,mode,&ctch);
            CppStmtBlock *blk   = new CppStmtCatch(Where(),ctch);
            CppScope     *saved = pushDown(blk);
	    SKIP_CR
            T = prsStmts(T,TL,CPRS(parsing|CPRS_Block),mods,mode);
	    popScope();
	    assert(saved == current);
	    ADD_STMT(blk,T,stmt)
	  }
          assert(0);
        case CPP_DO: {
            CppStmtDo *sb;
            CppScope  *saved = pushDown(sb = new CppStmtDo(Where()));
	    T = prsStmts(T,TL,CPRS(parsing|CPRS_Stmt),mods,mode);
            popScope();
            assert(saved == current);
            T = tokGetRef(T,&Ref,0,1);
            assert(SAME_REF_PI(Ref,CPP_POOL,CPP_WHILE));
            T = tokGetRef(T,&Ref,0,1);
            assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_BR));
            if (closeStmt(T,TL,&T2)) {
               sb->cnd = prsExpr(T,T2,parsing,mods,mode);
               T = tokGetRef(T2,&Ref,0,1);
               assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_CLS_BR));
               T = tokGetRef(T,&Ref,0,1);
               assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_SCOLON));
            }
	    ADD_STMT(sb,(const Token *)0,stmt) 
  	  } assert(0);
        case CPP_SWITCH:
        case CPP_WHILE:
	as_while: {
          T = tokGetRef(T,&Ref,0,1);
          assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_BR));
          if (closeStmt(T,TL,&T2)) {
            CppExpr *cnd  = prsExpr(T,T2,parsing,mods,mode);
            T = nextTok(T2,0);
            CppStmtBlock *sb;
            CppScope     *saved;
            switch (idx) {
  	      case CPP_WHILE:  saved = pushDown(sb = new CppStmtWhile(Where(),cnd));  break;
  	      case CPP_SWITCH: saved = pushDown(sb = new CppStmtSwitch(Where(),cnd)); break;
  	      case PCO_AT:     saved = pushDown(sb = new CppStmtAt(Where(),cnd));     break;
  	      case PCO_BEFORE: saved = pushDown(sb = new CppStmtBefore(Where(),cnd)); break;
	      default:         assert(0);
	    }
	    T = prsStmts(T,TL,CPRS(parsing|CPRS_Stmt),mods,mode);
            popScope();
            assert(saved == current);
	    ADD_STMT(sb,(const Token *)0,stmt)
	  }
	} assert(0);
        case CPP_IF:
	  T = tokGetRef(T,&Ref,0,1);
          assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_BR));
          if (closeStmt(T,TL,&T2)) {
            CppExpr   *cnd   = prsExpr(T,T2,parsing,mods,mode);
	    CppStmtIf *si    = new CppStmtIf(Where(),cnd);
            CppScope  *saved = pushDown(&si->tru);
            T = prsStmts(nextTok(T2,0),TL,CPRS(parsing|CPRS_Stmt),mods,mode);
            popScope();
            assert(saved == current);
            T = tokGetRef(T0 = T,&Ref,0,1);
            if (SAME_REF_PI(Ref,CPP_POOL,CPP_ELSE)) {
              saved = pushDown(si->fls = new CppScope);
              T = prsStmts(T,TL,CPRS(parsing|CPRS_Stmt),mods,mode);
              popScope();
              assert(saved == current);
	    } else {
              T = rescanning(T0);
	    }
            ADD_STMT(si,(const Token *)0,stmt)
	  }
	  continue;
        case CPP_CASE:
          if (closeStmt(T,TL,&T2,PUNC_COLON)) {
            CppExpr     *val = prsExpr(T,T2,parsing,mods,mode);
  	    CppStmtExpr *rx  = new CppStmtExpr(Where(),CSTMT_case,val);
            ADD_STMT(rx,T2,stmt)
	  }
          assert(0);
        case CPP_DELETE: T2 = tokGetRef(T,&Ref,0,1);
                         if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_SQ)) {
                           T2 = tokGetRef(T2,&Ref,0,1);
                           if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_CLS_SQ)) {
                             T = T2;
                             st = CSTMT_del_arr; goto as_throw;
			   }
                           rescanning(T);
			 }
                         st = CSTMT_delete; goto as_throw;
        case CPP_RETURN: st = CSTMT_return; goto as_throw;
        case CPP_THROW:  st = CSTMT_throw;
         as_throw:            
          if (closeStmt(T,TL,&T2)) {
            CppExpr     *val = prsExpr(T,T2,parsing,mods,mode);
  	    CppStmtExpr *rx  = new CppStmtExpr(Where(),st,val);
            ADD_STMT(rx,T2,stmt)
	  }
          assert(0);
        case CPP_CONST:    dcl =  1; goto stmts;    
        case CPP_STATIC:   dcl = -1; goto stmts;              
        case CPP_CONTINUE: st = CSTMT_cont; goto smpl_stmt;
        case CPP_BREAK:    st = CSTMT_break;
	        smpl_stmt: { CppStmt *ss  = new CppStmt(Where(),st);
	                     ADD_STMT(ss,0,close);
	                   }
        }
        break;
      case PRC_POOL: switch (idx = Ref.index) {
        case PRC_FORK: {
	    CppExpr     *rplx = 0;
	    const Token *T2   = tokGetRef(T,&Ref,0,1),
	                *TE;
	    if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_SQ)) {
	      if (TE = findClose(T2,TL,TOK_PUNC_CLS_SQ,PUNC_OPN_SQ)) {
		CppExpr rslt(CXPR_U32,-1);
		rplx = prsExpr(T2,TE,CPRS(CPRS_Expr|CPRS_Fork),mods,mode);
		if (rslt.eval(rplx) <= 0) {
		  reportError(S_ERROR(STS_SYNTAX),"Non-constant array bound");
		} else if (rslt.x.i64 < 0) {
		  reportError(S_ERROR(STS_SYNTAX),"Negative array bound not allowed");
		}
		TE   = nextTok(TE,1);
		T2   = tokGetRef(TE,&Ref,0,1);
	      } else {
		reportError(S_ERROR(STS_SYNTAX),"Couldn't find closing ']'");
	      }
	    }
	    int blk = SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_CR);
	    if (blk || rplx) {
	      CppStmtFork *sb;
	      CppScope    *saved = pushDown(sb = new CppStmtFork(Where(),rplx));
	      eCPRS        prsf  = CPRS(parsing|CPRS_Fork);
	      if (blk) {
		TE   = findClose(T2,TL,TOK_PUNC_CLS_CR,PUNC_OPN_CR);
	      } else {
		T2   = rescanning(TE);
	        TE   = TL;
		prsf = CPRS(parsing|CPRS_Stmt);
	      }
              if (TE) {
		T = prsStmts(T2,TE,prsf,mods,mode);
		if (blk) {
		  assert(T == TE);
		  T = tokGetRef(T,&Ref,0,1);
		}
	      } else {
		reportError(S_ERROR(STS_SYNTAX),"Couldn't find closing '%c'", 
                                                                  blk ? '}' : ';');
	      }
	      popScope();
	      assert(saved == current);
	      ADD_STMT(sb,(const Token *)0,stmt) 
	    } else {
	      rescanning(T);
	    }
	    assert(!rplx);
  	  } 
	} 
        break;
      case PRCOPS_POOL: switch (idx = Ref.index) {
	  case PCO_AT:     goto as_while;
	  case PCO_BEFORE: goto as_while;
        }
        break;
    }
    switch (Ref.pool) {
    case CPP_POOL: switch (Ref.index) {
                   case CPP_DEFAULT:   goto as_label;
                   case CPP_THIS:      goto stmts;
                   case CPP_TEMPLATE: 
                     Ts = T = prsTemplate(T,TL,parsing,mods,mode);
                     continue;
                   case CPP_GOTO: 
		     if (closeStmt(T,TL,&T2)) {
		       CppExpr *xpr = prsExpr(T,T2,CPRS_Label,mods,mode);
		       if (xpr) {
			 CppStmtExpr *x = new CppStmtExpr(Where(),CSTMT_goto,xpr);
			 ADD_STMT(x,T2,stmt)
		       }
		       goto end_expr;
		     } else {
		       reportError(S_ERROR(STS_SYNTAX),"No valid termination");
		     }
		     continue;
                   case CPP_TYPEDEF:
                     Ts = T = prsTypeDef(nextTok(T,1),TL,
                                         CPRS(parsing|CPRS_Type),mods,mode);
                     continue;
                   }
                   if (CPP_PROTECTED >= Ref.index) {
		     assert(!typ);
                     if (CPP_TYPENAME == Ref.index) {
		       typ = CppType::TypeName;
		     } else {
		       goto try_typ;
		     }
		   }
                   if (0 == typ && !(typ = CppType::cppType(Ref))) {
                     assert(0);
		   }
                   break;
    default:       if (checkName(Ref)) {
    as_label:
                     tmp = Ref;
                     T2  = tokGetRef(T,&Ref,0,1);
                     if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_COLON)) {
                       CppStmtLabel *lbl = new CppStmtLabel(Where(),CSTMT_Label,tmp);
                       ADD_STMT(lbl,T2,block)
                       continue;
                     }
                   }
    }      
   stmts:
    if (0 == (x = stmtTyp(rescanning(Ts),TL,parsing,mods,mode,typ))) {
     try_typ:
      Ts = T = prsType(rescanning(Ts),TL,parsing,mods,mode);
    } else if (closeStmt(Ts,TL,&T2)) {
      if (dcl || x < 0) {
        Ts = T = prsDecl(Ts,T2,parsing,mods,mode);
      } else {
        { CppExpr *xpr = prsExpr(Ts,T2,parsing,mods,mode);
	  if (xpr) {
	    CppStmtExpr *x = new CppStmtExpr(Where(),CSTMT_Expr,xpr);
	    ADD_STMT(x,T2,stmt)
          }
	}
      end_expr:
        Ts = nextTok(T2,0);
      }
    } else {
      assert(0);
    }
  stmt:
    if (pars_svd & CPRS_Stmt) break; 
  block:
    typ = 0;
    continue;
  close:
    T = tokGetRef(T,&Ref,0,1);
    assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_SCOLON));
    goto stmt;
  }

 done:
  return T;

#undef ADD_STMT
#undef SKIP_CR
}

int CppContext::typeList(const Token *T0,const Token *TL,eCPRS parsing,
                                         ePcMod mods,ePCMD mode,CppTypeV *typ)
{
  poolRef      Ref;
  int          l = 0,
               c = 0,
               nm;
  const Token *T,
              *TE;
  CppScope    *scp = current;
  CppTypeV    *trf = 0;
                     
  T = tokGetRef(T0,&Ref,0,1);

  for (;;) {
    switch (Ref.pool) {
    case PUNCTUATION_POOL:
                       switch (Ref.index) {
                       case PUNC_OPN_BR:  l =  0; goto done;
                       case PUNC_CLS_BR:  l = -1; goto done;
                       case PUNC_COMMA:   if (trf) {
                                            l = 1; goto done;
		                          }
                                          T = tokGetRef(T,&Ref,0,1);
                                          continue;                    
		       }
                       assert(0);
    case QUOTES_POOL:
    case INTEGER_POOL: goto done;
    case CPP_POOL:     switch (Ref.index) {
                       case CPP_VOID:
  	               case CPP_BOOL:
                       case CPP_SHORT:
                       case CPP_INT:
                       case CPP_LONG:
                       case CPP_FLOAT:
                       case CPP_SIGNED:
           	       case CPP_DOUBLE:
             	       case CPP_OPERATOR:
             	       case CPP_STRUCT:
             	       case CPP_TYPENAME:
                       case CPP_VOLATILE:
                       case CPP_UNSIGNED:
                       case CPP_CONST:    l = 1; goto done;
                       case CPP_THIS:            goto done;
                       }
                       goto as_name;
    case CPPOPS_POOL:  switch (Ref.index) {
                       case COP_SCOPE:    T = tokGetRef(T,&Ref,0,1);
		                          goto dwn_scp;
                       case COP_LT:       if (trf && trf->isTmplt()) {
                                            if (TE = closeTmpltArg(&T,TL,1)) {
                                              T = advanceTo(T,TE);
                                              T = tokGetRef(T,&Ref,0,1);
                                              T = tokGetRef(T,&Ref,0,1);
     		                              continue;
					    }
					  }
                       case COP_ELLIPSIS: l = 1; goto done;
                       default:           l = (0 != trf);
                                          goto done;
                       }
                       break;
    dwn_scp:
    default:           if (nm = checkName(Ref)) {
                        as_name:
                         if (trf) {
                           l = 1; goto done;
		         }
                         CppDecl  *dcl = 0;
                         CppScope *nms = 0;
                         switch (scp->findObj(Ref,&trf,&dcl,&nms)) {
                         case 2: trf = dcl->typ;
			         if (0 == trf || !trf->isTypNm()) {
                                   goto done;
			         }
                         case 1: T = tokGetRef(T,&Ref,0,1);
                                 switch (Ref.pool) {
			         case PUNCTUATION_POOL: switch(Ref.index) {
			           case PUNC_OPN_BR: goto done;
			           }
			         }
                                 continue;
                         case 3: scp = nms;
                                 T = tokGetRef(T,&Ref,0,1);
                                 if (!SAME_REF_PI(Ref,CPPOPS_POOL,COP_SCOPE)) {
                                   l = 1; goto done;} 
                                 T = tokGetRef(T,&Ref,0,1,TL); c++;
                                 goto dwn_scp;
		         }
                         if (typ && (scp = typ->Scope(1))) {
			   typ = 0;
                           goto as_name;
		         }
                       }
                       if (closeStmt(T,TL,&TE,PUNC_COMMA)) {
                         for (; T < TE ; c++, T = tokGetRef(T,&Ref,0,1,TE)) {
                           switch (Ref.pool) {
		  	   case GNU_POOL: switch(Ref.index) {
                             case GNU___CONST: l = 1;
                                               goto done;
			     } break;
		  	   case CPPOPS_POOL: switch(Ref.index) {
                             case COP_MINUS:
			     case COP_PLUS:    goto done;
			     } break;
			   case PUNCTUATION_POOL: switch(Ref.index) {
			     case PUNC_OPN_BR: goto done;
			     } break;
			   }                           
		         }
                         if (T >= TE) {
                           switch (Ref.pool) {
		  	   case CPPOPS_POOL: switch(Ref.index) {
                             case COP_B_AND:
                             case COP_MULTIPLY: l = 1;
			                        goto done;
			     } break;
			   }                           
                           if (1 == c && nm && checkName(Ref)) {
                             l = 1;
			   }
                           goto done;
		         }
		         T = tokGetRef(T,&Ref,0,1,TE); c++;
                         switch (Ref.pool) {
		         case PUNCTUATION_POOL: switch (Ref.index) {
			   case PUNC_COMMA: T   = tokGetRef(T,&Ref,0,1);
                                            c   = 0; 
                                            scp = current;
			   default:         continue;
		           }
		         }
                       }
    }
    assert(0);
  }
 done:
  rescanning(T0);
  return l;
}

int CppScope::isParent(CppScope *of) const
{
  while (of && of->up) {
    if (this == of) return 1;
    of = of->up;
  }

  return 0;
}

const CppTypeRef *CppTypeCls::findCastFn(CppTypeV **typ) const
{
  const CppTypeV *scan = castFn;

  for (; scan ; scan = scan->next) {
    const CppTypeRef *ref = scan->cRef();
    const CppTypeV   *tfn = ref->typ;
    if (tfn == typ[0]) {
      return ref;
    }
  }

  return 0;
}

const Token *CppContext::getSub(const Token *T,const Token *TL,
                                eCPRS parsing,ePcMod mods,ePCMD mode,CppTypeV *outer,
                                int nms_only)
{
  assert(outer);
  const CppTypeV   *sub     = outer,
                   *ovr     = 0,
                   *typ     = 0,
                   *it;
  const CppDecl    *dcl;
  const CppTypeRef *tst;
  int               non_lcl = 0;
  CppTypeRef       *ref     = 0,
                   *rf2;
  CppScope         *scp;
  const CppScope   *inner   = 0;

  while (sub && (tst = sub->cRef())) {
    ref = const_cast<CppTypeRef *>(tst);
    sub = tst->sub;
  }

  if (!ref) {
    assert(0);
  }

  poolRef         Ref;
  int             tmplt = 0;

  T = tokGetRef(T,&Ref,0,1);

  if (SAME_REF_PI(Ref,CPPOPS_POOL,COP_MULTIPLY)) {
    ref->set_any();
    goto done;  
  }

  if (SAME_REF_PI(Ref,CPP_POOL,CPP_TEMPLATE)) {
    tmplt = 1;
    T = tokGetRef(T,&Ref,0,1);
  }

  if (SAME_REF_PI(Ref,CPP_POOL,CPP_OPERATOR)) {
    poolRef      clsr;
    const Token *T2;
    T = tokGetRef(T2 = T,&Ref,0,1);
    switch (Ref.pool) {
    case PUNCTUATION_POOL: switch (Ref.index) {
      case PUNC_OPN_BR:
      case PUNC_OPN_SQ: T = tokGetRef(T,&clsr,0,1);
   	                assert(PUNCTUATION_POOL == clsr.pool);
                        goto as_func;
      }
    case CPPOPS_POOL:   goto as_func;
    case CPP_POOL: switch (Ref.index) {
      case CPP_DELETE:
      case CPP_NEW:
                        goto as_func;
      }
    }
    CppTypeCls *clss = outer->cTypCls(1); 
    CppTypeV   *typ[2];
    typ[1] = typ[0] = 0;
    T = prsType(rescanning(T2),T,parsing,PCX(mods|PCX_TYPE_ONLY),
                                 mode,outer,&typ[0]);
    const CppTypeV *tfn = clss->findCastFn(&typ[0]);
    assert(tfn);
    ref->sub = const_cast<CppTypeV *>(tfn)->cRef();
    goto done;
  }

 as_func:
  if (scp = ref->Scope(1)) {
    inner = scp;
    if (!(typ = scp->Type())) {
      typ = ref->typ;
    }
    if (typ && SAME_REF(Ref,typ->Name())) {
      if (ovr = typ->ovrld) {
        ref->sub = new CppTypeRef(Where(),T,ovr);
        goto done;
      }
    }
  } else {
    if (tst = ref->cRef()) {
      if (NULL_REF(tst->name)) {
        if (typ = tst->typ) {
	  inner = typ->Scope(1);
	  ovr   = typ->ovrld;
	}
      } else if (ref->isUnres()) {
	goto unres;
      } else {
	inner = current->findNameSpace(tst->name);
      }
    }
    non_lcl = 1;
  }

  if (inner) {
    poolRef Nxt;
    tokGetRef(T,&Nxt,0,1);
    rescanning(T);
    int allowed = 1;
    if (SAME_REF_PI(Nxt,PUNCTUATION_POOL,PUNC_OPN_BR) ||
        SAME_REF_PI(Nxt,PUNCTUATION_POOL,PUNC_OPN_SQ) ||
        SAME_REF_PI(Nxt,PUNCTUATION_POOL,PUNC_COMMA)  ||
        Nxt.pool == CPPOPS_POOL ||
        (parsing & CPRS_FuncArg)) {
      allowed |= 2;
    }
    if (SAME_REF_PI(Nxt,CPPOPS_POOL,COP_SCOPE)        ||
        SAME_REF_PI(Nxt,PUNCTUATION_POOL,PUNC_SCOLON)) {
      allowed |= (4|2);
    }
    int fix = 0;
    const CppTypeV *scan = typ;
    for (;; fix = 1, inner = scan->isFunc() ? 0
	                                    : scan->Scope(1)) {
      if (inner) {
	CppScope *s2 = 0;
        CppDecl  *d2 = 0;
        CppTypeV *t2 = 0;
        int sts = inner->findObj(Ref,&t2,
                                     (allowed & 2) ? &d2 : 0,
				     (allowed & 4) ? &s2 : 0);
        if (t2) {
          assert(!ref->sub);
          if (fix) {
            ref->typ = scan;
	  }
          if (ref->typ == t2) {
            int       i = 0;
            CppTypeV *cls;
            if (cls = t2->cTypCls(1,&i)) {
              assert(!i);
              t2 = cls;
	    }
	  }
          ref->sub = new CppTypeRef(Where(),T,t2);
          goto done;
        } else if (d2) {
          if (fix) {
            ref->typ = scan;
	  }
          assert(!ref->sub);
          if (ref->args) {
            ref->sub = new CppTypeRef(Where(),T,d2);
	  } else {
            ref->args = const_cast<CppDecl *>(d2);
            ref->set_dcl();
	  }
          goto done;
        } else if (s2) {
          ref->sub = new CppTypeRef(Where(),s2);
          goto done;
	}
      }
      if (ovr) {
        scan = scan->next;
        if (scan == ovr) break;
      } else {
        if (!(scan && (ovr = scan->ovrld))) break;
        scan = ovr;
      }
    } 
    if (typ && typ->cTypCls() && !typ->hasBod()) {
      // ok ?
    } else {
#     if DBGLVL > 1
      fprintf(stderr," Couldn't locate sub-item: '%s' in %s\n", 
	      strDeref(Ref),ref->describe());
#     endif
    }
    goto unres;
  } else if ((tst = outer->cRef()) && (typ = tst->typ)) {
    sub = typ->ovrld;
    while (typ) {
      if (inner = typ->Scope()) {
	CppScope *s2 = 0;
        CppDecl  *d2 = 0;
        CppTypeV *t2 = 0;
        int sts = inner->findObj(Ref,&t2,&d2,&s2);
        if (t2) {
          ref->typ = t2;
          goto fix_it;
        } else if (d2) {
          assert(!ref->args && !ref->sub);
          ref->args = d2;
          ref->set_dcl();
          goto fix_it;
        }
      }
      if (typ == tst->typ) {
        typ = sub;
      } else {
        typ = typ->next;
        if (typ == sub) break;
      }
    }
   unres:
    assert(!ref->sub);
    (rf2 = new CppTypeRef(Where(),Ref))->set_unres();
    ref->sub = rf2;
    if (tmplt) rf2->set_tmplt();
    goto done;
   fix_it:
    const_cast<CppTypeRef *>(tst)->typ = typ;
    goto done;
  } else {
    assert(0);
  }

 set_scp:
  ref->set_scp(const_cast<CppScope *>(inner),1);

 done:
  return T;
}

int CppTypeV::maybeCnstrctr(const CppTypeV *of,const CppTypeRef *ref,
                            const CppScope **ret) const
{
  if (this && of) {
    if (this == of) return 1;

    if (SAME_REF(Name(),of->Name())) {
      const CppTypeRef *rfp;
      while (rfp = of->cRef()) {
        if (!(of = rfp->sub)) {
          of = rfp->typ;
	}
      }
      const CppTypeCls *pt  = of->cTypCls();
      if (pt) {
	const CppTypeV   *tst = this;
        const CppTypeRef *rft = cRef();
        if (rft) {
          tst = rft->typ;
	}
        if (tst == of) {
          if (ret) *ret = of->Scope();
	  return 1;
	}
        CppTypeV *ovr = ovrld;
        if (ovr) do {
          if (tst == ovr) {
            if (ret) *ret = ovr->Scope();
            return 1;
	  }
          ovr = ovr->next;
	} while (ovr != ovrld);

        return -1;
      }
    }

  } else if (ref && !(ref->x.f.ref || ref->x.f.indrct)) {

    return ref->Sub()->maybeCnstrctr(ref,0,ret);

  }

  return 0;
}

int CppContext::mtchInst(CppTypeRef *rf0,int c,CppDecl **slf,const CppTypeV *othr,int flgs)
{
  int of = othr->isFunc();
 
  if ((of & XF_FN) && !(flgs & XF_FN)) return -1;

  CppDecl          *a,
                   *tst[c];
  const CppTypeRef *ref = othr->cRef();
  const CppType    *to;
  int               ct  =  0,
                    ret;
  const CppScope   *scp;

  if (ref) {
    for (a = ref->args; a ; a = a->next) {
      if (ct > c) return -1;
      tst[ct++] = a;
    }
    if (c != ct) return -1;
  } else if (scp = othr->Scope()) {
    const CppScope *ts = scp->up;
    for (a = ts->args; a ; a = a->next) {
      if (ct > c) return -1;
      tst[ct++] = a;
    }    
    if (c != ct) return -1;
  } else {
    assert(0);
  }

  ret = 0;

  while (ct--) {
    const CppTypeV   *stv = slf[ct]->typ,
                     *otv = tst[ct]->typ;
    const CppTypeRef *s = 0,
    	             *o = 0,
                     *t;
    const CppDecl    *dcl;
    eCT               oct,
	              sct;
    if (!stv) {
      const CppExpr *xpr = slf[ct]->Val(0);
      CppTypeV      *tbj = 0;
      CppDecl       *dbj = 0;
      int            sts;
      switch (xpr->opr) {
      case CXPR_MULTIPLY:
	                 // ???
      case CXPR_U32:     switch (otv->getCT()) {
	                   case PCT_QUAL_SCLR: continue;
	                 }
                         if (ret < 2) ret = 2;
                         continue;
      case CXPR_REF:     sts = current->findObj(xpr->x.ref,&tbj,&dbj,0);
                         if (dcl = dbj) {
                           stv = dcl->typ;
			 } else if (!(stv = tbj)) {
                           if (ret < 2) ret = 2;
                           continue;
			 }
	                 break;
      case CXPR_REF_DCL: stv = xpr->x.dcl->typ; break;
      case CXPR_GT:
      case CXPR_L_AND:   stv = CppType::Bool;
      }
      assert(stv);
    }
    if (stv != otv) {
      while (t = stv->cRef()) {
        if (!(stv = (s = t)->sub)) {
          if (t->x.f.dcl) {
            stv = t->args->typ;
          } else {
            stv = t->typ;
	  }
          if (!stv) {
            assert(t->x.f.unres || t->x.f.anon);
            if (ret < 2) ret = 2;
            goto end;
	  }
	}
      }
      while (t = otv->cRef()) {
        if (!(otv = (o = t)->sub)) {
	  otv = t->typ;
          if (!otv) {
            assert(t->x.f.unres || t->x.f.anon);
            if (ret < 2) ret = 2;
            goto end;
	  }
        }
      }
      if (s && o) {
        if (s->typ != o->typ) {
          if (ret < 2) ret = 2;
        } else {
        
        }
      } else if (s) {
	if (s->isTypNm()) {
          assert(otv->cTyp() || otv->isTypNm());
	} else {
          if (ret < 2) ret = 2;
	}
      } else if (o) {       
	if (o->isTypNm()) {
          assert(stv->cTyp());
	} else {
          if (ret < 2) ret = 2;
	}
      } else if (otv != stv) {
        oct = otv->getCT();
        sct = stv->getCT();
        if (oct != sct) switch (oct) {
          case PCT_SCALAR: switch (sct) {
	    case PCT_ENUM: if (ret < 1) ret = 1;
	  } 
	}
	switch (oct) {
	  case PCT_MODULE:
	  case PCT_ENTITY:
	  case PCT_ARCHITECTURE:
	  case PCT_PROCESS:
	  case PCT_STRUCT:
	  case PCT_CLASS: {
            const CppTypeCls *otc = otv->cTypCls(),
                             *stc = stv->cTypCls();
            if (stc && otc->x.as_int != stc->x.as_int) {
              if (ret < 1) ret = 1;
	    }
          } break;
          default: {
            const CppType *ot = otv->cTyp(),
                          *st = stv->cTyp();
          } break;
	}
      }
    }
   end:;
  }

  return ret;
}

void CppContext::fixTmpltTyp(CppTypeRef *ti)
{
  const CppTypeV *s = ti->typ,
                 *b[3];
  int             m[3];
  CppTypeV       *o;

  if (s && (o = s->ovrld)) {
    CppDecl *a    = ti->args;
    int      c    = 0,
             flgs = ti->isFunc();

    m[2] = m[1] = m[0] = 0;
    b[2] = b[1] = b[0] = 0;

    while (a) {c++; a = a->next;}

    CppDecl *slf[c];

    for (c = 0,a = ti->args; a ; a = a->next) {
       slf[c++] = a;
    }

    int mtch = mtchInst(ti,c,slf,s,flgs);

    if (mtch >= 0) {
      b[mtch] = s; m[mtch]++;
    }

    do {
      if (mtch = mtchInst(ti,c,slf,o,flgs)) {
        if (mtch >= 0) {
          b[mtch] = o; m[mtch]++;
	}
      }
    } while ((o = o->next) != s->ovrld);

    for (int i = 0; i < 3 ; i++) {
      if (m[i]) {
        ti->typ = b[i];
        if (m[i] > 1) {
          ti->set_ambig();
        } else {
          break;
	}
      }
    }
  }
}

void CppContext::popScope(CppScope *used,CppScope *saved)
{
  popScope();

  if (used) {    
    assert(current == used);
    current = saved;
  } else {
    assert(current == saved);
  }
}

const Token *CppContext::prsType(const Token *T,const Token *TL,
                                 eCPRS parsing,ePcMod mods,ePCMD mode,
                                 CppTypeV *prnt,
                                 CppTypeV **pTyp,CppDecl **pRetD)
{
  poolRef        Ref;
  const   Token *TS        = T,
                *T0        = T;
  eCPRS          pars_prnt = parsing;

  T = tokGetRef(T0,&Ref,0,1);
  parsing = CPRS((parsing|CPRS_Type) & ~CPRS_Obj);

  switch (Ref.pool) {
    case CPP_POOL: switch (Ref.index) {
      case CPP_CLASS:  return prsClass(T,TL,CPRS(parsing|CPRS_Class), mods,mode,pTyp);
      case CPP_UNION:  return prsClass(T,TL,CPRS(parsing|CPRS_Union), mods,mode,pTyp);
      case CPP_STRUCT: return prsClass(T,TL,CPRS(parsing|CPRS_Struct),mods,mode,pTyp);
    }
    break;
    case PRC_POOL: switch (Ref.index) {
      case PRC_ARCHITECTURE: return prsClass(T,TL,CPRS(parsing|CPRS_ArchTop),mods,mode,pTyp);
      case PRC_ENTITY:       return prsClass(T,TL,CPRS(parsing|CPRS_EntTop), mods,mode,pTyp);
      case PRC_MODULE:       return prsClass(T,TL,CPRS(parsing|CPRS_ModTop), mods,mode,pTyp);
    }
  }

  CppTypeRef       *ref     = 0;
  const CppTypeRef *sub;
  CppTypeV         *typ[2];
  CppScope         *nms;
  const CppScope   *use_scp;
  const Token      *in_br   = 0,
                   *Tf      = 0,
                   *val     = 0,
                   *TE;
  CppStmtDecl      *ds      = 0;
  poolRef           func    = NullRef;
  int               sgnd    = 0,
                    cnst    = 0,
                    ibd     = 0,
                    tmplt,
                    fix_ref,
                    reset   = 0;
  ePcMod            fm      = PCX_NONE;
  ePcMod            dm      = PCX_NONE;
  eTYPX             xtra    = TYPX_NONE;
  CppDecl          *fn_args = 0;
  CppExpr          *fn_attr = 0;

  typ[0] = pTyp ? *pTyp
                : 0,
  typ[1] = 0;

#define NEW_TYP_I(t,i) {\
      if (!ref) {ref = CppTypeRef::create(Where(),T0,t,i,&cnst,&sgnd,&xtra);}\
      Ref = NullRef;}

#define NEW_TYP(t) NEW_TYP_I(t,0)

#define RET_TYPE(t)  {if (pTyp && 0 == *pTyp && (mods & (PCX_RET_BASE_TYPE))) { \
		      *pTyp = t;\
                      if (!t->isRgstrd()) {typ[0] = addType(t); ref = 0;}}}

#define ONLY_TYPE(Tl) {if (mods & (PCX_TYPE_ONLY)) { T = rescanning(Tl);\
                                                     goto ret_typ;}}

#define FIX_SC if (sgnd || cnst) { if (!ref) { assert(typ[0]); NEW_TYP(typ[0]) }\
                                   else { ref->fix_sc(&cnst,&sgnd,&xtra); }}

  for (; !NULL_REF(Ref) ; T = tokGetRef(T0 = T,&Ref,0,1,TL)) {
   retry:
    switch (Ref.pool) {
    case PUNCTUATION_POOL: switch (Ref.index) {
      case PUNC_CLS_BR:
        assert(in_br);
	ibd--;
      fn_ptr:
        T  = tokGetRef(T0 = T,&Ref,0,1);
        fm = PCX(mods|PCX_FN_PTR);
        goto strctr;
      case PUNC_OPN_BR: {
	const Token *TC;
	if (typ[0]) { 
	  ibd++;
        } else if (NULL_REF(func)) {
          if (sgnd) {
            NEW_TYP(CppType::Int)
          } else if (closeStmt(T,TL,&TC)) {
            T = prsType(T,TC,parsing,mods,mode,prnt,typ);
            assert(T == TC);
            T = nextTok(T,0);
            continue;
	  }
	}  
       as_func:
        fm      = PCX(fm|(mods&PCX_FN_ATTR));
       strctr:
        use_scp = 0;
	if (NULL_REF(func)) {
          if ((mods & PCX_CNSTRCTR) ||
              (typ[0] && typ[0]->maybeCnstrctr(prnt,ref,&use_scp))) {
            mods    = PCX(mods & ~PCX_CNSTRCTR);
            eCT isa = typ[0]->getCT();
            switch (isa) {
            case PCT_NONE:
              typ[0] = prnt;
            case PCT_ARCHITECTURE:
            case PCT_MODULE:
            case PCT_STRUCT:
            case PCT_CLASS:
 	      func = typ[0]->Name();
              assert(0 == prnt || SAME_REF(func,prnt->Name()));
              ref  = CppTypeRef::create(Where(),T0,typ[0],0,&cnst,&sgnd);
              fm   = PCX(fm|PCX_CNSTRCTR|PCX_UNAMBIGUOUS);
	    }
	  } else if (typ[0]) {
            if (typ[0]->isFunc()) {
              const CppTypeRef *outer = typ[0]->cRef(),
	                       *sub   = outer;
              while (sub->sub) {
                outer   = sub;
                sub     = sub->sub;
                use_scp = outer->Scope(1);
	      }
              func = sub->Name();
            } 
	  }
	} else if (ref && ref->sub) {
          int         i   = 0;
          CppTypeCls *cls = ref->cTypCls(1,&i);
          if (!i && cls) {
            use_scp = cls->Scope(1);
	  }
	}
        if (NULL_REF(func)) {
          if (ref && ref->isFunc()) {
            assert(ref->isTmplt());
	    func = ref->Name();
            goto use_ref;
	  }
          in_br = T;
        } else {
	  if (typ[0]) {
	    NEW_TYP(typ[0]);
	  } else {
	   use_ref:
            typ[0] = const_cast<CppTypeV *>(ref->typ);
	  }
          fm = PCX(fm|(mods&PCX_FN_ATTR));
#         if DBGLVL > 2
	  fprintf(stderr," >F:%s\t(%d)\t((class CppTypeRef *)0x%llX)\n",strDeref(func),line,(U64)ref);
#         endif
	  if (fn_args) {
	    assert(!ref->args);
	    ref->args = fn_args;
	    fn_args   = 0;
	  }
	  if (fix_ref = (ref->args || ref->sub)) {
            ref = new CppTypeRef(Where(),T,ref);
	  }
          if (reset = (fm & PCX_CNSTRCTR)) {
            ref->set_cnstrctr();
          } else if (reset = (fm & PCX_DESTRCTR)) {
            ref->set_destrctr();
	  }
          ref->set_func(func);

          if (mods & PCX_EXTERN)    { ref->set_ext();   fm = PCX(fm|PCX_UNAMBIGUOUS);}
          if (mods & PCX_PUBLIC)    { ref->set_pblc();  fm = PCX(fm|PCX_UNAMBIGUOUS);}
          if (mods & PCX_PRIVATE)   { ref->set_prvt();  fm = PCX(fm|PCX_UNAMBIGUOUS);}
          if (mods & PCX_PROTECTED) { ref->set_prot();  fm = PCX(fm|PCX_UNAMBIGUOUS);}
          if (mods & PCX_TEMPLATE)  { ref->set_tmplt(); }
          if (mods & PCX_TYPEDEF)   { ref->set_typdf(); }
          if (fm   & PCX_VIRTUAL)   { ref->set_vrtl();  fm = PCX(fm|PCX_UNAMBIGUOUS);}
          if (fm   & PCX_INLINE)    { ref->set_inln();  fm = PCX(fm|PCX_UNAMBIGUOUS);}
          if (fm   & PCX_FN_PTR)    { ref->set_func(XF_FN_CLSD|XF_FN_PTR);
                                                        fm = PCX(fm|PCX_UNAMBIGUOUS);}
          if (fm   & PCX_STATIC)    { ref->set_sttc();  }
          if (fm   & PCX_EXPLICIT)  { ref->set_xplct(); }
	  if (parsing
                   & CPRS_ModBody)  { ref->set_mod();   }

	  CppStmtType *dcl_stmt =  0;
	  CppScope    *saved    =  0;
          int          has_bod  = -1;
          if (fm & PCX_UNAMBIGUOUS) {
            fm = PCX(fm & ~PCX_UNAMBIGUOUS);
	  } else {
            int tl = typeList(T,TL,parsing,mods,mode,ref);
            if (!tl) {
              CppTypeV  *tdcl = typ[0];
              if (!tdcl) tdcl = ref;
              assert(tdcl);
              const Token *T2 = findClose(T,TL,TOK_PUNC_CLS_BR,PUNC_OPN_BR);
              if (T2) {
                CppExpr *val = prsExpr(T,T2,parsing,mods,mode);
                CppDecl *dcl = new CppDecl(tdcl,func,val,mods);
                dcl->assgnd = 0;
                current->addDecl(dcl);
                T = T2;
                if (T < TL) {
  	  	  T = tokGetRef(T,&Ref,0,1);
		}
                goto not_fn;
	      }
	    }
	  }
          if ((fm & PCX_FN_PTR) && !(mods & PCX_TYPEDEF)) {
            if (!ref->isRgstrd()) {
              assert(SAME_REF(ref->name,func));
              ref->name = NullRef;
	    }
            ds = new CppStmtDecl(new CppDecl(ref,func));
	    current->addDecl(ds->dcl);
            addStmt(ds);
	  } else {
            dcl_stmt = new CppStmtType(ref,Where(),1);
            addStmt(dcl_stmt);
	  }
          saved = (ref->Scope() && (has_bod = ref->hasBod()))
                   ? 0
         	   : pushScope(SCP_FUNCTION,ref,const_cast<CppScope *>(use_scp),
                                            typ[0]);
          if (fm & PCX_FN_CAST) {
            CppTypeCls *cls = prnt->cTypCls(1);
            assert(cls);
            cls->addCastFn(ref);
            ref->set_rgstrd();
            ref->set_cast();
	  } else {
            if (saved && has_bod < 0) {
              T = getFnArgs(T,TL,parsing,PCX(fm & ~ PCX_FN_ATTR),mode,&ref->args,0,0);
              ref->regType(saved,const_cast<CppScope *>(use_scp));
	    } else {
              CppDecl  *args = 0; // ???
              T = getFnArgs(T,TL,parsing,PCX(fm & ~ PCX_FN_ATTR),mode,&args,0,0);
              if (!ref->isRgstrd()) {
                assert(0);
	      }
	    }
	  }
          for (;;) {
            if (T >= TL) {
              goto close_fn;
	    }
            T = tokGetRef(T0 = T,&Ref,0,1,TL);
            if (SAME_REF_PI(Ref,CPP_POOL,CPP_CONST)) {
              ref->set_cnst_t();
              if (T >= TL) break;
	      T = tokGetRef(T,&Ref,0,1,TL);
	    }
            if (SAME_REF_PI(Ref,CPP_POOL,CPP_THROW)) {
              const Token *T2 = tokGetRef(T,&Ref,0,1),
                          *TE;
              assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_BR));
              if (closeStmt(T2,TL,&TE)) {
                ref->set_throw(new CppExpr(T2,TE));
                T = nextTok(TE,0);
                if (T >= TL) break;
                T = tokGetRef(T,&Ref,0,1,TL);
  	      } else {
                assert(0);
	      }
	    } 
            if (SAME_REF_PI(Ref,GNU_POOL,GNU___ATTRIBUTE__)) {
              T = prsAttr(T,TL,&ref->attr);
              continue;
	    }
            if (SAME_REF_PI(Ref,GNU_POOL,GNU___ASM)) {
              T = prsASM(T,TL,&ref->attr);
              continue;
	    }
            if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_COLON)) {
              if (fix_ref) {
                const CppTypeV *ovr = ref->typ;
	      }
       	      assert(saved && ((fm|mods) & PCX_CNSTRCTR));
              T = prsCnstrctrs(T,TL,parsing,fm,mode,ref);
	      T = tokGetRef(T0 = T,&Ref,0,1);
              assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_CR));
              ref->set_bod(saved,const_cast<CppScope *>(use_scp),dcl_stmt);
              T = prsStmts(T,TL,CPRS(parsing|CPRS_Block),
                                PCX(fm & ~ PCX_FN_ATTR),mode);
                tokGetRef(T,&Ref,0,1); rescanning(T);
                assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_CLS_CR));
		popScope(const_cast<CppScope *>(use_scp),saved);
              typ[1] = ref;
              goto ret_typ2;
            }
            if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_CR)) {
              assert(saved);
              ref->set_bod(saved,const_cast<CppScope *>(use_scp),dcl_stmt);
              T = prsStmts(T,TL,CPRS((parsing & ~CPRS_Func)|CPRS_Block),
			   PCX(fm & ~ PCX_FN_ATTR),mode);
              tokGetRef(T,&Ref,0,1); rescanning(T);
              assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_CLS_CR));
              popScope(const_cast<CppScope *>(use_scp),saved);
              typ[1] = ref;
              goto ret_typ2;
    	    } else {
              switch (Ref.pool) {
              case PUNCTUATION_POOL:
                switch (Ref.index) {
		case PUNC_SCOLON: reset = PCX_DECL;
	       close_fn:
		                  if (T < TL) TL = T;
                case PUNC_COMMA:  if (T > TL) T = rescanning(TL);
                                  T0    = T;
                                  if (ref->x.f.func & XF_FN_OPEN) {
                                     ref->closeFunc(&xtra);
				  }
                                  goto end_func;
		case PUNC_CLS_BR: if (ibd) {
				    ibd--; 
				    continue;
				  }
    	        } break;
              case CPP_POOL:
                switch (Ref.index) {
                case CPP_NOEXCEPT:
                                  if (ref) {
                                      ref->set_noexcept();
                                  } else {
                                    assert(0);
                                    xtra = TYPX(xtra|TYPX_NOEXCEPT);
                                  }
                                  continue;
                } break;      
              case CPPOPS_POOL:
                switch (Ref.index) {
                case COP_ASSIGN:  if (ds) {
	  	                    CppExpr *xpr = prsExpr(T,TL,parsing,mods,mode);
                                    ds->dcl->setVal(0,xpr);
		                  } else {
                                    T = tokGetRef(T,&Ref,0,1);
		                    assert(SAME_REF_PI(Ref,INTEGER_POOL,INT_0));
                                    ref->set_pure(); 
		                  }
                                  goto end_func;
		} break;
              case NULL_POOL:     goto end_func;
              case GNU_POOL:
		switch (Ref.index) {
                case GNU___ASM__:
                case GNU___ASM:   T = prsASM(T,TL,&ref->attr,Ref.index == GNU___ASM ? XPRF_ASM
                                                                                    : XPRF_ASM_);
				  goto end_func;
		} 
	      }
	      assert(0);
              T  = rescanning(T0);
              break;
	    }
	  }
	 end_func:
          typ[1] = ref;
	  if (fn_attr) {
	    assert(!ref->attr);
	    ref->attr = fn_attr; fn_attr = 0;
	  }
          if (xtra & TYPX_NOEXCEPT) {
              ref->set_noexcept(); xtra = TYPX(xtra & ~TYPX_NOEXCEPT);
          }
          if (mods & PCX_TYPEDEF) {
            if (T < TL) {
              T = tokGetRef(T0 = T,&Ref,0,1);
              assert(T >= TL && SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_CLS_BR));
	    }
	  } else if (XF_FN_PTR & ref->isFunc()) {
            reset = 0;
	  }
          if (saved) {
            if (use_scp) {
              popScope();
              assert(current == use_scp);
              current = saved;
	    } else {
              assert(ref->isRgstrd());
              popScope();
              assert(current == saved);
            }
	  }
         not_fn:
	  fm   = PCX_NONE;
          func = NullRef;
          ref  = 0;
          if (reset) {
            typ[0] = 0;
	  }
        }
        if (T < TL) continue;
        T0 = TL;
        goto prs_it;
      }
      case PUNC_SCOLON:
        goto done;
      case PUNC_COMMA:
      reset:
        ref  = 0;
        typ[0] = 0;
        continue;
      case PUNC_OPN_SQ:
        NEW_TYP(typ[0])
	  T = getIndices(rescanning(T0),TL,ref);
        continue;
      case PUNC_CLS_CR:
        T = rescanning(T0);
        goto done;
      case PUNC_PERIOD:
        T = rescanning(T0);
        goto prs_it;
      case PUNC_COLON:
        if (ref) switch(ref->getCT()) {
	  case PCT_QUAL_SCLR: 
	  case PCT_SCALAR: 
	    const Token *T2;
	    closeStmt(T,TL,&T2,PUNC_COMMA);
            CppExpr *xpr = prsExpr(T,T2,parsing,mods,mode);
	    assert(xpr);
            ref->set_width(xpr);
	    T = rescanning(T2);
	    continue;    
        } else if (typ[0] && (pars_prnt & (CPRS_Class|CPRS_Struct))) { // anon field
           Ref = NullStrRef;
           goto fld_name;
        }
      default: assert(0);
    }
    case CPPOPS_POOL:
    cppops:
      if (fm & PCX_OPERATOR) {
        poolRef nxt;
        tokGetRef(T,&nxt,0,1);
        rescanning(T);
        if (SAME_REF_PI(nxt,PUNCTUATION_POOL,PUNC_OPN_BR)) {
          assert(NULL_REF(func));
	  func = Ref;
          continue;
	}
      }
      switch (Ref.index) {
      case COP_MULTIPLY:
                as_fptr: if (T0 == in_br) {
	                   for (;;) {
                             T = tokGetRef(T,&func,0,1);
			     if (!SAME_REF_PI(func,GNU_POOL,GNU___ATTRIBUTE__)) break;
			       assert(!fn_attr);
			       T = prsAttr(T,TL,&fn_attr);
			   } 
			   if (SAME_REF_PI(func,PUNCTUATION_POOL,PUNC_CLS_BR)) {
			     func = BadRef;
                             continue;
			   } else {
			     T = tokGetRef(T0 = T,&Ref,0,1,TL);
			     if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_BR)) {
			       assert(0 == fn_args);
			       T = getFnArgs(T,TL,parsing,PCX_NONE,mode,&fn_args,0,0);
                               continue;
			     }
			   }
		           goto retry;
	                 }
 	                 if (ref && !(ref->x.f.indrct || ref->x.f.ref)) {
			   assert(!typ[1]);
			   typ[0] = ref;
			 }
                         NEW_TYP(typ[0])
	                 RET_TYPE(ref)
			 ONLY_TYPE(T0)
			 assert(typ[0]);
			 ref = 0;
                         NEW_TYP(typ[0])
			 ref->x.f.indrct++;
			 typ[1] = ref;
                         continue;
      case COP_B_AND:    NEW_TYP(typ[0])
                         RET_TYPE(ref)
			 ONLY_TYPE(T0)
                         NEW_TYP(typ[0])
	                 ref->set_ref();
                         continue;
      case COP_SCOPE:  	 if (!ref) {
                           if (!typ[0]) {
                             NEW_TYP(0)
                             ref->set_scp(Root());
			   } else {
                             NEW_TYP(typ[0])
			   }
                         }
                         if (ref->typ == CppType::TypeName) {
                           CppDecl  *dcl = 0;
                           CppScope *nms = 0;
                           CppTypeV *tsb = 0;
                           int       sts = current->findObj(ref->Name(),
                                                            &tsb,&dcl,&nms);
                           switch (sts) {
                           case 1: ref->typ = tsb;
			           break;
			   case 2: assert(dcl->typ->isTypNm());
			           ref->set_dcl(dcl);
                                   break;
                           case 3: ref->typ = 0;
                                   ref->set_nmspc(nms);
				   break;
			   }
		         }
                         T = getSub(T,TL,parsing,mods,mode,ref);
                         if (ref->isAny(1)) {
                           assert(in_br);
                           T0 = in_br = T;
                           goto as_fptr;                           
			 } else {
                           const CppTypeRef *sub = ref->Sub();
			   if (sub->isFunc()) {
                             func  = sub->Name();
                             int s = sub->isStrctr();
                             if (s > 0) {
                               fm = PCX(fm|PCX_CNSTRCTR);
			     } else if (s < 0) {
                               fm = PCX(fm|PCX_DESTRCTR);
			     }			    
			   }
			 }
                         continue;
      case COP_ELLIPSIS: // assert(TS == T0 && (parsing & CPRS_Args));
	                 assert(!ref);
                         NEW_TYP(&CppTypeRef::Ellipsis)
                         continue;
      case COP_B_NEG:    if (NULL_REF(func) 
                             && (prnt || (prnt = current->Type()))) {
                           eCT isa = prnt->getCT();
                           switch (isa) {
                             case PCT_ARCHITECTURE:
                             case PCT_MODULE:
                             case PCT_STRUCT:
                             case PCT_CLASS:
 	                      Ref = prnt->Name();
                              ref = CppTypeRef::create(Where(),T0,prnt,0,
                                                            &cnst,&sgnd);
                              ref->set_destrctr();
                              fm  = PCX(fm|PCX_DESTRCTR);
                              T   = tokGetRef(T,&func,0,1);
                              assert(SAME_REF(Ref,func));
                              String d_nm("~");
                              d_nm += strDeref(func);
                              func  = strSaveStr(d_nm);
                              T     = tokGetRef(T,&Ref,0,1);
                              assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,
                                                     PUNC_OPN_BR));
                              goto strctr;
	                  }
   	                }
      case COP_LT:      tmplt = 0;
                        if (!ref) {
		           NEW_TYP(typ[0])
		           tmplt = 1;
		        }
            tmplt_ref:  if (ref->typ == CppType::TypeName) {
                          ref->typ = current->findType(ref->name);
                          assert(ref->typ);
                          tmplt = ref->typ->isTmplt();
	                }
            tmplt_sub:  sub = ref->Sub();
                        if (!(tmplt || sub->isTmplt())) {
                          assert(parsing & CPRS_Friends);
                          const_cast<CppTypeRef *>(sub)->set_frnd_arg();
			}
            tmplt_inst: TE = closeTmpltArg(&T,TL,1);
                        if (TE) {
	                  CppTypeRef *inst = const_cast<CppTypeRef *>(sub);
		          assert(!inst->args);
		          T = getTmpltInstArgs(T,TE,parsing,
                                               PCX((mods|PCX_T_INST) & ~PCX_TYPE_ONLY),
                                               mode,inst);
                          if (!typ[0]) {
			    typ[0] = ref;
			  }
			}
	                continue;
      case COP_ASSIGN:  if (mods & PCX_TYPE_ONLY) {
                          T = rescanning(T0);
                          goto prs_it;
			}
                        val = T0 = T;
	                goto fix_sc;
      default:          goto expression;
      }
    case CPP_POOL: switch (Ref.index) {
      case CPP_UNSIGNED: if (ref) ref->set_unsgnd();
	                 else     sgnd = -1;
                         continue;
      case CPP_SIGNED:   if (ref) ref->set_sgnd();
	                 else     sgnd = 1;
                         continue;
      case CPP_CONST:    if (ref) {
                           ref->set_cnst_t();
                         } else {
                           assert(!(cnst & 1));
	                   cnst |= 1;
			 } 
                         continue;
      case CPP_MUTABLE:  if (ref) {
                           ref->set_cnst_t(-1);
                         } else {
                           assert(!(cnst & 2));
	                   cnst |= 2;
			 } 
                         continue;
      case CPP_TYPENAME: 
                         T = tokGetRef(T0 = T,&Ref,0,1);
	                 if (checkName(Ref)) {
                           poolRef tnm = Ref;
                           NEW_TYP(CppType::TypeName)
			   ref->set_typnm(tnm);
			 } else {
                           assert(!typ[0]);
                           typ[0] = CppType::TypeName;
			 }
                         continue;
      case CPP_STATIC:   mods = PCX(mods | PCX_STATIC);
	                 continue;
      default:           goto as_name;
      }
    case GNU_POOL: switch (Ref.index) {
      case GNU___CONST:           if (ref) {
                                    ref->set_gnu_cnst_t();
                                  } else {
 	                            assert(!(cnst & 2));
  	                            cnst |= 2;
			          }
                                  continue;
      case GNU___ATTRIBUTE__:     assert (!ref || !ref->attr);
	                          NEW_TYP(typ[0])
            		          T0 = T = prsAttr(T,TL,&ref->attr);
                                  continue;
      case GNU___EXTENSION__:     xtra = TYPX(xtra|TYPX_GEXT);
                                  continue;
      case GNU___RESTRICT:        NEW_TYP(typ[0])
    	                          ref->set_gnu_rstrct();
                                  Ref = NullRef;
                                  continue;
      case GNU___BUILTIN_VA_LIST: assert(!ref);
                                  ref = &CppTypeRef::VaList;
                                  continue;
      case GNU___TYPEOF__:
      case GNU___TYPEOF:        { int          __ = (GNU___TYPEOF__ == Ref.index);
				  const Token *TE;
                                  T = tokGetRef(T,&Ref,0,1);
                                  assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,
                                                         PUNC_OPN_BR));
				  closeStmt(T,TL,&TE);
                                  CppExpr *xpr = prsExpr(T,TE,parsing,mods,mode);
                                  ref = new CppTypeRef(Where(),xpr); 
                                  ref->set_typof(__);
				  T = tokGetRef(TE,&Ref,0,1);
                                } continue;
      case GNU___ASM:
      case GNU___ASM__:           if (!ref) {
			            ref = CppTypeRef::create(Where(),T,typ[0]);
			          }
	                          T = prsASM(T,TL,&ref->attr,Ref.index == GNU___ASM ? XPRF_ASM
                                                                                    : XPRF_ASM_);
				  continue;
      case GNU___INLINE:          fm = PCX(fm | PCX_INLINE); // ???
                                  continue;
      default:                    reportError(S_ERROR(STS_INTERNAL),
                                              "%s unrecognised here",strDeref(Ref));
                                  assert(0);
      }
      assert(0);        
    as_name:
    default:
      int oprtr;
      if (typ[0]) {
        if ((oprtr = SAME_REF_PI(Ref,CPP_POOL,CPP_OPERATOR))
                                   || (parsing & CPRS_Func)) {
          if (oprtr) {
	   as_operator:
            T = tokGetRef(T0 = T,&func,0,1);
            poolRef arr;
            switch (func.pool) {
            case CPP_POOL: switch (func.index) {
              case CPP_DELETE: arr = CppConst::delete_arr; goto qry_arr;
              case CPP_NEW:    arr = CppConst::new_arr;
              qry_arr:
                T = tokGetRef(T0 = T,&Ref,0,1);
                if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_SQ)) {
                  T = tokGetRef(T0 = T,&Ref,0,1);
                  assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_CLS_SQ));
		  func = arr;
		} else {
                  T = rescanning(T0);
		}
	      }
	      break;
            case PUNCTUATION_POOL: switch (func.index) {
              case PUNC_OPN_BR:
              case PUNC_OPN_SQ:
                T = tokGetRef(T,&Ref,0,1);
                assert(PUNCTUATION_POOL == Ref.pool);
 	        switch (Ref.index) {
                  case PUNC_OPN_BR: assert(PUNC_CLS_BR == Ref.index);
                  case PUNC_OPN_SQ: assert(PUNC_CLS_SQ == Ref.index);
	        }
                if (!(fm & PCX_FN_CAST)) {
                  T = tokGetRef(T,&Ref,0,1);
		  assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_BR));
		}
                goto as_func;
	      }
	    }
	  } else {
            func = Ref;
	  }
          continue;
	}
        switch (typ[0]->getCT()) {
        case PCT_QUAL_SCLR: break;
        default:            goto fix_sc;
        }
      } else {
        if (SAME_REF_PI(Ref,CPP_POOL,CPP_OPERATOR)) {
          tokGetRef(T,&Ref,0,1);
          rescanning(T);
          if (CPPOPS_POOL == Ref.pool) goto as_operator;
          const Token *TE;
          if (closeStmt(T,TL,&TE,PUNC_OPN_BR,1)) {
            if (typ[0] || ref) {
              assert (T == TE);
	    } else {
              fm = PCX(fm|PCX_FN_CAST);
              T = prsType(T,TE,CPRS(parsing|CPRS_Cast),mods,mode,prnt,&typ[0]);
              T = rescanning(TE);
	    }
            goto as_operator;
	  }
          reportError(S_ERROR(STS_INTERNAL),
		      "%s unrecognised here",strDeref(Ref));
	  assert(0);
	}
      }
      if (ref) {
        if (NULL_REF(ref->name) && CppType::TypeName == ref->typ
                                && (mods & PCX_TYPEDEF)) {
          CppTypeV *typ = current->findType(Ref);
          assert(typ);
          ref->typ = typ;
          continue;
	}
        if (ref->x.f.indrct || ref->x.f.ref
                            || ref->x.f.typof
    	                    || ref->isTypNm()) goto fix_sc;
      }
     find_type_c:
      nms = current;
     find_type:
      if (typ[1] = nms->findType(Ref)) {
        if (typ[0]) {
          switch (typ[1]->name.pool) {
            case CPP_POOL: switch (typ[1]->name.index) {
              case CPP_INT: 
              case CPP_LONG: switch (typ[0]->name.pool) {
	        case CPP_POOL: switch (typ[0]->name.index) {
                  case CPP_LONG: if (typ[0]->name.index == typ[1]->name.index) {
                                   NEW_TYP(typ[0])
                                   ref->set_lng();
                                   continue;
		                 }
		                 break;
	          case CPP_INT:  typ[0] = typ[1];
		                 continue;
	        }
	      }
	    }
	  }
          goto fix_sc;
	}
	typ[0] = typ[1];
        typ[1] = 0;
	if (dm & PCX_VOLATILE) {
	  NEW_TYP(typ[0])
          ref->set_vol();
	}
	dm = PCX_NONE;
	if (0 == ref && typ[0]->isTmplt()) {
          T = tokGetRef(T0 = T,&Ref,0,1);
	  if (SAME_REF_PI(Ref,CPPOPS_POOL,COP_LT)) {
	    NEW_TYP(typ[0])
	    typ[0] = 0;
            T0     = T;
            sub    = ref;
            goto tmplt_inst;
	  }
	  T = rescanning(T0);
	}
	if (ref) {
          if (!ref->typ) {
            ref->typ = typ[0];
          } else if (ref->typ != typ[0]) {
            if (typ[0]->isFunc()) {
              assert(ref->hasBod() <= 0);
              Tf = T0;
	    }
          }
	}
        T0 = T;
        FIX_SC
	if (fm & PCX_OPERATOR) {
          assert(NULL_REF(func));
	  poolRef nxt;
	  T = tokGetRef(T,&nxt,0,1);
	  if (SAME_REF_PI(nxt,PUNCTUATION_POOL,PUNC_OPN_BR)) {
	    func.pool  = CPP_POOL;
	    func.index = CPP_OPERATOR;
	  } else {
            func = nxt;
            T    = tokGetRef(T,&nxt,0,1);
            assert(SAME_REF_PI(nxt,PUNCTUATION_POOL,PUNC_OPN_BR));
	  }
          goto as_func;
	}
        if (typ[0]->isFunc()) {
          
        } else switch (typ[0]->getCT()) {
          case PCT_ENUM:
	  case PCT_QUAL_SCLR: continue;
	}
        if (typ[0]->maybeCnstrctr(prnt,ref) && (!ref || ref->onlyInst())
	                                    && !(parsing & CPRS_Cast)) {
	  poolRef nxt;
	  assert(T0 == T);
          T = tokGetRef(T0,&nxt,0,1);
          if (SAME_REF_PI(nxt,PUNCTUATION_POOL,PUNC_OPN_BR)) {
            assert(NULL_REF(func));
            goto strctr;
	  }
          if (CPPOPS_POOL == nxt.pool) switch (nxt.index) {
          case COP_B_AND:
          case COP_MULTIPLY: Ref = nxt; goto cppops;
	  }
          T = rescanning(T0);
	} else {
          CppTypeCls *cls;
          int         indrct = 0;
          if ((cls = typ[0]->cTypCls(1,&indrct)) && !indrct) {
  	    poolRef nxt;
	    assert(T0 == T);
            T = tokGetRef(T0,&nxt,0,1);
            if (SAME_REF_PI(nxt,PUNCTUATION_POOL,PUNC_OPN_BR)) {
              assert(NULL_REF(func));
              if (isPntr(&T,TL,&func)) {
                goto fn_ptr;
	      }
              val = Tf = TS;
  	    } else if (SAME_REF_PI(nxt,CPPOPS_POOL,COP_SCOPE)) {
              assert(!ref);
              NEW_TYP(typ[0])
	      T = T0 = getSub(T,TL,parsing,mods,mode,ref);
              T = tokGetRef(T0,&nxt,0,1);
              if (SAME_REF_PI(nxt,PUNCTUATION_POOL,PUNC_OPN_BR)) {
                int c = ref->Sub()->isStrctr();
                if (c) {
                  fm = PCX(fm | c > 0 ? PCX_CNSTRCTR
			              : PCX_DESTRCTR);
                  assert(NULL_REF(func));
                  func = ref->Sub()->Name();
                  goto strctr;
		}
	      }
              T = rescanning(T0);
  	    } else {
              T = rescanning(T0);
	    }
  	  }
	}
        goto prs_it;
      } else if (nms = current->findNameSpace(Ref)) {
        T = tokGetRef(T,&Ref,0,1);
        if (SAME_REF_PI(Ref,CPPOPS_POOL,COP_SCOPE)) {
          T = tokGetRef(T,&Ref,0,1);
          goto find_type;
	}
        reportError(S_ERROR(STS_INTERNAL),
		    "%s unrecognised here",strDeref(Ref));
	assert(0);
      } else {
        switch (Ref.pool) {
	  case CPP_POOL: switch (Ref.index) {
#define SET_TYPE(C,T)\
	    case C:            assert(!(typ[0] || ref));\
	                       typ[0] = CppType::Void; continue;
            SET_TYPE(CPP_VOID,CppType::Void);
            SET_TYPE(CPP_CHAR,CppType::Char);
            SET_TYPE(CPP_BOOL,CppType::Bool);

	    case CPP_SIGNED:   assert (0 == sgnd); sgnd =  1; continue;
  	    case CPP_UNSIGNED: assert (0 == sgnd); sgnd = -1; continue;
  	    case CPP_VIRTUAL:  assert(!fm);
                               fm = PCX_VIRTUAL;              continue;
  	    case CPP_OPERATOR: fm = PCX(fm | PCX_OPERATOR);   continue;
  	    case CPP_INLINE:   fm = PCX(fm | PCX_INLINE);     continue;
  	    case CPP_EXPLICIT: fm = PCX(fm | PCX_EXPLICIT);   continue;
  	    case CPP_STATIC:   fm = PCX(fm | PCX_STATIC);     continue;
	    case CPP_PUBLIC:   mods = PCX((mods|PCX_PUBLIC)    & ~PCX_PMASK);
                               goto colon;
	    case CPP_PROTECTED:mods = PCX((mods|PCX_PROTECTED) & ~PCX_PMASK);
                               goto colon;
	    case CPP_PRIVATE:  mods = PCX((mods|PCX_PRIVATE)   & ~PCX_PMASK);
                      	      colon:
                               T = tokGetRef(T0 = T,&Ref,0,1);
                               assert(SAME_REF_PI(Ref,
					  PUNCTUATION_POOL,PUNC_COLON));
                               continue;
	    case CPP_REGISTER: dm = PCX_REGISTER; continue;
	    case CPP_VOLATILE: dm = PCX_VOLATILE; continue;
	    case CPP_ENUM:     T0 = T = prsEnum(T,TL,parsing,mods,mode,&typ[0]);
	                       goto prs_it;
	    case CPP_TEMPLATE: T0 = T = prsTemplate(T,TL,parsing,mods,mode);
                               goto done; 
	    case CPP_UNION:
	    case CPP_STRUCT:   assert(!(typ[0] || ref));
                               T0 = T = prsType(T,TL,parsing,
                                                PCX(mods|PCX_TYPE_ONLY),
                                                mode,prnt,&typ[0]);
			       FIX_SC
                               goto prs_it;
	    case CPP_FRIEND:   T0 = T = prsFriend(T,TL,parsing,mods,mode,prnt);
                               goto done;
	    case CPP_SIZEOF:   SET_TYPE(CPP_INT,CppType::Int);
                               goto prs_it;
	    case CPP_RETURN:
	    default:           reportError(S_ERROR(STS_INTERNAL),
					   "%s unrecognised here",strDeref(Ref));
	                       assert(0);
	  } ; break ;
	  case GNU_POOL: switch (Ref.index) {
	    case GNU___CONST:  assert(!(cnst & 2));
	                       cnst |= 2; 
			       continue;
	    default:           reportError(S_ERROR(STS_INTERNAL),
					   "%s unrecognised here",strDeref(Ref));
	                       assert(0);
	  } ; break ;
	}
        if (!(typ[0] || ref)) {
          if (!checkName(Ref)) {
	    if (SAME_REF_PI(Ref,CHAR_POOL,CHR_DOLLAR)) {
	      goto prs_it;
	    }
	    assert(0);
	  }
          if (!(typ[0] = current->findType(Ref))) {
            CppScope *nms = current->findNameSpace(Ref);
            if (nms) {
              ref = new CppTypeRef(Where(),Ref);
              ref->set_nmspc(nms);
              continue;
	    }
	    if (mods & PCX_T_INST) {
              T = rescanning(T0);
              goto done;
	    }
            ref = new CppTypeRef(Where(),Ref);
            ref->set_anon();
            if (xtra & TYPX_GEXT) {
                ref->set_gnu_xn(); xtra = TYPX(xtra & ~TYPX_GEXT);
            }
            current->addType(ref);
	  }
          T0  = T;
	}
       fix_sc: {
          FIX_SC 
	  const Token *T2 = T0;
          if (!val) {
            poolRef nm = Ref;
            T = tokGetRef(T0 = T,&Ref,0,1);
	    if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_BR)) {
              func = nm;
	      goto as_func;
	    }
            if (SAME_REF_PI(Ref,CPPOPS_POOL,COP_LT)) {
              tmplt = 0;
              if (typ[1]) {
                if (ref) {
		  CppType *tt = typ[1]->cTyp();
		  assert(tt && tt->x.f.tmplt);
		  typ[0] = ref;
                  ref    = 0;
		}
                NEW_TYP(typ[1]);
	        goto tmplt_sub;
	      } else if (ref) {
                if (!typ[0]) {
                  typ[0] = ref;
                  ref    = 0;
                  Ref    = nm;
		  T      = rescanning(T0);
                  goto find_type_c;
		}
	      }
  	    }
	  }
          T0 = rescanning(T2);
        } goto prs_it;
      }
    }
  }

 prs_it:
  if (!typ[1]) {
    if (ref != typ[0]) {
      typ[1] = (CppTypeV *)ref;
    }
    if (0 == typ[0] && sgnd) {
      NEW_TYP(typ[0])
      if (sgnd < 0) {
	ref->set_unsgnd();
      } else {
	ref->set_sgnd();
      }
    }
  }
  if (ref && ref->x.f.typnm && ref->sub
                            && !(mods & PCX_TYPEDEF)) {
    const CppTypeRef *sub = ref->Sub();
    const CppTypeV   *ts  = current->findType(sub->Name(),0,1);
    if (!ts) {
      CppTypeRef *lcl = new CppTypeRef(Where(),0,sub);
      lcl->name = sub->Name();
      addType(lcl);
    }
  }
  if (fm) {
    mods = PCX(mods|fm);
    fm   = PCX(fm & ~(PCX_FN_ATTR));
  }

  assert(!(sgnd || cnst || xtra || fm || fn_args));

 fld_name:
  if (typ[0] || ref) {
    T = Tf ? rescanning(Tf)
           : T0;
    if (!(mods & (PCX_TYPE_ONLY))) {
      CppTypeV *tx[2];
      tx[0] = typ[0];
      tx[1] = typ[1];
      if (val) {
        assert((Tf && 0 == ref) || ref == tx[0]
                                || (ref && ref->Sub()->isDecl()));
        const Token *TE;
        if (closeStmt(T,TL,&TE,PUNC_COMMA)) {
          CppExpr *dflt = prsExpr(T,TE,parsing,mods,mode);
          assert(dflt);
          if (ref) {
            if (ref->Sub()->isDecl()) {
              CppDecl *dcl = new CppDecl(ref,NullRef,dflt,mods);
              current->addDecl(dcl);
	    } else {
   	      ref->set_dflt(dflt);
	    }
	  }
          T = TE;
        }
      } else if (T < TL) {
        T = prsDecl(T,TL,parsing,mods,mode,prnt,tx,pRetD);
        if (tx[0]) {
          typ[1] = tx[1] ? tx[1]
	                 : tx[0];
        } else {
          typ[1] = typ[0] = 0; 
	}
      }
    }
    goto ret_typ2_reg;
  }
  goto done;

 ret_typ:
  if (sgnd || cnst || xtra) {
    NEW_TYP(typ[0])  
  }
  typ[1] = ref ? (CppTypeV *)ref
               : (CppTypeV *)typ[0];
 ret_typ2_reg:
  if (typ[1] && !typ[1]->isRgstrd()) {
    addType(typ[1]);
  }
 ret_typ2:
  if (pTyp) {
    if ((PCX_DECL == reset) && !(mods & (PCX_TYPE_ONLY))) {
      pTyp[1] = pTyp[0] = 0;
    } else if (pTyp) {
      if (typ[0]) {
        pTyp[0] = typ[0];
        pTyp[1] = typ[1];
      } else {
        pTyp[0] = typ[1];
      }
    }
  }
 done:
  return T;

 expression:
  return 0;

#undef RET_TYPE
}

void CppTypeRef::init(const Location *src,const Token *T,const CppTypeV *t)
{
  if (src) {
    source   = *src;
    source.set(T);
  }
  next        = 0;
  sub         = 0;
  typ         = t;
  index       = 0;
  attr        = 0;
  scp         = 0;
  args        = 0;
  inits       = 0;
  memset(x.as_chrs,0,sizeof(flags));
}

CppTypeRef::CppTypeRef(const Location *src,const Token *T,const CppTypeV *from)
 : CppTypeV(NullRef)
{
  init(src,T,from);
}

CppTypeRef::CppTypeRef(const Location *src,const Token *T,const CppDecl *dcl)
 : CppTypeV(NullRef)
{
  init(src,T);

  args = const_cast<CppDecl *>(dcl);
  set_dcl();
}

CppTypeRef::CppTypeRef(const Location *src,CppScope *s)
  : CppTypeV(NullRef)
{
  init(src);
  scp = s;
}

CppTypeRef::CppTypeRef(const Location *src,poolRef nm)
 : CppTypeV(nm)
{
  init(src);
}

CppTypeRef::CppTypeRef(const Location *src,CppExpr *x)
{
  init(src);
  attr = x;
}

CppTypeRef *CppTypeRef::create(const Location *src,const Token *T,
                               const CppTypeV *from,int indirect,
                               int *cnst,int *sgnd,eTYPX *xtra)
{
  const CppType *f_typ = 0;
  CppTypeRef    *ref   = 0;

  if (from) {
    const CppTypeRef *frf = 0;

    f_typ = from->cTyp();

    if (f_typ) {
      ref = new CppTypeRef(src,T,f_typ);
    } else {
      if (frf = from->cRef()) {
        ref               = new CppTypeRef(src,T,frf);
        ref->index        = frf->index->copy();
        ref->x.f.inst_ref = frf->x.f.inst | frf->x.f.inst_ref;
      }
    }
//  assert(ref && (ref->typ || indirect || ref->x.f.indrct));
    if (indirect > 0) {
      ref->x.f.indrct += indirect;
    }
  } else {
    if (!f_typ) {
      if (sgnd && *sgnd) {
	f_typ = CppType::Int;
      } else {
	f_typ = CppType::Void;
      }
    }
    ref = new CppTypeRef(src,T,f_typ);
  }

  ref->fix_sc(cnst,sgnd,xtra);

  return ref;
}

const Token *CppContext::prsDecl(const Token *T,const Token *TL,
                                 eCPRS parsing,ePcMod mods,ePCMD mode,
                                 CppTypeV *prnt,CppTypeV **pTyp,CppDecl **pRet)
{
  const Token *TE,
              *T0;
  int          idx;
  int          count = 0;
  CppTypeV    *typ[2];

  mods = PCX(mods|PCX_RET_BASE_TYPE);

  if (!(mods & PCX_TYPEDEF)) mods = PCX(mods|PCX_AUTODECL);

  if (!pTyp) {
    pTyp = typ;
    typ[1] = typ[0] = 0;
  }

  for (; T < TL ; count++) {
    idx = closeStmt(T0 = T,TL,&TE);
    T   = prsDeclItem(T,TE,parsing,mods,mode,prnt,pTyp,pRet);
    if (pTyp[1]) {
      if (pTyp[1]->isFunc() && pTyp[1]->hasBod()) {
	pTyp[1] = pTyp[0] = 0;
      } else if (!(PCX_CAST & mods)) {
	pTyp[1] = 0;
      }
    }
    if (SAME_TOK_PI(T,PUNCTUATION_POOL,PUNC_SCOLON)) {
      pTyp[0] = 0;
      return T;
    }
    if (pRet) {
      CppDecl *decl = *pRet;
      if (decl) pRet = &decl->next;
    } else {
      assert(mods & (PCX_TYPEDEF|PCX_AUTODECL));
    }

   retest:
    if (T >= TL) break;

      T = skip1(TE = T,TL);
      switch(TE->pi.pool) {
      case PUNCTUATION_POOL: switch (TE->pi.index) {
        case PUNC_CLS_CR: if (doClsFunc(pTyp)) {
                            pTyp[1] = pTyp[0] = 0;
	                  }
        case PUNC_CLS_BR:
        case PUNC_SCOLON: goto done;
        case PUNC_COMMA:  goto check;
        }
      case WHITESPACE_POOL:
        goto retest;
      default:
        rescanning(T0);
        reportError(S_ERROR(STS_SYNTAX),"Missing terminator");
        goto done;
      }

   check:
      assert(!(mods & PCX_TEMPLATE));
  }

 done:
  return T;
}

const Token *CppContext::getIndices(const Token *T,const Token *TL,CppTypeRef *ref)
{
  poolRef       Ref;
  const Token  *TX;
  CppIndex    **pIndx = &ref->index;

  assert(!*pIndx);

  while (T < TL) {
    T = tokGetRef(TX = T,&Ref,0,1);
    switch (Ref.pool) {
      case CPPOPS_POOL: switch (Ref.index) {
        case COP_ASSIGN: T = rescanning(TX); goto done;
      }
      break;
      case PUNCTUATION_POOL: switch (Ref.index) {
        case PUNC_COMMA:
        case PUNC_SCOLON: T = rescanning(TX);
        case PUNC_CLS_BR: goto done;
        case PUNC_OPN_SQ: {
          if (TX = findClose(T,TL,TOK_PUNC_CLS_SQ,PUNC_OPN_SQ)) {
            *pIndx = new CppIndex(0,prsExpr(T,TX,svd_prsng,svd_mods,svd_mode));
            pIndx  = &(*pIndx)->next;
          } else {
	    assert(SAME_TOK_PI(TL,PUNCTUATION_POOL,PUNC_CLS_SQ));
	  }
          T = tokGetRef(TX,&Ref,0,1);
          assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_CLS_SQ));
          continue;
        }
      }    
    }
    assert(0);
  }  

 done:
  return T;
}

bool CppContext::checkName(poolRef Ref) {
  bool ok = 0;
  if (FIRST_USER_POOL <= Ref.pool) ok = 1;
  if (!ok) switch (Ref.pool) {
    case GNU_POOL:
    case MATH_POOL:
    case LABELS_POOL:
    case BUILTIN_POOL:
    case SYSTEMC_POOL:
    case VERILOG_POOL:
    case COMMANDS_POOL:
      ok = 1;
  }
  return ok;
}

const Token *CppContext::prsEnum(const Token *T0,const Token *TL,
                                 eCPRS parsing,ePcMod mods,ePCMD mode,
                                 CppTypeV **pTyp) 
{
  CppType     *enm = 0;
  const Token *T   = T0;
  poolRef      Ref;

  assert(!*pTyp);
  assert(T < TL);

  source.set(T);
  T = tokGetRef(T,&Ref,0,1);
  if (checkName(Ref)) {
    CppTypeV *e = current->findType(Ref);
    if (e && (enm = e->cTyp())) {
      if (PCT_ENUM == enm->typ) {
	*pTyp = new CppTypeRef(Where(),T0,enm);
	goto done;
      }
      reportError(S_ERROR(STS_SYNTAX),"Don't recognize enum '%s'",strDeref(Ref));
    } 
    enm = new CppType(Where(),PCT_ENUM,Ref);
    T   = tokGetRef(T,&Ref,0,1);
  } else {
    enm = new CppType(Where(),PCT_ENUM);
  }
  if (mods & PCX_TYPEDEF) {
    enm->set_typdf();
  }
  *pTyp = enm;
  addStmt(new CppStmtType(enm,Where()));
  if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_CR)) {
    enm->set_bod(current);
    const Token *TE = findClose(T,TL,TOK_PUNC_CLS_CR,PUNC_OPN_CR);
    while (T < TE) {
      poolRef      name;
      const Token *T2  = T;
      CppExpr     *val = 0;
      T = tokGetRef(T,&name,0,1);
      if (!checkName(name)) {
	if (SAME_REF_PI(name,PUNCTUATION_POOL,PUNC_CLS_CR)) {
	  break;
	}
	reportError(S_ERROR(STS_SYNTAX),"Confused about '%s'",
                                        strDeref(name));
      }
      T = tokGetRef(T,&Ref,0,1);
      if (SAME_REF_PI(Ref,CPPOPS_POOL,COP_ASSIGN)) {
        if (closeStmt(T,TL,&T2,PUNC_COMMA)) {
          val = prsExpr(T,T2,parsing,mods,mode);
          T   = T2;
          if (T < TL) T = nextTok(T,0);
	} else {
          assert(0);
	}
      } else if (!(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_COMMA) ||
		   SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_CLS_CR))) {
	reportError(S_ERROR(STS_SYNTAX),"Expected ',' or '}'");
      }
      CppDecl *ei = new CppDecl(enm,name,val,mods);
      current->addDecl(ei);
    }
  } else {
    assert(0);
  }

 done:
  return T;
}

int CppDecl::sameThing(CppDecl *decl)
{
  if (is_alias) {
    return static_cast<CppDeclAlias *>(this)->alias->sameThing(decl);
  }

  if (typ == decl->typ) {
    return 1;
  }
  CppTypeRef *self = typ->cRef(),
             *othr = decl->typ->cRef();

  if (self && othr) {
    if (self->typ == othr->typ) {
      CppTypeRef::flags fs = self->x,
                        fo = othr->x; 
      if (0 == memcmp(fs.as_chrs,fo.as_chrs,sizeof(CppTypeRef::flags))) {
        CppIndex *si = self->index,
	         *oi = othr->index;
        while (si && oi) {
          if (!si->expr->sameAs(oi->expr)) goto diff;
          si = si->next;
          oi = oi->next;
	}
        if (si || oi) goto diff;
        return 1;
      }
    }
  }

 diff:
  // TBI ???
  return -1;
}

CppDecl *CppScope::addDecl(CppDecl *decl,int posn)
{
  CppDecl *have = decls.findord(decl,(listCmp)strRefCmp);
  if (have) {
    if (!have->sameThing(decl)) {
    }
  } else {
    if (decl->is_arg) {
      decl = new CppDeclAlias(decl);
    }

    decls.addord(decl,(listCmp)strRefCmp);
  }

  if (!decl->is_arg) {
    CppTypeV *cls;
    switch (sc_typ) {
    case SCP_TYPE: cls = dynamic_cast<CppTypeV *>(this);
                  if (cls) switch (cls->getCT()) {
	  	    case PCT_PROCESS:
                      if (INIT_NONE == decl->init0) {
                        decl->init0 = INIT_PROC_2;
		      }
		   }
    }

    addStmt(new CppStmtDecl(decl),posn);
  }

  return decl;
}

const Token *CppContext::prsDeclItem(const Token *T0,const Token *TL,
                                     eCPRS parsing,ePcMod mods,ePCMD mode,
                                     CppTypeV *prnt,CppTypeV **pTyp,CppDecl **pRet)
{
  poolRef      Ref    = NullRef,
               name   = NullRef,
               target = NullRef;
  const Token *in_br  = 0,
              *val    = 0;
  int          assgnd = 0;
  CppExpr     *width  = 0;
  CppTypeV    *typ[2];
  CppTypeRef  *ref    = 0;
  int          rf_md  = 0;               
  ePcMod       pcx    = PCX(mods & (PCX_FIELD|PCX_DECL|PCX_EXTENSION));
  eTYPX        xtra   = TYPX_NONE;
  bool         t_lcl  = 1;
  int          tmpltd;

  typ[1] = 0;
  if (pTyp && ((typ[0] = pTyp[1]) || 
               (typ[0] = pTyp[0]))) {
    t_lcl  = 0;
  } else {
    typ[0] = 0;
  }

#define RET_TYPE if (pTyp && !*pTyp) { if (ref) *pTyp = ref;\
                                       else if (typ[0]) *pTyp = typ[0];}

  if (pcx) {
    mods = PCX(mods & ~pcx);
    if (pcx & PCX_EXTENSION) {
        xtra = TYPX(xtra|TYPX_GEXT_PCX);
    }
  }
  const Token *TS = T0;
  const Token *T  = T0;
  const Token *Tn;
  const Token *T2;
  for (;;) {
    if ((T2 = T) >= TL || val) {
      if (typ[0] || NULL_REF(name)) break;
      goto get_typ;
    }
    T = tokGetRef(TS = T,&Ref,0,1);
    if (T > TL) {
      Ref = NullRef;
      T   = rescanning(TL);
    }
    switch (Ref.pool) {
    case WHITESPACE_POOL:
      T = skipWht(T);
      continue;
    case OPERATORS_POOL:
      assert(0);
    case PUNCTUATION_POOL: switch (Ref.index) {
      case PUNC_OPN_BR: if (typ[0]) {
                          if (pTyp && 0 == *pTyp 
                                   && (mods & PCX_RET_BASE_TYPE)) {
	                    *pTyp = typ[0];
	                  }
	                  if (!(parsing & CPRS_Type)) {
			    const Token *T2 = findClose(T,TL,TOK_PUNC_CLS_BR,PUNC_OPN_BR);
                            if (T2) {                           
                              const Token *Tc = skipWht(T2,TL,0);
                              if (!SAME_TOK_PI(Tc,PUNCTUATION_POOL,PUNC_OPN_CR)) {
                                if (!typeList(rescanning(T),
                                              T2,parsing,mods,mode)) {
                                  T = rescanning(TS);
	      	                  goto as_assign;
				}
		              }
		            }
                            rescanning(T);
			  }
	                }
  	                if (NULL_REF(name)) {
                          T = T0;
                          if (t_lcl) typ[0] = 0;     
                        } else {
                          T = Tn;
			}
                        if (!(parsing & (CPRS_Type|CPRS_FuncArg))) {
                          val = T;
			  goto decls; 
			} else {
                          if (ref) {
                            if (T > T0) {
                              typ[0] = ref;
			    } else {
			      ref->recycle();
			    }
			  }
			  rescanning(T);
                          T = prsType(T,TL,CPRS(parsing|CPRS_Func),mods,
                                      mode,prnt,typ,pRet);
			}
                        if (pTyp) {
                          pTyp[0] = typ[0];
                          pTyp[1] = typ[1];
			}
	                goto done;
      case PUNC_OPN_CR:
        assert(0); 
      case PUNC_OPN_SQ: 
        assert(typ[0]);
        if (!ref) {
	  ref = CppTypeRef::create(Where(),T2,typ[0],0,0,0,&xtra);
	}
        if (pTyp && !*pTyp) {
          *pTyp = ref;
	}
        T = getIndices(rescanning(T2),TL,ref);
        T = tokGetRef(TS = T,&Ref,0,1);
        if (SAME_REF_PI(Ref,CPPOPS_POOL,COP_ASSIGN)) {
          val    = T;
          target = name;
          assgnd = ASSGN_ARR;
	} else {
          T      = rescanning(TS);
	}
        goto decls;
      case PUNC_SCOLON:
      case PUNC_COMMA: 
        T = rescanning(T2);
        goto decls;
      case PUNC_PERIOD:
        if (!(mods & PCX_M_INST)) {
          mods = PCX(mods|PCX_M_INST);
          if (!typ[0]) goto get_typ;
        }
        // drop thru
      case PUNC_COLON: {
        const Token *Tx;
        if (closeStmt(T,TL,&Tx,PUNC_COMMA)) {
          CppExpr  *xpr = prsExpr(T,Tx,parsing,mods,mode);
          CppTypeV *self;
	  CppDecl  *dcl;
	  if (PUNC_PERIOD == Ref.index 
              && ( CXPR_CALL   == xpr->opr ||
                   CXPR_ASSIGN == xpr->opr)
              && (self = dynamic_cast<CppTypeV *>(current))
              && (PCT_MODULE       == self->getCT() ||
                  PCT_ARCHITECTURE == self->getCT())) {
	    CppExpr   *cur = xpr,
	              *lhs;
	    int        d   = 0;
	    CppIndex  *idx = 0;
	    for (;;) switch ((lhs = cur->x.hs.l)->opr) {
	      default:         assert(0);
	      case CXPR_INDEX: idx         = new CppIndex(idx,lhs->x.hs.r);
			       cur->x.hs.l = 0;
                               cur         = lhs;
                               break;
	      case CXPR_REF:   goto hv_nm;
	    }
	   hv_nm:
	    name        = lhs->x.ref;
	    lhs->opr    = CXPR_REF_DCL;
	    xpr->x.hs.l = lhs;
	    if (idx) {
	      CppTypeRef *irf = CppTypeRef::create(Where(),T,typ[0]);
	      irf->index = idx;
	      typ[0]     = irf; 
	    }
	    lhs->x.dcl = dcl = new CppDecl(typ[0],name,xpr,mods);
            switch(xpr->opr) {
            case CXPR_ASSIGN: xpr->opr = CXPR_BND_ARCH;
                              xpr      = xpr->x.hs.r;
                              break;
            case CXPR_CALL:   xpr->opr = CXPR_BIND;
                              break;
	    }
            xpr = xpr->x.hs.r;
	    xpr->MarkBind();
	    current->addDecl(dcl);
	  } else if (typ[0] || ref) {
            if (!ref) {
              ref = CppTypeRef::create(Where(),T,typ[0]);
	    }
            ref->set_width(xpr);
	    dcl = new CppDecl(ref,name,xpr,mods);
	    current->addDecl(dcl);
	  } else {
            width = xpr;
	  }
          T      = Tx;
	  typ[0] = typ[1] = 0;
	  name   = NullRef;
	}
        continue;
      }
      default: reportError(S_ERROR(STS_INTERNAL),
                           "%s unrecognised here",strDeref(Ref));
               assert(0);
    }
    case GNU_POOL:
      switch (Ref.index) {
        case GNU___BUILTIN_VA_LIST:
          assert(!ref);
          ref = &CppTypeRef::VaList;
          continue;
        case GNU___ATTRIBUTE__:
          if (!ref) {
            ref = CppTypeRef::create(Where(),T2,typ[0]);
	  }
	  T = prsAttr(T,TL,&ref->attr);
          continue;
        case GNU___ASM:
        case GNU___ASM__:
          if (!ref) {
            ref = CppTypeRef::create(Where(),T2,typ[0]);
	  }
	  T = prsASM(T,TL,&ref->attr,Ref.index == GNU___ASM ? XPRF_ASM
                                                            : XPRF_ASM_);
          continue;
        case GNU___EXTENSION__:
          if (!ref) {
            ref = CppTypeRef::create(Where(),T2,typ[0]);
	  }
	  ref->set_gnu_xn();
          continue;
        case GNU___CONST:
          if (!ref) {
            ref = CppTypeRef::create(Where(),T2,typ[0]);
	  }
	  ref->set_gnu_cnst_t();
          continue;
        case GNU___RESTRICT:
          if (!ref) {
            ref = CppTypeRef::create(Where(),T2,typ[0]);
	  }
	  ref->set_gnu_rstrct();
          Ref = NullRef;
          continue;
      }
      assert(0);
    case CPPOPS_POOL: {
      tmpltd = 0;
      switch (Ref.index) {
      case COP_MULTIPLY: if (pTyp && !*pTyp)   goto get_typ_n;
                         if (!(ref || typ[0])) goto get_typ;
                         if (ref) ref->x.f.indrct++;
	                 else     ref = CppTypeRef::create(Where(),T2,typ[0],1);
                         T0 = T;
                         continue;
      case COP_B_AND:    if (pTyp && !*pTyp) { get_typ_n:
                           assert(!typ[0] && !ref && !NULL_REF(name));
                                               goto get_typ;
	                 }
                         if (!(ref || typ[0])) goto get_typ;
                         if (ref) ref->set_ref();
			 else     ref = CppTypeRef::create(Where(),T2,typ[0],-1);
                         T0 = T;
                         continue;
      case COP_ASSIGN:   assgnd = ASSGN_EQ;
      as_assign:         val    = T;
	                 target = name;                         
                         name   = NullRef;
                         goto get_typ_b;
      case COP_ELLIPSIS: assert(TS == T0 && (parsing & CPRS_Args));
	                 assert(!(typ[0] || ref));
                         typ[0] = CppTypeRef::create(Where(),T2,&CppTypeRef::Ellipsis);
                         name   = Ref;
                         Ref    = NullRef;
                         continue;     
      case COP_GT:       assert(mods & PCX_T_INST);
	                 if (!NULL_REF(name)) goto get_typ;
			 continue;
      case COP_LT:       if (0 == typ[0]) {
                           goto get_typ;
			 }
	                 if (!NULL_REF(name)) {
                           if (ref) {
                             if (!typ[1]) {
                               typ[1] = ref;
                               ref    = 0;
                               rf_md |= 2;
                               T      = rescanning(Tn);
                               continue;
			     }
			   } else {
                             tmpltd = -1;
   			     goto scp_tmplt;
			   }
                           assert(0);
                           goto decls;
                         }
                         tmpltd=1;
                         goto scp_tmplt;
      case COP_SCOPE:    if (ref) {
                           if (0 == rf_md && !NULL_REF(name)) {
                             assert(!typ[1]);
                             typ[1] = ref;
                             rf_md |= 1;
			     CppScope *nms = 0;
                             CppTypeV *t[2];
                             t[1] = t[0] = 0;
                             switch (current->findObj(name,t,0,&nms)) {
                             default: assert(0);
                             case 1:  ref = new CppTypeRef(Where(),TS,t[0]);
                                      break;
                             case 3:  ref = new CppTypeRef(Where(),nms);
                                      ref->set_nmspc();
			     }
                             goto get_sub;
			   }
			   assert(!tmpltd);
                           goto get_sub;
			 }
              scp_tmplt: if (typ[0]) {
                           if (!NULL_REF(name)) {
                             CppTypeV *t = current->findType(name);
                             assert(t);
			     ref    = new CppTypeRef(Where(),T,t);
                             rf_md |= 1;
                             goto get_sub;
			   }
			 } else if (NULL_REF(name)) {
                           ref = new CppTypeRef(Where(),Root());
                           goto get_sub;  
			 } else {
                           CppScope *nms = 0;
                           CppTypeV *t[2];
                           switch (current->findObj(name,t,0,&nms)) {
                           default: assert(0);
                           case 1:  ref = new CppTypeRef(Where(),TS,t[0]);
                                    goto get_sub;
                           case 3:  ref = new CppTypeRef(Where(),nms);
                                    ref->set_nmspc();
                                    goto get_sub;
			   }
                           name = NullRef;
			 }
 	                 ref = new CppTypeRef(Where(),T,typ[0]);
                        get_sub:
                         if (tmpltd) {
 	                   const CppTypeRef *sub = ref->Sub();
                           if (!sub->isTmplt()) {
                             const CppScope *scp = sub->Scope(1);
                             
			   }
                           const Token *TE;
                           if (closeStmt(T,TL,&TE,0,2)) {
                             T = getTmpltInstArgs(T,TE,parsing,PCX(mods|PCX_T_INST),mode,ref);
                             continue;
			   }
                           assert(0);
			 } else {
                           T = getSub(T,TL,parsing,mods,mode,ref);
                           if (ref->isFunc()) {
                             typ[0] = ref;
                             if (ref->Sub()->isStrctr()) {
                               mods = PCX(mods|PCX_CNSTRCTR);
			     }
                             CppScope *svd = current;
                             T  = prsType(TS = T,TL,CPRS(parsing|CPRS_Func),mods,
                                          mode,prnt,typ,pRet);
                             assert(svd == current);
                             if (T > TL) {
                               T = rescanning(TL);
			     }
			     goto done;
			   }
		  	 }
	                 continue;
      case COP_L_NEG:    goto as_expr;
      }
      assert(0);
    }
    case CPP_POOL: switch (Ref.index) {
      case CPP_THIS:     assert(TS == T0);
                         val = T0;
                         goto decls;
      case CPP_ENUM:     typ[0] = 0;
	                 T = prsEnum(T,TL,parsing,mods,mode,typ);
                         if (typ[0] && NULL_REF(typ[0]->Name())) typ[0] = 0;
                         continue;
      case CPP_INLINE:   typ[0] = 0;
      case CPP_OPERATOR: T = prsType(rescanning(T2),
                                     TL,CPRS(parsing|CPRS_Func),mods,
                                     mode,prnt,typ,pRet);
 	                 if (pTyp) {
			   if (!*pTyp) {
                             pTyp[0] = typ[0];
			   }
                           pTyp[1] = typ[1];
			 }
			 goto done;
      case CPP_STRUCT:
      case CPP_CLASS:
      case CPP_UNION: 
      case CPP_VOID:     assert(!typ[0]); 
                         T = prsType(rescanning(T2),
                                     TL,CPRS(parsing & ~CPRS_Obj),mods,mode,0,typ,pRet);
                         if (typ[0] && NULL_REF(typ[0]->Name())) typ[0] = 0;
                         continue;
      case CPP_TEMPLATE: T = prsTemplate(T,TL,parsing,mods,mode,prnt);
                         continue;
      case CPP_DEFAULT:  assgnd = ASSGN_DFLT|ASSGN_EQ;
        	         goto as_assign;
      default:           if (CPP_VOLATILE >= Ref.index) goto get_typ_b;
	                 if (CPP_NEW      <= Ref.index) goto as_expr;
      }
      assert(0);
     as_expr:
      val = TS;
      goto decls;
    }
  get_typ:
    if (! NULL_REF(name)) {
     get_typ_b:
      if (typ[0]) {
        if (! NULL_REF(name)) {
	  continue;
	}
      } else {
       get_typ_f:
        if (pTyp && !*pTyp) {
           T  = prsType(rescanning(T0),
                        TL,parsing,PCX(mods|PCX_TYPE_ONLY|PCX_RET_BASE_TYPE),
                        mode,prnt,typ,pRet);
           if (tryClsFunc(&T,TL,typ)) {
             pTyp[1] = pTyp[0] = 0;
             goto done;
	   }
           *pTyp = typ[0];
	} else {
           T  = prsType(rescanning(T0),
                        TL,parsing,PCX(mods|PCX_TYPE_ONLY),
                        mode,prnt,typ,pRet);
           if (!typ[0]) {
             val = T0;
             goto decls;
	   }
	}
      }
      name = NullRef;
      continue;
    }
    if (checkName(Ref)) {
      name = Ref;
      Tn   = TS;
    }
  }

 decls:
  if (ref && 0 == rf_md) {
    typ[1] = ref;
  }
  if (!typ[1]) {
    typ[1] = typ[0];
  }
  if (mods & PCX_TYPEDEF) {
    goto as_typedef;
  } else {
    CppExpr *xpr  = 0;
    eINIT    ini0 = (assgnd & ASSGN_DFLT) ? INIT_INST_D 
                                          : INIT_NONE;
    int      vlc  = 0;
    if (val) {
     as_val:
      if (ASSGN_ARR == assgnd) {
	T = tokGetRef(rescanning(val),&Ref,0,1);
        if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_CR)
            && closeStmt(T,TL,&T2)) {
          xpr = prsExpr(T,T2,parsing,mods,mode);
          T   = tokGetRef(T2,&Ref,0,1);
          assert(T == TL);
	} else {
          xpr = prsExpr(rescanning(val),TL,parsing,mods,mode);
	}
      } else {
        xpr = prsExpr(rescanning(val,0),TL,parsing,mods,mode);
      }
      T    = TL;
      name = target;
    } else if (typ[0]) {
      switch (typ[0]->getCT()) {
        case PCT_PROCESS: CppTypeCls *sct = dynamic_cast<CppTypeCls *>(current);
	                  if (sct && (PCT_MODULE       == sct->getCT() ||
		 		      PCT_ARCHITECTURE == sct->getCT())) {
                            xpr  = new CppExpr(CXPR_REF,CppConst::this_);
	                    ini0 = INIT_PROC_0;
                            vlc  = 2;
	                  }
                          break;
      }
    }
    if (parsing & CPRS_Args) {
      if (NULL_REF(name)) {
	if (!(parsing & CPRS_TypeExprOK)) {
  	  //fprintf(stderr," -plug-");
	  assert(!val);
          goto done;
	} else if (mods & PCX_T_INST) {
  	  //fprintf(stderr," -type ok-");
          if (typ[0] || typ[1]) {
            goto typ_ok;
	  }
          if (!val) {
 	    val = T0;
            goto as_val;
          }
          assert(xpr);
          goto as_name;
        } 
        if (!(typ[0] && typ[0]->isTypename(&name))) {
          int i = current->AnonCount();
          if (i >= 0) {
            name.pool  = INTEGER_POOL;
            name.index = INT_tokens[current->AnonCount()].id.pi.index;
	  } else {
            name.pool  = CPPOPS_POOL;
            name.index = COP_QMARK;
	  }
	}
      }
      if (typ[0]) {
       typ_ok:
        goto as_name;
      }
    } else {
      if (ref) {
	if (!ref->isRgstrd()) {
	  current->addType(ref);
	}
	if (pTyp && !pTyp[1]) {
	  pTyp[1] = ref;
	}
      }      
      if (xpr || !NULL_REF(name)) {
       as_name:
	CppDecl *dcl;
        switch (vlc) {
	  case 3:  dcl = new CppDecl3(typ[1],name,xpr,0,0,mods); break;
	  case 2:  dcl = new CppDecl2(typ[1],name,xpr,0,  mods); break;
	  default: dcl = new CppDecl (typ[1],name,xpr,    mods); break;
	}
        dcl->setAssgnd(assgnd);
        dcl->init0 = ini0;
        if (INIT_INST_D ==ini0) {
          String sflg("_dfltd_");
          sflg += strDeref(name);
	  poolRef fnm = strSaveStr(sflg);
          CppDecl *dcl_f = new CppDecl (CppType::BitField,fnm,
                                        CppExpr::Int0,PCX_PRIVATE);
	  dcl_f->init0   = INIT_INT_0;
          if (CXPR_NEW == xpr->opr) {
	    dcl->init1   = INIT_SIG_DLT;
	  }
	  addDecl(dcl_f,-1);
	}
	if (pRet) {
	  *pRet = dcl;
	} else {
	  addDecl(dcl);
          if (pTyp) {
            pTyp[1] = new CppTypeRef(Where(),0,dcl);
	  }
	}
      }
    }
  }
  goto done;

 finish:
  if (mods & PCX_TYPEDEF) {
   as_typedef:
    assert (!val);
    if (!NULL_REF(name)) {
      assert(checkName(name));
      if (ref) {
        if (NULL_REF(ref->name)) goto reuse;
        if (!ref->isRgstrd()) {
          current->addType(ref);
        }
        typ[0] = ref;
      } else {
        if (typ[0] && NULL_REF(typ[0]->name) && !typ[0]->isRgstrd()) {
          typ[0]->name = name;
          typ[0]->setTypdf();
          current->addType(typ[0]);
          goto done;
        }
      }
      ref = CppTypeRef::create(Where(),T2,typ[0]);
      reuse:
      ref->name = name;
      ref->set_typdf();
      current->addType(ref);
    }
  }
 done:
  return T;
}

const Token *CppContext::prsUsing(const Token *T,const Token *TL,
                                  eCPRS parsing,ePcMod mods,ePCMD mode)
{
  int           nms_only;
  poolRef       Ref;
  CppStmtUsing *use  = 0;

  T = tokGetRef(T,&Ref,0,1);

  if (nms_only = SAME_REF_PI(Ref,CPP_POOL,CPP_NAMESPACE)) {

    T = tokGetRef(T,&Ref,0,1);

  }

  if (checkName(Ref)) {
    CppScope *nms = 0;
    if (!nms_only) {
      CppTypeV *typ = current->findType(Ref);
      if (typ) {
        use = new CppStmtUsing(Where(),typ);
        goto ok;
      } 
    }
    nms = current->findNameSpace(Ref);
    if (nms) {
      use                    = new CppStmtUsing(Where(),nms);
      use->CppScopeRef::next = current->nm_spcs;
      current->nm_spcs       = use;
    } else {
      use  = new CppStmtUsing(Where(),Ref);
    }
   ok:
    addStmt(use);
    T = tokGetRef(T,&Ref,0,1);
  } else {
    assert(!nms_only && SAME_REF_PI(Ref,CPPOPS_POOL,COP_SCOPE));
  }

  int         s;
  CppTypeRef *top = 0;
  while ((s = SAME_REF_PI(Ref,CPPOPS_POOL,COP_SCOPE)) ||
              SAME_REF_PI(Ref,CPPOPS_POOL,COP_LT)) {
    if (!use) {
      use = new CppStmtUsing(Where(),&root);
    }
    if (!top) {
      if (use->obj) {
        top = use->obj->cRef();
        if (!top) {
          use->obj = top = new CppTypeRef(Where(),T,use->obj); 
        }
      } else {
        reportError(S_ERROR(STS_SYNTAX),"Couldn't find scope '%s'",
                                        strDeref(use->pend));
      }
    }
    if (s) {
      T = getSub(T,TL,parsing,mods,mode,top,nms_only);
    } else {
      const Token *TE = closeTmpltArg(&T,TL,1);
      T  = getTmpltInstArgs(T,TE,parsing,mods,mode,top);
    }
    T = tokGetRef(T,&Ref,0,1);
  }

  assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_SCOLON)); 

  assert(use);

  use->nmspc = nms_only;

  return T;
}

const CppStmt *CppNamedScope::lastStmt() const
{
  CppStmtNmdBlk *c_next = ctrl_seg ? ctrl_seg->next_seg
                                   : 0;

  return c_next ? c_next->start
                : 0;
}

const CppStmt *CppNamedScope::firstStmt() const
{
  return ctrl_seg ? ctrl_seg->start
                  : 0;
}

const Token *CppContext::prsNameSpace(const Token *T,const Token *TL,
                                      eCPRS parsing,ePcMod mods,ePCMD mode)
{
   poolRef Ref;

   T = tokGetRef(T,&Ref,0,1);

   if (checkName(Ref)) {
     CppNamedScope *nms   = dynamic_cast<CppNamedScope *>(
                                         current->findNameSpace(Ref));
     CppScope      *saved;
     CppStmt      **cont;
     if (nms) {
       saved = pushDown(nms);
       cont  = nms->p_stmt;
     } else {
       saved = addScope(nms = new CppNamedScope(Ref));
       cont  = &nms->stmt0;
     }
     CppStmtNmdBlk *stmt = new CppStmtNmdBlk(Where(),nms),
                   *last = nms->ctrl_seg;
     if (last) {
       last->next_seg = stmt;
     }

     addStmt(stmt);

     T = tokGetRef(T,&Ref,0,1);

     if (SAME_REF_PI(Ref,GNU_POOL,GNU___ATTRIBUTE__)) {
       T = skipToken(T,TL,TOK_PUNC_OPN_BR);
       T = skipToken(T,TL,TOK_PUNC_OPN_BR);
       T = tokGetRef(T,&Ref,0,1);
       if (SAME_REF_PI(Ref,GNU_POOL,GNU___VISIBILITY__)) {
	 T = skipToken(T,TL,TOK_PUNC_OPN_BR);
	 T = skipToken(T,TL,TOK_QUT_DOUBLE);
	 T = tokGetRef(T,&Ref,0,1);
	 switch (Ref.pool) {
	 case VERILOG_POOL: switch (Ref.index) {
	   case VER_DEFAULT: stmt->vis = VIS_DEFAULT; goto ok;
	   }
           break;
	 case CPP_POOL: switch (Ref.index) {
	   case CPP_DEFAULT: stmt->vis = VIS_DEFAULT; goto ok;
	   }
	 }
	 assert(0);
       ok:
	 T = skipToken(T,TL,TOK_QUT_DOUBLE);
         T = skipToken(T,TL,TOK_PUNC_CLS_BR);
       } else if (SAME_REF_PI(Ref,GNU_POOL,GNU___ABI_TAG__)) {
	 T = skipToken(T,TL,TOK_PUNC_OPN_BR);
	 T = skipToken(T,TL,TOK_QUT_DOUBLE);
	 T = tokGetRef(T,&Ref,0,1);
         stmt->tag[0] = Ref;
         stmt->tag0   = TAG_ABI;
       ok2:
	 T = skipToken(T,TL,TOK_QUT_DOUBLE);
         T = skipToken(T,TL,TOK_PUNC_CLS_BR);           
       }
       T = skipToken(T,TL,TOK_PUNC_CLS_BR);
       T = skipToken(T,TL,TOK_PUNC_CLS_BR);

       T = tokGetRef(T,&Ref,0,1);
     }

     T = prsPCtok(T,TL,parsing,mods,mode,0);
 
     stmt->start   = *cont;
     nms->ctrl_seg = stmt;

     current = saved;

     T = tokGetRef(T,&Ref,0,1);
  }

   return T;
}

const Token *CppContext::prsFriend(const Token *T,const Token *TL,
                       eCPRS parsing,ePcMod mods,ePCMD mode,
                       CppTypeV *pCls)
{
  parsing = CPRS(parsing|CPRS_Friends);

  if (!pCls) {
    pCls = current->Type();
  }

  CppTypeCls *typ = pCls->cTypCls(1);

  assert(typ);

  CppTypeV **ref = typ->frnds;

  if (ref) {
    int n = 0;
    while (ref[++n]);
    REALLOC2(typ->frnds,n+2,CppTypeV *);
    ref    = &typ->frnds[n];
    *ref   = 0;
    ref[1] = 0;
  } else {
    ref = typ->frnds = CALLOC2(2,CppTypeV *);
  }

  const Token *TE;
  closeStmt(T,TL,&TE);
  const Token *T2 = prsType(T,TE,parsing,mods,mode,0,ref);
  if (ref[1]) {
    ref[0] = ref[1];
    ref[1] = 0;
  }
  T = T2;
  tryClsFunc(&T,TL,ref);

  return T; 
}

const Token *CppContext::prsClsObj(const Token *T0,const Token *TL,
                                   eCPRS parsing,ePcMod mods,ePCMD mode,
                                   CppTypeV **pTyp)
{
  const Token *T = nextTok(T0,0),
              *T2;
  bool         ok;

  closeStmt(T,TL,&T2);

  parsing =  CPRS((parsing & ~CPRS_Obj)|CPRS_Type);

  switch (T0->pi.pool) {
    case CPP_POOL:
    switch (T0->pi.index) {
      case CPP_CLASS:  ok = prsClass(T,T2,CPRS(parsing|CPRS_Class),mods,mode,pTyp);  break;
      case CPP_UNION:  ok = prsClass(T,T2,CPRS(parsing|CPRS_Union),mods,mode,pTyp);  break;
      case CPP_STRUCT: ok = prsClass(T,T2,CPRS(parsing|CPRS_Struct),mods,mode,pTyp); break;
    }
    case PRC_POOL:
    switch (T0->pi.index) {
      case PRC_ENTITY:       ok = prsClass(T,T2,CPRS(parsing|CPRS_Entity),mods,mode,pTyp); break;
      case PRC_ARCHITECTURE: ok = prsClass(T,T2,CPRS(parsing|CPRS_Architecture),mods,mode,pTyp); break;
      case PRC_MODULE:       ok = prsClass(T,T2,CPRS(parsing|CPRS_Module),mods,mode,pTyp); break;
    }
  }

  T = T2;

  return skip1(T,TL);
}

const Token *CppContext::skip1(const Token *T,const Token *TL)
{
  if (T < TL) {
    T = nextTok(T,0);
  }

  return T;
}

const Token *CppContext::skipWht(const Token *T,const Token *TL,int nl)
{
  do {
    if ((TL && T >= TL)
            || WHITESPACE_POOL != T->pi.pool) return T;
    int l;
    switch (T->pi.index) {
    case WHT_NL:;
      /* drop thru */
    default:
      T = nextTok(T,0,nl);
    }
  } while (WHITESPACE_POOL == T->pi.pool);

  return T;  
}

const Token *CppContext::prsPCtok(const Token *T,const Token *TL,eCPRS parsing,ePcMod mods,ePCMD mode,CppTypeV *prnt)
{
  int          l,
               old_line = line;
  tokPoolRef  *tpr;
  poolRef      Ref;
  const Token *T0       = T;
  const Token *T2;
  const Token *Ts       = 0;
  CppTypeV    *typ[2];
  eTYPX        xtra     = TYPX_NONE;

  typ[1] = typ[0] = 0;

  while (T < TL /* || (T = popStack()) */) {
    assert(Ts < T);
    Ts = T;
    switch (T->pi.pool) {
      case WHITESPACE_POOL:
        T = skipWht(T);
        if (T0 == Ts) {
          T0 = T;
	}
        continue;
      case PUNCTUATION_POOL: 
        switch (T->pi.index) {
	  case PUNC_CLS_CR: 
            goto done;
	  case PUNC_OPN_CR: {
            T2 = findClose(T,TL,TOK_PUNC_CLS_CR);
            if (! T2) {
              reportError(S_ERROR(STS_SYNTAX),"Couldn't find closing '}'");
              T2 = TL;
	    }
            blk_dpth++;
            T = prsPCtok(nextTok(T,0),T2,parsing,mods,mode,typ[0]);
            T = nextTok(T,0);
            blk_dpth--;
            continue;
          } break;
          case PUNC_OPN_BR: {
	    T = prsType(rescanning(T0),
                        TL,parsing,PCX(mods|PCX_AUTODECL),mode,prnt,typ);
            goto end_fn_chk;
          } break;
          case PUNC_OPN_SQ:
	    T  = prsDeclItem(rescanning(T0),
                             TL,parsing,mods,mode,prnt,typ);
            T0 = T;
            continue;
 	  case PUNC_COMMA:
 	  case PUNC_COLON:
 	  case PUNC_PERIOD:
            T0 = T = prsDecl(rescanning(T0),
                             TL,parsing,PCX(mods|PCX_AUTODECL),
                             mode,prnt,typ);
            continue;
  	  case PUNC_SCOLON: // see exit below
            if (T != T0) {
              T0  = prsDeclItem(T0,T,parsing,mods,mode,prnt);
	      assert(T == T0);
	    }
            typ[1] = typ[0] = 0;
            T0  = T = skip1(T,TL);
            continue;
	}
        assert(0);
        break;
      case PRP_POOL:
        switch (T->pi.index) {
	  case PRP_HASH:
            T0 = T = prsLnDrctv(nextTok(T,0));
            continue;
	}
        assert(0);
        break;
      case COMMENTS_POOL:
        switch (T->pi.index) {
 	  case CMT_EOL:      T = skip2nl(T); continue;
	  case CMT_STRTCMNT: T = skip2ec(T); continue;
  	}
        break;
      case PRC_POOL:
        switch (T->pi.index) {
	case PRC_ENTITY:
        case PRC_MODULE:
        case PRC_ARCHITECTURE: 
            assert(!typ[0]);
	    source.set(T);
            T0 = T = prsClsObj(T,TL,parsing,mods,mode,typ);
            typ[0] = 0;
            continue;
	case PRC_PROCESS:
	    source.set(T);
            T0 = T = prsProcess(nextTok(T,0),TL,parsing,mods,mode,typ);
            continue;
	case PRC_FORK: // fork() call
	    goto add;
        default:
            assert(0);
	}
      case GNU_POOL:
        switch (T->pi.index) {
        case GNU___TYPEOF:
        case GNU___INLINE:      
        case GNU___CONST:      
        case GNU___ATTRIBUTE__: goto as_type;
	case GNU___EXTENSION__: xtra = TYPX(xtra|TYPX_GEXT);
	                        T    = nextTok(T,0);
                                continue;
  	default:                assert(0);
	}
      case CPP_POOL:
        switch (T->pi.index) {
	  case CPP_TYPEDEF:
            assert(!(mods&PCX_TYPEDEF));
            if (closeStmt(T,TL,&T2)) {
              *dbg  += "T";
              T      = prsTypeDef(nextTok(T,1),T2,CPRS(parsing|CPRS_Type),mods,mode);
              T2     = skip1(T2,TL);
              T0 = T = rescanning(T2);
              *dbg  -= "T";
	    } else {
              reportError(S_ERROR(STS_SYNTAX),"Couldn't find end of typedef");
	    }
            continue;
 	  case CPP_ENUM: {
            T               = nextTok(T,0);
            CppTypeV *te[2] = {0,0};
            T               = prsEnum(T,TL,parsing,mods,mode,te);
            if (!NULL_REF(te[0]->Name())) {
              current->addType(te[0]);
	    } else if (!(mods & PCX_TYPEDEF)) {
              current->addTypeAnon(te[0]);
	    }
            T0 = T = prsDeclItem(T,TL,parsing,mods,mode,prnt,te);
          } continue;
  	  case CPP_UNION:
          case CPP_STRUCT: 
  	  case CPP_CLASS: {
 	      ePcMod mx = mods;
              if (xtra & TYPX_GEXT) {mx   = PCX(mx|PCX_EXTENSION);
                                     xtra = TYPX(xtra &~ TYPX_GEXT);}
              T0 = T = prsClsObj(T,TL,parsing,mx,mode);
	    }
            if (mods & PCX_TEMPLATE) goto done;
            continue;
	  case CPP_FRIEND:
            T0 = T = prsFriend(nextTok(T,0),TL,parsing,mods,mode,prnt);
            continue;
  	  case CPP_NAMESPACE:
            T0 = T = prsNameSpace(nextTok(T,0),TL,parsing,mods,mode);
            continue;
  	  case CPP_USING:
            T0 = T = prsUsing(nextTok(T,0),TL,parsing,mods,mode);
            continue;
	  case CPP_EXTERN: {
            ePcMod       add    = PCX_EXTERN,
                         rm     = PCX_NONE;
            eCPRS        sub    = CPRS_Decl;
            const Token *TE     = 0;
            CppScope    *saved  = 0;
            const char  *x      = "X";
            T  = nextTok(T,0);
            T2 = tokGetRef(T,&Ref,0,1);
            if (SAME_REF_PI(Ref,QUOTES_POOL,QUT_DOUBLE)) {
              T2      = tokGetRef(T2,&Ref,0,1);
              x = strDeref(Ref);
              if      (0 == strcmp("C",  x)) { add = PCX_C;   rm = PCX_CPP; }
              else if (0 == strcmp("C++",x)) { add = PCX_CPP; rm = PCX_C; }
              else                           { assert(0); }
              sub = CPRS_Block;
              T   = tokGetRef(T2,&Ref,0,1);
              assert(SAME_REF_PI(Ref,QUOTES_POOL,QUT_DOUBLE));
              T2  = tokGetRef(T, &Ref,0,1);
              if (SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_OPN_CR)) {
                TE = findClose(T2,TL,TOK_PUNC_CLS_CR,PUNC_OPN_CR);
                assert(TE);
                eSCPT st = SCP_C;
                switch (add) { case PCX_CPP: st = SCP_CPP; break; }
		if (!(mods & add)) {
                  CppStmtBlock *xc = new CppStmtBlock(Where(),
                                                      CSTMT_Block,st);
                  addStmt(xc);
	          saved = pushDown(xc);
	        }
	      } else {              
                T2 = rescanning(T);
	      }
	    } else {
	      T2 = rescanning(T);
	    }
            *dbg += x;
            if (0 == TE) {
              closeStmt(T2,TL,&TE);
              rescanning(T2);
	    }
            T = prsPCtok(T2,TE,sub,PCX((mods|add) & ~ rm),mode);
            if (saved) {
              popScope();
              assert(saved == current);
	    }
            if (T == TE && T < TL) T = nextTok(T,1);
            T0 = T;
            *dbg -= x;
          } continue;
          case CPP_CONST:
          case CPP_VOID:
  	  case CPP_BOOL:
  	  case CPP_CHAR:
          case CPP_SHORT:
          case CPP_INT:
          case CPP_LONG:
          case CPP_FLOAT:
          case CPP_SIGNED:
          case CPP_MUTABLE:
          case CPP_UNSIGNED:
	  case CPP_DOUBLE:
  	  case CPP_OPERATOR:
  	  case CPP_VOLATILE:
  	  case CPP_TYPENAME:
  	  case CPP_DEFAULT:
	   as_type:
            typ[1] = typ[0] = 0;
            T = prsType(rescanning(T0),
                        TL,parsing,PCX(mods|PCX_AUTODECL),mode,prnt,typ);
            goto end_fn_chk;
	  case CPP_STATIC: 
            typ[1] = typ[0] = 0;
	    T = nextTok(T,0);
            T = prsType(T,TL,parsing,PCX(mods|PCX_STATIC),mode,0,typ);
	   end_fn_chk:
	    mods = PCX(mods & ~(PCX_INLINE|PCX_VIRTUAL));
            if (doClsFunc(typ)) {
	      SKIP_CLS_CR(T,TL) 
	    }
            T0 = T;
            if ((typ[0] && (typ[0]->isStrctr() ||
                            (XF_FN_CLSD & typ[0]->isFunc()))) ||
                (typ[1] && (XF_FN & typ[1]->isFunc()))) {
              goto clr_typs;
	    }
            continue; 
  	  case CPP_PRIVATE:   mods = PCX((mods & ~PCX_PMASK) | PCX_PRIVATE);
                              goto skp_c;
          case CPP_PROTECTED: mods = PCX((mods & ~PCX_PMASK) | PCX_PROTECTED);
                              goto skp_c;
	  case CPP_PUBLIC:    mods = PCX((mods & ~PCX_PMASK) | PCX_PUBLIC);
	   skp_c: T0 = T = tokGetRef(nextTok(T,0),&Ref,0,1);
	          assert(SAME_REF_PI(Ref,PUNCTUATION_POOL,PUNC_COLON));
                  goto clr_typs;
  	  case CPP_VIRTUAL:   mods = PCX(mods|PCX_VIRTUAL);  goto new_stmt;
   	  case CPP_INLINE:    mods = PCX(mods|PCX_INLINE);   goto new_stmt;
   	  case CPP_EXPLICIT:  mods = PCX(mods|PCX_EXPLICIT);
	   new_stmt:
            assert(T0 == Ts);
	    T0 = T = nextTok(T,0);
	   clr_typs:
            typ[1] = typ[0] = 0;
            continue;
  	  case CPP_TEMPLATE:
            T0 = T = prsTemplate(nextTok(T,0),TL,parsing,mods,mode,prnt);
            continue;
	  default:        
            assert(0);
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
      case TOK_POOL_SPEC:
        T = rgstrPool(tpr = (tokPoolRef *)T);
        l = PoolLoad (tpr->name.buff,tpr->xt.extra,fmode);
        assert(l == tpr->xt.extra);
        continue;
      case TOK_FULL_REF:
        T = addToken(T,1,0,0);
        T = addToken(T,1,0,0);
        T = addToken(T,0,0,0);
        continue;
    }
  add:
    T = addToken(T,0);
  }

  for (T = T0; T  < TL ; T0 = nextTok(T,0)) {
    T = prsDeclItem(rescanning(T0),TL,parsing,mods,mode,prnt,typ);
    if (T >= TL) break; 
  }

  assert(T <= TL);

 done:
  return T;
}

static const char *dumpPP;

int pcEfile(int *argc,const char ***argv,void *var,int pls,int mns)
{
  dumpPP = argEquals(argc,argv);

  return 0;
}

int CppContext::prsPCtok(const char *src,File &tok,File *deps,ePCMD md)
{
  poolRef      Ref,
               old_curr = curr_file;
  const Token *T;
  Filename     tmp;

  cmode = md;
  mtk   = new MappedFile(tok);

  // set_deps(src,tok.str(),deps->str());

  curr_file = strSaveStr(tok2src(tok,tmp));
  line      = 1;

  if (!mtk->base()) Exit(mtk->Error());

  file_TL = &(T = file_T0 = (Token *)mtk->base())[(mtk->Size())/sizeof(Token)];

  T = prsPCtok(T,file_TL,CPRS_Top,PCX_CPP,md);

  if (line_map) {
    line_map[1] = file_T0;
  }

  if (dumpPP) {
    FILE *out = fopen(dumpPP,"w");
    dumpLnMap(out,file_TL);
    fclose(out);
  }

  return STS_NORMAL;  
}


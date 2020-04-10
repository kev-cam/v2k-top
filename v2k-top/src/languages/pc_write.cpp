/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * pc_write_cpp_rcsid() {return "$Id: pc_write.cpp,v 1.51 2012/10/22 08:52:03 cvs Exp $";}

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
#define  NEED_PP
#define  NEED_CPP
#define  NEED_PRC
#define  NEED_CPPOPS
#define  NEED_GNU
#define  NEED_SYSTEMC
#include "tokpool.h"
#define PC_POOL_DESC
#include "parser.h"
#define PARC_OBJ
#include "dump.h"
#include "pc.h"

void *CppStrmData::UserFn2(Stream *out,void *vsync)
{
  if (vsync) {
    Location *sync = (Location *)vsync;
    loc            = *sync;
    return  const_cast<char *>("\n");
  } else {
    if (*nl == '\n') loc.file_num[0]++;
  }
  return const_cast<char *>(nl);
}

const char *CppStrmData::CsdNL(Stream &out,const Location *loc) 
{
  UsrStrmData *usd = out.UsrData();
  CppStrmData *csd = usd ? dynamic_cast<CppStrmData *>(usd) 
                         : 0;
  if (csd) {
    const char *nl = (const char *)csd->UserFn2(&out,
						const_cast<Location *>(loc));
    return nl;
  }
  return "/* ??? */";
}

const Token *Location::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  int l = file_num[0];

  if (l <= 0) return T0;

  if (flgs & WFLG_NO_LOCATION) {
    NL_SYNC(out,this);
  } else {
    UsrStrmData *usd = out.UsrData();
    CppStrmData *csd = usd ? dynamic_cast<CppStrmData *>(usd)
                           : 0;
 
    if (csd && SAME_REF(file_name,csd->loc.file_name)) {
      while (l > csd->loc.file_num[0]) {
        out.printf("\n");
        // out.printf("/* %d */",file_num[0]); 
	csd->loc.file_num[0]++;
      }
    } else {
      if (!(csd && csd->dmp && (csd->dmp->Flags() & DMP_DEBUG))) {
        if (file_num[0]) {
          out.printf("\n# %d \"%s\"",file_num[0],strDeref(file_name)); 

          for (int i = 1;
               i < sizeof(file_num)/sizeof(*file_num) && file_num[i] >= 0;
               i++) {
            out.printf(" %d",file_num[i]);
          }
	}
        out.printf("%s",NL_SYNC(out,this));
      }
    }
  }

  return tok;
}

void *CppStrmData::UserFn1(Stream *out,void *VT)
{
  const Token *T = (const Token *)VT;

  if (cpp) {
    if (out->Open(cpp,"rw")) {
      cpp = 0;
    } else {
      ExitMsg(S_ERROR(Error()),"can't open %s",cpp->str());
    }
  }

  if (curr) {
    if (flgs & WFLG_SYSC2PC) {
      out->printf("#define V2K_DO_INC_SYSC\n"
                  "#include \"systemc.h\"\n"
                  "#include \"parc.h\"\n"
                  "using namespace parc;\n");
    } else {
      tokReconInMem(curr,(T-curr)-1,out->Fp());

      T    = curr;
      curr = 0;
    }

    UsrStrmData *usd = out->UsrData();
    CppStrmData *csd = usd ? dynamic_cast<CppStrmData *>(usd)
                           : 0;
 
    if (csd && csd->dmp && (csd->dmp->Flags() & DMP_DEBUG)) {
      out->Flush();
      String fnm(strDeref(csd->dbg_loc.file_name));
      int fd = out->Fd();
      Stream rescan;
      rescan.Open(fnm,"r");
      csd->dbg_loc.file_num[0] = 3;
      int ch;
      String line;
      while (rescan.gets(&line) >= 0) {
        csd->dbg_loc.file_num[0]++;
      }
      rescan.Close();
      envExpand(&fnm);
      out->printf("\n# %d \"%s\"",csd->dbg_loc.file_num[0],fnm.str());
      out->printf("%s",NL_SYNC((*out),&csd->dbg_loc));
    }
  }

  return (void *)T;
}

CppStrmData::CppStrmData(File *out,const Token *T, dump_ctrl *pd,eWFLG f) 
 : nl(""),
   cpp(out),
   curr(T),
   dmp(pd),
   flgs(f)
{
  if (dmp && (dmp->Flags() & DMP_DEBUG)) {
    nl = "\n";
  }
}

int CppContext::reWrite(File &out,eWFLG flgs)
{
  dump_ctrl *dmp = DumpList;

  for (; dmp ; dmp = dmp->next) {
    // if (DMPR_CPP == dmp->Dmpr()) {
      break;
    // }
  }

  const Token *T        = file_T0;
  CppScope    *scan     = &root;
  Stream      *out_strm = new Stream;
  CppStrmData *data     = new CppStrmData(&out,T,dmp,flgs);

  out_strm->setUsrData(data);

  if (dmp && (dmp->Flags() & DMP_DEBUG)) {
    data->dbg_loc.file_name   = strSaveStr(out);
    data->dbg_loc.file_num[0] = 1;
  }

  while (scan) {
    switch (scan->sc_typ) {
      case SCP_NAMESPACE: scan->clrCtrl();                   break;
      default:            T = scan->Write(*out_strm,T,flgs); break;
    }
    scan = scan->next;
    if (scan == &root) break;
  }

  if (data->curr) out_strm->UserFn2(const_cast<Token *>(T));

  delete out_strm;

  return 0;
}

const Token *CppScope::Write(Stream &out,const Token *T0,eWFLG flgs,const CppTypeV *cls) const
{
  const CppStmt     *s_scan = firstStmt(),
                    *e_scan = lastStmt();
  const Token       *T      = T0;
  int                cr     = (SCP_C == sc_typ),
                     br     = 0,
                     jn     = 0;
  eWFLG              cx     = WFLG(flgs & (WFLG_CONSTRUCT|WFLG_DESTRUCT)),
                     frk    = WFLG(flgs & WFLG_FORK);
  const CppStmtFork *fp     = 0;
  const CppTypeScp  *proc   = 0;

  flgs = WFLG(flgs & ~(cx|WFLG_FORK));

  if (frk & WFLG_FORK0) {
     flgs = WFLG(flgs | WFLG_FORK1);
     const CppScope *up = this;
     do {
       up = up->up;
       if (0 == jn && (fp = dynamic_cast<const CppStmtFork *>(up))) {
	 jn = fp->active;
       }
     } while (!(proc = dynamic_cast<const CppTypeScp *>(up)));
     fp   = dynamic_cast<const CppStmtFork *>(this);
  }

  if ((s_scan && (flgs & WFLG_ADD_CR))
              || (flgs & WFLG_FUNCTION)) cr |= 2; 

  if (cr & 1) out.printf("extern \"C\"");
  if (cr & 3) out.printf(" {\%s",NL(out));

  if (cx & WFLG_CONSTRUCT) {
    if (up->hasSttcRngs()) {
      out.printf("__slices = 0;%s",NL(out));
    }
    for (int i = INIT_CNSTRCT_1; i <= INIT_CNSTRCT_L; i++) {
      cls->WriteInitX(out,"_m",(eINIT)i);
    }
  }

  for (; s_scan != e_scan; s_scan = s_scan->next, br++) {
    if (fp) {
      int r0 = fp->label(br);
      if (0 == br) {
	int c = fp->count;
	out.printf("_W = initFork(_wait,%d,%d",jn,c);
	for (int i = 0; i < c ; i++) {
	  int rp = fp->label(i);
	  out.printf(",%d,resume%d",proc->fmap[rp],rp,NL(out));
	}
	out.printf(");%s",NL(out));
      }
      out.printf("%sL%d: ",NL(out),r0);
      fp->active = proc->fmap[r0];
    }
    if (flgs & WFLG_PROC_ANY) {
      const CppStmtLabel *start = dynamic_cast<const CppStmtLabel *>(s_scan);
      if (start) {
	if (WFLG_PROC_STTC & flgs) {
          return T;
	}
        flgs = WFLG(flgs & ~WFLG_PROC_ANY);
        out.printf("L0: ");
        continue;
      } else if (WFLG_PROC_DYN & flgs) {
        continue;
      }
    }
    T = s_scan->Write(out,T,flgs);
    if (fp) {
      out.printf("%sgoto J%d;",NL(out),fp->label(0));
    }
  }

  if (fp) {
    out.printf("%sJ%d: if (!joinFork(_wait,%d)) return;",
	                         NL(out),fp->label(0),jn);
  }

  if (cx & WFLG_DESTRUCT) {
    cls->WriteInitX(out,"_m",INIT_DSTRCT);
  }

  if (cr & 3) out.printf("%s}%s",NL(out),NL(out));

  return T;
}

void printCString(Stream &out,const char *str,const char *qut = "\"") 
{
  int c;
  if (qut) out.printf("%s",qut);
  while (c =*str++) switch (c) {
  case '\n': out.printf("\\n"); break;
  default:   out.printf("%c",c);
  }
  if (qut) out.printf("%s",qut);
}

const Token *WriteTok(Stream &out,const Token *T,const Token *TL)
{
  while (T < TL) {
    out.printf("%s",tokDeref(T++));
  }

  return TL;
}

bool CppExpr::isAddr() const
{
  assert(CXPR_REF_DCL == opr);

  CppTypeRef *ref = x.dcl->typ->cRef();

  if (ref) {
    return ref->x.f.indrct || ref->index;
  }

  return 0;
}

const Token *CppExpr::WritePortBind(Stream &out,const Token *T0,eWFLG flgs,const CppExpr *item) const
{
  switch (item->opr) {
  default:           assert(0);
  case CXPR_COMMA:   WritePortBind(out,T0,flgs,item->x.hs.l);
                     WritePortBind(out,T0,flgs,item->x.hs.r);
		     break;
  case CXPR_BND_SIG: out.printf("bind("); 
                     x.hs.l->Write(out,0);
		     out.printf(",typeid("); 
		     x.hs.l->Write(out,0);
		     out.printf("->");
		     item->x.hs.l->Write(out,0,flgs);
		     out.printf("),(void_signal **)&("); 
		     x.hs.l->Write(out,0);
		     out.printf("->");
		     item->x.hs.l->Write(out,0,flgs);
		     out.printf("),typeid(");
		     item->x.hs.r->Write(out,0,flgs);
		     out.printf("),(void_signal *)(");
		     item->x.hs.r->Write(out,0,flgs);
		     out.printf("))");
		     out.printf("; "); 
  }

  return T0;
}

void doDisambiguate(Stream &out,const CppScope *ownr)
{
  if (ownr) {
    if (ownr->up) {
      doDisambiguate(out,ownr->up);
    }

    if (SCP_NAMESPACE == ownr->sc_typ) {
      out.printf("%s::",strDeref(ownr->Name()));
    } 
  }
}

void WriteArgs(Stream &out,const CppDecl *arg,eWFLG flgs = WFLG_NONE)
{
  while (arg) {
    arg->Write(out,0,WFLG(flgs|WFLG_MINIMAL|WFLG_NO_CLOSE|WFLG_NO_PPP));
    if (!(arg = arg->next)) break;
      out.printf(",");
  }
}

const Token *CppExpr::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  if (!this) return T0;

  const char    *op   = "",
                *spcr = " ";
  const CppExpr *lhs,
                *rhs  = this;

  static void *lbl[] = { &&uni_post, &&bi_op, &&uni_op };
 redo:
  switch (rhs->opr) {
#define CPPOPS_TDF
#define CPPOP(s,n,u,...) case CXPR_##n: op = #s ; goto *lbl[u+1];
#include "cppops.inc"
  case CXPR_BECOMES: out.printf("parc::Becomes(");
                     x.hs.l->Write(out,0); out.printf(",");
                     x.hs.r->Write(out,0); out.printf(")");
                     goto done;
  case CXPR_BEFORE:
  case CXPR_AT:      op = "";           goto bi_op;
  case CXPR_LABEL:
  case CXPR_COLON:   op = ":" ;         goto bi_op;
  case CXPR_COMMA:   op = "," ;         goto bi_op;
  case CXPR_LIST:    op = " " ;         goto bi_op;
  case CXPR_OBJ:     op = "." ;         goto bi_op;
  case CXPR_TYPNM:   op = " typename "; goto bi_op;
  case CXPR_INST_INDEX:
                     rhs = rhs->x.hs.r; spcr = "__";             goto redo;
  case CXPR_POINT:   rhs = rhs->x.hs.r; spcr = "0.";             goto redo;
  case CXPR_I32:     out.printf("%s%d ",spcr,rhs->x_i32());      goto done;
  case CXPR_U32:     out.printf("%s%u ",spcr,rhs->x_u32());      goto done;
  case CXPR_I64:     out.printf(" %L ",rhs->x.i64);              goto done;
  case CXPR_U64:     out.printf(" %uL ",rhs->x.u64);             goto done;
  case CXPR_DBL:     out.printf(" %g ",rhs->x.d);                goto done;
  case CXPR_TOK:     WriteTok(out,rhs->x.tk.l,rhs->x.tk.r);      goto done;
  case CXPR_REF:     out.printf("%s",strDeref(rhs->x.ref));      goto done;
  case CXPR_STRS:    printCString(out,strDeref(rhs->x.ref),"'"); goto done;
  case CXPR_STRD:    printCString(out,strDeref(rhs->x.ref));     goto done;
  case CXPR_STRCAT:  rhs->x.hs.l->Write(out,0); out.printf(" "); 
                     rhs->x.hs.r->Write(out,0);
                     goto done;
  case CXPR_REF_DCL: out.printf("%s",strDeref(rhs->x.dcl->name));
                     goto done;
  case CXPR_BLOCK:   out.printf("{");
                     rhs->x.scp->Write(out,0);
                     out.printf("}");
                     goto done;
  case CXPR_SC_AT:   out.printf("@"); 
  case CXPR_SUB:     out.printf("("); 
                     rhs->x.hs.l->Write(out,0); out.printf(")");
                     goto done;
  case CXPR_T_INST:  rhs->x.hs.l->Write(out,0); out.printf("<"); 
                     rhs->x.hs.r->Write(out,0); out.printf("> ");
                     goto done;
  case CXPR_CALL_MM: out.printf("_m->");
  case CXPR_CALL:    rhs->x.hs.l->Write(out,0);
                     out.printf("(");
                     if (rhs->x.hs.r) rhs->x.hs.r->Write(out,0,flgs); out.printf(")"); 
                     goto done;
  case CXPR_CALL2:   rhs->x.hs.l->Write(out,0);
                     out.printf(" ");
                     if (x.hs.r) x.hs.r->Write(out,0);
                     goto done;
  case CXPR_COERCE:  rhs->x.hs.l->Write(out,0,WFLG_COERCE);
                     out.printf("(");
                     rhs->x.hs.r->Write(out,0);
                     out.printf(")");
                     goto done;
  case CXPR_BND_ARCH:out.printf("_create(this,\"");
                     rhs->x.hs.l->Write(out,0);
                     out.printf("\",\"");
                     rhs = x.hs.r;
                     rhs->x.hs.l->Write(out,0);
                     out.printf("\");%s\t\t",NL(out));
		     rhs = rhs->x.hs.r;
                     goto bind;                     
  case CXPR_BIND:    out.printf("_create(this,\"");
                     rhs->x.hs.l->Write(out,0);
                     out.printf("\",0);%s\t\t",NL(out));
		     rhs = x.hs.r;
                    bind:
		     WritePortBind(out,0,flgs,rhs);
		    finish:
                     goto done;
  case CXPR_CAST:    out.printf("(");
                     rhs->x.hs.l->Write(out,0);
                     op = ")" ;
                     goto uni_op;
  case CXPR_DEREF:   op = "*" ;
                     goto uni_op;
  case CXPR_CAST2:   out.printf("("); 
                     rhs->x.hs.l->Write(out,0); out.printf(")");
                     rhs->x.hs.r->Write(out,0);
                     goto done;
  case CXPR_CALLI:   out.printf("("); 
                     rhs->x.hs.l->Write(out,0); out.printf(")");
                     rhs->x.hs.r->Write(out,0);
                     goto done;
  case CXPR_INDEX:   rhs->x.hs.l->Write(out,0);
                     out.printf("[");
                     rhs->x.hs.r->Write(out,0);
                     out.printf("]");
                     goto done;
  case CXPR_CMNT:    out.printf("/* ");
                     rhs->x.hs.l->Write(out,0);
                     out.printf("  */");
                     goto done;
  case CXPR_INST_SC: rhs->x.hs.r->Write(out,0);
                     goto done;
  case CXPR_TYPE:    rhs->x.typ->Write(out,0,WFLG(flgs|WFLG_NO_CLOSE|WFLG_MINIMAL));
                     goto done;
  case CXPR_TYPE_NAME_D:
                     doDisambiguate(out,rhs->x.typ->Scope(2));
  case CXPR_TYPE_NAME:
                     out.printf("%s",strDeref(rhs->x.typ->Name()));
		     if (rhs->x.typ->isTmplt()) {
		       const CppTypeRef *ref = rhs->x.typ->cRef();
		       if (ref) {
			 int i = 0;
			 while (ref->x.f.indrct || ref->x.f.inst_ref) {
			   i += ref->x.f.indrct;  ref = ref->typ->cRef();
			 } 
			 out.printf("<");
			 WriteArgs(out,ref->args,WFLG(flgs|WFLG_NO_TYPE|WFLG_INST_T));
			 out.printf("> ");
		       }
		     }
                     goto done;
  case CXPR_POSEDGE: out.printf("posedge ");  rhs->x.hs.l->Write(out,0); goto done;
  case CXPR_NEGEDGE: out.printf("negedge ");  rhs->x.hs.l->Write(out,0); goto done;
  case CXPR_NEW:     out.printf("new ");      rhs->x.hs.l->Write(out,0); goto done;
  case CXPR_SIZEOF:  out.printf("sizeof ");   rhs->x.hs.l->Write(out,0); goto done;
  case CXPR_RANGE:   op = "dynSlice("; goto slice;
  case CXPR_STTC_RNG:if (flgs & WFLG_PROC_STTC) { 
                       op = "sttcSlice(&_m->__slices,";
		     } else {
                       op = "sttcSlice(&__slices,";
		     }
  slice:             rhs->x.hs.l->Write(out,0);
                     out.printf("%s%s",x.hs.l->isAddr() ? "->"
				                        : ".", op);
                     rhs->x.hs.r->Write(out,0); 
                     out.printf(")");         goto done;
  default:           assert(0);
  }
  return T0;

 bi_op:
  if (rhs->x.hs.l) rhs->x.hs.l->Write(out);
 uni_op:
  out.printf("%s",op);
  if (rhs->x.hs.r) rhs->x.hs.r->Write(out);
  return T0;

 uni_post:
  if (rhs->x.hs.r) rhs->x.hs.r->Write(out);
  out.printf("%s",op);
 done:
  return T0;
}

static const char *PPPstring[] = {"",        "public:", "protected:", "private:"};
enum                             {PPP_NONE=0,PPP_PUBLIC,PPP_PROTECTED,PPP_PRIVATE};
static int         PubPrtPrv   = -1;

void WritePPP(Stream &out,int ppp)
{
  if (PubPrtPrv >= 0 && PubPrtPrv != ppp) {
    if (PubPrtPrv = ppp) {
      out.printf("%s%s",PPPstring[ppp],NL(out));
    }
  }
}

const Token *CppDecl::Write(Stream &out,const Token *T0,
                            eWFLG flgs,const char *sep) const
{
  const char *nm_s;

  if (INIT_INIT_C == init0) {
    return T0;
  }

  if ((flgs & WFLG_CLASS) && !(flgs & WFLG_NO_PPP)) {
    int ppp = 0;

         if (pblc) ppp = PPP_PUBLIC; 
    else if (prot) ppp = PPP_PROTECTED; 
    else if (prvt) ppp = PPP_PRIVATE; 

    WritePPP(out,ppp);
  }

  if (NULL_REF(name)) {
    if (Val()) {
      Val()->Write(out);
      goto cls;
    } else {
      T0 = typ->Write(out,T0,WFLG(flgs|WFLG_NO_CLOSE|WFLG_MINIMAL));
    }
  } else {
    nm_s = strDeref(name);
    if (typ && ! (flgs & WFLG_NO_TYPE)) {
      const char **name_p = &nm_s;
      T0 = typ->Write(out,T0,WFLG(flgs|WFLG_NO_CLOSE|WFLG_MINIMAL),name_p);
    }
    if (nm_s) {
      if (dot) out.printf(".");
      out.printf("%s",nm_s);
    }
  }
  if (Val()) {
    static const char *assgn_opn[] = {"(","=","={"};
    static const char *assgn_cls[] = {")","", "}"};
                              
    int skp = (flgs & WFLG_CLASS) && (INIT_CONST != init0);
    out.printf(skp ? " /* = "  // */
                   : assgn_opn[assgnd]);
    Val()->Write(out,0,flgs);
    out.printf(skp ? " */"
                   : assgn_cls[assgnd]);
  }
 cls:
  if (!(flgs & WFLG_NO_CLOSE)) {
    out.printf("%s%s",sep,NL(out));
  }
  return T0;
}

void WriteArgs(Stream &out,const CppScope *scp,eWFLG flgs,int up)
{
  while (scp && up-- > 0) {
    scp = scp->up;
  }

  assert((flgs & WFLG_DEBUG) || (scp && SCP_TEMPLATE == scp->sc_typ));

  if (scp) {
    WriteArgs(out,scp->args,WFLG_TEMPLATE);
  }
}

void CppTypeRef::WriteAliases(Stream &out,const char *ptr_nm) const
{
  if (x.f.pblc) {
    typ->WriteAliases(out,ptr_nm);
  }
}

void CppTypeCls::WriteAliases(Stream &out,const char *ptr_nm) const
{
  const CppScope *scp = Scope();

  CppDecl *dcl = scp->decls.first();

  for (; dcl ; dcl = dcl->next) {
    const char *dcl_nm = strDeref(dcl->name);
    if ('_' != *dcl_nm || 0 == strncmp("_sz_",dcl_nm,4)) {
      out.printf ("\t\ttypeof(%s->%s) &%s = %s->%s;%s",
                              ptr_nm,dcl_nm,dcl_nm,ptr_nm,dcl_nm,NL(out));
    }
  }

  switch (typ) {
  case PCT_ARCHITECTURE:
    if (inherits) inherits->WriteAliases(out,ptr_nm);
  }
}

int CppTypeCls::WriteInit0(Stream &out,const char *ptr_nm,const char *punc) const
{
  const CppScope *scp    = Scope();
  int             uninit = 0;

  CppDecl *dcl = scp->decls.first();
  for (; dcl ; dcl = dcl->next) {
    eINIT ini = dcl->init0;
    if (dcl->val[0]) switch (INIT_PHASE & ini) {
    case INIT_CNSTRCT_0:
      const CppTypeRef *typ    = dcl->typ->cRef();
      if (typ && typ->index) {
	uninit++;
	continue;
      }
      const char       *dcl_nm = strDeref(dcl->name);
      if (punc) {
        out.printf ("%s",punc);
        punc = 0;
      } else {
	out.printf (",%s\t\t",NL(out));
      }
      out.printf ("%s(",dcl_nm);
      switch (ini & ~INIT_PHASE) {
      case INIT_IF_NULL: 
      case INIT_NULL:    out.printf ("0");        break;
      default:           dcl->val[0]->Write(out); break;
      }
      out.printf (")");
    }
  }

  return uninit;
}

void CppTypeCls::WriteInitX(Stream &out,const char *ptr_nm,
                            eINIT phase,eWFLG flgs) const
{
  const CppScope *scp = Scope();

  CppDecl *dcl = scp->decls.first();
  for (; dcl ; dcl = dcl->next) {
    if (NULL_REF(dcl->name)) continue;
    const char       *dcl_nm = strDeref(dcl->name);
    const CppTypeRef *typ    = dcl->typ->cRef();
    int               d      = 0;

    if (typ) for (CppIndex *scn = typ->index; scn ; scn = scn->next) ++d;

    for (int i = 0 ; i <= 2 ; i++) { 
      CppExpr *val = dcl->Val(i);
      eINIT    ini = dcl->Init(i);
      if (val && phase == (INIT_PHASE & ini)) {
        int op = ini & ~INIT_PHASE;
        if (INIT_CNSTRCT_0 == phase && op != INIT_IF_NULL) {
          continue;
	} 
        const char *clsr   = ";";
        CppExpr    *xpr    = dcl->Val(i);

#       define PRNT_INDCS {for (int k=0; k++ < d; out.printf ("[__%d]",k));}

	out.printf ("\t\t");
	if (d) {
	  int dbg = (flgs & WFLG_DEBUG),
              j   = 1;
	  for (CppIndex *scn = typ->index; scn ; scn = scn->next,j++) {
	    if (INIT_INST_M == ini) {
	      if (1 ==j) {
		out.printf ("memset(%s,0,sizeof(%s));",dcl_nm,dcl_nm);
	      }
	      out.printf ("for(int __%d=0,__last%d=",j,j);
	      scn->expr->Write(out,0,flgs);
	      out.printf (";__%d >= 0 && __%d < __last%d;__%d++){%s",j,j,j,j,NL_DBG(out));
	    }
	  }	  
	  if (INIT_BIND != op) { 
	    typ->typ->Write(out,0,WFLG_MINIMAL);
	    out.printf (" **__%s = &(%s",dcl_nm,dcl_nm); PRNT_INDCS
  	    out.printf ("),* %s = 0;%s", dcl_nm,NL_DBG(out));
	  }
	}
	if (INIT_INIT_C == ini) {
	  dcl->typ->Write(out,0,WFLG(flgs|WFLG_NO_CLOSE|WFLG_MINIMAL|WFLG_NO_NL));
	}
        switch (op) {
        case INIT_BIND:     out.printf ("%s->bind_children();",dcl_nm);
	                    if (d) {
			      out.printf (" %s->setIdx(%d",dcl_nm,d);
			      for (int j = d; j > 0; j--) out.printf (",__%d",j);
			      out.printf (");");
			      for (int j = d; j > 0; j--) out.printf ("}");
                            }
                            goto nl;
        case INIT_DLT:      out.printf ("delete %s",   dcl_nm);        goto nl;
        case INIT_DLT_ARR:  out.printf ("delete [] %s",dcl_nm);        goto nl;
        case INIT_DLT_DFLT: out.printf ("if (_dfltd_%s) {delete %s; %s = 0", 
                                              dcl_nm, dcl_nm, dcl_nm);
	                    clsr = ";}"; goto nl;
        case INIT_VAL:      break;
        case INIT_ASSIGN:   out.printf ("%s = ",dcl_nm);               break;
        case INIT_MTHD0:    out.printf ("%s.",  dcl_nm);               break;
        case INIT_MTHD1:    out.printf ("%s->", dcl_nm);               break;
        case INIT_MTHDA:    if (d) {
			      out.printf ("*__%s = ",dcl_nm);
			    }
			    out.printf ("%s = %s->",dcl_nm,dcl_nm);    break;
        case INIT_MTHD_MB:  out.printf ("for (int _i=0; _i < _MB; _i++) {%s[_i].",
                                                    dcl_nm); 
                            clsr = ";}"; break;
        case INIT_IF_NULL:  out.printf ("if (0 == %s) {_dfltd_%s = 1; %s = ", 
                                              dcl_nm, dcl_nm, dcl_nm);
	                    clsr = ";}"; break;
        case INIT_CLL_INIT: dcl->Val(i)->x.hs.l->Write(out);
                            out.printf ("(%s,",dcl_nm);
                            xpr = dcl->Val(i)->x.hs.r;
	                    clsr = ");"; break;
        }
        xpr->Write(out,0,flgs);
       nl:
        out.printf ("%s%s",clsr,NL(out));
      }
    }
  }
}

static const CppType *CurrEnum;

const Token *CppType::Write(Stream &out,const Token *T0,eWFLG flgs,const char **dcl_nm) const
{
  const Token *T = Where()->Tok();
  if (this) { 
    int              c        = 0;
    const char      *preamble = 0;
    const char      *base     = 0;
    const char      *s_nm     = 0;
    const CppTypeV  *in       = 0;
    const CppTypeV **frnds    = 0;
    const char      *punc     = "";
    int              ppp_svd  = PubPrtPrv;

    if (!NULL_REF(name)) s_nm = strDeref(name);
    if (x.f.tmplt) {
      if (flgs & WFLG_MINIMAL) {
	if (flgs & WFLG_DISAMBIG) {
	  doDisambiguate(out,Scope());
	}
	c = 5; goto name;
      }
      out.printf("%stemplate<",NL(out));
      const CppScope *args = dynamic_cast<const CppScope *>(this);
      WriteArgs(out,args,flgs,1);
      out.printf(">%s",NL(out));
    }
    switch (typ) {
    case PCT_MODULE:       PubPrtPrv = 3;
                           preamble  = "class %s ";
                           base      = "Module";
                                                        c=5; break;
    case PCT_PROCESS:      PubPrtPrv = 3;
                           preamble  = "class %s ";
                           base      = "Proc";
                                                        c=3; break;
    case PCT_ARCHITECTURE: PubPrtPrv = 3;
                           preamble  = "class %s ";
                           base      = strDeref(findEntity()->Name());
                                                        c=6; break;
    case PCT_ENTITY:       PubPrtPrv = 3;
                           preamble  = "class %s ";
                           base      = "Module ";
                                                        c=7; break;
    case PCT_STRUCT:       PubPrtPrv = 1;
                           preamble  = s_nm ? "struct %s "
			                    : "struct ";   
                                                        c=2; break;
    case PCT_CLASS:        PubPrtPrv = 3;
                           preamble  = "class %s ";     c=1; break;
    case PCT_UNION:        preamble  = "union ";        c=1; break;
    case PCT_ENUM:         preamble  = "enum ";         c=4; break;
    case PCT_QUAL_SCLR:
    case PCT_SCALAR:                                    c=0; break;
    default: assert(0);
    }
    if (flgs & WFLG_SYSC2PC) {
      base = 0;
      switch (c) { case 5: preamble = "module %s"; c=1; break;
      }
    } else {
      switch (c) { case 7:
                   case 5:
                   case 3:
                   case 6: T = FLUSH_OUT(out,T); 
      }
    }
    if (preamble) switch (c) {
    case 4:  if (CurrEnum != this) {
	       // assert(!CurrEnum);
	       if (x.f.typdf) out.printf("%stypedef ",NL(out));
               CurrEnum = this;
               out.printf(preamble);
               if (x.f.typdf) goto skp_nm;
               break;
	     }
             goto done;
    case 1:
    case 3:
    case 5:
    case 6:
    case 7:  flgs =WFLG(flgs|WFLG_CLASS);
             if (flgs & WFLG_MINIMAL) break;
    default: if (x.f.typdf) out.printf("typedef ");
             out.printf(preamble,s_nm,s_nm);
             goto skp_nm;
    }
   name:
    if (s_nm) {
      if (flgs & WFLG_COERCE) {
        out.printf("val_");
      }
      out.printf("%s ",s_nm);
    }
   skp_nm:
    switch (c) {
    case 1:
    case 2:
    case 3:
    case 5:
    case 6:
    case 7:
      if (flgs & (WFLG_MINIMAL|WFLG_FRIEND)) {
        goto done;
      }
      if (base || (in = Inherits())) punc = ": ";
      while (in) {
        const CppTypeRef *ref = in->cRef();
        if (0 == ref || !ref->x.f.stub) { 
	  out.printf(punc);
	  punc = ",";
	  if (ref) {
	    if (ref->x.f.pblc) out.printf("public ");
	    if (ref->x.f.prvt) out.printf("private ");
	    if (ref->x.f.vrtl) out.printf("virtual ");
	    poolRef nm = ref->Name();
	    if (NULL_REF(nm)) nm = ref->typ->Name();
	    out.printf("%s ",strDeref(nm));
	  } else {
	    assert(0);
	  }
	}
        if (!(in = in->next)) break;
      }
      if (base) out.printf("%s public %s",punc,base);
      break;
    case 4:
      out.printf("{");
      goto done;
    }
    const CppScope *scp;
    const char     *p_nm;
    const CppTypeV *p_typ;
    if (scp = Scope()) {
      out.printf(" {%s",NL(out));
      switch (c) {
      case 3: if (scp->usesChan()) out.printf("\teChanSts _csts;%s",NL(out));
	      if (scp->usesWait()) {
		int wp = 1 + scp->forks();
		out.printf("\tEvent _wait[%d];%s",wp,NL(out));
	      }
              if (scp->usesStts()) out.printf("\tint _ists;%s",     NL(out));
              if (p_typ = scp->up->Type()) {
                out.printf("\t%s *_m;Module*_M(){return _m;}%s",p_nm = strDeref(p_typ->name),NL(out));
	      }
      }
      if (frnds = Friends()) {
        const CppTypeV *frnd; 
        while (frnd = *(frnds++)) {
          out.printf("friend ");
          frnd->Write(out,T,WFLG(flgs|WFLG_FRIEND));
          out.printf(";%s",NL(out));
	}
      }
      int rp,i;
      switch (c) {
      case 3:  T = scp->Write(out,T,WFLG(flgs|WFLG_PROC_STTC));
	       out.printf("\tstatic const int _MB = %d;%s",1+scp->forks(),NL(out));
	       out.printf("\tinline void _all(int __state = -1) {%s",NL(out));
               if (p_typ) {
  	         p_typ->WriteAliases(out,"_m");
	       }
               WriteInitX(out,"_m",INIT_INIT_C,WFLG(flgs|WFLG_PROC_STTC));
               out.printf("\t\tint _W; switch (__state) {%s",NL(out));
               rp = scp->rsmPoints();
               out.printf("\t\t\tdefault: ");
               WriteInitX(out,"_m",INIT_INIT_0,WFLG(flgs|WFLG_PROC_STTC));
               if (rp) {
                 for (i = 0; i < rp ; i++) {
                   out.printf("\t\t\tcase %d: ",i);
		   out.printf("_W = %d; ",scp->forks() ? scp->forkWait(i) 
                                                       : 0);
                   out.printf("goto L%d;%s",i,NL(out));
                 }
               }
               out.printf("\t\t}%s",NL(out));
               T = scp->Write(out,T,WFLG_PROC_DYN);
               break;
      case 5:  if (scp->hasSttcRngs()) {
                 out.printf("public: Slice *__slices;",NL(out));
	       }
      case 6:
      case 7:  T = scp->Write(out,T,flgs);
	       goto skp_cls;               
      default: T = scp->Write(out,T,flgs);
      }
      out.printf(" }%s",NL(out));
     skp_cls:
      switch (c) {
      case 3:  out.printf("\tstatic void start(Proc *_pv) "
			  "{%s *_pp = (%s *)_pv;_pp->_all();};%s",
                          s_nm,s_nm,NL(out));
	       for (i = 0; i < rp ; i++) {
                 out.printf("\tstatic int resume%d(Proc *_pv) "
                            "{%s *_pp = (%s *)_pv;_pp->_all(%d);return 0;};%s",
                            i,s_nm,s_nm,i,NL(out));
               }
  	       if (p_typ) {
 	         out.printf("\tfriend class %s;%s",p_nm,NL(out));
	       }
 	       out.printf("\tvoid init() {%s",NL(out));
                 if (p_typ) {
	           p_typ->WriteAliases(out,"_m");
		 }
		 WriteInitX(out,"_m",INIT_INIT_C,WFLG(flgs|WFLG_PROC_STTC));
                 if (scp->usesWait()) { 
		   const CppTypeScp *proc = dynamic_cast<const CppTypeScp *>(scp);
                   out.printf("initWait(_wait,sizeof(_wait)/sizeof(*_wait)%s);%s",
                                                                p_typ ? ",_m" : "",NL(out));
       	         }
       	         out.printf("\tstart(this);}%s",NL(out));
		 if (p_typ) {
		   out.printf("public: %s(%s *_mod) : _m(_mod)",s_nm,p_nm);
		   WriteInit0(out,"_m",",");
		 } else {
		   out.printf("public: %s()",s_nm);
		   WriteInit0(out,"_m"," : ");
		 }
	         out.printf("{");
	         if (p_typ) {
	           p_typ->WriteAliases(out,"_m");
		 }
		 WriteInitX(out,"_m",INIT_INIT_C,WFLG(flgs|WFLG_PROC_STTC));
                 for (int i = INIT_CNSTRCT_1; i <= INIT_CNSTRCT_L; i++) {
                   WriteInitX(out,"_m",(eINIT)i);
	         }
	         if (!p_typ) {
	           out.printf("init();%s",NL(out));
		 }
                 out.printf("}%s",NL(out));
                 out.printf("\t~%s() {",s_nm);
	         if (scp->usesWait()) {
		   out.printf("for(int i=sizeof(_wait)/sizeof(*_wait);--i>=0;cancel(&_wait[i]));%s",NL(out));
		 }
                 WriteInitX(out,"_m",INIT_DSTRCT);
	         out.printf("}%s",NL(out));
               out.printf("}%s",NL(out));
               break;
      case 7:
      case 5: 
      case 6:  if (!ovrld) {
                 out.printf("\tpublic: %s(Module *up = 0,const char *nm = 0) : %s(up,nm)",
			                                                    s_nm,base);
                 WriteInit0(out,"_m",",");
	         out.printf("{%s",NL(out));       
                 if (scp->hasSttcRngs()) {
                   out.printf("__slices = 0;%s",NL(out));
	         }
                 for (int i = INIT_CNSTRCT_1; i <= INIT_CNSTRCT_L; i++) {
                    WriteInitX(out,"_m",(eINIT)i,flgs);
                 }
                 out.printf("\tif (up->isRoot()) bind_children(); post_bind();",NL(out));
	         out.printf("}%s",NL(out));       
	       }
               out.printf("\tpublic: void bind_children() {%s",NL(out));
	       WriteInitX(out,"_m",INIT_CNSTRCT_0,flgs);
                 WriteInitX(out,"_m",INIT_KIDS_0,flgs);
                 WriteInitX(out,"_m",INIT_KIDS_1,flgs);
	         out.printf("}%s",NL(out));
               out.printf("\tpublic: void init_prcs() {%s",NL(out));
                 WriteInitX(out,"_m",INIT_INIT_0,flgs);
	         out.printf("}%s",NL(out));
               out.printf("\tpublic: ~%s() {%s",s_nm,NL(out));
                 WriteInitX(out,"_m",INIT_DSTRCT,flgs);
                 if (scp->hasSttcRngs()) {
                   out.printf("Slice *s = __slices; __slices = 0; delete s; %s",NL(out));
	         }
	         out.printf("}%s",NL(out));
               out.printf("\tpublic: static %s *_create(Module *up = 0,"
			            "const char *nm = 0,const char *arch = 0) {",s_nm);
                 switch (c) {
	         default: out.printf("return new %s(up,nm);%s",s_nm,NL(out)); break;
	         case 7:  out.printf("return dynamic_cast<%s *>(NewArch(up,nm,\"%s\",arch));%s",
                                                               s_nm,s_nm,NL(out)); break;
	         }
	         out.printf("}%s",NL(out));
	       out.printf("}%s",NL(out));
               break;
      }
    }
    if (x.f.typdf) {
      out.printf(" %s",strDeref(Name()));
    }
    if (!(flgs & WFLG_NO_CLOSE)) {
      out.printf(";%s",NL(out));
    }
    switch (c) {
    case 6: out.printf("static const Arch %s__%s(\"%s\",\"%s\",(create_fn)%s::_create);%s",
		       base,s_nm,base,s_nm,s_nm,NL(out));
    }
 done:
    PubPrtPrv = ppp_svd;
  }
  return T;
}

const Token *CppTypeRef::Write(Stream &out,const Token *T0,eWFLG flgs,const char **dcl_nm) const
{
  const Token *T = Where()->Tok();

  if (this) { 
    int             dbg    = (flgs & WFLG_DEBUG),
                    min    = (dbg) ? 0
                                   : (flgs & WFLG_MINIMAL),
                    bo     = (flgs & WFLG_BODY_ONLY),
                    rwr    = x.f.rewrite;
    const CppTypeV *to_typ = typ;
    CppIndex       *idx;
    const CppScope *scp;

    if (rwr) {
      FLUSH_OUT(out,T0);
      out.printf("%s",NL_SYNC(out,Where()));
    }

    flgs    = WFLG(flgs & ~(WFLG_CLASS|WFLG_BODY_ONLY));
    if (min) {
      if ((flgs & WFLG_INST_T) && !typ) {
        out.printf("%s ",strDeref(name));
        return T;
      }
    }
    if (WFLG_NO_NL & flgs) {
      flgs = WFLG(flgs & ~WFLG_NO_NL);
    } else {
      out.printf("%s",NL_DBG(out));
    }
    if (x.f.tmplt) {
      out.printf("template ");
    }
    if (x.f.ext)         {if (SAME_REF_PI(name,SYSTEMC_POOL,
                                               SYSC_SC_END_OF_HEADER)) {
                            FLUSH_OUT(out,T);
	                    return T;
                          }
                          out.printf("extern "); min = 1;}
   
    if (x.f.typnm) {
                          out.printf("typename ");
                          if (to_typ == CppType::TypeName) {
                            to_typ = 0;
                            out.printf("%s ",strDeref(name));
			  }
    }
    if (x.f.nmspc) {
                          out.printf("%s ",strDeref(name));
                          if (x.f.scp && sub) {
                            out.printf("::");
                            assert(!to_typ);
                            to_typ = sub;
			  }
    }
    if (x.f.typdf) {
      if (min) {
       quick:             out.printf("%s ",strDeref(name));
   	                  for (int i = x.f.indrct; i-- > 0;) out.printf("*");
	return T;
      } else {
                          out.printf("typedef ");
      }
    }
    if (x.f.pblc)        {out.printf("public: ");    WritePPP(out,PPP_PUBLIC);}
    if (x.f.prot)        {out.printf("protected: "); WritePPP(out,PPP_PROTECTED);}
    if (x.f.prvt)        {out.printf("private: ");   WritePPP(out,PPP_PRIVATE);}
    if (x.f.mut)          out.printf("mutable ");
    if (x.f.vrtl)         out.printf("virtual ");
    if (x.f.inln)         out.printf("inline ");
    if (x.f.xplct)        out.printf("explicit ");
    if (x.f.vol)          out.printf("volatile ");
    if (x.f.gnu_xn)       out.printf("__extension__ ");
    if (x.f.sttc)         out.printf("static ");
    if (x.f.reg)          out.printf("register ");
    if (x.f.cnst_src)     out.printf("const ");
    if (x.f.gnu_cnst_src) out.printf("__vconst ");
    if (x.f.sgnd > 0)     out.printf("signed ");
    if (x.f.sgnd < 0)     out.printf("unsigned ");
    if (x.f.lng)          out.printf("long ");
    if (x.f.ref)          out.printf("&");
    if (to_typ) {
      if (!(x.f.cnstrctr || x.f.destrctr || bo)) {
        T  = to_typ->Write(out,T,WFLG(flgs|WFLG_NO_CLOSE
                                          |WFLG_MINIMAL|WFLG_NO_LOCATION));
        bo |= to_typ->isFunc();
      }
      if (x.f.inst) {
        out.printf("<");
        WriteArgs(out,args,WFLG(flgs|WFLG_NO_TYPE|WFLG_INST_T));
        out.printf("> ");  
      }
    }
    for (int i = x.f.indrct; i-- > 0;) out.printf("*");
    if (x.f.inst) goto done;
    if (x.f.gnu_rstrct)   out.printf("__restrict ");
    scp = 0;
    if (x.f.func) {
      if (!bo) {
	const char *fnm;
        if (x.f.func & XF_FN_PTR) {
          out.printf("(*%s)",fnm = strDeref(name));
        } else if (BAD_REF(name)) {
          out.printf("/* ??? */");
        } else {
          out.printf("%s",fnm = strDeref(name));
        }
        out.printf("(");
        WriteArgs(out,args,flgs);
        out.printf(")");
        if (x.f.thrw) {
          out.printf(" throw ");
          attr->Write(out,0);
        }
        if (x.f.pure) {
          out.printf(" = 0;%s",NL_DBG(out));
        }
      }
      if (x.f.ext || x.f.typdf || !x.f.bod) goto no_scp;
      flgs = WFLG(flgs|WFLG_FUNCTION|WFLG_NO_PPP);
      if (inits) {
        CppInit *init = inits->list;
        const char *pnc = " : ";
        for (; init->decl ; pnc = ",",init++) {
          out.printf("%s",pnc);
          if (init->decl) {
            out.printf("%s",strDeref(init->decl->name));
	  } else {
            assert(0);
	  }
          out.printf("(");
          init->expr->Write(out,T);
	  out.printf(")");
	}
        const CppTypeCls *cls = dynamic_cast<const CppTypeCls *>(typ);
        if (cls) {
          cls->WriteInit0(out,"_m",pnc);
	}
      }
    }      
    scp = Scope();
   no_scp:
    if (x.f.cnst_trg)     out.printf("const ");
    if (x.f.gnu_cnst_trg) out.printf("__const ");
    if (scp) {
      eWFLG fx = WFLG(flgs|WFLG_ADD_CR);
      if (x.f.cnstrctr) fx = WFLG(fx|WFLG_CONSTRUCT);
      if (x.f.destrctr) fx = WFLG(fx|WFLG_DESTRUCT);
      T = scp->Write(out,T,fx,typ);
    }
    out.printf(" ");
    idx = index;
    if ((x.f.typdf && !x.f.func) || (idx && dcl_nm)) {
      if (dcl_nm) {
        assert(*dcl_nm);
        out.printf("%s",*dcl_nm);
        *dcl_nm = 0;
      } else {
        out.printf("%s",strDeref(Name()));
      }
      for (; idx ; idx = idx->next) {
        out.printf("[");
        if (idx->expr) {
          idx->expr->Write(out,0);
	}
        out.printf("]");
      }
      if (!(flgs & WFLG_NO_CLOSE)) {
        out.printf(";%s",NL_DBG(out));
      }
    } else if (x.f.func && !scp) {
      if (!(flgs & WFLG_NO_CLOSE)) {
        out.printf(";%s",NL_DBG(out));
      }
    } else if (x.f.width) {
      out.printf("%s:",*dcl_nm); *dcl_nm = 0;
      attr->Write(out,0);
    } else if (flgs & WFLG_TEMPLATE) {
      out.printf("%s",strDeref(Name()));
    }

   done:
    if (sub) {
      out.printf("::");
      T = sub->Write(out,T,flgs);
    }
  }
  return T;
}

const Token *CppStmtType::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  const Token *T = source.Write(out,T0,flgs);
  switch (typ) {
  case CSTMT_Fn2Mod:   out.printf("module %s {%s",
                                  strDeref(tv->Name()),NL(out));
                       flgs = WFLG(flgs | WFLG_BODY_ONLY);
                       T = tv->Write(out,T,flgs);
                       out.printf("}%s",NL(out));
                       break;
  case CSTMT_ScThread: if (flgs & WFLG_SYSC2PC) {
                         out.printf("process %s",strDeref(tv->Name()));
                         flgs = WFLG(flgs | WFLG_BODY_ONLY);
                       }
  default:             T = tv->Write(out,T,flgs);
  }

  return T;
}

const Token *CppStmtExpr::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  T0 = source.Write(out,T0,flgs);

  switch (typ) {
    case CSTMT_ScCmnt: out.printf("/* ");     break;
    case CSTMT_case:   out.printf("case ");   break;
    case CSTMT_goto:   out.printf("goto ");   break;
    case CSTMT_return: out.printf("return "); break;
  }

  if (expr) {
    T0 = expr->Write(out,T0);
  }

  switch (typ) {
    case CSTMT_ScCmnt: out.printf(" */"); break;
    case CSTMT_case:   out.printf(": ");  break;
    default:           out.printf(";%s",NL(out));
  }

  return T0;
}

const Token *CppStmtDecl::Write(Stream &out,const Token *T0,
                                eWFLG flgs,const char *sep) const
{
  T0 = source.Write(out,T0);
  char buff[64];
  const CppType *et = CurrEnum;
  if (et) {
    if (dcl->typ == et) {
      strcpy(buff,"}");
      if (dcl->next && dcl->next->typ == et) {
        strcpy(buff,",");
      } else if (et->x.f.typdf) {
        sprintf(buff,"} %s;",strDeref(CurrEnum->name));
        et = 0;
      } else {
        strcpy(buff,"};");
        et = 0;
      }
      sep = buff;
    } else {
      // assert(0);
    }
  }

  T0 = dcl->Write(out,T0,WFLG(flgs|WFLG_DISAMBIG),sep);

  CurrEnum = et;
  return T0; 
}

const Token *CppStmtLabel::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  switch (typ) {
  case CSTMT_ScProc: out.printf("Process %s { ",strDeref(name)); break;
  default:           out.printf("%s:; ",strDeref(name));         break;
  }
  return source.Tok();
}

const Token *CppStmtPragma::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  out.printf("#pragma ");
  if (out.Fp()) {
    tokReconInMem(start,1 + finish - start,out.Fp());
  }
  return source.Tok();
}

const Token *CppStmtUsing::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  const char *nms = nmspc ? " namespace"
                          : "";
  if (obj) {
    out.printf("using%s %s;%s",nms,strDeref(obj->Name()),NL(out));
  } else {
    out.printf("using%s %s;%s",nms,strDeref(pend),NL(out));
  }
  return source.Tok();
}

const Token *CppStmtBlock::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  if (stmt0) {
    eWFLG prop = WFLG(flgs & (WFLG_SYSC2PC|WFLG_FORK));
    if (stmt0->next) {
      CppScope::Write(out,0,WFLG(WFLG_ADD_CR | prop));
    } else {
      CppScope::Write(out,0,prop);
      goto close_it;
    }
  } else {
   close_it:
    if (flgs & WFLG_ADD_SCOLON) {
      out.printf(";%s",NL(out));
    }
  }
  return source.Tok();
}

const Token *CppStmtCatch::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  out.printf("catch (",NL(out));
  WriteArgs(out,args,flgs);
  out.printf(")",NL(out));

  CppStmtBlock::Write(out,T0,flgs);

  return source.Tok();
}

const Token *CppStmtNmdBlk::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  CppNamedScope *chk = const_cast<CppNamedScope *>(nm_spc);
  CppStmtNmdBlk *blk = const_cast<CppStmtNmdBlk *>(this);

  if (chk->wr_seg) {
    assert(blk == chk->wr_seg); // ???
  } else {
    chk->wr_seg = blk;
    out.printf("namespace %s {%s",strDeref(nm_spc->name),NL(out));
    nm_spc->Write(out,0,flgs);
    out.printf("}%s",NL(out));
    chk->wr_seg = 0;
  } 

  return source.Tok();
}

const Token *CppStmtIf::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  T0 = source.Write(out,T0,flgs);

  out.printf("%sif (",NL(out));
  cnd->Write(out,0);
  out.printf(") {");
  const Token *T = tru.Write(out,0,flgs);
  if (fls) {
    out.printf("} else {");
    T = fls->Write(out,0,flgs);
  }
  out.printf("}%s",NL(out));
  return T;
}

const Token *CppStmtFor::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  T0 = source.Write(out,T0,flgs);

  out.printf("%sfor (",NL(out));
  if (cntrs) {
    assert(!init);
    cntrs->Write(out,0);
  } else {
    if (init) {
      assert(!cntrs);
      init->Write(out,0);
    }
    out.printf(";");
  }
  if (cnd) cnd->Write(out,0);
  out.printf(";");
  if (tail) tail->Write(out,0);
  out.printf(")");
  CppStmtBlock::Write(out,0,flgs);
  out.printf(";");

  return T0;
}

const Token *CppStmtWhile::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  T0 = source.Write(out,T0,flgs);

  out.printf("%swhile (",NL(out));
  cnd->Write(out,0);
  out.printf(")");
  CppStmtBlock::Write(out,0,flgs);
  out.printf(";");

  return T0;
}

const Token *CppStmtFork::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  T0 = source.Write(out,T0,flgs);

  return CppStmtBlock::Write(out,0,WFLG(flgs|WFLG_FORK0));
}

const Token *CppStmtDo::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  T0 = source.Write(out,T0,flgs);

  out.printf("%sdo ",NL(out));
  CppStmtBlock::Write(out,0,flgs);
  out.printf(" while (",NL(out));
  T0 = cnd->Write(out,0);
  out.printf(");");
  return T0;
}

const Token *CppStmtSwitch::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  T0 = source.Write(out,T0,flgs);

  out.printf("%sswitch (",NL(out));
  cnd->Write(out,0);
  out.printf(")");
  return CppStmtBlock::Write(out,0,flgs);
}

const Token *CppStmtAt::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  T0 = source.Write(out,T0,flgs);

  out.printf("{");
  cnd->Write(out,0);
  out.printf(";");
  const Token *T = CppStmtBlock::Write(out,T0,flgs);
  out.printf("}");
  return T;
}

const Token *CppStmtBefore::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  T0 = source.Write(out,T0,flgs);

  out.printf("{");
  cnd->Write(out,0);
  out.printf(";");
  const Token *T = CppStmtBlock::Write(out,T0,flgs);
  out.printf("}");
  return T;
}

const Token *CppStmtTmplRef::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  T0 = source.Write(out,T0,flgs);

  if (xi & PCX_EXTERN) out.printf("extern ");
  out.printf("template ");
  if (xi & PCX_T_INST) out.printf("<> ");
  const Token *T = tmpl[0]->Write(out,T0,flgs);
  out.printf(";");
  return T;
}

const Token *CppStmt::Write(Stream &out,const Token *T0,eWFLG flgs) const
{
  const char *really = typeid(this).name();

  switch (typ) {
  case CSTMT_break: out.printf(" break;%s",NL(out));
                    break;
  case CSTMT_cont:  out.printf(" continue;%s",NL(out));
                    break;
  default:          assert(0);
  }

  return source.Tok();
}


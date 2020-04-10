/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * cdump_cpp_rcsid() {return "$Id: cdump.cpp,v 1.19 2012/10/16 22:38:45 cvs Exp $";}

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
#define  NEED_VERILOG
#define  NEED_SPECIAL
#include "tokpool.h"
#include "verilog.h"
#include "dump.h"

extern "C" eSTS cSetDF(int *argc,const char ***argv,void *var)
{
  return verSetDF(argc,argv,var);
}

extern "C" eSTS cDumpC(int *argc,const char ***argv,void *var)
{
  return cSetDF(argc,argv,(void *)(new cdump(&DumpList)));
}

void cdump::dump(const DrvStrnth *ds)
{
  int         i    = 2;
  const char *punc = ",";

  Strm()->printf(")");

  for (; i-- ; ds++, punc=")") {
    const char *str = "";
    switch (ds->strnth) {
    case DS_HIGHZ:  str = "highz";
    case DS_WEAK:   str = "weak";
    case DS_PULL:   str = "pull";
    case DS_STRONG: str = "strong";
    case DS_SUPPLY: str = "supply";
    default:;
    }
    Strm()->printf("%d%s",ds->lvl,punc);
  }
}

void cdump::dump(const Expr *expr)
{
# define MAPS
# define ROOT          vd()->root
# define XTRA_ARG
# define STRM_ARG
# define DUMP(x,s)     dump(x)
# define VFN_EXPR(vfn) vfn->val()->expr
# define KEEP_DEFS
# include "dump_expr.inc"
}

void cdump::dump(const ValueFn *vfn,VlType typ,void *xdf,void *xtnd)
{
#include "dump_value.inc"
}

void cdump::dump(const Nature *nat)
{
  if (!Strm()) setStrm(STDOUT_FILENO);

  Strm()->printf("\nnature %s",strDump(nat->name));
  if (nat->parent >= 0) {
    Disc *up = disc_map(nat->parent);
    if (nat->flags & DF_DISCIPLINE) {
      Strm()->printf(" : %s.%s",strDump(up->name),
                              nat->flags & DF_FLOW ? "flow" : "potential");
    } else {
      Strm()->printf(" : %s",strDump(up->name));
    }
  }
  Strm()->printf("\n  ");

  dump(nat->attr_list,";\n  ",AM_TYPE_OK);

  Strm()->printf("\nendnature\n");
}

void cdump::dump(const Disc *disc)
{
  poolRef nm;

  if (!Strm()) setStrm(STDOUT_FILENO);

  Strm()->printf("\ndiscipline %s",strDump(disc->name));
  if (disc->parent >= 0) {
    Strm()->printf(" : %s",strDump(disc_map(disc->parent)->name));
  }
  Strm()->printf("\n");

  if (disc->potential >= 0) {
    Nature *potential = nature_map(disc->potential);
    if (SAME_REF(NullRef,potential->name))
    {
      nm.pool  = VERILOG_POOL;
      nm.index = VER_POTENTIAL;
      if (potential->parent >= 0) {
        Strm()->printf("  potential %s;\n",
                     strDump(nature_map(potential->parent)->name));
      }
    } else {
      nm = NullRef;
      Strm()->printf("  potential %s;\n",
                   strDump(potential->name));
    }
    if (potential->attr_list) {
      Strm()->printf("  ");
      dump(potential->attr_list,";\n  ",nm,AM_TYPE_OK);
      Strm()->printf("\n");
    }
  }
  if (disc->flow >= 0) {
    Nature *flow = nature_map(disc->flow);
    if (SAME_REF(NullRef,flow->name))
    {
      nm.pool  = VERILOG_POOL;
      nm.index = VER_FLOW;
      if (flow->parent >= 0) {
        Strm()->printf("  flow      %s;\n",
                     strDump(nature_map(flow->parent)->name));
      }
    } else {
      nm = NullRef;
      Strm()->printf("  flow      %s;\n",
                   strDump(flow->name));
    }
    if (flow->attr_list) {
      Strm()->printf("  ");
      dump(flow->attr_list,";\n  ",nm,AM_TYPE_OK);
      Strm()->printf("\n");
    }
  }

  if (disc->attr_list) {
    Strm()->printf("  ");
    dump(disc->attr_list,";\n  ",AM_TYPE_OK);
    Strm()->printf("\n");
  }

  Strm()->printf("enddiscipline\n");
}

void cdump::dump(const Range *rng)
{
  if (rng->next) dump(rng->next);

  if (rng->range) {
    Strm()->printf(" %s ",rng->exclude ? "exclude" : "from");
    if (OPR_RANGE == rng->range->opr()->self) {
      Strm()->printf("%s",rng->inc_strt ? "[" : "(");
      dump(rng->range);
      Strm()->printf("%s",rng->inc_fnsh ? "]" : ")");
    } else {
      dump(rng->range);
    }
  }
}

void cdump::dump(const Attr *attr,const char *all,poolRef obj,eAttrMode mode)
{
  const Attr *scan = attr;
  const char *punc = "",
             *np,
             *vp   = 0,
             *ass  = "=";
  String      value,
              onm(SAME_REF(NullRef,obj) ? ""
                                        : strDump(obj));
  poolRef     nm;

  if (onm.len() > 0) onm += ".";

  if (scan) do {
    Expr            *expr = 0;
    const AttrAnyU  *au   = (AttrAnyU *)scan;
    const AttrAnyU2 *au2  = (AttrAnyU2 *)scan;
    const char      *rti  = "";
    VlType           at   = ATTR_VT(scan->typ);

    if (scan->typ & ATTR_USR) {
      nm  = au->name();
      switch (at) {
      default:        vp  = au->strValue(&value);                break;
      case VT_EXPR:   at  = VL_TYPE((expr = au->expr())->typ);   break;
      }
    } else {
      nm  = scan->name();
      switch (at) {
      default:        vp  = scan->strValue(&value);              break;
      case VT_EXPR:   at  = VL_TYPE((expr = scan->expr())->typ); break;
      }
    }

    switch (at) {
      case VT_DOUBLE: rti = "real ";    break;
      case VT_U64:    rti = "time ";    break;
      case VT_INT:    rti = "integer "; break;

      case VT_LOGIC:  if (VT_ALIAS != au2->value2.expr->typ) {
                        Strm()->printf("[");
                        dump(au2->value2.expr);
                        Strm()->printf("] ");
	              }
                      expr = au2->value.expr;
    }

    if (expr) {
      vp  = expr->strValue(&value);
    }
  hv_rti:
    if (SPECIAL_POOL == nm.pool) {
      ass = np = "";
    } else {
      np  = strDump(scan->name());
    }
    Strm()->printf("%s%s%s%s%s%s",punc,rti,onm.str(),np,ass,vp);

    if (scan->typ & ATTR_RNGD) {
      dump(((RngdAttr *)scan)->rng_lst);
    }
    punc  = all;
  } while (all && (scan = scan->next));
}

void cdump::dump_attr(const Attr *attr_list,const char *indent,const char *punc)
{
  if (attr_list) {
    Strm()->printf("%s/* ",indent);
    dump(attr_list,",",AM_UNTYPED);
    Strm()->printf(" */%s",punc);
  }
}

void cdump::dump(const Attr *attr,const char *all,eAttrMode mode)
{
  dump(attr,all,NullRef,mode);
}

void cdump::dump(const Branch *br,const char *all)
{
  const Branch *scan = br;
  const char   *punc = "";
  int           c;

  do {
    Strm()->printf("%s(",punc);
    dump(scan->p1);
    Strm()->printf(",");
    dump(scan->p1);
    Strm()->printf(")");
    punc = " ";
    for (c=0;scan->name[c].pool;c++,punc=",") {
      Strm()->printf("%s%s",punc,strDump(scan->name[c]));
    }
    punc = all;
  } while (all && (scan = scan->next));
}

int cdump::dump_ports(const PrtdObj *po ,DpMode mode,
                      const char *punc0,const char *indent)
{
  PortDir       ret  = PRT_NC;
  const Port   *port = po->port()->first();
  int           n    = 0;
  Attr         *attr;
  String        str;
  const char   *punc = punc0;

  for (; port ; port = port->next) {
    int r = 0;
    for (; r < port->pckd; r++) {
      if (port->rng[r] && VT_ALIAS != port->rng[r]->typ) {
        StringStream ss(&str);
        cdump      vdmp(&ss);
        str = "";
        ss.printf("<");
        vdmp.dump(port->rng[r]);
        ss.printf("> ");
      }
    }
    if (!port->pckd) {
      str = "<1>";
    }
    const char *prt_name = strDump(port->name);
    switch (mode) {
    case DM_BIND:     if (port->io & PRT_IO_LSTD) {
                        Strm()->printf("    %s.bind(_%s);\n",
                                       prt_name,prt_name);
                        goto do_attrs;
                      }
                      break;
    case DM_PORT:     if (port->io & PRT_IO_LSTD) {
                        const char *dir = (port->io & PRT_OUT) ? "Out"
			                                       : "In";
                        Strm()->printf("%s%s%s _%s",punc0,dir,str.str(),
                                                    prt_name);
                        goto do_attrs;
                      }
                      break;
    case DM_HIER:     if (port->io & (PRT_HIER)) {
                        Strm()->printf("%s%s%c%s",indent,punc,
                                       port->io & PRT_XPRTD ? '-'
                                                            : '+',
                                       prt_name);
                        goto do_attrs;
                      }
                      break;
    case DM_INPUT:    if (port->io & PRT_IN) {
                        if (str.len()) {
                          Strm()->printf("%s%s %s %s%s",
				   n ? ";\n" : "",
                                   punc0,str.str(),indent,prt_name);
                        } else {
                          Strm()->printf("%s%s%s%s",
                                   str.str(),indent,punc,prt_name);
                        }
                        goto do_attrs;
                      }
                      break;
    case DM_OUTPUT:   if (port->io & PRT_OUT) {
                        if (str.len()) {
                          Strm()->printf("%s%s %s %s%s",
				   n ? ";\n" : "",
                                   punc0,str.str(),indent,prt_name);
                        } else {
                          Strm()->printf("%s%s%s%s",
                                   str.str(),indent,punc,prt_name);
                        }
                        goto do_attrs;
                      }
                      break;
    case DM_DISC:     if (port->dsc_indx >= 0 && !(port->io & PRT_DSC_DFLTD)) {
                        Strm()->printf("  %s %s;\n",
                                   strDump(po->disc_map(port->dsc_indx)->name),
                                   prt_name);
                        n++;
                      }
                      break;
    case DM_REG:      if (port->ptyp == PT_REG) {
                        if (str.len()) {
                          Strm()->printf("%s%s %s %s%s",
				   n ? ";\n" : "",
                                   punc0,str.str(),indent,prt_name);
                        } else {
                          Strm()->printf("%s%s%s",
                                       indent,punc,prt_name);
                        }
                        goto do_attrs;
                      }
                      break;
    case DM_WIRE:     if (port->ptyp == PT_WIRE) {
                        if (str.len()) {
                          Strm()->printf("%s%s %s %s%s",
				   n ? ";\n" : "",
                                   punc0,str.str(),indent,prt_name);
                        } else {
                          Strm()->printf("%s%s%s%s",
                                   str.str(),indent,punc,prt_name);
                        }
                        goto do_attrs;
                      }
                      break;
    case DM_REAL:     if (port->ptyp == PT_REAL) {
                        Strm()->printf("%s%s%s",indent,punc,prt_name);
                        goto do_attrs;
                      }
                      break;
    case DM_INTEGER:  if (port->ptyp == PT_INT) {
                        Strm()->printf("%s%s%s",indent,punc,prt_name);
                        goto do_attrs;
                      }
                      break;
     }
     continue;

  do_attrs:
     if (port->unpckd) {
       int r = port->pckd;
       for (; r < port->unpckd; r++) {
	 Strm()->printf("[");
	 dump(port->rng[r]);
	 Strm()->printf("]");
       }
     }
     if ((attr = port->attr_list)) {
       int any = 0;
       for (; attr ; attr = attr->next) switch (attr->pool) {
	 case SPECIAL_POOL: if (SPC_INITIAL == attr->index) {
                              Strm()->printf(" = ");
                              dump(attr,0,NullRef,AM_TYPE_OK);
                              break;
	                    }
         default:           if (!any) {
                              any = 1;
                              Strm()->printf(" (* ");
                            }
                            dump(attr,0,NullRef,AM_TYPE_OK);
       }
       if (any) Strm()->printf(" *)");
     }
     n++;
     punc   = ",";
     indent = "";
  }

  return n;
}

int cdump::dump_ports(const PrtdObj *po,DpMode mode,const char *punc)
{
  return dump_ports(po,mode,punc,"");
}

int cdump::dump_ports(const PrtdObj *po ,DpMode mode)
{
  return dump_ports(po,mode,"","");
}

void cdump::dump_ports(const PrtdObj *po,String *indent)
{
  if (dump_ports(po,DM_REG,   "  reg "))    Strm()->printf(";\n");

  dump_ports(po,DM_DISC);

  if (dump_ports(po,DM_REAL,   "  real "))    Strm()->printf(";\n");
  if (dump_ports(po,DM_INTEGER,"  integer ")) Strm()->printf(";\n");
}

void cdump::dump(const Module *mod)
{
  Branch     *br;
  const char *m_name = strDeref(mod->name);

  if (!Strm()) setStrm(STDOUT_FILENO);

  Strm()->printf("\nclass %s : MODULE_CLASS {\n  public:\n",m_name);

  if (dump_ports(mod,DM_INPUT, "  In"))  Strm()->printf(";\n");
  if (dump_ports(mod,DM_OUTPUT,"  Out")) Strm()->printf(";\n");
  if (dump_ports(mod,DM_REG,   "  Reg")) Strm()->printf(";\n");

  mode = DMODE(DMODE_STRUCTURAL|DMODE_DECLARE);

  if (mod->stmts) {
    String indent("  ");
    dump(mod->stmts,mod,&indent);
  }

  Strm()->printf("\n  %s(char *name",m_name);
  if (mod->io & PRT_INOUT) {
    dump_ports(mod,DM_PORT,",");
  }

  Strm()->printf(") INIT_MODULE(name) {\n");
  if (mod->io & PRT_INOUT) {
    dump_ports(mod,DM_BIND);
  }

  mode = DMODE(DMODE_STRUCTURAL|DMODE_DEFINE);

  if (mod->stmts) {
    String indent("    ");
    dump(mod->stmts,mod,&indent);
  }

  Strm()->printf("  }\n");

  dump_ports(mod,DM_DISC);

  mode = DMODE_BEHAVIORAL;

  if (dump_ports(mod,DM_REAL,   "  real "))    Strm()->printf(";\n");
  if (dump_ports(mod,DM_INTEGER,"  integer ")) Strm()->printf(";\n");
  if (dump_ports(mod,DM_WIRE,   "  wire "))    Strm()->printf(";\n");

  if (Flags() & DMP_VERBOSE) {
    if (dump_ports(mod,DM_HIER, "  /* "))    Strm()->printf(" */\n");
  }

  if (mod->parm()->first()) {
    Strm()->printf("  parameter ");
    dump(mod->parm()->first(),",\n            ",AM_TYPE_OK);
    Strm()->printf(";\n");
  }

  if (mod->branch()->first()) {
    Strm()->printf("  branch ");
    dump(mod->branch()->first(),",\n         ");
    Strm()->printf(";\n");
  }

  if (mod->stmts) {
    String indent("  ");
    dump(mod->stmts,mod,&indent);
  }

  Strm()->printf("}\n");
}

void cdump::dump(const Prim *prim)
{
  if (!Strm()) setStrm(STDOUT_FILENO);

  Strm()->printf("\nprimitive %s",strDump(prim->name));

  if (prim->io & PRT_INOUT) {
    Strm()->printf("(");
    dump_ports(prim,DM_EXTERNAL);
    Strm()->printf(")");
  }
  Strm()->printf(";\n");

  if (dump_ports(prim,DM_INPUT,"  input "))   Strm()->printf(";\n");
  if (dump_ports(prim,DM_OUTPUT,"  output ")) Strm()->printf(";\n");
  if (dump_ports(prim,DM_REG,   "  reg "))    Strm()->printf(";\n");

  if (prim->stmt) {
    String indent("    ");
    Strm()->printf("  initial\n");
    dump(prim->stmt,prim,&indent);
  }

  if (prim->table) {
    Table *scan = prim->table;
    Strm()->printf("  table\n");
    for (; scan ; scan=scan->next) {
      int i = 0;
      Strm()->printf("    ");
      for (; i < prim->in+prim->seq+prim->out ; i++) {
        const char *cp = prim->tblString((eTable)(scan->entry[i]));
        if ((i == prim->in) || (prim->seq && i == prim->in+1)) {
          Strm()->printf(" : ");
        }
        Strm()->printf(i < prim->in ? "%-5s"
                                    : " %s ",cp);
      }
      Strm()->printf(";\n");
    }
    Strm()->printf("  endtable\n");
  }

  Strm()->printf("endprimitive\n");
}

void cdump::dump(const Func *func,const ContextObj *rt,String *indent,bool use_indent)
{
  if (!Strm()) setStrm(STDOUT_FILENO);

  Strm()->printf(use_indent ? "\n%sfunction "
                            : " function ",   indent->str());

  switch(func->ret) {
  case FUNC_REAL: Strm()->printf("real ");    break;
  case FUNC_INT:  Strm()->printf("integer "); break;
  case FUNC_RNG:  Strm()->printf("[");
                  dump(func->range);
                  Strm()->printf("] ");       break;
  }

  Strm()->printf("%s",strDump(func->name));

  if (func->glob && (func->io & PRT_INOUT)) {
    Strm()->printf("(");
    dump_ports(func,DM_EXTERNAL);
    Strm()->printf(")");
  }
  Strm()->printf(";\n");

  if (dump_ports(func,DM_INPUT,"  input "))   Strm()->printf(";\n");
  if (dump_ports(func,DM_OUTPUT,"  output ")) Strm()->printf(";\n");

  dump_ports(func,DM_DISC);

  if (dump_ports(func,DM_REAL,   "  real "))    Strm()->printf(";\n");
  if (dump_ports(func,DM_INTEGER,"  integer ")) Strm()->printf(";\n");

  if (func->parm()->first()) {
    Strm()->printf("  parameter ");
    dump(func->parm()->first(),",\n            ",AM_TYPE_OK);
    Strm()->printf(";\n");
  }

  if (func->stmts) {
    String indent("  ");
    dump(func->stmts,rt,&indent);
  }

  Strm()->printf("endfunction\n");
}

void cdump::dump(const Task *task,const Stmt *stmt,
                 const ContextObj *rt,String *indent)
{
  if (!Strm()) setStrm(STDOUT_FILENO);

  Strm()->printf("\n%stask %s",indent->str(),strDump(task->name));

  if (task->glob && (task->io & PRT_INOUT)) {
    Strm()->printf("(");
    dump_ports(task,DM_EXTERNAL);
    Strm()->printf(")");
  }
  Strm()->printf(";\n");

  if (dump_ports(task,DM_INPUT,"  input "))   Strm()->printf(";\n");
  if (dump_ports(task,DM_OUTPUT,"  output ")) Strm()->printf(";\n");

  dump_ports(task,DM_DISC);

  if (dump_ports(task,DM_REAL,   "  real "))    Strm()->printf(";\n");
  if (dump_ports(task,DM_INTEGER,"  integer ")) Strm()->printf(";\n");

  if (task->parm()->first()) {
    Strm()->printf("  parameter ");
    dump(task->parm()->first(),",\n            ",AM_TYPE_OK);
    Strm()->printf(";\n");
  }

  if (task->stmts) {
    *indent += "  ";
    dump(task->stmts,rt,indent);
    *indent -= "  ";
  }

  Strm()->printf("%sendtask\n",indent->str());
}

void cdump::dump(const StmtEvent *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%s-> ",indent->str());
  dump(stmt->expr);
  Strm()->printf(";\n");
}

void cdump::dump(const StmtDisable *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%sdisable ",indent->str());
  dump(stmt->expr);
  Strm()->printf(";\n");
}

void cdump::dump(const StmtRelease *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%srelease ",indent->str());
  dump(stmt->expr);
  Strm()->printf(";\n");
}

void cdump::dump(const StmtDeassign *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%sdeassign ",indent->str());
  dump(stmt->expr);
  Strm()->printf(";\n");
}

void cdump::dump(const StmtForce *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%sforce ",indent->str());
  dump(stmt->expr);
  Strm()->printf(";\n");
}

void cdump::dump(const StmtAssign *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%sassign ",indent->str());
  if (stmt->delay) {
    Strm()->printf("#(");
    dump(stmt->delay);
    Strm()->printf(") ");
  }
  if (stmt->ds[0].strnth) {
    Strm()->printf("(");
    dump(stmt->ds);
    Strm()->printf(") ");
  }
  dump(stmt->expr);
  Strm()->printf(";\n");
}

void cdump::dump(const StmtWhile *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%s while (",indent->str());
  dump(stmt->expr);
  Strm()->printf(")\n");
  *indent += "  ";
  dump(stmt->child,rt,indent);
  *indent -= "  ";
}

void cdump::dump(const StmtAlways *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%salways\n",indent->str());
  *indent += "  ";
  dump(stmt->child,rt,indent);
  *indent -= "  ";
}

void cdump::dump(const StmtWait *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%swait (",indent->str());
  dump(stmt->expr);
  Strm()->printf(")\n");
  *indent += "  ";
  dump(stmt->child,rt,indent);
  *indent -= "  ";
}

void cdump::dump(const StmtForever *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%sforever\n",indent->str());
  *indent += "  ";
  dump(stmt->child,rt,indent);
  *indent -= "  ";
}

void cdump::dump_case(const StmtCase *stmt,const ContextObj *rt,
                      String *indent,const char *cs)
{
  case_s *lp = stmt->list;
  Strm()->printf("%s%s (",indent->str(),cs);
  dump(stmt->expr);
  Strm()->printf(")");
  *indent += "  ";
  for (;lp;lp = lp->next) {
    Strm()->printf("\n%s",indent->str());
    if (lp->expr)
      dump(lp->expr);
    else
      Strm()->printf("default");
    Strm()->printf(" :\n");
    *indent += "  ";
    dump(lp->child,rt,indent);
    *indent -= "  ";
  }
  *indent -= "  ";
  Strm()->printf("%sendcase\n",indent->str());
}

void cdump::dump(const StmtCase *stmt,const ContextObj *rt,String *indent)
{
  dump_case(stmt,rt,indent,"case");
}

void cdump::dump(const StmtCaseX *stmt,const ContextObj *rt,String *indent)
{
  dump_case(stmt,rt,indent,"casex");
}

void cdump::dump(const StmtCaseZ *stmt,const ContextObj *rt,String *indent)
{
  dump_case(stmt,rt,indent,"casez");
}

void cdump::dump(const StmtInst *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf(indent->str());

  if (!(mode & DMODE_DEFINE)) {
    Strm()->printf("%s ",strDump(stmt->name));
  }

  if (!NULL_REF(stmt->drv)) {
  }
  if (stmt->param) {
    Strm()->printf("#(");
    dump(stmt->param);
    Strm()->printf(") ");
  }
  if (stmt->inst) {
    Expr *xpr = stmt->inst;
    if (mode & DMODE_DECLARE) {
      for (;;) switch(xpr->op()) {
      case OPR_LIST1: dump(xpr->lhs()->lhs());
                      Strm()->printf(",");
                      xpr = xpr->rhs();
                      break;
      default:        dump(xpr->lhs()); goto done;
      }
    } else if (mode & DMODE_DEFINE) {
      for (;;) switch(xpr->op()) {
      case OPR_LIST1: dump(xpr->lhs());
                      Strm()->printf(";\n%s",indent->str());
                      xpr = xpr->rhs();
                      break;
      default:        dump(xpr); goto done;
      }
    } else {
      dump(xpr);
    }
  }
done:
  Strm()->printf(";\n");
}

genVal *cdump::findGen(poolRef trg)
{
  assert(("NIY",0));
}


void cdump::dump(const StmtGen *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%sgenerate %s (",indent->str(),strDump(stmt->cntr));
  dump(stmt->expr);
  Strm()->printf(")\n");
  *indent += "  ";
  dump(stmt->child,rt,indent);
  *indent -= "  ";
}

void cdump::dump(const StmtFor *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%sfor(",indent->str());
  dump(stmt->ass1);
  Strm()->printf(";");
  dump(stmt->expr);
  Strm()->printf(";");
  dump(stmt->ass2);
  Strm()->printf(")\n");
  *indent += "  ";
  dump(stmt->child,rt,indent);
  *indent -= "  ";
}

void cdump::dump(const StmtRepeat *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%srepeat(",indent->str());
  dump(stmt->expr);
  Strm()->printf(")\n");
  *indent += "  ";
  dump(stmt->child,rt,indent);
  *indent -= "  ";
}

void cdump::dump(const StmtDelay *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%s#(",indent->str());
  dump(stmt->expr);
  Strm()->printf(")\n");
  *indent += "  ";
  dump(stmt->child,rt,indent);
  *indent -= "  ";
}

void cdump::dump(const StmtInit *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%sinitial\n",indent->str());
  *indent += "  ";
  dump(stmt->child,rt,indent);
  *indent -= "  ";
}

void cdump::dump(const StmtAt *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%s@(",indent->str());
  dump(stmt->expr);
  Strm()->printf(")\n");
  *indent += "  ";
  dump(stmt->child,rt,indent);
  *indent -= "  ";
}

void cdump::dump(const StmtIfNone *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%sifnone (",indent->str());
  dump(stmt->expr);
  Strm()->printf(")\n",indent->str());
  *indent += "  ";
  dump(stmt->child_t,rt,indent);
  *indent -= "  ";
  if (stmt->child_f) {
    Strm()->printf("%selse\n",indent->str());
    *indent += "  ";
    dump(stmt->child_f,rt,indent);
    *indent -= "  ";
  }
}

void cdump::dump(const StmtIf *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%sif (",indent->str());
  dump(stmt->expr);
  Strm()->printf(")\n",indent->str());
  *indent += "  ";
  dump(stmt->child_t,rt,indent);
  *indent -= "  ";
  if (stmt->child_f) {
    Strm()->printf("%selse\n",indent->str());
    *indent += "  ";
    dump(stmt->child_f,rt,indent);
    *indent -= "  ";
  }
}

void cdump::dump(const StmtFork *stmt,const ContextObj *rt,String *indent)
{
  *indent += "  ";
  if (NULL_REF(stmt->name))
    Strm()->printf("%sfork\n",indent->str());
  else {
    Strm()->printf("%sfork : %s\n",indent->str(),strDump(stmt->name));
    if (stmt->lcls) {
      Attr *parm;
      if ((parm = stmt->lcls->parm()->first())) {
        String indent2(",\n            ");
        indent2 += indent->str();
        Strm()->printf("%s  parameter ",indent->str());
        dump(parm,indent2.str(),AM_TYPE_OK);
        Strm()->printf(";\n");
      }
      dump_ports(stmt->lcls,indent);
    }
  }
  dump(stmt->child,rt,indent);
  *indent -= "  ";
  Strm()->printf("%sjoin\n",indent->str());
}

void cdump::dump(const StmtBlock *stmt,const ContextObj *rt,String *indent)
{
  if (NULL_REF(stmt->name)) {
    Strm()->printf("%sbegin\n",indent->str());
    *indent += "  ";
  } else {
    Strm()->printf("%sbegin : %s\n",indent->str(),strDump(stmt->name));
    *indent += "  ";
    if (stmt->lcls) {
      Attr *parm;
      if ((parm = stmt->lcls->parm()->first())) {
        String indent2(",\n            ");
        indent2 += indent->str();
        Strm()->printf("%s  parameter ",indent->str());
        dump(parm,indent2.str(),AM_TYPE_OK);
        Strm()->printf(";\n");
      }
      dump_ports(stmt->lcls,indent);
    }
  }
  dump(stmt->child,rt,indent);
  *indent -= "  ";
  Strm()->printf("%send\n",indent->str());
}

void cdump::dump(const StmtSpec *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%sspecify\n",indent->str());
  *indent += "  ";
  if (stmt->lcls) {
    Attr *parm;
    if ((parm = stmt->lcls->parm()->first())) {
      String indent2(",\n            ");
      indent2 += indent->str();
      Strm()->printf("%s  specparam ",indent->str());
      dump(parm,indent2.str(),AM_TYPE_OK);
      Strm()->printf(";\n");
    }
  }
  if (stmt->child) {
    dump(stmt->child,rt,indent);
  }
  *indent -= "  ";
  Strm()->printf("%sendspecify\n",indent->str());
}

void cdump::dump(const StmtFunc *stmt,const ContextObj *rt,String *indent)
{
  dump(funcObj(stmt),rt,indent,true);
}

void cdump::dump(const StmtFuncA *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%sanalog",indent->str());
  dump(funcObj(stmt),rt,indent,false);
}

void cdump::dump(const StmtQC *stmt,const ContextObj *rt,String *indent)
{

}

void cdump::dump(const StmtTask *stmt,const ContextObj *rt,String *indent)
{
  dump(taskObj(stmt),stmt,rt,indent);
}

void cdump::dump(const StmtAnalog *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%sanalog\n",indent->str());
  *indent += "  ";
  dump(stmt->child,rt,indent);
  *indent -= "  ";
}

void cdump::dump(const StmtExpr *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%s",indent->str());
  dump(stmt->expr);
  Strm()->printf(";\n");
}

void cdump::dump(const StmtDefparam *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%s defparam ",indent->str());
  dump(stmt->expr);
  Strm()->printf(";\n");
}

void cdump::dump(const Stmt *stmt,const ContextObj *rt,String *indent)
{
  if (stmt) {
    int n = 0,
        m = mode;
    for (; stmt ; stmt = stmt->next) {
      if (Flags() & DMP_VERBOSE) {
        Strm()->printf(" // Statement %d\n",n++);
      }
      switch(stmt->stmt_typ()) {
#define STATEMENT(e,t,s) case e: if (s & m) dump((t*)stmt,rt,indent); break;
#include "statement.inc"
      default: assert(0);
      }
    }
  } else {
    Strm()->printf("%s//NULL;\n",indent->str());
  }
}

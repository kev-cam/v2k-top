/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * dump_cpp_rcsid() {return "$Id: dump.cpp,v 1.74 2012/10/16 22:38:45 cvs Exp $";}

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

class dump_ctrl *DumpList;

void const vdump::PrttyPrnt(const char *str,int mode)
{
  switch (mode) {
  case PRTY_INST: Strm()->printf("\n%s",str);
  }
}

#define prtty_prnt(s) { if (s) PrttyPrnt(s,prtty_mode); }

void vdump::dump(const DrvStrnth *ds)
{
  int         i    = 2;
  const char *punc = ",";

  Strm()->printf("(");

  for (; i-- ; ds++, punc=") ") {
    const char *str = "";
    switch (ds->strnth) {
    case DS_HIGHZ:  str = "highz";  break;
    case DS_WEAK:   str = "weak";   break;
    case DS_PULL:   str = "pull";   break;
    case DS_STRONG: str = "strong"; break;
    case DS_SUPPLY: str = "supply"; break;
    default:;
    }
    Strm()->printf("%s%d%s",str,ds->lvl,punc);
  }
}


void vdump::dump(const Expr *expr)
{
# define MAPS
# define ROOT          vd()->root
# define XTRA_ARG
# define STRM_ARG
# define DUMP(x,s)     dump(x)
# define VFN_EXPR(vfn) vfn->val()->expr
# define KEEP_DEFS
# define FIND_GEN(nm)  findGen(nm)
# include "dump_expr.inc"
}

void vdump::dump(const ValueFn *vfn,VlType typ,void *xdf,void *xtnd)
{
#include "dump_value.inc"
}

void vdump::dump(const Nature *nat)
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

void vdump::dump(const Disc *disc)
{
  poolRef nm;

  if (!Strm()) setStrm(STDOUT_FILENO);

  Strm()->printf("\ndiscipline %s",strDump(disc->name));
  if (disc->parent >= 0) {
    Strm()->printf(" : %s",strDump(disc_map(disc->parent)->name));
  }
  Strm()->printf("\n");

  if ((disc->flags & DF_AD) != DF_AD) {
    Strm()->printf("  domain %s;\n",(disc->flags & DF_DIGITAL) ? "discrete"
		                                               : "continuous");
  }

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

void vdump::dump(const Range *rng)
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

void vdump::dump(const Attr *attr,const char *all,poolRef obj,eAttrMode mode)
{
  const Attr *scan = attr;
  const char *punc = "",
             *np,
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
    const char      *rti  = "",
                    *vp   = 0;
    VlType           at   = ATTR_VT(scan->typ),
                     et   = at;

    if (scan->typ & ATTR_USR) {
      nm  = au->name();
      switch (at) {
      default:        if (au->hasValue()) {
                        vp = au->strValue(&value);
	              }
	              break;
      case VT_EXPR:   et  = VL_TYPE((expr = au->expr())->typ);   break;
      }
    } else {
      nm  = scan->name();
      switch (at) {
      default:        if (scan->hasValue()) {
                        vp = scan->strValue(&value);
	              }
	              break;
      case VT_EXPR:   et  = VL_TYPE((expr = scan->expr())->typ); break;
      }
    }

    switch (et) {
      case VT_DOUBLE: if (mode & AM_REAL_OK) rti = "real ";    break;
      case VT_U64:    if (mode & AM_TIME_OK) rti = "time ";    break;
      case VT_INT:    if (mode & AM_INT_OK)  rti = "integer "; break;

      case VT_LOGIC:  if (at == ATTR_LOGIC) {
                        if (VT_ALIAS != au2->value2.expr->typ) {
                          Strm()->printf("[");
                          dump(au2->value2.expr);
                          Strm()->printf("] ");
	                }
                      }
      default:;
    }

    if (expr) {
      vp  = expr->strValue(&value);
    }
    if (SPECIAL_POOL == nm.pool) {
      ass = np = "";
    } else {
      np  = strDump(scan->name());
    }
    if (vp) {
      Strm()->printf("%s%s%s%s%s%s",punc,rti,onm.str(),np,ass,vp);
    } else {
      Strm()->printf("%s%s%s%s",    punc,rti,onm.str(),np);
    }
    if (scan->typ & ATTR_RNGD) {
      dump(((RngdAttr *)scan)->rng_lst);
    }
    dump_attr(scan->attr," ","");
    punc  = all;
  } while (all && (scan = scan->next));

  if (punc == all) {
    Strm()->printf("%s",all);
  }
}

void vdump::dump(const Attr *attr,const char *all,eAttrMode mode)
{
  dump(attr,all,NullRef,mode);
}

void vdump::dump(const Branch *br,const char *all)
{
  const Branch *scan = br;
  const char   *punc = "";
  int           c;

  do {
    if (!scan->name) {
      punc = " /* ";
    }
    Strm()->printf("%s(",punc);
    dump(scan->p1);
    if (scan->p2) {
      Strm()->printf(",");
      dump(scan->p2);
    }
    Strm()->printf(")");
    if (scan->name) {
      punc = " ";
      for (c=0;scan->name[c].pool;c++,punc=",") {
        Strm()->printf("%s%s",punc,strDump(scan->name[c]));
      }
    } else {
      punc = "";
      Strm()->printf(" */");
    }
    punc = all;
  } while (all && (scan = scan->next));
}

int vdump::dump_ports(const PrtdObj *po ,DpMode mode,
                      const char *punc0,const char *indent)
{
  PortDir       ret  = PRT_NC;
  const Port   *port = po->port()->first();
  int           n    = 0,
                hd_r = 0,
                hd_d = 0,
                diff = 1,
                pt;
  Attr         *attr;
  String        str;
  const char   *sp   = "",
               *punc = punc0;

  for (; port ; port = port->next) {
    if ((pt = port->ptyp) & PT_SIGNED) {
      if (!*sp) diff = 1;
      sp = " signed";
      pt = port->ptyp & ~PT_SIGNED;
    } else {
      if (*sp)  diff = 1;
      sp = "";
    }
    str   = "";
    if (pt != PT_ALIAS) {
      int r = 0;
      for (; r < port->pckd ; r++) {
        if ((port->rng[r] && (diff || VT_ALIAS != port->rng[r]->typ))) {
          StringStream ss(&str);
          vdump        vdmp(&ss);
          if ((hd_r = (0 != port->rng))) {
            ss.printf("[");
            vdmp.dump(port->rng[r]);
            ss.printf("] ");
          }
          diff = 1;
        }
      }
    }
    if (port->dly && VT_ALIAS != port->dly->typ) {
      if ((hd_d = (0 != port->dly))) {
        StringStream ss(&str);
        vdump        vdmp(&ss);
        ss.printf("#");
        vdmp.dump(port->dly);
        ss.printf(" ");
      }
    }
    if (!diff && ((hd_r && !port->rng) ||
                  (hd_d && !port->dly))) {
      diff = 1;
      str  = "";
    }
    switch (mode) {
    case DM_EXTERNAL: if (pt == PT_ALIAS) {
                        Strm()->printf("%s%s",indent,punc);
                        dump(port->rng[0]);
                        goto skip_attrs;
                      }
                      if (port->io & PRT_IO_LSTD) {
                        Strm()->printf("%s%s%s",indent,punc,strDump(port->name));
                        goto do_attrs;
                      }
                      break;
    case DM_HIER:     if (port->io & (PRT_HIER|PRT_XPRTD)) {
                        Strm()->printf("%s%s%c%s",indent,punc,
                                       port->io & PRT_XPRTD ? '-'
                                                            : '+',
                                       strDump(port->name));
                        goto do_attrs;
                      }
                      break;
    case DM_INOUT:    if ((port->io & PRT_OUT) &&
			  (port->io & PRT_IN))   goto dmp_port;
                      break;
    case DM_OUTPUT:   if ((port->io & PRT_OUT) &&
			  !(port->io & PRT_IN))  goto dmp_port;
                      break;
    case DM_INPUT:    if ((port->io & PRT_IN)  &&
			  !(port->io & PRT_OUT)) goto dmp_port;
                      break;
                    dmp_port:
                     if (diff) {
                        Strm()->printf("%s%s%s %s %s%s",
                                 n ? ";\n" : "",
                                 punc0,sp,str.str(),indent,strDump(port->name));
                        diff = 0;
                      } else {
                        Strm()->printf("%s%s%s",
                                       indent,punc,strDump(port->name));
                      }
                      goto do_attrs;
    case DM_DISC:     if (port->dsc_indx >= 0 && !(port->io & PRT_DSC_DFLTD)) {
                        Strm()->printf("  %s %s;\n",
                                   strDump(po->disc_map(port->dsc_indx)->name),
                                   strDump(port->name));
                        n++;
                      }
                      break;
    case DM_REG:      pt = pt & ~PT_SIGNED;
                      if (pt == PT_REG) {
                        if (diff) {
                          Strm()->printf("%s%s %s %s%s%s",
				   n ? ";\n" : "",
                                   punc0,sp,str.str(),
                                   indent,strDump(port->name));
                          diff = 0;
                        } else {
                          Strm()->printf("%s%s%s",
                                       indent,punc,strDump(port->name));
                        }
                        goto do_attrs;
                      }
                      break;
    case DM_WIRE:     if (pt == PT_WIRE) {
                        if (diff) {
			  Strm()->printf("%s%s",
			                 n ? ";\n" : "",
                                         punc0);
                          if (port->ds0) {
                            DrvStrnth ds[2];
                            ds[0].lvl = 0; ds[0].strnth = port->ds0;
                            ds[1].lvl = 1; ds[1].strnth = port->ds1;
                            dump(ds);
                          }
                          Strm()->printf("%s %s %s%s",
                                   sp,str.str(),indent,strDump(port->name));
                          diff = 0;
                        } else {
                          Strm()->printf("%s%s%s",
                                   indent,punc,strDump(port->name));
                        }
                        goto do_attrs;
                      }
                      break;
    case DM_REAL:     if (pt == PT_REAL) {
                        Strm()->printf("%s%s%s",indent,punc,strDump(port->name));
                        goto do_attrs;
                      }
                      break;
    case DM_INTEGER:  if (pt == PT_INT) {
                        Strm()->printf("%s%s%s%s",indent,punc,str.str(),strDump(port->name));
                        goto do_attrs;
                      }
                      break;
    case DM_GENVAR:   if (pt == PT_GENVAR) {
                        Strm()->printf("%s%s%s",indent,punc,strDump(port->name));
                        goto do_attrs;
                      }
                      break;
    case DM_TIME:     if (pt == PT_TIME) {
                        Strm()->printf("%s%s%s",indent,punc,strDump(port->name));
                        goto do_attrs;
                      }
                      break;
    case DM_EVENT:    if (pt == PT_EVENT) {
                        Strm()->printf("%s%s%s",indent,punc,strDump(port->name));
                        goto do_attrs;
                      }
                      break;
    default:;
    }
    continue;

  do_attrs:
    if (port->unpckd) {
      int r = 0;
      for (; r < port->unpckd; r++) {
        Strm()->printf("[");
        dump(port->rng[r + port->pckd]);
        Strm()->printf("]");
      }
    }
  skip_attrs:
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

int vdump::dump_ports(const PrtdObj *po,DpMode mode,const char *punc)
{
  return dump_ports(po,mode,punc,"");
}

int vdump::dump_ports(const PrtdObj *po ,DpMode mode)
{
  return dump_ports(po,mode,"","");
}

void vdump::dump_ports(const PrtdObj *po,String *indent)
{
  if (dump_ports(po,DM_REG,   "  reg "))    Strm()->printf(";\n");

  dump_ports(po,DM_DISC);

  if (dump_ports(po,DM_REAL,   "  real "))    Strm()->printf(";\n");
  if (dump_ports(po,DM_INTEGER,"  integer ")) Strm()->printf(";\n");
}

void vdump::dump(const Module *mod)
{
  if (!Strm()) setStrm(STDOUT_FILENO);

  Strm()->printf("\n%smodule %s",mod->macro ? "macro"
                                          : "",
                               strDeref(mod->name));

  if (mod->io & PRT_INOUT) {
    Strm()->printf("(");
    dump_ports(mod,DM_EXTERNAL);
    Strm()->printf(")");
  }
  Strm()->printf(";\n");

  dump_attr(mod->attr_list,"  ","\n");

  if (mod->parm()->first()) {
    Strm()->printf("  parameter ");
    dump(mod->parm()->first(),",\n            ",AM_TYPE_OK);
    Strm()->printf(";\n");
  }

  if (dump_ports(mod,DM_INOUT, "  inout "))  Strm()->printf(";\n");
  if (dump_ports(mod,DM_INPUT, "  input "))  Strm()->printf(";\n");
  if (dump_ports(mod,DM_OUTPUT,"  output ")) Strm()->printf(";\n");
  if (dump_ports(mod,DM_REG,   "  reg "))    Strm()->printf(";\n");

  if (!(Flags() & DMP_NO_ANALOG)) {
    dump_ports(mod,DM_DISC);
  }

  if (dump_ports(mod,DM_REAL,   "  real "))    Strm()->printf(";\n");
  if (dump_ports(mod,DM_INTEGER,"  integer ")) Strm()->printf(";\n");
  if (dump_ports(mod,DM_WIRE,   "  wire "))    Strm()->printf(";\n");
  if (dump_ports(mod,DM_GENVAR, "  genvar "))  Strm()->printf(";\n");
  if (dump_ports(mod,DM_TIME,   "  time "))    Strm()->printf(";\n");
  if (dump_ports(mod,DM_EVENT,  "  event "))   Strm()->printf(";\n");

  if (Flags() & DMP_VERBOSE) {
    if (dump_ports(mod,DM_HIER, "  /* "))    Strm()->printf(" */\n");

    int      u  = 0;
    Unknown *pu;
    while ((pu = mod->unknown_map(u++))) {
      Strm()->printf("  // ? %3d: %s\n",u,strDeref(pu->ref));
    }
  }

  if (mod->branch()->first() && !(Flags() & DMP_NO_ANALOG)) {
    Strm()->printf("  branch ");
    dump(mod->branch()->first(),",\n         ");
    Strm()->printf(";\n");
  }

  if (mod->stmts) {
    String indent("  ");
    dump(mod->stmts,mod,&indent);
  }

  Strm()->printf("endmodule\n");
}

void vdump::dump(const Prim *prim)
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

void vdump::dump(const Func *func,const ContextObj *rt,String *indent,bool use_indent)
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
  default:;
  }

  Strm()->printf("%s",strDump(func->name));

  if (func->glob && (func->io & PRT_INOUT)) {
    Strm()->printf("(");
    dump_ports(func,DM_EXTERNAL);
    Strm()->printf(")");
  }
  Strm()->printf(";\n");

  if (dump_ports(func,DM_INPUT,  "  input "))   Strm()->printf(";\n");
  if (dump_ports(func,DM_OUTPUT, "  output "))  Strm()->printf(";\n");

  dump_ports(func,DM_DISC);

  if (dump_ports(func,DM_REAL,   "  real "))    Strm()->printf(";\n");
  if (dump_ports(func,DM_INTEGER,"  integer ")) Strm()->printf(";\n");
  if (dump_ports(func,DM_TIME,   "  time "))    Strm()->printf(";\n");

  if (func->parm()->first()) {
    Strm()->printf("  parameter ");
    dump(func->parm()->first(),",\n            ",AM_TYPE_OK);
    Strm()->printf(";\n");
  }

  if (func->stmts) {
    String indent("  ");
    dump(func->stmts,rt,&indent);
  }

  Strm()->printf("%sendfunction\n",indent->str());
}

void vdump::dump(const Task *task,const Stmt *stmt,
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
  if (dump_ports(task,DM_TIME,   "  time "))    Strm()->printf(";\n");

  if (task->parm()->first()) {
    Strm()->printf("  parameter ");
    dump(task->parm()->first(),",\n            ",AM_TYPE_OK);
    Strm()->printf(";\n");
  }

  dumpUsage(stmt,indent);

  if (task->stmts) {
    *indent += "  ";
    dump(task->stmts,rt,indent);
    *indent -= "  ";
  }

  Strm()->printf("%sendtask\n",indent->str());
}

void vdump::dump(const StmtEvent *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%s-> ",indent->str());
  dump(stmt->expr);
  Strm()->printf(";\n");
}

void vdump::dump(const StmtDisable *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%sdisable ",indent->str());
  dump(stmt->expr);
  Strm()->printf(";\n");
}

void vdump::dump(const StmtRelease *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%srelease ",indent->str());
  dump(stmt->expr);
  Strm()->printf(";\n");
}

void vdump::dump(const StmtDeassign *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%sdeassign ",indent->str());
  dump(stmt->expr);
  Strm()->printf(";\n");
}

void vdump::dump(const StmtForce *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%sforce ",indent->str());
  dumpUsage(stmt,indent);
  dump(stmt->expr);
  Strm()->printf(";\n");
}

void vdump::dump(const StmtAssign *stmt,const ContextObj *rt,String *indent)
{
  if (!(Flags() & DMP_NO_DIGITAL)) {
    dumpUsage(stmt,indent);
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
}

void vdump::dump(const StmtWhile *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%s while (",indent->str());
  dump(stmt->expr);
  Strm()->printf(")\n");
  *indent += "  ";
  dump(stmt->child,rt,indent);
  *indent -= "  ";
}

void vdump::dump_attr(const Attr *attr_list,const char *indent,const char *punc)
{
  if (attr_list) {
    Strm()->printf("%s(* ",indent);
    dump(attr_list,",",AM_UNTYPED);
    Strm()->printf(" *)%s",punc);
  }
}

void vdump::dump(const StmtAlways *stmt,const ContextObj *rt,String *indent)
{
  if (!(Flags() & DMP_NO_DIGITAL)) {
    const char *ind = indent->str();
    dump_attr(stmt->attr_list,ind,"\n");
    Strm()->printf("%salways\n",ind);
    *indent += "  ";
    dumpUsage(stmt,indent);
    dump(stmt->child,rt,indent);
    *indent -= "  ";
  }
}

void vdump::dump(const StmtWait *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%swait (",indent->str());
  dump(stmt->expr);
  Strm()->printf(")\n");
  *indent += "  ";
  dump(stmt->child,rt,indent);
  *indent -= "  ";
}

void vdump::dump(const StmtForever *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%sforever\n",indent->str());
  *indent += "  ";
  dump(stmt->child,rt,indent);
  *indent -= "  ";
}

void vdump::dump_case(const StmtCase *stmt,const ContextObj *rt,
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

void vdump::dump(const StmtCase *stmt,const ContextObj *rt,String *indent)
{
  dump_case(stmt,rt,indent,"case");
}

void vdump::dump(const StmtCaseX *stmt,const ContextObj *rt,String *indent)
{
  dump_case(stmt,rt,indent,"casex");
}

void vdump::dump(const StmtCaseZ *stmt,const ContextObj *rt,String *indent)
{
  dump_case(stmt,rt,indent,"casez");
}

void vdump::dump(const StmtInst *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%s%s ",indent->str(),strDump(stmt->name));

  if (!NULL_REF(stmt->drv)) {
  }
  if (stmt->udp & IUDP_DRV) {
    dump(stmt->ds);
  }
  if (stmt->param) {
    Strm()->printf("#(");
    dump(stmt->param);
    Strm()->printf(") ");
  }
  if (stmt->inst) {
    int pm = Prtty(PRTY_INST);
    dump(stmt->inst);
    Prtty(pm);
  }
  Strm()->printf(";\n");
}

genVal *vdump::findGen(poolRef trg)
{
  genVal *scan = gen_stck;

  while (scan && !SAME_REF(scan->nm,trg)) {scan = scan->next;}

  return scan;
}

void vdump::dump(const StmtGen *stmt,const ContextObj *rt,String *indent)
{
  if (flags & DMP_AG_UNROLL) {
    const Expr *l = stmt->expr->Left(),
               *r = stmt->expr->Right(),
               *c = 0;
    if (l->vtx() & r->vtx() & VTX_CONST) {
      genVal tmp;
      tmp.genvar.set_bx(BX_INTEGER);
      tmp.genvar.pval()->i = 0;
      tmp.nm   = stmt->cntr;
      genStckPush(&tmp);
      *indent += "  ";
      if (r->op() == OPR_LIST1) {
        c = r->Right();
        r = r->Left();
      }
      int il = l->i32(),
	  ir = r->i32(),
          s;
      if (c) {
        s  = c->i32();
      } else {
        s  = (ir > il) ?  1
                       : -1;
      }
      for (tmp.genvar.pval()->i  = il;
           tmp.genvar.pval()->i <= ir;
           tmp.genvar.pval()->i += s) {
        Strm()->printf("%s// generated %s = %d\n",
                       indent->str() +2,strDump(stmt->cntr),
                                        tmp.genvar.pval()->i);
        dump(stmt->child,rt,indent);
      }
      *indent -= "  ";
      genStckPop();
      return;
    }
  }
 no_unroll:
  Strm()->printf("%sgenerate %s (",indent->str(),strDump(stmt->cntr));
  dump(stmt->expr);
  Strm()->printf(")\n");
  *indent += "  ";
  dump(stmt->child,rt,indent);
  *indent -= "  ";
}

void vdump::dump(const StmtFor *stmt,const ContextObj *rt,String *indent)
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

void vdump::dump(const StmtRepeat *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%srepeat(",indent->str());
  dump(stmt->expr);
  Strm()->printf(")\n");
  *indent += "  ";
  dump(stmt->child,rt,indent);
  *indent -= "  ";
}

void vdump::dump(const StmtDelay *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%s#(",indent->str());
  dump(stmt->expr);
  Strm()->printf(")\n");
  *indent += "  ";
  dump(stmt->child,rt,indent);
  *indent -= "  ";
}

void vdump::dump(const StmtInit *stmt,const ContextObj *rt,String *indent)
{
  if (!(Flags() & DMP_NO_DIGITAL)) {
    Strm()->printf("%sinitial\n",indent->str());
    *indent += "  ";
    dumpUsage(stmt,indent);
    dump(stmt->child,rt,indent);
    *indent -= "  ";
  }
}

void vdump::dump(const StmtAt *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%s@(",indent->str());
  dump(stmt->expr);
  Strm()->printf(")\n");
  *indent += "  ";
  dump(stmt->child,rt,indent);
  *indent -= "  ";
}

void vdump::dump(const StmtIfNone *stmt,const ContextObj *rt,String *indent)
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

void vdump::dump(const StmtIf *stmt,const ContextObj *rt,String *indent)
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

void vdump::dump(const StmtFork *stmt,const ContextObj *rt,String *indent)
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

void vdump::dump(const StmtBlock *stmt,const ContextObj *rt,String *indent)
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

void vdump::dump(const StmtSpec *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%sspecify\n",indent->str());
  *indent += "  ";
  if (stmt->lcls) {
    Attr *parm;
    if ((parm = stmt->lcls->parm()->first())) {
      String indent2(",\n            ");
      indent2 += indent->str();
      Strm()->printf("%s  specparam ",indent->str());
      dump(parm,indent2.str(),AM_UNTYPED);
      Strm()->printf(";\n");
    }
  }
  if (stmt->child) {
    dump(stmt->child,rt,indent);
  }
  *indent -= "  ";
  Strm()->printf("%sendspecify\n",indent->str());
}

void vdump::dump(const StmtFunc *stmt,const ContextObj *rt,String *indent)
{
  dump(funcObj(stmt),rt,indent,true);
}

void vdump::dump(const StmtFuncA *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%sanalog",indent->str());
  dump(funcObj(stmt),rt,indent,false);
}

void vdump::dump(const StmtQC *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("`language \"%s\" %s\n`endlanguage\n",
                 strDeref(stmt->lang),stmt->str->str());
}

void vdump::dump(const StmtTask *stmt,const ContextObj *rt,String *indent)
{
  dump(taskObj(stmt),stmt,rt,indent);
}

void vdump::dump(const StmtAnalog *stmt,const ContextObj *rt,String *indent)
{
  if (!(Flags() & DMP_NO_ANALOG)) {
    Strm()->printf("%sanalog\n",indent->str());
    *indent += "  ";
    dumpUsage(stmt,indent);
    dump(stmt->child,rt,indent);
    *indent -= "  ";
  }
}

void vdump::dump(const StmtExpr *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%s",indent->str());
  dump(stmt->expr);
  dump_attr(stmt->attr_list," "," ");
  Strm()->printf(";\n");
}

void vdump::dump(const StmtDefparam *stmt,const ContextObj *rt,String *indent)
{
  Strm()->printf("%s defparam ",indent->str());
  dump(stmt->expr);
  Strm()->printf(";\n");
}

static const char *usage[] = {"Read",
                              "Driven",
                              "Sense",
                              "Level",
                              "Probe",
                              "Contrib",
                              "A-Read",
                              "Value",
                              0};

void vdump::dumpUsage(const Stmt *stmt,String *indent)
{
  int n = stmt->usage()->count();

  if (n) {
    while (n--) {
      Usage   *pu   = stmt->usage_map(n);
      if (Flags() & DMP_USAGE) {
        if (Flags() & DMP_VERBOSE) {
          Strm()->printf("%s// %08x ",indent->str(),pu->xpr);
        } else {
          Strm()->printf("%s// ",indent->str());
        }
	dump(pu->xpr);
	const char *punc = ":\t";
	int         u    = pu->use,
	            i    = 0,
	            m    = 1;
	for (; u ; i++, m <<= 1) if (u & m) {
	  u &= ~m;
	  Strm()->printf("%s%s",punc,usage[i]);
	  punc = ",";
        }
        Strm()->printf("\n");
      }
    }
  }
}

#if DBGLVL > 0
void dumpUsage(const Stmt *stmt)
{
  vdump  tmp;
  String indent;

  tmp.setStrm(STDERR_FILENO)->printf(
                       "\n// Usage (%d)\n",stmt->usage()->mode());
  tmp.dumpUsage(stmt,&indent);
}
#endif

void vdump::dump(const Stmt *stmt,const ContextObj *rt,String *indent)
{
  if (stmt) {
    int n;
    for (n = 0; stmt ; stmt = stmt->next) {
      if (Flags() & DMP_VERBOSE) {
        Strm()->printf(" // Statement %d\n",n++);
      }
      if (Flags() & DMP_LN_DRCTV) {
        Strm()->printf("`line %d \"%s\" %d\n",
                       stmt->line,stmt->File(rt),stmt->level);
      }
      switch(stmt->stmt_typ()) {
#define STATEMENT(e,t,m) case e: dump((t*)stmt,rt,indent); break;
#include "statement.inc"
      default: assert(0);
      }
    }
  } else {
    Strm()->printf("%s/*NULL*/;\n",indent->str());
  }
}

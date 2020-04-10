/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  fxd_pool_h_rcsid
#define fxd_pool_h_rcsid() {return "$Id: fxd_pool.h,v 1.5 2007/11/08 02:01:00 dkc Exp $";} /* RCS ID */

#ifndef SKIP_NULL_POOL
FXD_POOL(NLL_, Null,       NULL,       null)
#endif
FXD_POOL(WHT_, Whitespace, WHITESPACE, whitespace)
FXD_POOL(QUT_, Quotes,     QUOTES,     quotes)
FXD_POOL(ESC_, Esc,        ESC,	       esc)
FXD_POOL(PUNC_,Punctuation,PUNCTUATION,punctuation)
FXD_POOL(CMT_, Comments,   COMMENTS,   comments)
FXD_POOL(OP_,  Operators,  OPERATORS,  operators)
FXD_POOL(MTH_, Math,       MATH,       math)
FXD_POOL(VPP,  Vpp,        VPP,        vpp)
FXD_POOL(VER_, Verilog,    VERILOG,    verilog)
FXD_POOL(FNC_, Func,       FUNC,       func)
FXD_POOL(TSK_, Tasks,      TASKS,      tasks)
FXD_POOL(SFN_, Sysfunc,    SYSFUNC,    sysfunc)
FXD_POOL(VF_,  Acctfvpi,   ACCTFVPI,   acctfvpi)
FXD_POOL(INT_, Integer,    INTEGER,    integer)
FXD_POOL(FLT_, Float,      FLOAT,      float)
FXD_POOL(GLB_, Global,     GLOBAL,     global)
FXD_POOL(CHR_, Char,       CHAR,       char)
FXD_POOL(SPC,  Special,    SPECIAL,    special)
FXD_POOL(SIG_, Signal,     SIGNAL,     signal)
FXD_POOL(ANL_, Analysis,   ANALYSIS,   analysis)
FXD_POOL(SDF_, Sdf,        SDF,        sdf)
FXD_POOL(TIM_, Timing,     TIMING,     timing)
FXD_POOL(SHL_, Commands,   COMMANDS,   commands)
FXD_POOL(EDGE_,Edges,      EDGES,      edges)
FXD_POOL(VHD_, Vhdl,       VHDL,       vhdl)
FXD_POOL(PRP_, Prp,        PRP,        prp)
FXD_POOL(CPP_, Cpp,        CPP,        cpp)
FXD_POOL(PRC_, Prc,        PRC,        prc)
FXD_POOL(COP_, Cppops,     CPPOPS,     cppops)
FXD_POOL(PCO_, Prcops,     PRCOPS,     prcops)
FXD_POOL(GNU_, Gnu,        GNU,        gnu)
FXD_POOL(LBL_, Labels,     LABELS,     labels)
FXD_POOL(BLT_, Builtin,    BUILTIN,    builtin)
FXD_POOL(SYSC_,SystemC,    SYSTEMC,    systemc)

#undef FXD_POOL

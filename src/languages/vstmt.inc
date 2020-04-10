/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: $Id: vstmt.inc,v 1.14 2007/02/01 06:49:08 dkc Exp $ */
  


inline VStmt()                 {mod =  0; index = -1; NEW_XTRA};
inline VStmt(Module *mp)       {mod = mp; index = -1; NEW_XTRA};
inline VStmt(Module *mp,int n) {mod = mp; index =  n; NEW_XTRA};
#undef VStmt
#undef NEW_XTRA

virtual eSTMT   	stmt_typ()				VSTMT_VMODE;
virtual bool    	set_mod(Inst *)				VSTMT_VMODE;
virtual int     	inst_count()				VSTMT_VMODE;
virtual poolRef 	inst_name(int,int *,int *,Inst *)	VSTMT_VMODE;
virtual int     	parm_count()				VSTMT_VMODE;
virtual poolRef 	parm_name(int)				VSTMT_VMODE;
virtual int     	parm_val(Inst *,int,Expr *,poolRef *)	VSTMT_VMODE;
virtual poolRef 	name()					VSTMT_VMODE;
virtual void            evalDfPrm(Inst *)			VSTMT_VMODE;
virtual void            reportPosn()				VSTMT_VMODE;
virtual int             stmtFileId()     			VSTMT_VMODE;
virtual int             stmtLine()     				VSTMT_VMODE;

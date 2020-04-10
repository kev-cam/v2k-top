/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */

#undef  objtyp_h_rcsid
#define objtyp_h_rcsid() {return "$Id: objtyp.h,v 1.11 2007/03/28 07:23:26 dkc Exp $";} /* RCS ID */

 
OT_DECL(OBJ_Null,      "",   "null",      FT_Null)
OT_DECL(OBJ_TypeSep,   "-",  "",          FT_Null)
OT_DECL(OBJ_Module,    "mod","module",    FT_Module)
OT_DECL(OBJ_Function,  "fnc","function",  FT_Null)
OT_DECL(OBJ_Task,      "tsk","task",      FT_Null)
OT_DECL(OBJ_Primitive, "prm","primitive", FT_Udp)
OT_DECL(OBJ_Nature,    "ntr","nature",    FT_Null)
OT_DECL(OBJ_Discipline,"dsc","discipline",FT_Null)

#undef OT_DECL

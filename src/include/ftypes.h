/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  ftypes_h_rcsid
#define ftypes_h_rcsid() {return "$Id: ftypes.h,v 1.19 2007/11/14 07:41:15 dkc Exp $";} /* RCS ID */
 
FT_DECL(FT_Null,   "")
FT_DECL(FT_Dir,    OS_DIR_SEP)
FT_DECL(FT_Token,  "tok")
FT_DECL(FT_Inter,  "i")
FT_DECL(FT_Verilog,"vl")
FT_DECL(FT_String, "str")
FT_DECL(FT_Gzip,   "gz")
FT_DECL(FT_Zip,    "zip")
FT_DECL(FT_Cmprssd,"Z")
FT_DECL(FT_Module, "mod")
FT_DECL(FT_Udp,    "udp")
FT_DECL(FT_Deps,   "dep")
FT_DECL(FT_Cdf,    "cdf")
FT_DECL(FT_Cpp,    "cpp")
FT_DECL(FT_Pc,     "pc")
FT_DECL(FT_Obj,    "o")

#undef FT_DECL

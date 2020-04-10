/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  av_types_h_rcsid
#define av_types_h_rcsid() {return "$Id: av_types.h,v 1.16 2007/03/28 07:23:26 dkc Exp $";} /* RCS ID */

 
AV_TYPE(VOID,     PRSN_VOID,  "Void")

AV_TYPE(SIGNED,   PRSN_VOID,  "Signed") /* bit flag */
/* Scalar */
AV_TYPE(BOOL, 	  PRSN_BIT,   "Bool")
AV_TYPE(BIT, 	  PRSN_BIT,   "Bit")
AV_TYPE(UINT,	  PRSN_32,    "Unsigned Integer")
AV_TYPE(INT,	  PRSN_32,    "Integer")
AV_TYPE(LUINT,	  PRSN_32,    "Sized Integer")
AV_TYPE(LINT,	  PRSN_32,    "Unsigned Sized Integer")
AV_TYPE(U64,	  PRSN_64,    "64-bit Unsigned Integer")
AV_TYPE(I64,	  PRSN_64,    "64-bit Integer")
AV_TYPE(DOUBLE,	  PRSN_DOUBLE,"Double")

AV_TYPE(TOK,	  PRSN_VOID,  "Token")
AV_TYPE(REF,	  PRSN_VOID,  "Reference")
AV_TYPE(REFL,	  PRSN_VOID,  "Local Reference")
AV_TYPE(REFG,	  PRSN_VOID,  "Global Reference")
AV_TYPE(STRING,	  PRSN_VOID,  "String")
AV_TYPE(POINTER,  PRSN_VOID,  "Pointer")
AV_TYPE(NATURE,	  PRSN_VOID,  "Nature")
AV_TYPE(DISC,	  PRSN_VOID,  "Discipline")
AV_TYPE(EXPR,	  PRSN_VOID,  "Expresion")
AV_TYPE(RANGE,    PRSN_SLICE, "Range")

AV_TYPE(PAD,      PRSN_VOID,  0 /* padding */)
/* Ordered */
AV_TYPE(DONTCARE, PRSN_LOGIC, "Don't Care")
AV_TYPE(NC,       PRSN_LOGIC, "Not Connected")
AV_TYPE(HIGHZ,    PRSN_LOGIC, "High Impedance")
AV_TYPE(UNKNOWN,  PRSN_LOGIC, "Unknown")
/* Scalar too */
AV_TYPE(ULOGIC,	  PRSN_LOGIC, "Logic")
AV_TYPE(LOGIC,	  PRSN_LOGIC, "Signed Logic")

#ifndef AV_ATTR_ONLY
AV_TYPE(SUB,	  PRSN_VOID,  "Sub-expression")
AV_TYPE(CALL,	  PRSN_VOID,  "Call")
AV_TYPE(ARRAY,	  PRSN_VOID,  "Array")
AV_TYPE(CAT,	  PRSN_VOID,  "Concatenation")
AV_TYPE(INDEX,	  PRSN_VOID,  "Index")
AV_TYPE(LVALUE,	  PRSN_VOID,  "L-Value")
AV_TYPE(SCALAR,	  PRSN_VOID,  "Scalar")
AV_TYPE(REFERENCE,PRSN_VOID,  "Reference")
AV_TYPE(LIST,	  PRSN_VOID,  "List")
AV_TYPE(FLOAT,	  PRSN_FLOAT, "Float")
AV_TYPE(SIZED,	  PRSN_VOID,  "Sized")
AV_TYPE(STRNTH,	  PRSN_VOID,  "Strength")
AV_TYPE(STR_LGC,  PRSN_VOID,  "Strength Logic")
AV_TYPE(NODE,	  PRSN_VOID,  "Node")
AV_TYPE(BRANCH,	  PRSN_VOID,  "Branch")
AV_TYPE(EVENT,	  PRSN_VOID,  "Event")
AV_TYPE(USER,	  PRSN_VOID,  "User")
AV_TYPE(ARG_LIST, PRSN_VOID,  "Argument List")
AV_TYPE(DT_DT_DT, PRSN_VOID,  "Ddt")
AV_TYPE(OPERATOR, PRSN_VOID,  "Operator")
AV_TYPE(ALIAS,	  PRSN_VOID,  "Alias")
AV_TYPE(SIZE,	  PRSN_32,    "Size")
#endif

#undef AV_ATTR_ONLY
#undef AV_TYPE

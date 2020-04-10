/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: $Id: base_expr.inc,v 1.12 2007/02/01 06:49:08 dkc Exp $ */
  


BASE_EXPR(BX_CONST,	BxConst,	VT_VOID,  	VTX_CONST,REF_UNKNOWN,CT_NONE,"Constant")
BASE_EXPR(BX_SIZE, 	BxSize,  	VT_SIZE,  	VTX_NONE, REF_UNKNOWN,CT_NONE,"Size")
BASE_EXPR(BX_BOOL,	BxBool,		VT_BOOL,  	VTX_NONE, REF_UNKNOWN,CT_NONE,"Bool")
BASE_EXPR(BX_UNSGND64,	BxUnsgnd64,	VT_U64,   	VTX_NONE, REF_UNKNOWN,CT_NONE,"Unsigned 64")
BASE_EXPR(BX_LOGIC_INT,	BxLogicInt,	VT_LUINT, 	VTX_NONE, REF_UNKNOWN,CT_NONE,"Sized Integer")
BASE_EXPR(BX_DOUBLE,	BxDouble,	VT_DOUBLE,	VTX_NONE, REF_UNKNOWN,CT_NONE,"Double")
BASE_EXPR(BX_REFERENCE,	BxReference,	VT_VOID,  	VTX_REF,  REF_UNKNOWN,CT_NONE,"Reference")
BASE_EXPR(BX_SCALAR,	BxScalar,	VT_SCALAR,	VTX_NONE, REF_UNKNOWN,CT_NONE,"Scalar")
BASE_EXPR(BX_LVALUE,	BxLvalue,	VT_LVALUE,	VTX_NONE, REF_UNKNOWN,CT_NONE,"L-Value")
BASE_EXPR(BX_STRING,	BxString,	VT_STRING,	VTX_NONE, REF_UNKNOWN,CT_NONE,"String")
BASE_EXPR(BX_SUB,	BxSub,		VT_SUB,   	VTX_NONE, REF_UNKNOWN,CT_NONE,"Sub")
BASE_EXPR(BX_UNSGNDINT,	BxUnsgndInt,	VT_UINT,  	VTX_NONE, REF_UNKNOWN,CT_NONE,"Unsigned 32")
BASE_EXPR(BX_INTEGER,	BxInteger,	VT_INT,   	VTX_NONE, REF_UNKNOWN,CT_NONE,"Integer")
BASE_EXPR(BX_ARRAY,	BxArray,	VT_ARRAY, 	VTX_NONE, REF_UNKNOWN,CT_NONE,"Array")
BASE_EXPR(BX_CAT,	BxCat,		VT_CAT,   	VTX_NONE, REF_UNKNOWN,CT_NONE,"Concatenation")
BASE_EXPR(BX_RANGE,	BxRange,	VT_RANGE,   	VTX_NONE, REF_UNKNOWN,CT_NONE,"Range")
BASE_EXPR(BX_SGND64,	BxSgnd64,	VT_I64,   	VTX_NONE, REF_UNKNOWN,CT_NONE,"Signed 64")
BASE_EXPR(BX_EXPRESSION,BxExpression,	VT_EXPR,  	VTX_NONE, REF_UNKNOWN,CT_NONE,"Expression")
BASE_EXPR(BX_VOID,	BxVoid,		VT_VOID,  	VTX_NONE, REF_UNKNOWN,CT_NONE,"Void")
BASE_EXPR(BX_HIER_REF,	BxHier,		VT_VOID,  	VTX_HIER, REF_UNKNOWN,CT_NONE,"Hierarchical Reference")
BASE_EXPR(BX_LOGIC,	BxLogic,	VT_LOGIC, 	VTX_NONE, REF_UNKNOWN,CT_NONE,"Logic")
BASE_EXPR(BX_UNKNOWN,	BxUnknown,	VT_UNKNOWN,	VTX_NONE, REF_UNKNOWN,CT_NONE,"Unknown")
BASE_EXPR(BX_DONTCARE,	BxDontCare,	VT_DONTCARE,	VTX_NONE, REF_UNKNOWN,CT_NONE,"Don't Care")
BASE_EXPR(BX_NC,	BxUnconnected,	VT_NC,		VTX_NONE, REF_UNKNOWN,CT_NONE,"Unconnected")
BASE_EXPR(BX_NODE,	BxNode,		VT_VOID,  	VTX_REF,  REF_UNKNOWN,CT_NONE,"Node")
BASE_EXPR(BX_LIST,	BxList,		VT_LIST,  	VTX_NONE, REF_UNKNOWN,CT_NONE,"List")
BASE_EXPR(BX_EVENT,	BxEvent,	VT_EVENT, 	VTX_NONE, REF_UNKNOWN,CT_NONE,"Event")
BASE_EXPR(BX_INDEX,	BxIndex,	VT_INDEX, 	VTX_NONE, REF_UNKNOWN,CT_NONE,"Index")
BASE_EXPR(BX_CALL,	BxCall,		VT_CALL,  	VTX_NONE, REF_UNKNOWN,CT_NONE,"Call")
BASE_EXPR(BX_POINTER,	BxPointer,	VT_VOID,  	VTX_NONE, REF_UNKNOWN,CT_NONE,"Pointer")
BASE_EXPR(BX_OPERATOR,	BxOperator,	VT_OPERATOR,	VTX_NONE, REF_UNKNOWN,CT_NONE,"Operator")

#undef BASE_EXPR
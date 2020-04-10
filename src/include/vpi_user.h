/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  vpi_user_h_rcsid
#define vpi_user_h_rcsid() {return "$Id: vpi_user.h,v 1.9 2007/03/28 07:23:26 dkc Exp $";} /* RCS ID */

 
/**********************************************************************
 * vpi_user.h
 *
 * IEEE 1364 1995 Verilog HDL Programming Language Interface (PLI).
 *
 * This file contains the constant definitions, structure definitions,
 * and routine declarations used by the Verilog PLI VPI procedural
 * interface.
 *
 * The file should be included with all C routines that use the VPI
 * routines.
 *********************************************************************/

#ifndef VPI_USER_H
#define VPI_USER_H

/* basic typedefs */

typedef unsigned long *vpiHandle;

/* Following are the constant definitions. They are divided into three    major areas:

   1) object types    2) access methods    3) properties

   Note that most of the object types can also be used as access
   methods, and that some methods can also be used a properties.
 */

/*********** OBJECT TYPES **********/
#define vpiAlways          1       /* always block */
#define vpiAssignStmt      2       /* quasi-continuous assignment */
#define vpiAssignment      3       /* procedural assignment */
#define vpiBegin           4       /* block statement */
#define vpiCase            5       /* case statement */
#define vpiCaseItem        6       /* case statement item */
#define vpiConstant        7       /* numerical constant or literal string */
#define vpiContAssign      8       /* continuous assignment */
#define vpiDeassign        9       /* deassignment statement */
#define vpiDefParam       10       /* defparam */
#define vpiDelayControl   11       /* delay statement (e.g. #10) */
#define vpiDisable        12       /* named block disable statement */
#define vpiEventControl   13       /* wait on event, e.g. e */
#define vpiEventStmt      14       /* event trigger, e.g. ->e */
#define vpiFor            15       /* for statement */
#define vpiForce          16       /* force statement */
#define vpiForever        17       /* forever statement */
#define vpiFork           18       /* fork-join block */
#define vpiFuncCall       19       /* HDL function call */
#define vpiFunction       20       /* HDL function */
#define vpiGate           21       /* primitive gate */
#define vpiIf             22       /* if statement */
#define vpiIfElse         23       /* if-else statement */
#define vpiInitial        24       /* initial block */
#define vpiIntegerVar     25       /* integer variable */
#define vpiInterModPath   26       /* intermodule wire delay */
#define vpiIterator       27       /* iterator */
#define vpiIODecl         28       /* input/output declaration */
#define vpiMemory         29       /* behavioral memory */
#define vpiMemoryWord     30       /* single word of memory */
#define vpiModPath        31       /* module path for path delays */
#define vpiModule         32       /* module instance */
#define vpiNamedBegin     33       /* named block statement */
#define vpiNamedEvent     34       /* event variable */
#define vpiNamedFork      35       /* named fork-join block */
#define vpiNet            36       /* scalar or vector net */
#define vpiNetBit         37       /* bit of vector net */
#define vpiNullStmt       38       /* a semicolon. Ie. #10 ; */
#define vpiOperation      39       /* behavioral operation */
#define vpiParamAssign    40       /* module parameter assignment */
#define vpiParameter      41       /* module parameter */
#define vpiPartSelect     42       /* part select */
#define vpiPathTerm       43       /* terminal of module path */
#define vpiPort           44       /* module port */
#define vpiPortBit        45       /* bit of vector module port */
#define vpiPrimTerm       46       /* primitive terminal */
#define vpiRealVar        47       /* real variable */
#define vpiReg            48       /* scalar or vector register */
#define vpiRegBit         49       /* bit of vector register net */
#define vpiRelease        50       /* release statement */
#define vpiRepeat         51       /* repeat statement */
#define vpiRepeatControl  52       /* repeat control in an assign stmt */
#define vpiSchedEvent     53       /* vpi_put_value() event */
#define vpiSpecParam      54       /* specparam */
#define vpiSwitch         55       /* transistor switch */
#define vpiSysFuncCall    56       /* system function call */
#define vpiSysTaskCall    57       /* system task call */
#define vpiTableEntry     58       /* UDP state table entry */
#define vpiTask           59       /* HDL task */
#define vpiTaskCall       60       /* HDL task call */
#define vpiTchk           61       /* timing check */
#define vpiTchkTerm       62       /* terminal of timing check */
#define vpiTimeVar        63       /* time variable */
#define vpiTimeQueue      64       /* simulation event queue */
#define vpiUdp            65       /* user-defined primitive */
#define vpiUdpDefn        66       /* UDP definition */
#define vpiUserSystf      67       /* user defined system task or function */
#define vpiVarSelect      68       /* variable array selection */
#define vpiWait           69       /* wait statement */
#define vpiWhile          70       /* while statement */

/*********** METHODS ***********/
/*********** methods used to traverse 1 to 1 relationships ***********/
#define vpiCondition      71       /* condition expression */
#define vpiDelay          72       /* net or gate delay */
#define vpiElseStmt       73       /* else statement */
#define vpiForIncStmt     74       /* increment statement in for loop */
#define vpiForInitStmt    75       /* initialization statement in for loop */
#define vpiHighConn       76       /* higher connection to port */
#define vpiLhs            77       /* left-hand side of assignment */
#define vpiIndex          78       /* index of var select, bit select, etc. */
#define vpiLeftRange      79       /* left range of vector or part select */
#define vpiLowConn        80       /* lower connection to port */
#define vpiParent         81       /* parent object */
#define vpiRhs            82       /* right-hand side of assignment */
#define vpiRightRange     83       /* right range of vector or part select */
#define vpiScope          84       /* containing scope object */
#define vpiSysTfCall      85       /* task function call */
#define vpiTchkDataTerm   86       /* timing check data term */
#define vpiTchkNotifier   87       /* timing check notifier */
#define vpiTchkRefTerm    88       /* timing check reference term */

/*********** methods used to traverse 1 to many relationships ***********/
#define vpiArgument       89       /* argument to (system) task or function */
#define vpiBit            90       /* bit of vector net or port */
#define vpiDriver         91       /* driver for a net */
#define vpiInternalScope  92       /* internal scope in module */
#define vpiLoad           93       /* load on net or register */
#define vpiModDataPathIn  94       /* data terminal of a module path */
#define vpiModPathIn      95       /* Input terminal of a module path */
#define vpiModPathOut     96       /* output terminal of a module path */
#define vpiOperand        97       /* operand of expression */
#define vpiPortInst       98       /* connected port instance */
#define vpiProcess        99       /* process in module */
#define vpiVariables     100       /* variables in module */
#define vpiUse           101       /* usage */

/****** methods which can traverse 1-to-1, or 1-to-many relationships ******/
#define vpiExpr          102       /* connected expression */
#define vpiPrimitive     103       /* primitive (gate, switch, UDP) */
#define vpiStmt          104       /* statement in process or task */

/*********** PROPERTIES ***********/
/*********** generic object properties ***********/
#define vpiUndefined        -1     /* undefined property */
#define vpiType              1     /* type of object */
#define vpiName              2     /* local name of object */
#define vpiFullName          3     /* full hierarchical name */
#define vpiSize              4     /* size of gate, net, port, etc. */
#define vpiFile              5     /* File name in which the object is used */
#define vpiLineNo            6     /* File line number where  object is used */

/*********** modules properties **********/
#define vpiTopModule           7   /* top-level module (boolean) */
#define vpiCellInstance        8   /* cell (boolean) */
#define vpiDefName             9   /* module definition name */
#define vpiProtected          10   /* source protected module (boolean) */
#define vpiTimeUnit           11   /* module time unit */
#define vpiTimePrecision      12   /* module time precision */
#define vpiDefNetType         13   /* default net type */
#define vpiUnconnDrive        14   /* unconnected port drive strength */
#define vpiHighZ               1   /* No default drive given */
#define vpiPull1               2   /* default pull1 drive */
#define vpiPull0               3   /* default pull0 drive */
#define vpiDefFile            15   /* File name where the module is defined */
#define vpiDefLineNo          16   /* File line number where module is defined*/
#define vpiDefDelayMode       17   /* Delay mode of the module */
#define vpiDelayModeNone       1   /* No delay mode specified */
#define vpiDelayModePath       2   /* Path delay mode */
#define vpiDelayModeDistrib    3   /* Distributed delay mode */
#define vpiDelayModeUnit       4   /* Unit delay mode */
#define vpiDelayModeZero       5   /* Zero delay mode */
#define vpiDelayModeMTM        6   /* min:typ:max delay mode */
#define vpiDefDecayTime       18   /* Decay time for trireg net */

/*********** port and net properties ***********/
#define vpiScalar             17   /* scalar (boolean) */
#define vpiVector             18   /* vector (boolean) */
#define vpiExplicitName       19   /* port is explicitly named */
#define vpiDirection          20   /* direction of port: */
#define vpiInput               1   /* input */
#define vpiOutput              2   /* output */
#define vpiInout               3   /* inout */
#define vpiMixedIO             4   /* mixed input-output */
#define vpiNoDirection         5   /* no direction */
#define vpiConnByName         21   /* connected by name (boolean) */

#define vpiNetType            22   /* net subtypes: */
#define vpiWire                1   /* wire net */
#define vpiWand                2   /* wire-and net */
#define vpiWor                 3   /* wire-or net */
#define vpiTri                 4   /* tri-state net */
#define vpiTri0                5   /* pull-down net */
#define vpiTri1                6   /* pull-up net */
#define vpiTriReg              7   /* tri state reg net */
#define vpiTriAnd              8   /* tri-state wire-and net */
#define vpiTriOr               9   /* tri-state wire-or net */
#define vpiSupply1            10   /* supply 1 net */
#define vpiSupply0            11   /* supply zero net */


#define vpiExplicitScalared   23   /* explicitly scalared (boolean) */
#define vpiExplicitVectored   24   /* explicitly vectored (boolean) */
#define vpiExpanded           25   /* expanded vector net (boolean) */
#define vpiImplicitDecl       26   /* implicitly declared net (boolean) */
#define vpiChargeStrength     27   /* charge decay strength of net */
#define vpiArray              28   /* variable array (boolean) */

#define vpiPortIndex          29   /* Port index */ /*********** gate and terminal properties ***********/
#define vpiTermIndex          30   /* Index of a primitive terminal */
#define vpiStrength0          31   /* 0-strength of net or gate */
#define vpiStrength1          32   /* 1-strength of net or gate */
#define vpiPrimType           33   /* prmitive subtypes: */
#define vpiAndPrim             1   /* and gate */
#define vpiNandPrim            2   /* nand gate */
#define vpiNorPrim             3   /* nor gate */
#define vpiOrPrim              4   /* or gate */
#define vpiXorPrim             5   /* xor gate */
#define vpiXnorPrim            6   /* xnor gate */
#define vpiBufPrim             7   /* buffer */
#define vpiNotPrim             8   /* not gate */
#define vpiBufif0Prim          9   /* zero-enabled buffer */
#define vpiBufif1Prim         10   /* one-enabled buffer */
#define vpiNotif0Prim         11   /* zero-enabled not gate */
#define vpiNotif1Prim         12   /* one-enabled not gate */
#define vpiNmosPrim           13   /* nmos switch */
#define vpiPmosPrim           14   /* pmos switch */
#define vpiCmosPrim           15   /* cmos switch */
#define vpiRnmosPrim          16   /* resistive nmos switch */
#define vpiRpmosPrim          17   /* resistive pmos switch */
#define vpiRcmosPrim          18   /* resistive cmos switch */
#define vpiRtranPrim          19   /* resistive bidirectional */
#define vpiRtranif0Prim       20   /* zero-enable resistive bidirectional */
#define vpiRtranif1Prim       21   /* one-enable resistive bidirectional */
#define vpiTranPrim           22   /* bidirectional */
#define vpiTranif0Prim        23   /* zero-enabled bidirectional */
#define vpiTranif1Prim        24   /* one-enabled bidirectional */
#define vpiPullupPrim         25   /* pullup */
#define vpiPulldownPrim       26   /* pulldown */
#define vpiSeqPrim            27   /* sequential UDP */
#define vpiCombPrim           28   /* combinational UDP */

/********** path, path terminal, timing check properties **********/
#define vpiPolarity           34   /* polarity of module path... */
#define vpiDataPolarity       35   /* ...or data path: */
#define vpiPositive            1   /* positive */
#define vpiNegative            2   /* negative */
#define vpiUnknown             3   /* unknown (unspecified) */

#define vpiEdge               36   /* edge type of module path: */
#define vpiNoEdge   0x00000000     /* no edge */
#define vpiEdge01   0x00000001     /* 0 -> 1 */
#define vpiEdge10   0x00000002     /* 1 -> 0 */
#define vpiEdge0x   0x00000004     /* 0 -> x */
#define vpiEdgex1   0x00000008     /* x -> 1 */
#define vpiEdge1x   0x00000010     /* 1 -> x */
#define vpiEdgex0   0x00000020     /* x -> 0 */
#define vpiPosedge  (vpiEdgex1  | vpiEdge01 | vpiEdge0x)
#define vpiNegedge  (vpiEdgex0  | vpiEdge10 | vpiEdge1x)
#define vpiAnyEdge  (vpiPosedge | vpiNegedge)

#define vpiPathType           37   /* path delay connection subtypes: */
#define vpiPathFull            1   /* ( a *> b ) */
#define vpiPathParallel        2   /* ( a => b ) */

#define vpiTchkType         38     /* timing check subtypes: */
#define vpiSetup               1   /* $setup */
#define vpiHold                2   /* $hold */
#define vpiPeriod              3   /* $period */
#define vpiWidth               4   /* $width */
#define vpiSkew                5   /* $skew */
#define vpiRecovery            6   /* $recovery */
#define vpiNoChange            7   /* $nochange */
#define vpiSetupHold           8   /* $setuphold */

/********** expression properties **********/
#define vpiOpType             39   /* operation subtypes: */
#define vpiMinusOp             1   /* unary minus */
#define vpiPlusOp              2   /* unary plus */
#define vpiNotOp               3   /* unary not */
#define vpiBitNegOp            4   /* bitwise negation */
#define vpiUnaryAndOp          5   /* bitwise reduction and */
#define vpiUnaryNandOp         6   /* bitwise reduction nand */
#define vpiUnaryOrOp           7   /* bitwise reduction or */
#define vpiUnaryNorOp          8   /* bitwise reduction nor */
#define vpiUnaryXorOp          9   /* bitwise reduction xor */
#define vpiUnaryXNorOp        10   /* bitwise reduction xnor */
#define vpiSubOp              11   /* binary subtraction */
#define vpiDivOp              12   /* binary division */
#define vpiModOp              13   /* binary modulus */
#define vpiEqOp               14   /* binary equality */
#define vpiNeqOp              15   /* binary inequality */
#define vpiCaseEqOp           16   /* case (x and z) equality */
#define vpiCaseNeqOp          17   /* case inequality */
#define vpiGtOp               18   /* binary greater than */
#define vpiGeOp               19   /* binary greater than or equal */
#define vpiLtOp               20   /* binary less than */
#define vpiLeOp               21   /* binary less than or equal */
#define vpiLShiftOp           22   /* binary left shift */
#define vpiRShiftOp           23   /* binary right shift */
#define vpiAddOp              24   /* binary addition */
#define vpiMultOp             25   /* binary multiplication */
#define vpiLogAndOp           26   /* binary logical and */
#define vpiLogOrOp            27   /* binary logical or */
#define vpiBitAndOp           28   /* binary bitwise and */
#define vpiBitOrOp            29   /* binary bitwise or */
#define vpiBitXorOp           30   /* binary bitwise xor */
#define vpiBitXNorOp          31   /* binary bitwise xnor */
#define vpiConditionOp        32   /* ternary conditional */
#define vpiConcatOp           33   /* n-ary concatenation */
#define vpiMultiConcatOp      34   /* repeated concatenation */
#define vpiEventOrOp          35   /* event or */
#define vpiNullOp             36   /* null operation */
#define vpiListOp             37   /* list of expressions */
#define vpiMinTypMaxOp        38   /* min:typ:max: delay expression */
#define vpiPosedgeOp          39   /* posedge */
#define vpiNegedgeOp          40   /* negedge */

#define vpiConstType          40   /* constant subtypes: */
#define vpiDecConst            1   /* decimal integer */
#define vpiRealConst           2   /* real */
#define vpiBinaryConst         3   /* binary integer */
#define vpiOctConst            4   /* octal integer */
#define vpiHexConst            5   /* hexadecimal integer */
#define vpiStringConst         6   /* string literal */


#define vpiBlocking           41   /* blocking assignment (boolean) */
#define vpiCaseType           42   /* case statement subtypes: */
#define vpiCaseExact           1   /* exact match */
#define vpiCaseX               2   /* ignore X's */
#define vpiCaseZ               3   /* ignore Z's */
#define vpiNetDeclAssign      43   /* assign part of decl (boolean) */

/********** system taskfunc properties *******************/
#define vpiSysFuncType        44   /* system function type */
#define vpiSysFuncInt          1   /* returns integer */
#define vpiSysFuncReal         2   /* returns real */
#define vpiSysFuncTime         3   /* returns time */
#define vpiSysFuncSized        4   /* returns sized */
#define vpiUserDefn           45   /* user defined system tf (boolean) */


#define vpiScheduled          46   /* is object vpiSchedEvent still scheduled */
/************ I/O related defines **************************/
#define VPI_MCD_STDOUT  0x00000001
#define VPI_MCD_STDERR  0x00000002
#define VPI_MCD_LOG     0x00000004

/*********************** STRUCTURE DEFINITIONS ****************************/
/************************** time structure ********************************/
typedef struct t_vpi_time {
  int type;                /* [vpiScaledRealTime,vpiSimTime,vpiSuppressTime]*/
  unsigned int high, low;  /* for vpiSimTime */
  double real;             /* for vpiScaledRealTime */
} s_vpi_time, *p_vpi_time;

/* time types */
#define vpiScaledRealTime 1
#define vpiSimTime        2
#define vpiSuppressTime   3

/************************** delay structures ******************************/
typedef struct t_vpi_delay {
  struct t_vpi_time *da;   /* ptr to user allocated array of delay values */
  int no_of_delays;        /* number of delays */
  int time_type;           /* [vpiScaledRealTime,vpiSimTime,vpiSuppressTime]*/
  int mtm_flag;            /* true for mtm values */
  int append_flag;         /* true for append */
  int pulsere_flag;        /* true for pulsere values */
} s_vpi_delay, *p_vpi_delay;

/************************** value structures ****************************/
/* vector value */
typedef struct t_vpi_vecval {
  /* following fields are repeated enough times to contain vector */
  int aval, bval;          /* bit encoding: ab: 00=0, 10=1, 11=X, 01=Z */
} s_vpi_vecval, *p_vpi_vecval;

/* strength (scalar) value */
typedef struct t_vpi_strengthval {
  int logic;               /* vpi[0,1,X,Z] */
  int s0, s1;              /* refer to strength coding below */
} s_vpi_strengthval, *p_vpi_strengthval;

/* strength values */
#define vpiSupplyDrive     0x80
#define vpiStrongDrive     0x40
#define vpiPullDrive       0x20
#define vpiWeakDrive       0x08
#define vpiLargeCharge     0x10
#define vpiMediumCharge    0x04
#define vpiSmallCharge     0x02
#define vpiHiZ             0x01

/* generic value */
typedef struct t_vpi_value {
  int format; /* vpi[[Bin,Oct,Dec,Hex]Str,Scalar,Int,Real,String,Vector,
		                          Strength,Suppress,Time,ObjType]Val */
  union     {
    char *str;                           /* string value */
    int scalar;                          /* vpi[0,1,X,Z] */
    int integer;                         /* integer value */
    double real;                         /* real value */
    struct t_vpi_time *time;             /* time value */
    struct t_vpi_vecval *vector;         /* vector value */
    struct t_vpi_strengthval *strength;  /* strength value */
    char *misc;                          /* ...other */
  } value;
} s_vpi_value, *p_vpi_value;

/* value formats */
#define vpiBinStrVal          1
#define vpiOctStrVal          2
#define vpiDecStrVal          3
#define vpiHexStrVal          4
#define vpiScalarVal          5
#define vpiIntVal             6
#define vpiRealVal            7
#define vpiStringVal          8
#define vpiVectorVal          9
#define vpiStrengthVal       10
#define vpiTimeVal           11
#define vpiObjTypeVal        12
#define vpiSuppressVal       13

/* delay modes */
#define vpiNoDelay            1
#define vpiInertialDelay      2
#define vpiTransportDelay     3
#define vpiPureTransportDelay 4

/* force and release flags */
#define vpiForceFlag          5
#define vpiReleaseFlag        6

/* scheduled event cancle flag */
#define vpiCancelEvent        7

/* bit mask for the flags argument to vpi_put_value() */
#define vpiReturnEvent        0x1000

/* scalar values */
#define vpi0                  0
#define vpi1                  1
#define vpiZ                  2
#define vpiX                  3
#define vpiH                  4
#define vpiL                  5
#define vpiDontCare           6 /*
#define vpiNoChange           7   Defined under vpiTchkType, but can be used here. */

/********************* system taskfunc structure ************************/
typedef struct t_vpi_systf_data {
  int type;            /* vpiSysTask, vpiSysFunc */
  int subtype;         /* vpiSys[Task, Func[Int,Real,Time,Sized]] */
  char *tfname;        /* first character must be `$' */
  int (*calltf)();
  int (*compiletf)();
  int (*sizetf)();     /* for vpiSysFuncSized callbacks only */
  char *user_data;
} s_vpi_systf_data, *p_vpi_systf_data;

#define vpiSysTask            1
#define vpiSysFunc            2 /* the subtypes are defined under the vpiSysFuncType property */

/**************** Verilog execution information structure ***************/
typedef struct t_vpi_vlog_info {
  int argc;
  char **argv;
  char *product;
  char *version;
} s_vpi_vlog_info, *p_vpi_vlog_info;

/**************** PLI error information structure ****************/
typedef struct t_vpi_error_info {
  int state;                    /* vpi[Compile,PLI,Run] */
  int level;                    /* vpi[Notice,Warning,Error,System,Internal]*/
  char *message;
  char *product;
  char *code;
  char *file;
  int line;
} s_vpi_error_info, *p_vpi_error_info;

/* error types */
#define vpiCompile              1
#define vpiPLI                  2
#define vpiRun                  3

#define vpiNotice               1
#define vpiWarning              2
#define vpiError                3
#define vpiSystem               4
#define vpiInternal             5

/************************* callback structures ****************************/
/* normal callback structure */
typedef struct t_cb_data {
  int reason;                   /* callback reason */
  int (*cb_rtn)();              /* call routine */
  vpiHandle obj;                /* trigger object */
  p_vpi_time *time;             /* callback time */
  p_vpi_value *value;           /* trigger object value */
  int index;                    /* index of the memory word or var select
				   which changed value */
  char *user_data;
} s_cb_data, *p_cb_data;

/* Callback Reasons */ /* Simulation-related */
#define cbValueChange             1
#define cbStmt                    2
#define cbForce                   3
#define cbRelease                 4

/* Time-related */
#define cbAtStartOfSimTime        5
#define cbReadWriteSynch          6
#define cbReadOnlySynch           7
#define cbNextSimTime             8
#define cbAfterDelay              9

/* Action-related */
#define cbEndOfCompile           10
#define cbStartOfSimulation      11
#define cbEndOfSimulation        12
#define cbError                  13
#define cbTchkViolation          14
#define cbStartOfSave            15
#define cbEndOfSave              16
#define cbStartOfRestart         17
#define cbEndOfRestart           18
#define cbStartOfReset           19
#define cbEndOfReset             20
#define cbEnterInteractive       21
#define cbExitInteractive        22
#define cbInteractiveScopeChange 23
#define cbUnresolvedSystf        24


#if defined(__STDC__) || defined(__cplusplus)
#ifndef PROTO_PARAMS
#define PROTO_PARAMS(params) params
#define DEFINED_PROTO_PARAMS
#endif
#ifndef EXTERN
#define EXTERN
#define DEFINED_EXTERN
#endif


#else
#ifndef PROTO_PARAMS
#define PROTO_PARAMS(params) (/* nothing */)
#define DEFINED_PROTO_PARAMS
#endif
#ifndef EXTERN
#define EXTERN extern
#define DEFINED_EXTERN
#endif


#endif /* __STDC__ */
/************************ FUNCTION DECLARATIONS *************************/
/* callback related */
EXTERN vpiHandle    vpi_register_cb     PROTO_PARAMS((p_cb_data cb_data_p));
EXTERN int          vpi_remove_cb       PROTO_PARAMS((vpiHandle cb_obj));
EXTERN void         vpi_get_cb_info     PROTO_PARAMS((vpiHandle object,p_cb_data cb_data_p));
EXTERN void         vpi_register_systf  PROTO_PARAMS((p_vpi_systf_data systf_data_p));
EXTERN void         vpi_get_systf_info  PROTO_PARAMS((vpiHandle object,p_vpi_systf_data systf_data_p));

/* for obtaining handles */
EXTERN vpiHandle    vpi_handle_by_name  PROTO_PARAMS((char *name,vpiHandle scope));
EXTERN vpiHandle    vpi_handle_by_index PROTO_PARAMS((vpiHandle object,int indx));

/* for traversing relationships */
EXTERN vpiHandle    vpi_handle          PROTO_PARAMS((int type,vpiHandle referenceHandle));
EXTERN vpiHandle    vpi_iterate         PROTO_PARAMS((int type,vpiHandle referenceHandle));
EXTERN vpiHandle    vpi_scan            PROTO_PARAMS((vpiHandle iterator));

/* for accesssing properties */
EXTERN int          vpi_get             PROTO_PARAMS((int property,vpiHandle object));
EXTERN char *       vpi_get_str         PROTO_PARAMS((int property,vpiHandle object));

/* delay processing */
EXTERN void         vpi_get_delays      PROTO_PARAMS((vpiHandle object,p_vpi_delay delay_p));
EXTERN void         vpi_put_delays      PROTO_PARAMS((vpiHandle object,p_vpi_delay delay_p));

/* value processing */
EXTERN void         vpi_get_value       PROTO_PARAMS((vpiHandle expr,p_vpi_value value_p));
EXTERN vpiHandle    vpi_put_value       PROTO_PARAMS((vpiHandle object,p_vpi_value value_p,p_vpi_time time_p, int flags));

/* time processing */
EXTERN void         vpi_get_time        PROTO_PARAMS((vpiHandle object,p_vpi_time time_p));

/* I/O routines */
EXTERN unsigned int vpi_mcd_open        PROTO_PARAMS((char *fileName));
EXTERN unsigned int vpi_mcd_close       PROTO_PARAMS((unsigned int mcd));
EXTERN char *       vpi_mcd_name        PROTO_PARAMS((unsigned int cd));
#ifndef VPI_IO_C
EXTERN int          vpi_mcd_printf      PROTO_PARAMS((unsigned int mcd,char *format,...));
EXTERN int          vpi_printf          PROTO_PARAMS((char *format,...));
#else
EXTERN int          vpi_mcd_printf      PROTO_PARAMS(());
EXTERN int          vpi_printf          PROTO_PARAMS(());
#endif

/* utility routines */
EXTERN int          vpi_compare_objects PROTO_PARAMS((vpiHandle object1,vpiHandle object2));
EXTERN int          vpi_chk_error       PROTO_PARAMS((p_vpi_error_info error_info_p));
EXTERN int          vpi_free_object     PROTO_PARAMS((vpiHandle object));
EXTERN int          vpi_get_vlog_info   PROTO_PARAMS((p_vpi_vlog_info vlog_info_p));


#ifdef DEFINED_PROTO_PARAMS
#undef DEFINED_PROTO_PARAMS
#undef PROTO_PARAMS
#endif

#ifdef DEFINED_EXTERN
#undef DEFINED_EXTERN
#undef EXTERN
#endif

/************************ GLOBAL VARIABLES *******************************/
extern void (*vlog_startup_routines[])(); /* array of function pointers, */
                                          /* last pointer should be null */
#endif /* VPI_USER_H */


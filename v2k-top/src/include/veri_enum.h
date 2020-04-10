/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  veri_enum_h_rcsid
#define veri_enum_h_rcsid() {return "$Id: veri_enum.h,v 1.67 2020/04/06 00:41:56 dkc Exp $";} /* RCS ID */

 
#ifndef VERI_ENUM_H
#define VERI_ENUM_H

#include "v2k_misc.h"

typedef enum {
  VMD_NONE  =  0,
  VMD_IEEE  =  1,
  VMD_VA    =  2,
  VMD_MS    =  4,
  VMD_DOINC =  8,
  VMD_DASHV = 16,
  VMD_CELL  = 32
} eVMD;

#define VMD(t) ((eVMD)(t))

typedef enum {
#define AV_TYPE(t,p,s) VT_##t,
#include "av_types.h"
  VT_LAST
} VlType;

#define VL_TYPE(t) ((VlType)(t))

typedef enum {
  AO_UNKNOWN,
  AO_NATURE,
  AO_DISC,
  AO_PORT,
  AO_MODULE,
  AO_MACRO,
  AO_PRIM,
  AO_TASK,
  AO_FUNC,
  AO_INST,
  AO_STMT,
  AO_LOCALS,
  AO_SDF

} eAOtype;

typedef enum {
#define AV_TYPE(t,p,s) ATTR_##t,
#define AV_ATTR_ONLY
#include "av_types.h"
  ATTR_LAST,

  ATTR_RNGD  = 64,
  ATTR_USR   = 128
} AttrType;

#define ATTR_TYPE(t) ((AttrType)(t))
#define ATTR_VT(t)   VL_TYPE((t) & (ATTR_RNGD -1))
#define ATTR_AT(t)   ATTR_TYPE((t) & (ATTR_RNGD -1))

/*! Statement Context (Bit-Flags) */
typedef enum {
  SC_NONE       = 0x00000000,

  SC_MODIFIER   = 0x00000001,

  SC_TASK_FUNC  = 0x00000002,
  SC_LEVEL      = 0x00000004,
  SC_ANALOG     = 0x00000008,
  SC_DIGITAL    = 0x00000010,
  SC_CALL       = 0x00000020,
  SC_INITIAL    = 0x00000040,
  SC_DFPRM      = 0x00000080,
  SC_CASE       = 0x00000100,
  SC_IF         = 0x00000200,
  SC_BLOCK      = 0x00000400,
  SC_COMPLETE   = 0x00000800,
  SC_MODULE     = 0x00001000,
  SC_NAT_DISC   = 0x00002000,
  SC_CHOICE     = 0x00004000,
  SC_PARAMETER  = 0x00008000,
  SC_RANGE      = 0x00010000,
  SC_BOOL       = 0x00020000,
  SC_REFERENCE  = 0x00040000,
  SC_ASSIGNMENT = 0x00080000,
  SC_LVALUE     = 0x00100000,
  SC_SENSE      = 0x00200000,
  SC_INSTANCE   = 0x00400000,
  SC_PORTS      = 0x00800000,
  SC_NUMBER     = 0x01000000,
  SC_LIST       = 0x02000000,
  SC_DELAY      = 0x04000000,
  SC_INDXD      = 0x08000000,
  SC_SPECIFY    = 0x10000000,
  SC_ASSIGN     = 0x20000000,
  SC_TOP        = 0x40000000,
  SC_HIER       = 0x80000000,

  SC_ATTR       = SC_PARAMETER,
# define IS_ATTR(c) ((SC_ATTR & (c)) && !(SC_INSTANCE & (c)))

  SC_FUNCTION   = SC_TASK_FUNC,
  SC_TASK       = SC_TASK_FUNC|SC_MODIFIER,

  SC_NATURE     = SC_NAT_DISC,
  SC_DISCIPLINE = SC_NAT_DISC|SC_MODIFIER,

  SC_ASGN_LST   = SC_ASSIGN|SC_LIST|SC_TOP,
  SC_LIST0      = SC_LIST|SC_TOP,

  SC_ALL        = ~0
} StmtCntx;

#define STMT_CNTX(c) ((StmtCntx)(c))

typedef enum {
  REF_NONE     = -1,
  REF_UNKNOWN  =  0,
  REF_MODIFIER =  1,
  REF_TASK,
  REF_SYS_TASK = REF_TASK|REF_MODIFIER,
  REF_FUNC,
  REF_SYS_FUNC = REF_FUNC|REF_MODIFIER,
  REF_PLI_FUNC,
  REF_ATTR,
  REF_STMT,
  REF_PORT,
  REF_PARM,
  REF_COUNTER,
  REF_BRANCH,
  REF_NATURE,
  REF_MODULE,
  REF_PRIM,
  REF_DISC,
  REF_EXPR,
  REF_ACCESS,
  REF_SIGNAL,
  REF_EVENT,
  REF_RETURN,
  REF_SCOPE,
  REF_INST,
  REF_VPRIM,
  REF_ROOT,
  REF_LIB,
  REF_SDF_PORT,
  REF_UNMATCHED,

  REF_LAST
} eREF;

typedef enum {
  USG_NONE      = 0,
  USG_READ      = 1,
  USG_DRIVEN    = 2,
  USG_SENSE     = 4,
  USG_LEVEL     = 8,

  USG_ANALOG    = 4, /* shift */

  USG_A_READ    = USG_READ    << USG_ANALOG,
  USG_CONTRIB   = USG_DRIVEN  << USG_ANALOG,
  USG_PROBE     = USG_SENSE   << USG_ANALOG,
  USG_A_LEVEL   = USG_LEVEL   << USG_ANALOG,

  USG_TO_GRND   = USG_CONTRIB << USG_ANALOG,
  USG_FLOW      = USG_TO_GRND << 1

} eUSG;

#define USG(u) ((eUSG)(u))

typedef struct {
 eREF typ;
 I32  index;
} refTup;

typedef struct {
 I32  scp,
      index;
} rrfTup;

typedef struct {
  I32    size;
  VlType vt:16;
  short  indirect;
} typDesc;

typedef enum {
  OT_UNSET  = -1,
  OT_VOID   =  0,
  OT_SIMPLE,
  OT_LEFT,
  OT_RIGHT,
  OT_RANGE
} opTyp;

typedef struct {
 U32   i;
 short b,
       w;
} szdInt;

typedef enum {
  RFF_UNKNOWN  = 1 << REF_UNKNOWN,
  RFF_ATTR     = 1 << REF_ATTR,
  RFF_STMT     = 1 << REF_STMT,
  RFF_PORT     = 1 << REF_PORT,
  RFF_PARM     = 1 << REF_PARM,
  RFF_COUNTER  = 1 << REF_COUNTER,
  RFF_BRANCH   = 1 << REF_BRANCH,
  RFF_NATURE   = 1 << REF_NATURE,
  RFF_MODULE   = 1 << REF_MODULE,
  RFF_PRIM     = 1 << REF_PRIM,
  RFF_DISC     = 1 << REF_DISC,
  RFF_EXPR     = 1 << REF_EXPR,
  RFF_FUNC     = 1 << REF_FUNC,
  RFF_TASK     = 1 << REF_TASK,
  RFF_SYS_FUNC = 1 << REF_SYS_FUNC,
  RFF_PLI_FUNC = 1 << REF_PLI_FUNC,
  RFF_SYS_TASK = 1 << REF_SYS_TASK,
  RFF_ACCESS   = 1 << REF_ACCESS,
  RFF_SIGNAL   = 1 << REF_SIGNAL,
  RFF_EVENT    = 1 << REF_EVENT,
  RFF_RETURN   = 1 << REF_RETURN,
  RFF_SCOPE    = 1 << REF_SCOPE,
  RFF_INST     = 1 << REF_INST,
  RFF_CALL     = RFF_ACCESS|RFF_FUNC|RFF_TASK|
                            RFF_SYS_FUNC|RFF_PLI_FUNC|RFF_SYS_TASK|RFF_MODULE,
  RFF_DRV      = RFF_SIGNAL|RFF_BRANCH|RFF_PORT,
  RFF_BIND     = RFF_DRV|REF_UNKNOWN,
  RFF_DATA     = RFF_DRV|RFF_ATTR|RFF_PARM|RFF_COUNTER,
  RFF_HIER     = RFF_FUNC|RFF_TASK|RFF_DATA,
  RFF_MP       = RFF_MODULE|RFF_PRIM,
  RFF_ALL      = ~RFF_UNKNOWN,
  RFF_ALL_LCL  = RFF_ALL & ~(RFF_MODULE|RFF_INST)
} eRFF;

#define RFF(r) ((eRFF)(1 << (r)))

typedef enum {
  DF_NONE          = 0,
  DF_DIGITAL       = 1,
  DF_ANALOG        = 2,
  DF_AD            = DF_DIGITAL|DF_ANALOG,
  DF_POTENTIAL     = 4,
  DF_FLOW          = 8,
  DF_CONSERVATIVE  = DF_POTENTIAL|DF_FLOW,
  DF_VOID          = DF_AD|DF_CONSERVATIVE,
  DF_OWN_FLOW      = 16,
  DF_OWN_POTENTIAL = 32,
  DF_DISCIPLINE    = 64

} DscFlgs;

#define DSC_FLGS(f) ((DscFlgs)(f))

typedef enum {
  ST_BIT      =   0,
  ST_WOR      =   1,
  ST_WAND     =   2,
  ST_STRENGTH =   4,
  ST_CHARGE   =   8,
  ST_TRI      =  16,
  ST_PULLUP   =  32,
  ST_PULLDOWN =  64,
  ST_VEC      = 128
} SigType;

#define SIG_TYPE(t) ((SigType)(t))

typedef enum {
  DRV_OFF,
  DRV_ON
} Driven;

typedef enum {
  LVL_0,
  LVL_1
} Level;

typedef enum {
  KNWN_NO,
  KNWN_YES
} Known;

typedef enum {
  CARE_NO,
  CARE_YES
} Care;

typedef enum {
  XMR_NONE     =   0,
  XMR_DRIVEN   =   1,
  XMR_SENSED   =   2,
  XMR_CONTRIB  =   4,
  XMR_PROBE    =   8,
  XMR_XIO      =   0xF, // Mask
  XMR_LCLSIG   =  16,
  XMR_RMTSIG   =  32,
  XMR_PARM     =  64,
  XMR_DATA     = 128,

  XMR_BNDSIG   = XMR_SENSED|XMR_RMTSIG
} eXMR;

#define XMR(x) ((eXMR)(x))

typedef enum {
  PRT_ANY           =   -1,
  PRT_NC            =    0,
  PRT_IN            =    1,                      /* Input port        */
  PRT_OUT           =    2,                      /* Output port       */
  PRT_INOUT         =    PRT_IN|PRT_OUT,
  PRT_IO_LSTD       =    4,                      /* Port              */
  PRT_INVALID       =    PRT_INOUT|PRT_IO_LSTD, /* shouldn't be all  */
  PRT_HIER          =    8,
  PRT_XPRTD         =   16,                      /* connected down    */
  PRT_NONLOCAL      =    PRT_INOUT|PRT_XPRTD|PRT_HIER,
  PRT_DSC_DFLTD     =   32,
  PRT_SCALARED      =   64,
  PRT_VECTORED      =  128,
  PRT_BRANCH        =  256,
  PRT_ALIASSED      =  512,
  PRT_PORT          =    PRT_ALIASSED|PRT_IO_LSTD,
# define PRT_XMR_SHFT    10
  PRT_DRIVEN        =    XMR_DRIVEN  << PRT_XMR_SHFT,
  PRT_SENSED        =    XMR_SENSED  << PRT_XMR_SHFT,
  PRT_CONTRIB       =    XMR_CONTRIB << PRT_XMR_SHFT,
  PRT_PROBE         =    XMR_PROBE   << PRT_XMR_SHFT,
  PRT_ANALOG        =    PRT_PROBE|PRT_CONTRIB
} PortDir;

typedef enum {
  PRT_LCLSIG        =    XMR_LCLSIG  << PRT_XMR_SHFT,
  PRT_BNDSIG        =    PRT_INOUT|PRT_IO_LSTD|PRT_XPRTD|PRT_SENSED|PRT_BRANCH|PRT_LCLSIG,
  PRT_RMTSIG        =    XMR_RMTSIG  << PRT_XMR_SHFT,
  PRT_PARM          =    XMR_PARM    << PRT_XMR_SHFT,
  PRT_DATA          =    XMR_DATA    << PRT_XMR_SHFT,
  PRT_XMR           =    PRT_DATA,
  PRT_XMR_MASK      =    ((unsigned)~0) << PRT_XMR_SHFT
} PortDirX;


#define PORT_DIR(d) ((PortDir)(d))

typedef enum {
  PT_NONE,

  PT_INT       =   1,
  PT_TIME      =   2,
  PT_REAL      =   3,
  PT_EVENT     =   4,
  PT_GENVAR    =   5,
  PT_ALIAS     =   6,
  PT_USER      =   7, // Any extended

  PT_TYPE_MSK  =   7,

  PT_SIGNED    =   8,
  PT_HIER      =  16,
  PT_REG       =  32,
  PT_WIRE      =  64,
  PT_AUTO      = 128,

  PT_PACKED    = 1 << 16
} PortTyp;

#define PORT_TYP(t) ((PortTyp)(t))

typedef enum {
  CHG_NONE,
  CHG_SMALL,
  CHG_MEDIUM,
  CHG_LARGE
} Charge;

typedef enum {
  DS_NONE  = 0,
  DS_NORM,
  DS_HIGHZ,
  DS_WEAK,
  DS_PULL,
  DS_STRONG,
  DS_SUPPLY
} Strength;

typedef enum {
  TBL_NONE   = -1,
  TBL_ZERO   =  0,
  TBL_ONE,
  TBL_X,
  TBL_UNKNWN,
  TBL_B,
  TBL_STAR,
  TBL_RISE,
  TBL_FALL,
  TBL_DASH,
  TBL_PTN_R,
  TBL_PTN_F,
  TBL_MAP    = 0x07,
  TBL_VW     = 0x80
} eTable;

#define TABLE_VW(a,b) (TBL_VW | ((a) << 3) | (b))

typedef enum {
  FUNC_VOID = 0,
  FUNC_INT  = 1,
  FUNC_REAL = 2,
  FUNC_RNG  = 4
} FuncRet;

typedef enum {
  VTX_NONE     =   0,
  VTX_REF      =   1,
  VTX_HIER     =   2,
  VTX_CONST    =   4,
  VTX_ALIASSED =   8,
  VTX_DUMPED   =  16,
  VTX_INUSE    =  32,
  VTX_RM       =  64,
  VTX_PERM     = 128
} VlTypXtra;

#define VL_TYP_XTRA(x) ((VlTypXtra)(x))

typedef enum {
  PRSN_VOID,
  PRSN_BIT,
  PRSN_8,
  PRSN_FLOAT,
  PRSN_32,
  PRSN_DOUBLE,
  PRSN_64,
  PRSN_LOGIC,
  PRSN_SLICE
} ePrcsn;

typedef enum {
  NQZX_NUMBER = 0,
  NQZX_QMARK,
  NQZX_HIGHZ,
  NQZX_DONTCARE
} eNQZX;

typedef enum {
  CT_NONE   =  0,
  CT_C,
  CT_PLI,
  CT_USER,
  CT_TASK,
  CT_FN,
  CT_GATE
} CllType;

#ifdef VERILOG3
typedef enum {
#define  OPERATOR(o,p,i,prcd,R,l,r,alt,sc,lv) o
#include "operator.inc"
} OPR;
#endif

#ifdef VERILOG4
typedef enum {
#define  PRIM(g,l) GATE_##g,
#include "vprim.inc"
  GATE_LAST
} eGATE;
#endif

typedef enum {
  PRC_TERMINAL,
  PRC_NONE,
  PRC_LIST0,
  PRC_ASSIGN,
  PRC_REPEAT,
  PRC_DE,
  PRC_EVENT,
  PRC_CONTRIB,
  PRC_LIST1,
  PRC_CHOOSE,
  PRC_CHOICE,
  PRC_EDGE,
  PRC_SUB,
  PRC_ARRAY,
  PRC_CAT,
  PRC_RANGE,
  PRC_BOOL,
  PRC_BIT_OR,
  PRC_BIT_XOR,
  PRC_BIT_AND,
  PRC_L_CMP,
  PRC_CMP,
  PRC_SHFT,
  PRC_ADD,
  PRC_MULT,
  PRC_UNARY,
  PRC_BIND,
  PRC_CALL,
  PRC_INDEX,
  PRC_POLARITY,
  PRC_HIER,
  PRC_TICK,
  PRC_DATA,
  PRC_ANY,

  PRC_MAX
} OprPrec;

typedef enum {
  XD_NONE   = 0,
  XD_FLAG   = 1, /* always used - expr. pointers aren't odd */
  XD_LOGIC  = 2,
  XD_HREF   = 4,
  XD_DLLR   = 8,
  XD_MASK   = 15 & ~XD_FLAG,
#define XD_XMR_SHFT 4
  XD_DRIVEN = XMR_DRIVEN << XD_XMR_SHFT,
  XD_SENSED = XMR_SENSED << XD_XMR_SHFT
} XData;

#define XDATA(x) ((XData)(((intptr_t)(x)) & XD_MASK))

typedef enum {
  BND_SET_NC = -1,
  BND_NC     =  0,
  BND_CONSTX =  1,
  BND_CONST0 =  2,
  BND_CONST1 =  3,
  BND_CONSTZ =  4,
# define  BND_CONST(b) (((unsigned int)b) <= BND_CONSTZ)
  BND_PORT   =  5,
  BND_UNK    =  6,
  BND_EXPR   =  7,
  BND_GLBL   =  8
} eBIND;

typedef enum {
  INDX_NONE   = 0,
  INDX_SIMPLE = 1,
  INDX_RANGE  = 2
} eINDX;

typedef enum {
  RVT_NONE = 0,
  RVT_INT,
  RVT_DOUBLE,
  RVT_TRIPLE,
  RVT_VAL1,
  RVT_VAL2,
  RVT_VAL3,
  RVT_VAL6,
  RVT_VAL12
} eRVT;

#define RVT(i) ((eRVT)(i))

typedef enum {
  TSPC_NONE = 0,
  TSPC_DELAY,
  TSPC_TIMINGCHECK
} eTSPC;

typedef enum /* eCHK */ {
  CHK_NONE = 0,
#define TIMING(t,s,c,p_min,p_max,v_min,v_max,in) CHK_##t,
#include "timing.inc"

  CHK_LAST_DELAY = CHK_DEVICE,
  CHK_LAST_CHECK = CHK_NOCHANGE
} eCHK;

typedef enum {
  PSP_NONE  = 0,
  PSP_LOCAL = 1,
  PSP_HIER  = 2,
  PSP_COND  = 4,
  PSP_EDGE  = 8
} ePSP;

#define PSP(i) ((ePSP)(i))

typedef enum {
  DMD_NONE  = 0,
  DMD_ABS   = 1,
  DMD_INCR  = 2
} eDMD;

typedef enum {
  EDG_NONE    = 0,
  EDG_START0  = 1,
  EDG_START1  = 2,
  EDG_STARTX  = 4,
  EDG_STARTZ  = 8,
  EDG_SE_SHFT = 4,
  EDG_END0    = EDG_START0 << EDG_SE_SHFT,
  EDG_END1    = EDG_START1 << EDG_SE_SHFT,
  EDG_ENDX    = EDG_STARTX << EDG_SE_SHFT,
  EDG_ENDZ    = EDG_STARTZ << EDG_SE_SHFT,

  EDG_POSEDGE = EDG_END1 | EDG_START0|EDG_STARTZ|EDG_STARTX,
  EDG_NEGEDGE = EDG_END0 | EDG_START1|EDG_STARTZ|EDG_STARTX,

  EDG_X1      = EDG_END1 | EDG_STARTX,
  EDG_X0      = EDG_END0 | EDG_STARTX,
  EDG_XZ      = EDG_ENDZ | EDG_STARTX,
  EDG_01      = EDG_END1 | EDG_START0,
  EDG_0Z      = EDG_ENDZ | EDG_START0,
  EDG_0X      = EDG_ENDX | EDG_START0,
  EDG_10      = EDG_END0 | EDG_START1,
  EDG_1Z      = EDG_ENDZ | EDG_START1,
  EDG_1X      = EDG_ENDX | EDG_START1,
  EDG_Z1      = EDG_END1 | EDG_STARTZ,
  EDG_Z0      = EDG_END0 | EDG_STARTZ,
  EDG_ZX      = EDG_ENDX | EDG_STARTZ

} eEDG;

#define EDG(e) ((eEDG)(e))

#endif /* VERI_ENUM_H */

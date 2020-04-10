/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  verilog_h_rcsid
#define verilog_h_rcsid() {return "$Id: verilog.h,v 1.86 2010/04/21 01:43:12 dkc Exp $";} /* RCS ID */

 
/*! \file
    Pre-processor and entry to parser
*/

#ifndef LANGUAGE_H
# include "language.h"
#endif
#ifndef FILE_H
# include "file.h"
#endif
#ifndef LIST_H
# include "list.h"
#endif
#ifndef DUMP_FLAGS
# include "dump.h"
#endif
#ifndef VERI_ENUM_H
# include "veri_enum.h"
#endif
#ifndef CG_ENUMS_H
# include "cg_enums.h"
#endif
#ifndef STRDB_H
# include "strdb.h"
#endif

typedef enum {
#define OT_DECL(e,s,l,f) e,
#include "objtyp.h"
OBJ_TypMax
} eObjTyp;
extern const eFT         ObjTypFT [OBJ_TypMax+1];
extern const char *const ObjTyp   [OBJ_TypMax+1],
                  *const ObjTypStr[OBJ_TypMax+1];

#ifdef __cplusplus
extern "C" {
#endif

int  InitVerilogTok();
int  tokVerilog    (FILE *,fwrite_fn,FILE *,eVMD mode);
int  tokVerilogStr (const char *,void **,int *,eVMD mode);
int  prsVerilog    (int *argc,const char ***argv,void *var,int,int);
int  checkNextArg  (const char *,const char *);
int  prsVerilogA   (int *argc,const char ***argv,void *var,int,int);
int  prsDefine     (int *argc,const char ***argv,void *var,int,int);
int  prsInclude    (int *argc,const char ***argv,void *var,int,int);
int  argXtra       (int *argc,const char ***argv,void *var,int,int);
int  addPliPath    (int *argc,const char ***argv,void *var,int,int);
int  prsUndef      (int *argc,const char ***argv,void *var,int,int);
void prsVMdone     ();
int  prsVM         (const char *,eVMD);

extern int prsVerSkipArg;

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
int tokVerilog    (Stream *,Stream *,eVMD mode);
#endif

#ifdef __cplusplus

#ifdef VERILOG3

class InfoRef { //!< A token ref. extended with file & line info
public:
  POOL_REF_FLDS
  poolRef      file;
  int          line;
  int          level;
  const Token *tok;

  inline InfoRef()              {pool=index=0;  file=NullRef;line=0;tok=0;}
  inline InfoRef(int p,int i)   {pool=p;index=i;file=NullRef;line=0;tok=0;}
  inline InfoRef(poolRef ref)   {pool=ref.pool;index=ref.index;
                                                file=NullRef;line=0;tok=0;}

  inline poolRef      ref()     const {return *(poolRef *)this;};
  inline const char  *source()  const {return strDeref(file);}
  inline int          line_no() const {return line;}
  inline const char  *str()     const {return strDeref(*(poolRef *)this);}

  inline operator poolRef() const {return ref();};
};

const char *strDerefArr(InfoRef *,int,String *,char *);

class AttrObj;
class Expr;
class Stmt;
class StmtCase;

typedef struct sHshdRef {    //!< For locating named objects
  struct sHshdRef *next;
  poolRef          name;
  int              scp,
                   indx,
                   dpth:16;
  eREF             ref:16;
} hshdRef;

#define LST(t,b,n,e) class t; class b;
#include "lists.inc"

#define LST(t,b,n,e) typedef List<b,true> b##List;
#define LST_TYPES
#define LST_NO_SPECIAL
#include "lists.inc"
typedef List<VerCntxtObj,true> VerCntxtObjList;


#define LST(t,b,n,e) typedef int t##Idx;
#define LST_TYPES
#include "lists.inc"

typedef enum {
#define LST(t,b,n,e) LST_##e,
#define LST_GLOBAL
#include "lists.inc"

  LST_MAX
} eLST;

class ContextObj;
class dumper;
template<bool del>
class VDataS {
 public:

  ListArrayItem<del>  lists[LST_MAX];
  InfoRef            *incStack;

  ContextObj         *root;                 //!< Current root object for parsing
  poolRef             def_dsc_nm,           //!< Default Discipline Name
                      def_net_type,         //!< Default Net Type
                      unconnected;
  Disc               *def_disc;             //!< Default Discipline
  DiscIdx             def_disc_id;          //!< Default Discipline Index
  StmtCntx            cntxt;                //!< Expression Context
  InfoRef            *expr,                 //!< Expression Tree
                      expr_term;
  InfoRef             posn;                 //!< Source Location
  eVMD                vmode;
  int                 expr_max,
                      expr_len,
                      expr_attr,
                      max_level,
                      init,
                      depth,
                      protect;
  int                 ts1,   ts2,
                      prcsn1,prcsn2;
  Expr               *cmpld_expr,
                     *expr_val;
  int                 mod_indx,
                      lib_indx,
                      cur_scp,
                      max_scp;
  Stmt               *process;
  int                *scp_stk;
  DmpFlgs             save;                 //!< Which objects to save
  Filename            srcname;

  int                 nxtInUse;
#define VDataSz  0x10000
#define VDataMsk  0xFFFF
  hshdRef            *hshdFree,
                     *hshdTbl  [VDataSz],
                     *hshdStart[VDataSz],
                     *hshdTblG [VDataSz];
  int                 hshdInUse[VDataSz];

  VDataS() {
    incStack     = 0;
    root         = 0;        
    def_dsc_nm   = NullRef;
    def_net_type = NullRef;
    unconnected  = NullRef;
    def_disc     = 0;   
    def_disc_id  = 0;
    cntxt        = SC_NONE;
    expr         = 0;
    vmode        = VMD_NONE;
    expr_max     = 0;
    expr_len     = 0;
    max_level    = 0;
    init         = 0;
    depth        = 0;
    protect      = 0;
    ts1          = 0;
    ts2          = 0;
    prcsn1       = 0;
    prcsn2       = 0;
    cmpld_expr   = 0;
    expr_val     = 0;
    mod_indx     = 0;
    lib_indx     = 0;
    cur_scp      = 0;
    max_scp      = 0;
    process      = 0;
    scp_stk      = 0;
    save         = DMP_NONE;
    nxtInUse     = 0;
    hshdFree     = 0;
    BZEROA(hshdTbl);
    BZEROA(hshdStart);
    BZEROA(hshdTblG);
    BZEROA(hshdInUse);
  }
};

typedef struct VDataS<true>  VDataD;
typedef struct VDataS<false> VDataC;

#endif

class model;
class VerilogObj {
#define VERILOG_OBJ
#ifdef VERILOG3
  static VDataC VD;
#endif
public:
  ~VerilogObj();
  VerilogObj(const File *,eVMD vmode = VMD_NONE);

  inline VerilogObj() {};

#ifdef VERILOG3
  inline VDataC       *vd()        const {return &VD;};

#define LST(t,b,n,e) inline t##List *n() const {\
                                return (t##List *)&vd()->lists[LST_##e].list;};
#define LST_GLOBAL
#include "lists.inc"
#define LST(t,b,n,e) inline b *n##_map(int i) const {\
                                return (b *)(vd()->lists[LST_##e].map[i]);};
#define LST_GLOBAL
#include "lists.inc"

# define GLOBAL_SCP 0
# define ROOT_SCP   1
  inline int           scp_dpth()   {return vd()->cur_scp;};
  inline int           scp_id()     {return scp_dpth() >= 0
                                                     ? vd()->scp_stk[scp_dpth()]
                                                     : -1;};

  inline const char   *source()     {if (!NULL_REF(vd()->posn.file))
                                      vd()->srcname = strDeref(vd()->posn.file);
                                     return vd()->srcname;};
  inline int           line_no()    {return vd()->posn.line;}
  inline int           inc_level()  {return vd()->posn.level;}
  inline void          recycle()    {delete this;}
  inline ContextObj   *Root() const {return vd()->root;};

  const Token *rescanning(const Token *T);
  const Token *getRefOpt(const Token *,const Token *,InfoRef *);
  const Token *getRef(const Token *,const Token *,InfoRef *);
  const Token *skipWhite(const Token *,const Token *);
  const Token *skipToken(const Token *,const Token *,const Token,
                         const char *where = 0);

  Nature      *ReadNature(const Token *,const Token *,NatureIdx);
  InfoRef      getRef(const Token *,const Token *);
  void         reportError(eSTS,const char *,...);
  void         reportError(eSTS,InfoRef &,const char *,...);
  bool         identifier(poolRef);
  bool         number(poolRef);
  bool         checkName(poolRef);
  bool         baseNature(NatureIdx,NatureIdx);
  NatureIdx    findNature(poolRef);
  DiscIdx      findDisc(poolRef);
  LibraryIdx   findLib(const char *);
  bool         findGlobObj(eRFF,poolRef,refTup *);
  bool         findLibObj(eRFF,int,poolRef,refTup *);
  hshdRef     *logGlobal(poolRef,eREF,int);
  void         logGlobals(eREF,const tokTable *);
  hshdRef     *saveRef(poolRef,eREF,int,int,int);
  hshdRef     *saveGlobRef(poolRef,eREF,int);
  void         clearRefs();
  Task        *taskObj(const Stmt *);
  Func        *funcObj(const Stmt *);
  void        *demap(const model *);
  void         Init();
  Expr        *evalNumber(InfoRef *,int,int);

  inline void resetExpr()              {vd()->expr_len = 0;};
  inline unsigned int
              hash(poolRef ref)        {unsigned int h;
                                        h  = ((ref.pool<<8)^ref.index);
                                        h ^= h >> 16;
                                        return VDataMsk & h;}
  inline void setSource(poolRef ref)   {vd()->posn.file = ref;}
  inline void setSource(const char *s) {vd()->posn.file = NullRef;
                                        vd()->srcname   = s;}

#endif

  void        resetAll();
  void        setTimescale(int,int,int,int);
  int         setLibrary(const char *);
  int         addLibrary(const char *);
  int         setCellDefine(int);
  int         setProtect(int);
  void        setUnconnected(poolRef);
  void        setDefaultNetType(poolRef);
  void        setDefaultDisc(poolRef);
  void        setSaveFlags(DmpFlgs);
  void        pushDiscipline(poolRef);
  void        popDiscipline(poolRef);
  int         reposPath(File *,const char *,const char *,const char *,eObjTyp);
  int         checkObject(eObjTyp,const char *,Filename *);
  int         loadObject(eObjTyp,const char *,Filename *);
  const char *libName();
};

#define NullVobj ((VerilogObj*)0)

#ifdef VERILOG3
# include "verilog3.h"
#endif

#ifdef VERILOG4
# include "verilog4.h"
#endif

extern const poolRef InitVal,
                     Specify,
                     Global,
                     Generate;

#endif /* __cplusplus */


#ifdef __cplusplus
extern "C" {
#endif

void verInit();
eSTS verSetSF (int *argc,const char ***argv,void *var);
eSTS verSetDF (int *argc,const char ***argv,void *var);
eSTS verDumpV (int *argc,const char ***argv,void *var);
eSTS verSetLib(int *argc,const char ***argv,void *var);
eSTS verAddLib(int *argc,const char ***argv,void *var);

typedef enum {
  ELAB_ALL   = -1,
  ELAB_NONE  =  0,
  ELAB_TOP   =  1,
  ELAB_HIER  =  2,
  ELAB_PARM  =  4,
  ELAB_DIMN  =  8,
  ELAB_BIND  = 16,
  ELAB_MKSI  = 32,
  ELAB_GTSI  = 64
} eELAB;

eSTS elaborate(int,int *);
eSTS setTopMod(const char *);
eSTS v2kStartSim();
int  setLibrary(const char *);
int  addLibrary(const char *);

extern int V2kAuto,
           V2kMinimize,
           V2kPortMode,
           V2kWireMode,
           V2kUnmatchedOK;

#define PORT_NOEXCESS  1
#define PORT_NODANGLE  2
#define PORT_NOUNCONN  4
#define PORT_WRNUNCNN  8
#define PORT_WRNUNKWN 16

#ifdef __cplusplus
}
#endif


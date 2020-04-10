/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  env_h_rcsid
#define env_h_rcsid() {return "$Id: env.h,v 1.55 2010/05/01 09:49:55 dkc Exp $";} /* RCS ID */
 
#ifndef ENV_H
#define ENV_H

#include "error.h"

#define V2K_OS          "V2K_OS"
#define V2K_BIN         "V2K_BIN"
#define V2K_ROOT        "V2K_ROOT"
#define V2K_IMPORT      "V2K_IMPORT"
#define V2K_OS_BIN      "V2K_OS_BIN"
#define V2K_OS_OBJ      "V2K_OS_OBJ"
#define V2K_OS_SO_EXT   "V2K_OS_SO_EXT"
#define V2K_LIB         "V2K_LIB"
#define V2K_CMPRSSRS    "V2K_CMPRSSRS"
#define V2K_LD_PATH     "V2K_LD_PATH"
#define V2K_OS_LIB      "V2K_OS_LIB"
#define V2K_SO_LIB      "V2K_SO_LIB"
#define V2K_INCLUDE     "V2K_INCLUDE"
#define V2K_SYSINCLUDE  "V2K_SYSINCLUDE"
#define V2K_LTOK_CCH    "V2K_LTOK_CCH"
#define V2K_STRINGS     "V2K_STRINGS"
#define V2K_REPOSITORY  "V2K_REPOSITORY"
#define V2K_LIBRARY     "V2K_LIBRARY"
#define V2K_DEFLIB      "V2K_DEFLIB"
#define V2K_LIB_PATH    "V2K_LIB_PATH"
#define V2K_CXX         "V2K_CXX"
#define V2K_CC          "V2K_CC"
#define V2K_CC_SIM      "V2K_CC_SIM"
#define V2K_CC_PC       "V2K_CC_PC"
#define V2K_CC_TMP      "V2K_CC_TMP"
#define V2K_CC_REPOS    "V2K_CC_REPOS"
#define V2K_CC_PC_FLAGS "V2K_CC_PC_FLAGS"
#define V2K_CC_LD_FLAGS "V2K_CC_LD_FLAGS"
#define V2K_LD_RPATH    "V2K_LD_RPATH"
#define V2K_CC_MODE     "V2K_CC_MODE"
#define V2K_SIM_FLAGS   "V2K_SIM_FLAGS"
#define V2K_PC_LIB      "V2K_PC_LIB"
#define V2K_THREAD_LIB  "V2K_THREAD_LIB"
#define V2K_CC_FLAGS    "V2K_CC_FLAGS"
#define V2K_CXX_FLAGS   "V2K_CXX_FLAGS"
#define V2K_CXX_LIBS    "V2K_CXX_LIBS"
#define V2K_CXX_LANG    "V2K_CXX_LANG"
#define V2K_PC_INC      "V2K_PC_INC"
#define V2K_PC_LD_FLAGS "V2K_PC_LD_FLAGS"
#define V2K_SIM_INC     "V2K_SIM_INC"
#define V2K_SIM_LIB     "V2K_SIM_LIB"
#define V2K_PLI_PATH    "V2K_PLI_PATH"
#define V2K_LIB_REPOS   "V2K_LIB_REPOS"
#define V2K_CSRC        "V2K_CSRC"
#define V2K_CODE_REPOS  "V2K_CODE_REPOS"
#define V2K_RC_PATH     "V2K_RC_PATH"
#define V2K_DYNLOADING  "V2K_DYNLOADING"
#define V2K_C_LINE_FRMT "V2K_C_LINE_FRMT"

typedef eSTS (*envCallBack)(const char *,const char *);

#ifdef __cplusplus
extern "C" {
#endif

int  envExpand(char *,int);
void envAdd(const char *,const char *);
eSTS envPathDo(envCallBack,const char *,const char *);

#ifdef __cplusplus
}

class String;
class Stream;

typedef enum {
  SHF_NONE    = 0,
  SHF_ECHO    = 1,
  SHF_VERBOSE = 2,
  SHF_HASH    = 4,
  SHF_BSDECHO = 8,
  SHF_TILDE   = 16

} eSHF;

#define SHF(f) ((eSHF)(f))

int envValue(char *,String *);
int envExpand(String *,Stream *,eSHF,int *,int **);
int envExpand(String *,Stream *,eSHF);
int envExpand(String *,Stream *);
int envExpand(String *);
int envPathFind(const char *,const char *,int,String *);
int envPath2var(const char *,const char *);

#include "pool.h"

typedef void *(*useHandlr)(const char *,const char *,const char*,int);

typedef enum {
  ENV_LOCAL  = 0,
  ENV_EXPORT = 1,
  ENV_SYSTEM = 2,
  ENV_IMPORT = 4,
//
  ENV_NEW    = 128
} eENV;

typedef enum {
  USE_READ   = 1,
  USE_SET    = 2,
  USE_ST_1ST = 4
} eUSE;

class Used {
public:
 char ever,
      again;

 inline void set()   {ever   = 0;
                      again  = USE_ST_1ST;};
 inline void reset() {again |= ((again & USE_READ) ? USE_SET
                                                   : USE_ST_1ST);};
 inline void read()  {ever  |= USE_READ;
                      again |= USE_READ;};
 inline void clear() {again  = 0;};
};

class EnvItem {

  struct EnvItem *next;
  char            flags;
  Used            used;
  int             nml,
                 *ell,
                  items,
                  start;
  char           *name_value;

  char *newNV(int,int);
  void  delNV(char *);
public:

  static EnvItem *List;

  friend class Env;

  EnvItem();
  ~EnvItem();

  EnvItem    *initEnv();
  void        resetUse();
  void       *forAllUsed(useHandlr);
  EnvItem    *add(const char *,const char *,int);
  EnvItem    *add(const char *,int,const char **,int);
  EnvItem    *have(const char *);
  int         split(int);
  int         Shift(int);
  EnvItem    *expEnv(const char *);
  const char *Value() const;
  EnvItem    *def(const char *nm,const char *vl);

  inline       EnvItem *frst()  const {return List;};
  inline       EnvItem *nxt()   const {return next;};
  inline const char    *nm_vl() const {return name_value;};
  inline       int      nm_ln() const {return nml;};
  inline       int      flgs()  const {return flags;}
  inline       bool     array() const {return ell != 0 || items != 1;};
  inline       int      Items()       {return items - start;};

  inline operator const char *() const {return name_value;};

  inline EnvItem *add(const char *nm,const char *vl) {
    return add(nm,vl,ENV_LOCAL);
  };

  bool change(const char *);
  bool change(int);
  bool sub(unsigned int,String *);

  inline void read() {used.read();};
};

class Env {
  const char *val;
  EnvItem    *item;
public:

  friend class EnvItem;

  const char *get(const char *);
  int         Shift();

  Env();
  inline ~Env() {};
  inline Env(const char *nm) {get(nm);};

  void destroy();

  inline EnvItem    *Item()    {return item;};
  inline const char *set_val() {return val = item ? item->Value()
                                                  : 0;};
  inline const char *Value()   {return item ? item->Value()
                                            : 0;};

  inline void frst()  {item = item->frst(); set_val();};
  inline void nxt()   {item = item->nxt();  set_val();};
  inline int  Items() {return item ? item->Items()
                                   : -1;};

  inline void set(const char *nm,const char *vl) {
                                       EnvItem::List->add(nm,vl,ENV_LOCAL);};
  inline void put(const char *nm,const char *vl) {
                                       EnvItem::List->add(nm,vl,ENV_EXPORT);};

  inline bool Change(const char *val) {bool b = item->change(val); set_val();
                                       return b;};

  inline operator const char *() {return val;};
  inline operator bool ()        {return item != 0;};

  inline bool     sub(unsigned int n,String *s) {return item ? item->sub(n,s)
                                                             : 0;};
};

EnvItem * envAddFlg(const char *,const char *,int);

#endif

#endif /* ENV_H */

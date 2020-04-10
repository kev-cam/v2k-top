/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  strfunc_h_rcsid
#define strfunc_h_rcsid() {return "$Id: strfunc.h,v 1.34 2007/09/28 21:53:33 dkc Exp $";} /* RCS ID */

 
#ifndef STRFUNC_H
#define STRFUNC_H

#include "assertions.h"
#include "str_pool-dsc.h"
#include "list.h"

#ifdef __cplusplus
class PoolStr {
public:
PLSTR_FIELDS
  inline const char *str() {return buff;};

  operator const char *() {return buff;};
};
#else
typedef struct {
PLSTR_FIELDS
} PoolStr;
#endif

extern const char NullString[1];

#define STR_FLDS char *buff;\
                 int   l;

#ifdef __cplusplus
typedef struct {
  STR_FLDS
} TmpString;

class File;
class Filename;

class String {
  STR_FLDS
  inline void swap(String *a,String *b) {
    TmpString t;
    t               = *(TmpString *)a;
    *(TmpString *)a = *(TmpString *)b;
    *(TmpString *)b = t;
  };
public:

  static FILE *DevNull;

  inline const char     *str()      const {return buff;};
  inline const Filename *FileName() const {return (Filename *)buff;};
  inline const int       len()      const {return l;};
  inline const char      lastCh()   const {return l ? buff[l-1] : 0;};

  inline String() {l=-2; newBuff(0);};
  ~String();

  int     cut(int c);
  char   *blank(int c);
  int     shift(int);
  String *printf(const char *,...);
  char   *newBuff(int);

  String(const char *,va_list);
  String(const char *);

  String *operator= (const char *);
  String  operator+ (const char *);
  String *operator+=(const char *);
  String *operator-=(const char *);
  bool    operator!=(const char *);
  bool    operator==(const char *);

  char operator[](int i) const {return buff[i];};

  operator const char *() const {return buff;};

  friend class StringStream;
};

class StringListItem : public ListItemT<StringListItem>, public String {
 public:

  inline StringListItem(const char *s) : String(s) {assert((void *)this == (void *)&next);}

  inline void recycle() {delete this;}
};

typedef List<StringListItem,1> StringList;

class StringArray {
 public:
  virtual const char * get(int i) const = 0;
  int items;
  inline const char * operator[](int i) const {return get(i);};
  inline operator int()                 const {ASSERT(items >= 0);
                                               return items;};
};

class StrStrArray : public StringArray {
  const String *str;
 public:
  inline void set(const String *s,int i) {str = s; items = i;};
  StrStrArray()                          {set(0,0);};
  StrStrArray(const String *s)           {set(s,-1);};
  StrStrArray(const String *s,int i)     {set(s, i);};

  virtual const char *get(int i) const   {ASSERT(items < 0 || items > i);
                                          return str[i].str();};
};

class ChStrArray : public StringArray {
  const char **str;
 public:
  inline void set(const char **s,int i) {str = s; items = i;};
  ChStrArray()                          {set(0,0);};
  ChStrArray(const char **s)            {set(s,-1);};
  ChStrArray(const char **s,int i)      {set(s, i);};

  virtual const char *get(int i)  const {ASSERT(items < 0 || items > i);
                                         return str[i];};
};

#else
typedef struct {
  STR_FLDS
} String;
#endif /* __cplusplus */


#endif /* STRFUNC_H */

/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ extern "C" const char * strfunc_cpp_rcsid() {return "$Id: strfunc.cpp,v 1.35 2012/10/16 22:30:51 cvs Exp $";}

#include "system.h"
#include "error.h"
#include "v2k_mem.h"
#define IGNORE_TOK_TABLE
#include "strfunc.h"
#include "strdb.h"
#include "tokpool.h"

const char NullString[1] = "";

#ifdef DEBUG
# define CHK_STR(s)  assert((s->l && strlen(s->buff) == s->l) ||\
                                             s->buff == NullString)
#else
# define CHK_STR(s)
#endif

FILE *String::DevNull = UNSET_PTR_VAL(FILE);

String::~String()
{
  CHK_STR(this);

  if (l > 0) Free(buff);

  buff = 0;
  l    = -1;
}

char *String::newBuff(int nl)
{
  if (l >= 0) {
    if (*buff) {
      Free(buff);
    } else {
      ASSERT(buff == NullString);
    }
  } else {
    ASSERT(-2 == l);
  }

  if ((l = nl)) {
    *(buff = MALLOC2_N(1 + l,char)) = 1;
    buff[l] = 0;
  } else {
    buff = (char *)NullString;
  }

  return buff;
}

String::String(const char *format,va_list pvar)
{
  int nl  = 2 * MAXPATHLEN,
      rsz = 1;

  if (UNSET_PTR(DevNull)) {DevNull = fopen(DEV_NULL,"w");}

#ifndef BAD_STDARG
  if (DevNull) {
    nl  = vfprintf(DevNull, format, pvar);
    rsz = 0;
  }
#endif

  l = -2;
  newBuff(nl);

  if (nl) {
    nl = vsprintf(buff, format, pvar);
    if (rsz) {
      l = strlen(buff);
      assert(l <= nl);
#ifdef BAD_STDARG
      if (0 == nl) *buff = 1;
#endif
      if (!l) newBuff(l);
    }
  }

done:
  CHK_STR(this);
}

String::String(const char *str)
{
  if (str) {
    int nl = strlen(str);

    l = -2;
    newBuff(nl);

    if (nl) strcpy(buff,str);
  } else {
    l    = -2;
    buff = 0;
  }
}

int String::cut(int c)
{
  int ok = 0;

  if (c < 0) c = 0;

  if (c <= l) {
    if (l > 0) {
      if (c == 0) {
        newBuff(0);
      } else {
        buff[l = c] = '\0';
      }
    }
    ok = 1;
  }

  CHK_STR(this);
  return ok;
}

char *String::blank(int c)
{
  int ok = 0;

  if (c < l) {
    cut(c);
  } else {
    newBuff(c);
    buff[c] = 0;
  }

  while (c-- > 0) buff[c] = ' ';

  CHK_STR(this);
  return buff;
}

int String::shift(int c)
{
  int ok = 0;

  if (c < l) {
    int n;
    for(n=0; buff[c+n]; n++) {
      buff[n] = buff[c+n];
    }
  }

  return cut(l - c);
}

String *String::operator=(const char *str)
{
  int l2 = strlen(str);

  if (l2 > l) {
    newBuff(l2);
  } else if (!l2) {
    newBuff(l2);
    goto done;
  } else {
    l = l2;
  }

  strcpy(buff,str);

done:
  CHK_STR(this);
  return this;
}

String String::operator+(const char *str)
{
  String ret;

  int lr = strlen(str);
  ret.l  = l + lr;

  if (ret.l) {
    ret.newBuff(ret.l);
    strcpy(ret.buff,buff);
    strcpy(&ret.buff[l],str);
  }

  CHK_STR(this);
  return ret;
}

String *String::operator+=(const char *str)
{
  int lr = strlen(str);

  if (lr) {
    TMPARR(char,tb,1 + l);
    strcpy(tb,buff);
    int ol = l;
    strcpy(newBuff(ol + lr),tb);
    strcat(&buff[ol],str);
    CHK_STR(this);
  }

  return this;
}

String *String::operator-=(const char *str)
{
  int lr =strlen(str);

  assert (l >= lr);

  buff[l -= lr] = '\0';

  if (0 == l) {
     Free(buff);
     buff = (char *)NullString;
  }

  return this;
}

bool String::operator!=(const char *str)
{
  if (str == buff || 0 == strcmp(str,buff)) return 0;

  return 1;
}

bool String::operator==(const char *str)
{
  if (str == buff || 0 == strcmp(str,buff)) return 1;

  return 0;
}

String *String::printf(const char *format,...)
{
  va_list pvar;

  va_start(pvar, format);

  String tmp(format,pvar);

  va_end(pvar);

  swap(this,&tmp);

  return this;
}



/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ const char * misc_c_rcsid() {return "$Id: misc.c,v 1.19 2012/10/16 22:30:51 cvs Exp $";}

#include "v2k_mem.h"
#include "error.h"
#include "v2k_misc.h"
#include "v2k_ll.h"

void CopyVirtual(const void *from,void *to)
{
  int sz = SIZEOF_VTAB;
  BCOPY(from,to,sz);
}

const Limit Limits[] = {
#if defined(UNIX) && ! defined(__CYGWIN__)
                        {RLIMIT_CPU,   0,     "cputime"},
                        {RLIMIT_FSIZE, 0x200, "filesize"},
                        {RLIMIT_DATA,  0x400, "datasize"},
                        {RLIMIT_STACK, 0x400, "stacksize"},
                        {RLIMIT_CORE,  0x400, "coredumpsize"},
		        {RLIMIT_NOFILE,0,     "descriptors"},

#ifdef RLIMIT_VMEM
                        {RLIMIT_VMEM,  0x400, "vmemoryuse"},
#endif
#else
#define NO_LIMITS
#endif
                        {-1,0,0}};

int setRlimit(int rl,I64 val)
{
#ifdef NO_LIMITS
  return -1;
#else
  struct rlimit rlim;

  if (val < 0) rlim.rlim_cur = RLIM_INFINITY;
  else         rlim.rlim_cur = val;

  rlim.rlim_max = RLIM_INFINITY;

  return setrlimit(rl, &rlim);
#endif
}

int getRlimit(int rl,I64 *ret)
{
#ifdef NO_LIMITS
  return -1;
#else
  struct rlimit rlim;

  if (getrlimit(rl, &rlim) < 0) return -1;

  SET_LL(*ret,rlim.rlim_cur);

  return RLIM_INFINITY != rlim.rlim_cur;
#endif
}

int getRlimitInt(int rl)
{
#ifdef NO_LIMITS
  return -1;
#else
  struct rlimit rlim;

  if (getrlimit(rl, &rlim) < 0) return -1;

  return rlim.rlim_cur;
#endif
}

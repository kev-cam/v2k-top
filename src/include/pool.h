/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  pool_h_rcsid
#define pool_h_rcsid() {return "$Id: pool.h,v 1.30 2007/07/10 23:31:07 dkc Exp $";} /* RCS ID */

 
#ifndef POOL_TEMPLATE
#define POOL_TEMPLATE

#include "v2k_mem.h"
#include "desc.h"

typedef enum {
  PMF_Trash     =  -1,
  PMF_None      =   0,
  PMF_InMem     =   1,
  PMF_Contig    =   2,
  PMF_OnDisk    =   4,
  PMF_Virtual   =   8,
  PMF_Mapped    =  16,
  PMF_Shared    =  32,
  PMF_Shadow    =  64,

  PMF_Locked    = 256,
  PMF_LargeBlks = 512
} ePMF;

#define PMF(f) ((ePMF)(f))

typedef enum {
  PM_None       = 0,
  PM_Trash      = PMF_Trash,
  PM_Mask       = PMF_Locked -1,
  PM_InMem      = PMF_InMem,
  PM_Contig     = PMF_Contig,
  PM_OnDisk     = PMF_OnDisk,
  PM_Virtual    = PMF_Virtual,
  PM_Mapped     = PMF_Mapped,
  PM_CtgMppd    = PMF_Contig|PMF_Mapped,
  PM_BaseModes  = PMF_InMem|PMF_Contig|PMF_OnDisk|PMF_Virtual|PMF_Mapped,
  PM_LoadFlags  = PM_BaseModes|PMF_Shared
} ePM;

extern ePM defPoolType;

#define PM(f) ((ePM)(f))

#include "defpmode.h"
#include "padding.h"
#include "strfunc.h"
#include "file.h"
#include "tref.h"

#ifdef __cplusplus

class BasePool {
#define THIS_CLASS   BasePool
#undef  VIRTUAL_MODE
#define VIRTUAL_MODE = 0
public:
#include "TemplatePool.vfn"
#undef  THIS_CLASS
#undef  VIRTUAL_MODE
#define VIRTUAL_MODE
};

#endif /* __cplusplus */

typedef struct { /* Overlay of TemplatePool */
  void *vfn_ptr;
#include "TemplatePool.fld"
} sPool;

#define MIN_CTG_POOL    0x100000
#define CTG_POOL_EXT    0x100000

#define CCH_BLK_SZ      0x10000
#define CCH_BLK(I)      ((I)>>16)
#define CCH_BLK_OFF(I)  ((I)&0xFFFF)

typedef enum {
  BI_LOCK     = 1,
  BI_WRITTEN  = 2,
  BI_READ     = 4,
  BI_EXTENDED = 8
} eBI;

#define PM_DATA_CMN  ePM pmode;

typedef struct {
  PM_DATA_CMN
} PMplData;

typedef struct {
  char     *data;
  I32       flags;
} BlockInfo;

typedef struct {
  PM_DATA_CMN
  I32       top_block;
  BlockInfo mapped[UNSIZED(top_block+1)];
} CchData;

void clearCache(CchData *);

typedef struct {
  PM_DATA_CMN
  I32       top_block;
  char     *flags;
} FlagData;

#ifdef __cplusplus
class MappedFile;
typedef struct {
  PM_DATA_CMN
  I32         ownr_prcss;
  FlagData    flag_data; /* overlay */
  MappedFile *file;
} MapData;
#endif

#define SIZEOF_CCH(n)    (sizeof(MapData) + SIZED(BlockInfo,n))

#endif /* POOL_TEMPLATE */

extern const char *strPoolMode[16];

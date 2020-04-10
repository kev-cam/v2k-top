/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  models_h_rcsid
#define models_h_rcsid() {return "$Id: models.h,v 1.12 2007/03/28 07:23:25 dkc Exp $";} /* RCS ID */
 
#ifndef  MODELS_H
#define  MODELS_H

typedef void (*prtCB)(int,PortDir,void *);

typedef enum {
  PIU_NONE  =  0,
  PIU_UNSET = -1,
  PIU_XMR   = -2
} ePIuse;

typedef struct {
  int           index;
  unsigned int  width;
  long          offset,
                used;  /* ePIuse */
  char          pckd,
                unpckd,
                xmr;
  int          *dimn;
} PortInfo;

typedef enum {
  CX_BAD   = -1,
  CX_SAME  =  0,
  CX_DIFF  =  1,
  CX_MERGE =  2
} eCX;

#define SI_NO_XTRA ((void *)-1)

typedef enum {
  DM_BAD       = -1,
  DM_FLAGS     =  0,
  DM_COUNT_DRV =  1,
  DM_COUNT_RCV =  2
} eDM;

class model {
public:
#define MODEL_VIRTUAL_MODE =0
#include "model.vfn"

};

typedef model *(*gateInfoFn)(int);

typedef struct {
  gateInfoFn   info;
  model      **gates;
} GateLib;

extern GateLib *V2KgateLib;

int  FixPortWidth(void *,PortInfo *,int,int);

#endif

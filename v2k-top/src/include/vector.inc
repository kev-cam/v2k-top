/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: $Id: vector.inc,v 1.8 2007/02/01 06:47:29 dkc Exp $ */
  


class VEC_CLASS : public VEC_PARENT {
public:
  DATA_TYPE drvn  DATA_WIDTH;
  DATA_TYPE lvl   DATA_WIDTH;
  DATA_TYPE knwn  DATA_WIDTH;
#ifdef DONT_CARE
  DATA_TYPE dc    DATA_WIDTH;
#endif
};

#ifdef VEC_ARRAY
class VEC_ARRAY : public VEC_PARENT {
public:
  struct {
    DATA_TYPE drvn  DATA_WIDTH;
    DATA_TYPE lvl   DATA_WIDTH;
    DATA_TYPE knwn  DATA_WIDTH;
#ifdef DONT_CARE
    DATA_TYPE dc    DATA_WIDTH;
#endif
  } array[1];
};
#endif

#undef  DATA_WIDTH
#define DATA_WIDTH
#undef  DATA_TYPE
#undef  VEC_CLASS
#undef  VEC_PARENT
#undef  VEC_ARRAY
#undef  DONT_CARE

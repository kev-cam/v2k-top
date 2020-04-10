/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: $Id: ports.inc,v 1.26 2007/02/01 06:49:08 dkc Exp $ */
  


      case VER_REAL:    p_io = PRT_NC;
                        st   = ST_VEC;
                        disc = -1;
                        drv  = DS_NORM;
                        pt   = PT_REAL;
                        val  = 0;
                        goto ports;
      case VER_GENVAR:  p_io = PRT_NC;
                        st   = ST_VEC;
                        disc = -1;
                        drv  = DS_NORM;
                        pt   = PT_GENVAR;
                        val  = 0;
                        goto ports;
      case VER_INTEGER: p_io = PRT_NC;
                        st   = ST_VEC;
                        disc = -1;
                        drv  = DS_NORM;
                        pt   = PT_INT;
                        val  = 0;
                        goto ports;
      case VER_TIME:    p_io = PRT_NC;
                        st   = ST_VEC;
                        disc = -1;
                        drv  = DS_NORM;
                        pt   = PT_TIME;
                        val  = 0;
                        goto ports;
      case VER_EVENT:   p_io = PRT_NC;
                        st   = ST_BIT;
                        drv  = DS_NORM;
                        disc = -1;
                        pt   = PT_EVENT;
                        val  = 0;
                        goto ports;
      case VER_WIRE:    p_io = PRT_NC;
                        st   = ST_BIT;
                        drv  = DS_NORM;
                        disc = -1;
                        pt   = PT_WIRE;
                        val  = 0;
                        goto ports;
      case VER_TRI:     p_io = PRT_NC;
                        st   = ST_TRI;
                        drv  = DS_NORM;
                        disc = -1;
                        pt   = PT_NONE;
                        val  = 0;
                        goto ports;
      case VER_TRI1:    p_io = PRT_NC;
                        st   = SIG_TYPE(ST_TRI|ST_PULLUP);
                        drv  = DS_NORM;
                        disc = -1;
                        pt   = PT_NONE;
                        val  = 1;
                        goto ports;
      case VER_SUPPLY0: p_io = PRT_NC;
                        st   = ST_STRENGTH;
                        drv  = DS_SUPPLY;
                        disc = -1;
                        pt   = PT_NONE;
                        val  = 0;
                        goto ports;
      case VER_WAND:    p_io = PRT_NC;
                        st   = ST_WAND;
                        drv  = DS_NORM;
                        disc = -1;
                        pt   = PT_NONE;
                        val  = 0;
                        goto ports;
      case VER_TRIAND:  p_io = PRT_NC;
                        st   = SIG_TYPE(ST_TRI|ST_WAND);
                        drv  = DS_NORM;
                        disc = -1;
                        pt   = PT_NONE;
                        val  = 0;
                        goto ports;
      case VER_TRI0:    p_io = PRT_NC;
                        st   = SIG_TYPE(ST_TRI|ST_PULLDOWN);
                        drv  = DS_NORM;
                        disc = -1;
                        pt   = PT_NONE;
                        val  = 0;
                        goto ports;
      case VER_SUPPLY1: p_io = PRT_NC;
                        st   = ST_STRENGTH;
                        drv  = DS_SUPPLY;
                        disc = -1;
                        pt   = PT_NONE;
                        val  = 1;
                        goto ports;
      case VER_WOR:     p_io = PRT_NC;
                        st   = ST_WOR;
                        drv  = DS_NORM;
                        disc = -1;
                        pt   = PT_NONE;
                        val  = 0;
                        goto ports;
      case VER_TRIOR:   p_io = PRT_NC;
                        st   = SIG_TYPE(ST_TRI|ST_WOR);
                        drv  = DS_NORM;
                        disc = -1;
                        pt   = PT_NONE;
                        val  = 0;
                        goto ports;
      case VER_TRIREG:  p_io = PRT_NC;
                        st   = SIG_TYPE(ST_TRI|ST_CHARGE);
                        drv  = DS_NORM;
                        disc = -1;
                        pt   = PT_REG;
                        val  = 0;
                        goto ports;
      case VER_REG:     p_io = PRT_NC;
                        st   = ST_BIT;
                        drv  = DS_NORM;
                        disc = -1;
                        pt   = PT_REG;
                        val  = 0;
#ifdef PPORTS
                        seq  = 1;
#endif
                        goto ports;
      case VER_INPUT:   p_io = PRT_IN;
                        goto port_dir;
      case VER_INOUT:   p_io = PRT_INOUT;
                        goto port_dir;
      case VER_OUTPUT:  p_io = PRT_OUT;
                        goto port_dir;

#ifndef PPORTS
 new_param:
        reportError(STS_SYNTAX,"Bad punctuation at 'parameter' ?");
#ifdef BPORTS
      case VER_SPECPARAM:
#endif
      case VER_PARAMETER:
#include "param.inc"

        if (!SAME_REF_PI(Ref1,PUNCTUATION_POOL,PUNC_SCOLON)) {
          reportError(STS_SYNTAX,"Missing ';'");
        }

        break;
#endif

#undef MPORTS
#undef FPORTS
#undef TPORTS
#undef PPORTS
#undef BPORTS

/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  fmode_h_rcsid
#define fmode_h_rcsid() {return "$Id: fmode.h,v 1.8 2007/03/28 07:23:26 dkc Exp $";} /* RCS ID */
 
FMODE(FM_READ,   'r','R')
FMODE(FM_WRITE,  'w','W')
FMODE(FM_APPEND, 'a','A')
FMODE(FM_RAW,    'd','D')
FMODE(FM_CREATE, 'c','C')
FMODE(FM_TRUNC,  't','T')
FMODE(FM_DCLOSE, 'k','K')
FMODE(FM_EXE,    'x','X')
FMODE(FM_ZCLOSE, 'z','Z')
FMODE(FM_MKDIR,  'p','P')
FMODE(FM_SHARED, 's','S')
FMODE(FM_ZPIPED, '|','-')
FMODE(FM_CMP,    'u','U')

#undef FMODE

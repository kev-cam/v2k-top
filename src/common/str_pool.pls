# Copyright (c) 1998,1999,2001,2002,2003 Kevin Cameron
# Distributed under the GNU Lesser General Public License
# RCS ID: $Id: str_pool.pls,v 1.6 2003/03/21 10:11:02 dkc Exp $
#


STRUCT plStr
  short l;
  char  VAR_ARRAY(buff);

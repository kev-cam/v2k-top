#!/bin/csh -f
# Copyright (c) 1998-2007 Kevin Cameron
# Distributed under the GNU Lesser General Public License
# RCS ID: $Id: gnprs.csh,v 1.10 2007/10/09 21:24:29 dkc Exp $
 
set out = $1
shift

set debug = ()

switch ($1)
  case -v*:
    set verbose
    set echo
    breaksw
  case -dbg:
    shift
    set debug = (debug)
endsw

if (! $?LD_LIBRARY_PATH) then
  setenv LD_LIBRARY_PATH /usr/local/lib
endif

setenv LD_LIBRARY_PATH ${OBJ_LIB_DIR}:$LD_LIBRARY_PATH
rm -f $out:q
set gnprs = $OBJ_EXE_DIR/genparse$CMODEX$OBJ_PRFX
while ( ! -x $gnprs )
  if ( -x $OBJ_EXE_DIR/genparse ) then
    set gnprs = $OBJ_EXE_DIR/genparse
    break
  else
    if ($?GNPRS_BLD) exit 1
    setenv GNPRS_BLD 1
    echo "Building genparse [$MK_SCRIPT gnprs_exe GNPRS_ONLY=1]"
    $MK_SCRIPT gnprs_exe GNPRS_ONLY=1\
			 CXX_FILES= PCXX_FILES=\
                         C_FILES="genparse.c language.c"
    set sts = $status
    if ($sts) exit $sts
  endif
end
$debug $gnprs $*:q > $out:q-tmp
set sts = $status
if (0 == $sts) mv $out:q-tmp $out:q

exit $sts

#!/bin/csh -f
# Copyright (c) 1998,1999,2001,2002,2003 Kevin Cameron
# Distributed under the GNU Lesser General Public License
# RCS ID: $Id: spice3.csh,v 1.5 2003/01/14 02:25:59 dkc Exp $
 
set exe     = "$0"
set scr_dir = "$exe:h"
set appl    = "$exe:t"
switch ($scr_dir)
 case */:
   set scr_dir = `echo $scr_dir | sed 's+/*$++'`
endsw
set root    = "$scr_dir:h"

switch ($root)
  case /*:
    breaksw
  default:
    set root = $cwd/$root
endsw

set app_dir = $appl:q
switch ($appl)
  case spice3:
  case nutmeg:
    set app_dir = spice
    breaksw
endsw

if ( -d $root/import/$app_dir ) then
  set root = $root/import/$app_dir
endif

eval `$scr_dir/mk -cshmc`

switch ($SYS_OS-$SYS_CPU)
  case sol5*:
    setenv SPICE_NO_DATASEG_CHECK TRUE
endsw

set exe_dir = $root/bin/$OBJ_DIR
set path    = ($path $exe_dir)

eval set ok = '$?'$SYS_LP
if $ok then
  eval setenv $SYS_LP '"${'${SYS_LP}}:$root/lib/$OBJ_DIR'"'
else
  setenv $SYS_LP "$root/lib/$OBJ_DIR"
endif

set cmode=""
set wrap=()
set args=()

while (1)
  switch ("$1")
    case -O:
      set cmode = -O
      shift
      breaksw
    case -g:
      set cmode = -g
      shift
      breaksw
    case -dbg:
      set wrap = debug
      shift
      breaksw
    case -doc:
      if ( ! $?BROWSER ) then
        set BROWSER=netscape
      endif
      exec $BROWSER $root/html/index.html
      breaksw
    case -help:
      echo "-dbg            Run under debugger"
      echo "-doc            Launch document browser"
      set args = ($args:q $1)
      shift
      breaksw
    default:
      exec $wrap $exe_dir/$appl$cmode $args:q $*:q
  endsw
end

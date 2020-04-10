#!/bin/csh -f
# Copyright (c) 1998,1999,2001,2002,2003 Kevin Cameron
# Distributed under the GNU Lesser General Public License
# RCS ID: $Id: v2k.csh,v 1.27 2009/07/14 18:12:30 dkc Exp $
 
set exe     = "$0"
switch ($exe:q)
 case /*.csh:
   set exe  = $exe:r
 case /*:
   breaksw
 case *.csh:
   set exe  = $exe:r
 default:
   set exe  = $cwd/$exe:q
endsw
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
    set root = `cd $root ; echo $cwd`
endsw

switch ($root)
  case */export:
    set root = $root:h
endsw

set mkarg = (-cshmc)
if ("$1" == "-lxrun") then
  set mkarg = (-uname "$2" -cshmc)
endif
eval `$scr_dir/mk $mkarg:q`

set app_dir = $appl:q
if ( -d $root/import/$app_dir ) then
  set root          = `cd $root/import/$app_dir ; echo $cwd`
  set exe_dir       = $root/bin/$OBJ_DIR
  setenv V2K_LD_PATH "$root/lib/$OBJ_DIR/"
else
  set    exe_dir    = $root/bin/$OBJ_DIR
  setenv V2K_LD_PATH "$root/lib/$OBJ_DIR/"
endif

set path = ($exe_dir $path:q)

eval set ok = '$?'$SYS_LP
if $ok then
  eval setenv $SYS_LP '"${'${SYS_LP}}:$V2K_LD_PATH'"'
else
  setenv $SYS_LP "$V2K_LD_PATH"
endif

set cmode=""
set wrap=()
set args=()
set exe=v2k
set perl=perl

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
    case -xv:
    case -vx:
      set echo
      set verbose
      shift
      breaksw
    case -ldd:
      set wrap = ldd
      shift
      breaksw
    case -strace:
      set wrap = strace
      shift
      breaksw
    case +X:
      unsetenv DISPLAY
      shift
      breaksw
    case -dbg:
      set wrap = debug
      shift
      breaksw
    case -cygcheck:
      set wrap  = cygcheck
      set exe   = $exe:q$cmode.exe
      set cmode = ""
      shift
      breaksw
    case -vlgrnd*:
      set wrap = (valgrind `echo $1 | sed s/-vlgrnd//`)
      shift
      breaksw
    case -memdbg:
      shift
      switch ($SYS_OS)
        case sol*:
	  setenv LD_PRELOAD watchmalloc.so.1
          switch ($1)
            case 2:
              setenv MALLOC_DEBUG WATCH
              breaksw
            case 3:
              setenv MALLOC_DEBUG WATCH,RW
              breaksw
            case 4:
              setenv MALLOC_DEBUG WATCH,RW,STOP
              breaksw
            default:
              unsetenv MALLOC_DEBUG
          endsw
          breaksw
        case lnx*:
          setenv MALLOC_TRACE malloc.trace
	  setenv MALLOC_CHECK_ 2
          breaksw
      endsw
      shift
      breaksw
    case -perl-dbg:
      setenv V2K_PRL_DBG 1
    case -perl:
      setenv V2K_EXE $exe_dir/$exe$cmode
      eval setenv $SYS_LP \${V2K_LD_PATH}/perl:\$$SYS_LP
      set wrap    = ($perl -I$root/export/pm)
      set exe_dir = $root/export/bin
      set exe     = ${exe}.pl
      shift
      breaksw
    case -xl:
      set wrap = ($scr_dir/v2v --xl --ignore1)
      shift
      breaksw
    case -vcs:
      set wrap = ($scr_dir/v2v --vcs --ignore1)
      shift
      breaksw
    case -lxrun:
      set wrap = (lxrun)
      shift
      shift
      breaksw
    case -time-out:
      shift
      csh -c "sleep $1 ; kill $$ &" >& /dev/null
      shift
      breaksw
    case -exe:
      shift
      set exe = $1
      shift
      breaksw
    case -echo:
      shift
      echo $wrap $exe_dir/$exe$cmode $args:q $*:q > /dev/tty
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
      exec $wrap $exe_dir/$exe$cmode $args:q $*:q
  endsw
end

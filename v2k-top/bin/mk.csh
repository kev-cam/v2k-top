#!/bin/csh -f
# Copyright (c) 1998,1999,2001,2002,2003 Kevin Cameron
# Distributed under the GNU Lesser General Public License
# RCS ID: $Id: mk.csh,v 1.61 2020/04/06 21:43:47 dkc Exp $
 
# set MAKE_FILES_PREF to change the order of defaults

if $?MK_DEBUG then
  set verbose
  set echo
endif

if ($?MK_TRACE && "$1" != "-cshmc") then
  echo $cwd
  echo $0 $*
endif

if ("$1" == "-env") then
  shift
  set hm = $home
  env | cut -d= -f1 | egrep -v "MK_64|MK_|MKDEPS|LD_LIBRARY_PATH|CMODE|UNAMEA|USER|PROJECT|USR_|WARN|PURIFY|_VERSION|^\!" | awk '{print "unsetenv ",$1}' > /tmp/mk.$$
  source /tmp/mk.$$ ; rm /tmp/mk.$$
  set home = $hm
endif

set pth=$0
switch ($pth)
  case /*:
    breaksw
  default:
    set pth = $cwd/$pth
endsw

if ! $?MK_SCRIPT then
  setenv MK_SCRIPT $pth
endif

set pth=$pth:h

set args     = ()
set subargs  = ()
set tries    = 0
set gmake    = gmake
set cmode    = -g
set targets  = 0
set show     = 0
set work_dir = $cwd
set ntv_cc   = cc
set ntv_dbg  = dbx
set use_dbg  = ()
set ntv_cpp  = c++

if $?MK_JOBS then
  set jobs = "$MK_JOBS"
else
  set jobs = ""
endif

if ( -f ~/.mkrc && "$1" != "-clean" ) then
  source ~/.mkrc
endif

if $?USR_CFLAGS then
  set defs   = ($USR_CFLAGS)
else
  set defs   = ()
endif

if $?USR_CXXFLAGS then
  set xdfs   = ($USR_CXXFLAGS)
else
  set xdfs   = ()
endif

unset mf

if $?UNAMEA then
  set uname = ($UNAMEA)
else
  set uname = (`uname -a | sed 's+;+=+g'`)
endif

if  ($#uname > 7) then
   set uname_orig = ($uname:q)
   set uname = ($uname_orig[1] $uname_orig[2] $uname_orig[3] $uname_orig[4])
@  e     = $#uname_orig - 2
   set i = 5
   while ( $i <= $e )
     set uname = ($uname:q+$uname_orig[$i])
@    i = $i + 1
   end
   set uname = ($uname:q $uname_orig[$i] $uname_orig[$#uname_orig])
endif

if ! $?MK_REALLY then
  setenv MK_REALLY 1
endif

new_uname:
setenv UNAMEA "$uname"
setenv SYS_LP LD_LIBRARY_PATH
@ unm_1 = $#uname - 1
@ unm_2 = $#uname - 2
switch ($uname[1]-$uname[3]-$uname[$unm_2]-$uname[$unm_1]-$uname[$#uname])
  case SunOS-5.{5,6}-i86*:
      setenv OBJ_DIR sx_obj
      setenv SYS_OS  sol5_5
      setenv SYS_CPU i86
      breaksw
  case SunOS-5.1?-i86*:
      setenv SYS_OS  sol5_10
      if ($?MK_64) then
        setenv OBJ_DIR sx_obj-64
        setenv SYS_CPU x64
      else
        setenv OBJ_DIR sx_obj
        setenv SYS_CPU i86
      endif
      breaksw
  case SunOS-5.?-i86*:
      setenv OBJ_DIR sx_obj
      setenv SYS_OS  sol5_7
      setenv SYS_CPU i86
      breaksw
  case SunOS-5.{5,6}-sun*:
      setenv OBJ_DIR sx_obj
      setenv SYS_OS  sol5_5
      setenv SYS_CPU sprc
      breaksw
  case SunOS-5.*-sun*:
      setenv OBJ_DIR sp_obj
      setenv SYS_OS  sol5_7
      setenv SYS_CPU sprc
      breaksw
  case Linux-2.2*-sparc-*:
      setenv OBJ_DIR lx2_obj
      setenv SYS_OS  lnx_220
      setenv SYS_CPU sprc
      set    gmake = make
      breaksw
  case Linux-2.4*-x86_64-*:
      setenv USR_LNX_64 YES
      setenv OBJ_DIR lx4_obj-64
      setenv SYS_OS  lnx_240
      setenv SYS_CPU i86
      set    gmake = make
      breaksw
  case Linux-2.6*-x86_64-*:
      setenv USR_LNX_64 YES
      setenv OBJ_DIR lx4_obj-64
      setenv SYS_OS  lnx_260
      setenv SYS_CPU i86
      set    gmake = make
      breaksw
  case Linux-3.0*-x86_64-*:
      setenv USR_LNX_64 YES
      setenv OBJ_DIR lx3_obj-64
      setenv SYS_OS  lnx_300
      setenv SYS_CPU x64
      set    gmake = make
      breaksw
  case Linux-3.1*-x86_64-*:
      setenv USR_LNX_64 YES
      setenv OBJ_DIR lx3_obj-64
      setenv SYS_OS  lnx_310
      setenv SYS_CPU x64
      set    gmake = make
      breaksw
  case Linux-3.2*-x86_64-*:
      setenv USR_LNX_64 YES
      setenv OBJ_DIR lx3_obj-64
      setenv SYS_OS  lnx_320
      setenv SYS_CPU x64
      set    gmake = make
      breaksw
  case Linux-2.4*:
      setenv OBJ_DIR lx4_obj
      setenv SYS_OS  lnx_240
      setenv SYS_CPU i86
      set    gmake = make
      breaksw
  case Linux-2.6*-x86_64-*:
      setenv OBJ_DIR lx6_obj-64
      setenv SYS_OS  lnx_260
      setenv SYS_CPU x64
      set    gmake = make
      breaksw
  case Linux-4.1*-x86_64-*:
      setenv OBJ_DIR lx8_obj-64
      setenv SYS_OS  lnx_410
      setenv SYS_CPU x64
      set    gmake = make
      breaksw
  case Linux-2.6*:
      setenv OBJ_DIR lx6_obj
      setenv SYS_OS  lnx_260
      setenv SYS_CPU i86
      set    gmake = make
      breaksw
  case Linux-3.0*:
      setenv OBJ_DIR lx3_obj
      setenv SYS_OS  lnx_300
      setenv SYS_CPU i86
      set    gmake = make
      breaksw
  case Linux-3.1*:
      setenv OBJ_DIR lx3_obj
      setenv SYS_OS  lnx_310
      setenv SYS_CPU i86
      set    gmake = make
      breaksw
  case Linux-3.2*:
      setenv OBJ_DIR lx3_obj
      setenv SYS_OS  lnx_320
      setenv SYS_CPU i86
      set    gmake = make
      breaksw
  case Linux-2.2*:
      setenv OBJ_DIR lx2_obj
      setenv SYS_OS  lnx_220
      setenv SYS_CPU i86
      set    gmake = make
      breaksw
  case Linux-2.0*:
      setenv OBJ_DIR lnx_obj
      setenv SYS_OS  lnx_200
      setenv SYS_CPU i86
      set    gmake = make
      breaksw
  case HP-UX-B*-*:
      setenv OBJ_DIR hpb_obj
      setenv SYS_OS  hpb
      setenv SYS_CPU PAR
      breaksw
  case Darwin*-Macintosh-powerpc:
      setenv OBJ_DIR ppc_obj
      setenv SYS_OS  mcd
      setenv SYS_CPU PPC
      set gmake    = make
      breaksw
  case CYGWIN*:
      setenv OBJ_DIR c86_obj
      setenv SYS_OS  cyw
      setenv SYS_CPU i86
      if $MK_REALLY then
        set  gmake = make
        set  args = ($args:q --unix)
      endif
      breaksw
  case HP-UX-A*-*:
      setenv OBJ_DIR hpa_obj
      setenv SYS_OS  hpa
      setenv SYS_CPU PAR
      breaksw
  default:
      setenv OBJ_DIR obj
      setenv SYS_OS  $uname[1]-$uname[3]
      setenv SYS_CPU $uname[5]
      if ( -f $pth/../os.local ) then
        source $pth/../os.local
      endif
endsw

# GNU bug workaround:

(limit descriptors 256>&/dev/null)
if ! $?status then
  limit descriptors 256
endif

setenv C_CMPLR   gcc
setenv CXX_CMPLR g++

setenv MK_FLAGS  ""

while ($#argv)
  switch ("$1")
    case -clean:
      breaksw
    case -C:
    case -cd:
      shift
      if ( ! -d $1 ) then
        echo "Can't cd to: $1"
        exit 1
      endif
      cd $1
      set work_dir = $cwd
      breaksw
    case -O:
      set args = ($args:q CMODE=-O)
      setenv CMODE -O
      breaksw
    case -g:
      set args = ($args:q CMODE=-g)
      setenv CMODE -g
      breaksw
    case -w:
      set args = ($args:q WARN=1)
      setenv CMODE -g
      breaksw
    case -vx:
    case -xv:
      set verbose
      set echo
      breaksw
    case -W:
      set args = ($args:q WARN=2)
      setenv CMODE -g
      breaksw
    case -wd:
      shift
      set work_dir = $1
      breaksw
    case -gO:
      set args = ($args:q CMODE=-gO)
      setenv CMODE -gO
      breaksw
    case -pg:
      unsetenv SHARED_LIBS
      set args = ($args:q CMODE=-pg)
      setenv CMODE -pg
      breaksw
    case -nothrd:
      setenv USR_NOTHREADS 1
      set defs = ($defs:q -DNOTHREADS)
      breaksw
    case -nosdf:
      setenv USR_NO_SDF 1
      set defs = ($defs:q -DNO_SDF)
      breaksw
    case -sttc:
      setenv SHARED_LIBS 0
      set subargs = ($subargs:q -sttc)
      breaksw
    case -shared:
    case -shrd:
      setenv SHARED_LIBS 1
      set subargs = ($subargs:q -shrd)
      breaksw
    case +shrd:
      setenv SHARED_LIBS 2
      set subargs = ($subargs:q +shrd)
      breaksw
    case -perl:
      setenv PERL_LIB 1
      set subargs = ($subargs:q -perl)
      breaksw
    case -efence
      switch ($SYS_OS)
        case lnx*:
          setenv DEBUG_LIBS "-L/usr/lib -lefence"
          breaksw
        default:
          setenv DEBUG_LIBS "-L/usr/local/lib -lefence"
          breaksw
      endsw
      breaksw
    case -dbg:
      setenv DEBUG_EXIT 2
      set args = ($args:q MK_DEBUGGER="debug -w $use_dbg")
      breaksw
    case -memdbg:
      switch ($SYS_OS)
        case sol*:
	  setenv LD_PRELOAD watchmalloc.so.1
          breaksw
      endsw
      breaksw
    case -wkshp:
      setenv DEBUG_EXIT 2
      set args = ($args:q MK_DEBUGGER="wkshp")
      breaksw
    case -i:
      setenv MK_FLAGS "$MK_FLAGS -i"
      set args = ($args:q -i)
      breaksw
    case -f:
      shift
      set mf = $1
      breaksw
    case -lmt:
      limit memorysize 20M
      limit datasize   20M
      limit stacksize  20M
      breaksw
    case -slmt:
      set subargs = ($subargs:q -lmt)
      breaksw
    case -rcs:
      set args = ($args:q CVSROOT=)
      breaksw
    case CLEAN:
    case clean:
      set args = ($args:q $1 )
      goto fast
    case setup:
      set args = ($args:q SETUP=1 setup)
      goto fast
    case -sfast:
      set subargs = ($subargs:q IGNORE_DEPS=1)
      breaksw
    case -FAST:
      set subargs = ($subargs:q $1:q)
fast:
    case -fast:
      set args = ($args:q IGNORE_DEPS=1)
      breaksw
    case -swig:
      set args = ($args:q HAVE_SWIG=1)
      breaksw
    case -sj:
      shift
      set subargs = ($subargs:q -j $1)
      breaksw
    case -sub:
      shift
      set subargs = ($subargs:q $1:q)
      breaksw
    case -sw:
      set args = ($args:q EXIT_WARN=1)
      breaksw
    case -sfw
      set path = (/usr/sfw/bin $path:q)
      breaksw
    case -m64
      setenv MK_64
      setenv OBJ_DIR sx_obj-64
      setenv SYS_CPU $SYS_CPU-64
      breaksw
    case -ntv:
      setenv MK_FLAGS "$MK_FLAGS -ntv"
      set args    = ($args:q C_CMPLR=$ntv_cc CXX_CMPLR=$ntv_cpp OBJ_PRFX=n)
      set use_dbg = "-$ntv_dbg"
      breaksw
    case -llvm:
      setenv MK_FLAGS "$MK_FLAGS -llvm"
      set args = ($args:q C_CMPLR=clang CXX_CMPLR=clang++)
      breaksw
    case -gnu:
      set args = ($args:q C_CMPLR=gcc CXX_CMPLR=g++)
      breaksw
    case -g77:
      set args = ($args:q FORTRAN=g77)
      breaksw
    case -D*:
      set defs = ($defs:q $1)
      breaksw
    case +w:
      setenv GCC_WALL -DNO_GCC_WALL
      breaksw
    case +x:
      unsetenv USR_SRC_EXTENSION
      breaksw
    case -uname:
      shift
      set uname = ($1)
      shift
      setenv MK_REALLY 0
      goto new_uname
    case -try:
      shift
      set tries = $1
      breaksw
    case -targ*:
      set targets = 1
      breaksw
    case -show:
      set show = 1
      breaksw
    case -gmk:
      shift
      set gmake = $1:q
      breaksw
    case -set:
      set
      exit
    case -printenv:
      printenv
      exit
    case -shmc:
      echo "SYS_OS=$SYS_OS;SYS_CPU=$SYS_CPU;OBJ_DIR=$OBJ_DIR;SYS_LP=$SYS_LP;"
      exit
    case -cshmc:
      echo "setenv SYS_OS  $SYS_OS;  setenv SYS_CPU $SYS_CPU;"
      echo "setenv OBJ_DIR $OBJ_DIR; setenv SYS_LP  $SYS_LP;"
      exit
    case -h:
    case -help:
      gmake -help | sed "s/Options/Gmake Options/"
      goto help
    case -j:
      shift
      set jobs = -j$1
      breaksw
    case -j*:
      set jobs = $1
      breaksw
    case -env:
      echo "mk: -env can only be the first argument"
      exit 1
    default:
      set args = ($args:q $1:q)
  endsw
  shift
end

if ($?MK_64) then
    set llp = ""
    foreach dll (`echo $LD_LIBRARY_PATH | tr : ' '`)
    if (-d $dll/64) then
      set llp = "${llp}:$dll/64"
    else if (-d $dll/amd64) then
      set llp = "${llp}:$dll/amd64"
    endif
  end
  setenv LD_LIBRARY_PATH $llp
  echo LD_LIBRARY_PATH $LD_LIBRARY_PATH
endif

if ( $?PROJECT ) then
  switch ($?PROJECT )
      case v2k:
      case ivl:
      case geda:
      case spice:
        breaksw
      default:
        if ( -f ~/$PROJECT-top ) then
	  # ???
        else
          unsetenv PROJECT
        endif
  endsw
endif

if ( ! $?PROJECT_ROOT ) then
  setenv PROJECT_ROOT ~
  set pth = ""
  foreach f (`echo $cwd | tr '/' ' '`)
    switch ($f)
      case v2k-*:
      case ivl-*:
      case geda-*:
      case spice:
      case spice-*:
        if ( -f $pth/$f/all.gmk ) then
          echo Setting "PROJECT_ROOT to $pth"
          setenv PROJECT_ROOT $pth
          break
        endif
    endsw
    set pth = "$pth/$f"
  end
endif

if ( ! $?PROJECT ) then
  set here = $cwd
  set curr = $here
  while ( "$curr" != "/" )
    setenv PROJECT $cwd:t
    cd ..
    set curr = $cwd
    if ("$curr" == "/") then
      setenv PROJECT none
      set proj_vrsn = (none top)
      echo "Project not set"
      cd $here
      goto skip
    endif
    switch ($PROJECT)
      case v2k-*:
      case ivl-*:
      case geda-*:
      case spice-*:
      case *-top:
      case *-{0,1,2,3,4,5,6,7,8,9}{0,1,2,3,4,5,6,7,8,9}.{0,1,2,3,4,5,6,7,8,9}{0,1,2,3,4,5,6,7,8,9}:
        break
    endsw
  end
  cd $here
  switch ($PROJECT)
    case *-*:
      set proj_vrsn = (`echo $PROJECT | sed 's+-+ +g'`)
      switch ($#proj_vrsn--$proj_vrsn[$#proj_vrsn])
        case 2--t*:
          set proj_vrsn = ($proj_vrsn[1] top)
        case 3--t*:
          set proj_vrsn = ($proj_vrsn[1]-$proj_vrsn[2] top)
      endsw
      echo "Project: $proj_vrsn[1] ($proj_vrsn[2])"
      setenv PROJECT $proj_vrsn[1]
  endsw
endif
skip:
switch ($PROJECT)
  case V2K*:
  case v2k*:
    setenv PROJECT_UC V2K
    setenv PROJECT    v2k
    breaksw
  case GEDA*:
  case geda*:
    setenv PROJECT_UC GEDA
    setenv PROJECT    geda
    breaksw
  case IVL*:
  case ivl*:
    setenv PROJECT_UC IVL
    setenv PROJECT    ivl
    breaksw
  case SPICE*:
  case spice*:
    set    PROJECT_UC SPICE
    setenv PROJECT    spice
    breaksw
  default:
#   echo "Unknown project ($PROJECT)"
    setenv PROJECT_UC `echo $PROJECT | tr '[a-z]-' '[A-Z]_'`
endsw
eval set hv_vrsn = '$?'${PROJECT_UC}_VERSION
if $hv_vrsn then
  eval set proj_vrsn = "($PROJECT" '$'${PROJECT_UC}_VERSION")"
else
  if $?proj_vrsn then
    eval setenv ${PROJECT_UC}_VERSION $proj_vrsn[2]
  else
    eval setenv ${PROJECT_UC}_VERSION top
  endif
endif
if ! $?proj_vrsn then
  eval echo Project: $PROJECT '"($'${PROJECT_UC}_VERSION')"'
  eval set proj_vrsn = "($PROJECT" '$'${PROJECT_UC}_VERSION")"
endif

if ( $#defs ) then
  setenv USR_CFLAGS "$defs:q"
  set args = ($args:q USR_CFLAGS=$defs:q)
endif

if ( $#xdfs ) then
  setenv USR_CXXFLAGS "$xdfs:q"
  set args = ($args:q USR_CXXFLAGS=$xdfs:q)
endif

set args = ($args:q CWD=$cwd)
(echo ~cvs) >& /dev/null
if ! $status then
  setenv CVS_HOME `echo ~cvs` >& /dev/null
  set args  = ($args:q CVS_HOME=$CVS_HOME)
endif

setenv WORK_DIR   $work_dir
setenv SOURCE_DIR $work_dir:t

setenv PROJECT_DIR $PROJECT_ROOT/$PROJECT-$proj_vrsn[2]
setenv CONFIG_DIR  $PROJECT_DIR/config

if ( -d $PROJECT_DIR/bin ) then
  setenv BIN_DIR $PROJECT_DIR/bin
  set path = ($PROJECT_DIR/bin $path)
endif

if ( $?cmode && ! $?CMODE ) then
  set CMODE = $cmode
  set args  = ($args:q CMODE=$cmode)
endif

if ! $?mf then
  if $?MAKE_FILES_PREF then
    set mf = ($MAKE_FILES_PREF)
  else
    set mf = (GNUmakefile all.gmk Makefile makefile)
  endif
endif

if ( ! $?MYTOPARCH ) then
  set MYTOPARCH = unknown
endif

if ( -f $MYTOPARCH-mk.arg ) then
  set ma = (`cat $MYTOPARCH-mk.arg`)
else
  if ( -f mk.arg ) then
    eval set ma = "(" `cat mk.arg | sed s=\"=\\"=g` ")"
  else
    set ma = ()
  endif
endif

if ( -f "$USER-mk.arg" ) then
  set ma = ($ma `cat "$USER-mk.arg"`)
endif

if ( "$subargs" != "" ) then
  set subargs = MAKE_SUB_ARGS="$subargs:q"
endif

if ( ! $?HOST ) then
  setenv HOST `hostname`
endif

if ( -f ~/unames/$HOST ) then
  set processors = 1
  source ~/unames/$HOST
@ top_jobs      = $processors / 2
  if ( $processors != 1 && $top_jobs != 1 && "" == $jobs ) then
    echo Using $top_jobs processes.
    set jobs = -j$top_jobs
@   make_dep_jobs = $processors * 3
    set args = ($args:q MAKE_DEP_JOBS=$make_dep_jobs)
  endif
endif

setenv MK_JOBS "$jobs"

foreach f ($mf)
  if ( -f $f ) then
    goto next
  endif
end

echo None of $mf found in $cwd.
exit 1

next:

if $targets then
  grep '^[a-z,A-Z,_,0-9]*:' $f | grep -v ':=' | cut -d: -f1 | sort -u
  exit
endif

if $show then
  printenv
  echo ARGS:
  foreach a ($args:q)
    echo "  $a"
  end
  exit
endif

retry:
  if $tries == 0 then
    set exec = exec
  else
    set exec = ()
  endif
  if $?MK_ECHO then
    echo $gmake -f $f $jobs $args:q $ma $subargs:q MAKEFILE=$f
  endif
  $exec $gmake -f $f $jobs $args:q $ma $subargs:q MAKEFILE=$f
  set sts = $status
  if 0 == $sts then
    exit $sts
  endif
  @ tries = $tries - 1
  goto retry

help:
cat <<_EOF
Local Options:
  -fast                       Skip dependency checking.
  -env                        Discard inherited environment
  -sj <#>                     Subdirectory job limit.
  -sw                         Stop on warnings.
  -rcs                        Use RCS instead of CVS.
  -g                          Code is built debug.
  -O                          Code is built optimized.
  -gO                         Code is built debug and optimized.
  -tb                         Make in test build directory.
  -ntv                        Use native C compiler.
  -gnu                        Use GNU C/C++ compiler.
  -g77                        Use GNU Fortran
  -cd  <directory>            Change to directory.
  -rls <id>                   Build for release (sets -O if no mode set).
  +w                          Switch off gcc warnings.
  +x                          Unset extensions.
  -targets                    Show targets.
  -show                       Show environment.
  -dbg                        Set debug.
  -lmt                        Limit memory use.
_EOF

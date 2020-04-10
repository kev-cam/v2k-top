#!/bin/sh
# Copyright (c) 1998,1999,2001,2002,2003 Kevin Cameron
# Distributed under the GNU Lesser General Public License
# RCS ID: $Id: mk.sh,v 1.11 2003/01/14 02:25:58 dkc Exp $
 
if [ -z "$MK_SCRIPT" ] ;  then
  MK_SCRIPT=$0; export MK_SCRIPT
fi

if [ "$1" = "-env" ] ; then
  shift
  hm=$HOME
  env | cut -d= -f1 | egrep -v "^!|PATH|CMODE|UNAMEA|USER|PROJECT|USR_|PURIFY|MK_|_VERSION" | awk '{print "unset ",$1}' > /tmp/mk.$$
  . /tmp/mk.$$ ; rm /tmp/mk.$$
  HOME=$hm
fi

pth=$0
pth=`dirname $pth`

args=
subargs=
tries=1
gmake=gmake
cmode=-g
targets=0
show=0
help=0
work_dir=`pwd`

if [ ! -z "$MK_JOBS" ] ; then
  jobs="$MK_JOBS"
else
  jobs=
fi

if [ -f $HOME/.mkrc-sh ] ; then
  . $HOME/.mkrc-sh
fi

if [ ! -z "$USR_CFLAGS" ] ; then
  defs="$USR_CFLAGS"
else
  defs=
fi

if [ ! -z "$USR_CXXFLAGS" ] ; then
  xdfs="$USR_CXXFLAGS"
else
  xdfs=
fi

unset mf

if [ -z "$UNAMEA" ] ; then
  uname=`uname -a`
else
  uname="$UNAMEA"
fi

UNAMEA="$uname";        export UNAMEA
SYS_LP=LD_LIBRARY_PATH; export SYS_LP
case "$uname" in
  SunOS*5.*i86*)
      OBJ_DIR=sx_obj
      SYS_OS=sol5_5
      SYS_CPU=i86
      ;;
  SunOS-5.*-sun*)
      OBJ_DIR=sp_obj
      SYS_OS=sol5_5
      SYS_CPU=sprc
      ;;
  Linux-2.0*)
      OBJ_DIR=lnx_obj
      SYS_OS=lnx_200
      SYS_CPU=i86
      gmake=make
      ;;
  HP-UX-B*-*)
      OBJ_DIR=hpb_obj
      SYS_OS=hpb
      SYS_CPU=PAR
      ;;
  CYGWIN*)
      OBJ_DIR=c86_obj
      SYS_OS=cyw
      SYS_CPU=i86
      gmake=make
      args="$args --unix"
      ;;
  HP-UX-A*-*)
      OBJ_DIR=hpa_obj
      SYS_OS=hpa
      SYS_CPU=PAR
      ;;
  *)
      OBJ_DIR=obj
      SYS_OS=$uname[1]-$uname[3]
      SYS_CPU=$uname[5]
esac
export OBJ_DIR SYS_OS SYS_CPU

# GNU bug workaround:

ulimit -n 256 >/dev/null 2>&1

C_CMPLR=gcc
CXX_CMPLR=g++
export C_CMPLR CXX_CMPLR

MK_FLAGS= ; export MK_FLAGS

while [ ! -z "$1" ] ; do
  case "$1" in
    -cd)
      shift
      cd $1
      work_dir=`pwd`
      ;;
    -O)
      args="$args CMODE=-O"
      CMODE=-O
      ;;
    -g)
      args="$args CMODE=-g"
      CMODE=-g
      ;;
    -w)
      args="$args WARN=1"
      CMODE=-g
      ;;
    -W)
      args="$args WARN=2"
      CMODE=-g
      ;;
    -wd)
      shift
      work_dir=$1
      ;;
    -gO)
      args="$args CMODE=-gO"
      CMODE=-gO
      ;;
    -shrd)
      SHARED_LIBS=1; export SHARED_LIBS
      ;;
    -sttc)
      unset SHARED_LIBS
      subargs="$subargs -sttc"
      ;;
    -efence)
      case $SYS_OS in
        lnx*)
          DEBUG_LIBS="-L/usr/lib -lefence"
          ;;
        *)
          DEBUG_LIBS="-L/usr/local/lib -lefence"
          ;;
      esac
      export DEBUG_LIBS
      ;;
    -dbg)
      DEBUG_EXIT=2; export DEBUG_EXIT
      args="$args DEBUGGER='debug -w'"
      ;;
    -i)
      setenv MK_FLAGS "$MK_FLAGS -i"
      args="$args -i"
      ;;
    -f)
      shift
      mf=$1
      ;;
    -lmt)
      limit memorysize 20M
      limit datasize   20M
      limit stacksize  20M
      ;;
    -rcs)
      args="$args CVSROOT="
      ;;
    CLEAN | clean)
      args="$args $1"
      args="$args IGNORE_DEPS=1"
      ;;
    setup)
      args="$args SETUP=1 setup"
      args="$args IGNORE_DEPS=1"
      ;;
    -fast)
      args="$args IGNORE_DEPS=1"
      ;;
    -sj)
      shift
      subargs="$subargs:q -j $1"
      ;;
    -sw)
      args="$args EXIT_WARN=1"
      ;;
    -ntv)
      args="$args C_CMPLR=ntv"
      ;;
    -gcc)
      args="$args C_CMPLR=gcc"
      ;;
    -GCC)
      args="$args C_CMPLR=gcc USR_C_CMPLR=gcc"
      ;;
    -g77)
      args="$args FORTRAN=g77"
      ;;
    -D*)
      defs="$defs:q $1"
      ;;
    +w)
      GCC_WALL=-DNO_GCC_WALL; export GCC_WALL
      ;;
    +x)
      unset USR_SRC_EXTENSION
      ;;
    -try)
      shift
      tries=$1
      ;;
    -targ*)
      targets=1
      ;;
    -show)
      show=1
      ;;
    -set)
      set
      exit
      ;;
    -shmc)
      echo "SYS_OS=$SYS_OS;SYS_CPU=$SYS_CPU;OBJ_DIR=$OBJ_DIR;SYS_LP=$SYS_LP;"
      exit
      ;;
    -cshmc)
      echo "setenv SYS_OS  $SYS_OS;  setenv SYS_CPU $SYS_CPU;"
      echo "setenv OBJ_DIR $OBJ_DIR; setenv SYS_LP  $SYS_LP;"
      exit
      ;;
    -h|-help)
      gmake -help | sed "s/Options/Gmake Options/"
      help=1
      ;;
    -j)
      shift
      jobs=-j$1
      ;;
    -j*)
      jobs=$1
      ;;
    *)
      args="$args $1"
  esac
  shift
done

if [ -z "$PROJECT_ROOT" ] ; then
  pth=""
  for f in `pwd | tr '/' ' '` ; do
    case $f in
      v2k-*|spice-*|geda-*)
        if [ ! -d "$pth/$f" ] ; then
          if [ -d "/$pth/$f" ] ; then
            pth="/$pth"
          else
            Echo "Can't determine PROJECT_ROOT"
            exit 1
          fi
        fi
        echo Setting "PROJECT_ROOT to $pth"
        PROJECT_ROOT=$pth
        break
        ;;
    esac
    pth="$pth/$f"
  done
  export PROJECT_ROOT
fi

if [ -z "$PROJECT" ] ; then
  drvd=1
  here=`pwd`
  curr="$here"
  root=`cd $PROJECT_ROOT; pwd`
  while [ "$curr" != "/" -a "$curr" != "$root" ] ; do
    proj=`basename $curr`
    cd ..
    curr=`pwd`
    if [ "$curr" = "/" ] ; then
      PROJECT=none
      echo "Project not set"
      exit
    fi
  done
  VERSION=`echo $proj | cut -d- -f2`
  cd "$here"
  case $proj in
    v2k*)   PROJECT=v2k;   proj=V2K   ;;
    spice*) PROJECT=spice; proj=SPICE ;;
  esac
else
  drvd=0
  case $PROJECT in
    V2K*|v2k*)     PROJECT=v2k;   PROJECT_UC=V2K   ;;
    SPICE*|spice*) PROJECT=spice; PROJECT_UC=SPICE ;;
    *)             echo "Unknown project ($PROJECT)"
                   PROJECT_UC=`echo $PROJECT | tr '[a-z]' '[A_Z]'`
                   PROJECT=`echo $PROJECT_UC | tr '[A-Z]' '[a_z]'` ;;
  esac
  eval 'VERSION="$'$PROJECT_UC'_VERSION"'
fi
export PROJECT
export PROJECT_UC
if [ -z "$VERSION" ] ; then VERSION=top ; fi
eval "${proj}_VERSION=$VERSION"
eval "export ${proj}_VERSION"
if [ 0 != $drvd ] ; then eval echo '"Project: $PROJECT ($'$proj'_VERSION)"' ; fi

if [ ! -z "$defs" ] ; then
  USR_CFLAGS="$defs"
  args="$args USR_CFLAGS=$defs"
fi

if [ ! -z "$xdfs" ] ; then
  USR_CXXFLAGS="$xdfs"
  args="$args USR_CFLAGS=$xdfs"
fi

(echo ~cvs) > /dev/null 2>&1
if [ 0 != $? ]  ; then
  CVS_HOME=`echo ~cvs` > /dev/null 2>&1
  export CVS_HOME
  args=$args CVS_HOME=$CVS_HOME CWD=$cwd
fi

WORK_DIR=$work_dir
SOURCE_DIR=`basename $work_dir`

eval 'PROJECT_DIR="$PROJECT_ROOT/$PROJECT-$'$proj'_VERSION"'
CONFIG_DIR=$PROJECT_DIR/config

export WORK_DIR SOURCE_DIR PROJECT_DIR CONFIG_DIR BIN_DIR

if [ -d $PROJECT_DIR/bin ] ; then
  PATH=$PROJECT_DIR/bin:$PATH
  export PATH
fi

if [ ! -z "$cmode" -o ! -z "$CMODE" ] ; then
  CMODE=$cmode
  args=$args CMODE=$cmode
fi
export CMODE

if [ -z "$mf" ] ; then
  if [ -z "$MAKE_FILES_PREF" ] ; then
    mf="GNUmakefile all.gmk Makefile makefile"
  else
    mf="$MAKE_FILES_PREF"
  fi
fi

if [ -z "$MYTOPARCH" ] ; then
  MYTOPARCH=unknown
fi

if [ -f $MYTOPARCH-mk.arg ] ; then
  ma=`cat $MYTOPARCH-mk.arg`
else
  if [ -f mk.arg ] ; then
    ma=`cat mk.arg`
  else
    ma=
  fi
fi

if [ -f $USER-mk.arg ] ; then
  ma=$ma `cat $USER-mk.arg`
fi

if [ "$subargs" != "" ] ; then
  subargs=MAKE_SUB_ARGS="$subargs"
fi

if [ -z "$HOST" ] ; then
  HOST=`hostname` ; export HOST
fi

MK_JOBS="$jobs"; export MK_JOBS

for f in $mf ; do
  if [ -f $f ] ; then break ; fi
done

if [ ! -f "$f" ] ; then
  echo "None of $mf found in $cwd."
  exit 1
fi

if [ 0 != $targets ] ; then
  grep '^[a-z,A-Z,_,0-9]*:' $f | grep -v ':=' | cut -d: -f1 | sort -u
  exit
fi

if [ 0 != $show ] ; then
  printenv
  echo ARGS:
  for a in $args ; do
    echo "  $a"
  done
  exit
fi

if [ 0 != $help ] ; then
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
  -gcc/-GCC                   Use GNU C compiler.
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
else
  while [ $tries ] ; do
    if [ ! -z "$MK_ECHO" ] ; then
      echo $gmake -f $f $jobs $args $ma $subargs MAKEFILE=$f
    fi
    $gmake -f $f $jobs $args $ma $subargs MAKEFILE=$f
    sts=$?
    if [ ! $sts ] ; then
      exit $sts
    fi
    case $tries in
      9) tries=8;;
      8) tries=7;;
      7) tries=6;;
      6) tries=5;;
      5) tries=4;;
      4) tries=3;;
      3) tries=2;;
      2) tries=1;;
      *) exit $sts;;
    esac
  done
fi


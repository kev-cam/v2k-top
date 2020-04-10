#!/bin/sh
# Copyright (c) 1998,1999,2001,2002,2003 Kevin Cameron
# Distributed under the GNU Lesser General Public License
# RCS ID: $Id: v2k.sh,v 1.6 2003/01/14 02:25:59 dkc Exp $
 
SCR_DIR=`dirname $0`
ROOT=`dirname $SCR_DIR`
PWD=`pwd`
case $ROOT in
 /*) ;;
 *)  ROOT=$PWD/$ROOT ;;
esac

if [ -d $ROOT/export ] ; then
  ROOT=$ROOT/export
fi

eval `$SCR_DIR/mk -shmc`

EXE_DIR=$ROOT/bin/$OBJ_DIR
PATH=$PATH:$EXE_DIR

eval $SYS_LP='$'$SYS_LP:$ROOT/lib/$OBJ_DIR
eval export $SYS_LP

cmode=
wrap=
args=

while [ 1 ] ; do
  case "$1" in
    -O)    cmode=-O;   shift;;
    -g)    cmode=-g;   shift;;
    -dbg)  wrap=debug; shift;;
    -doc)  if [ -z "$BROWSER" ] ; then BROWSER=netscape ; fi
           exec $BROWSER $ROOT/html/index.html;;
    -help) echo "-dbg            Run under debugger"
           echo "-doc            Launch document browser"
           args="$args $1"
           shift;;
    *)     exec $wrap $EXE_DIR/v2k$cmode $args $*;;
  esac
done

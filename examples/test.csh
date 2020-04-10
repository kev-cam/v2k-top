#!/bin/csh -fvx


switch ($0)
  case */*:
    set exe = $0
    cd $exe:h
    breaksw
endsw

set rls = ..

set path = ($rls:q/bin $path:q)

foreach v ($rls/examples/*.v)
echo Doing: v2k -vdmp=m $v -elab -code -sim
   v2k -vdmp=m $v -elab -code -sim
end


#!/bin/csh -f
# Copyright (c) 1998,1999,2001,2002,2003 Kevin Cameron
# Distributed under the GNU Lesser General Public License
# RCS ID: $Id: perl.csh,v 1.11 2003/01/14 02:26:22 dkc Exp $
 

mk perl_dirs

set xf  = ()
set pos = i386-linux
set unm = (`uname -a`)
foreach pdir (/usr/local/lib /usr/lib)
  foreach pos (`cd $pdir ; ls -dr perl5/5.*`)
    set os = (`cd $pdir/$pos ; ls -d {i*,sun*}-{linux,solaris}`)
    set wp = $pdir:h/bin/perl
    if $#os then
      set xf = (PERL_INST=$pdir PERL_IDIR=$pos[1] PERL_OS=$os[1] LD=g++)
      goto next
    endif
  end
end
echo "No perl-5 ?"
exit 1

next:
set mkarg = all

while ($#argv)
  switch ($1)
    case -rebuild:
      shift
      goto rebuild
    default:
      echo "Bad argument: $1"
      exit 1
  endsw
  shift
end

mk -cd ../common    -swig -perl +shrd $xf
if ($status) exit 1
mk -cd ../languages -swig -perl +shrd $xf
if ($status) exit 1
mk -cd ../models    -swig -perl +shrd $xf
if ($status) exit 1

mk -swig -perl +shrd clean

rebuild:
mk -swig -perl -shrd $xf $mkarg:q $*:q
if ($status) exit 1

sed s+PERL+$wp+ <v2k.pl >../../export/bin/v2k.pl
chmod +x ../../export/bin/v2k.pl
mkdir -p ../../export/pm
cp libv2kprl.pm ../../export/pm

set echo
../../export/bin/v2k.csh -perl -vdmp=m ../../examples/traffic.v

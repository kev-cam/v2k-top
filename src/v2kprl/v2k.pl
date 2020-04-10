#!PERL
# Copyright (c) 1998,1999,2001,2002,2003 Kevin Cameron
# Distributed under the GNU Lesser General Public License
# RCS ID: $Id: v2k.pl,v 1.3 2003/01/14 02:26:23 dkc Exp $
 

# Perl5 script for testing simple example

use libv2kprl;

if ($ENV{"V2K_PRL_DBG"} eq "1") {
    $exe = "PERL";
    `ddd $exe $$ </dev/null >/dev/null 2>&1 &`;
    sleep 10;
}

# Call init. function

if ($sts = libv2kprl::PerlV2Kinit($ENV{"V2K_EXE"})) {
    die ("Failed to initialize V2K");
} elsif ($sts = libv2kprl::PerlV2Kdo(0,-1,"ARGV")) {
    print STDERR "V2K returned $sts\n";
}

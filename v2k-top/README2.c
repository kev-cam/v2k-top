/* Copyright (c) 1998,1999,2001,2002 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */

What to do:

1. Unpack the code - it will be a directory tree starting %PROJECT(,/...)

	create a link from your bin directory to %PROJECT(,/export/bin/mk)
	or
	put %PROJECT(,/export/bin)% on your path -
			[csh: set path = ($path:q `pwd`/export/bin)]
			[sh:  PATH=$PATH:`pwd`/export/bin]

4. Test the application:

	%PROJECT(,/bin/v2k)% [-help] ....

5. Send feedback to:

	admin@v-ms.com


Tips:

 Solaris:

  If you are running on Solaris and experience long compile times it may
  be because you have an old 'as' - download and build the GNU binutils to
  get a better one, you'll need to check that it actually gets used too as
  gcc will normally use the native 'as' first.

 Cygwin:

  You'll need a link from /bin/sh to bash for the mk.sh script, and a working
  perl (in /usr/local/bin).

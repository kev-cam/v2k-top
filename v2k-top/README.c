/* Copyright (c) 1998,1999,2001,2002 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */

/* RUN ./configure to process this file and set up links */

# 1 "README"

What to do:

1. Unpack the code - it will be a directory tree starting  %PROJECT(,/...)

2. Either:

       create a link from your bin directory to %PROJECT(,/bin/mk)
     or
       put %PROJECT(,/bin)% on your path -
			[csh: set path = ($path:q `pwd`/bin)]
			[sh:  PATH=$PATH:`pwd`/bin]

     you may also need to set the environment variables

       PROJECT      v2k
       PROJECT_ROOT <the directory above this>

3. Execute:

       cd PROJECT(.../,)
       ./configure
       mk setup
       mk apps -try 2 -shrd  # try -sttc instead if you have 
                            ~ problems with shared libraries
       mk exports -i
       mk docs -i
       v2k -doc &
      [# for a faster version:
       mk apps -O ]

       # for Perl module - needs SWIG.
       (cd src/v2kprl ; ./perl.csh)

     If you need to rebuild from scratch try this to remove bad intermediate
     files:

       mk -cd src CLEAN

4. Test the application:

       /.../%PROJECT(,/bin/v2k)% [-help] ....

5. Send feedback to:

       feedback@v-ms.com


Tips:

 Solaris:

  If you are running on Solaris and experience long compile times it may
  be because you have an old 'as' - download and build the GNU binutils to
  get a better one, you'll need to check that it actually gets used too as
  gcc may use the native 'as' first.

 Cygwin:

  You'll need a link from /bin/sh to bash for the mk.sh script, and a working
  perl (in /usr/local/bin).

 Re-building:

  Running 'mk -sj 2' from the 'command' directory is probably the fastest
  way to rebuild.

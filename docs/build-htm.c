<html>
#include "color-htm.c"

<h1>Directory Layout</h1>

The source directories are set up for multi-platform development, platform dependent 
code is placed in subdirectories named according to the target (e.g. lx6_obj or
lnx_260-i86 for Linux 2.6 kernel on X86). The object directories (e.g. lx6_obj) can
be soft-links to a seperate directory structure, the platform dependent source
directories (e.g. lnx_260-i86) are under CVS and not movable.
<p>
The structure in CVS is as follows:
<pre>
V2K Top/config^^^^^^^^^^^^^Configuration files - GNU make files for commmon tasks
^^^^^^^^^^^^^^^^^^^^^^^^^^^platform specific in the <os>-<cpu> directories
       /docs^^^^^^^^^^^^^^^Templates and make-file for genrating HTML
       /src^^^^^^^^^^^^^^^^Source code for libraries and applications.
       /command^^^^^^^^^^^^Command line interface (main)
       /common^^^^^^^^^^^^^Common code (support library)
           /include^^^^^^^^Headers
           /languages^^^^^^Parsers/Elaborators
           /models^^^^^^^^^Verilog Primitives
           /v2ksim^^^^^^^^^Simulator Kernel
           /v2kprl^^^^^^^^^Swig Perl Interface
       /bin^^^^^^^^^^^^^^^^Scripts
       /import^^^^^^^^^^^^^Copies/soft-links for importing other projects
       /export^^^^^^^^^^^^^Release diretories
           /bin^^^^^^^^^^^^Scripts
               /&lt;os&gt;-&lt;cpu&gt; Platform dependent executables
           /lib
               /&lt;os&gt;-&lt;cpu&gt; Platform dependent libraries
       /examples^^^^^^^^^^^Examples

</pre>

<h1>Make Rules</h1>

The compiler rules are set so that object code is created directly in the appropriate
target directories. Dependency files (*deps.gmk in the object directories) are created
and used automatically unless "mk -fast" is used.
 
<h2>Static vs. Dynamic Libraries</h2>

The directories under src which are libraries listed in <a href="../../liblist.gmk">
liblist.gmk</a>, and the cross directory dependencies are handled so that "mk" in
the src/command directory will build everything. Libraries can be static or dynamic
and which to use depends on the platform and debugging capabilities - dynamic gives
quicker rebuilds during development. When using dynamic libraries some linking
is done by manually loading the libraries and looking for specific routines - see
<a href="../../src/common/dyn.cpp">dyn.cpp</a> for details.

<html>
#include "color-htm.c"

<br>
<h1 align=center>Configuration</h1>

<h2>Supported Operating Systems and Platforms</h2>

Run 'mk -shmc', and see if the settings for SYS_OS,SYS_CPU,OBJ_DIR etc. are
valid for your machine.
<p>
<pre>
<li>SYS_OS  - Operating System
<li>SYS_CPU - Platform
<li>OBJ_DIR - Name of sub-directory for platform dependent files
<li>SYS_LP  - Shared library load path environment variable
</pre>
<p>
Known configurations are:
<br>
<br>
#include "html/os.lst"
<br>
New configurations can be added by editing the <a href="../../bin/mk.csh">mk</a> script.
<br>
<br>
<h2>Environment & Make</h2>

Include paths in the GNU make files (usually *.gmk) use the environment
variables set by the <a href="../../bin/mk.csh">mk</a> script to select the
correct files for the operating systems and platform - as shown in the
file <a href="../../config/common.gmk">common.gmk</a> and 
<a href="../../config/includes.gmk">includes.gmk</a>.
<p>
If you want to add a new configuration you will need to add (or soft-link)
the appropriate files in the <a href="../../config/">config</a> and
<a href="../../src/include/">include</a> directories.

<br clear=all><br><hr>$Id: os-htm.c,v 1.4 2005/04/12 23:46:44 dkc Exp $
</html>

<html>
#include "color-htm.c"
<br>
<br>
<h1 align=center>Command Line Interface</h1>
<hr>
<b>v2k</b>
<a href="#cm"><i>-&lt;cmode&gt;</i></a> 
<a href="#hlp">-help</a> 
<a href="#bug">-dbg</a>
<a href="#doc">-doc</a>
<a href="#v">*.v *.V</a>
<a href="#va">*.va</a>
<a href="#sdf">*.sdf</a>
<a href="#f">*.f</a>
<a href="#lib">-library</a>
<a href="#elb">-elaborate</a>
<a href="#cod">-codegen</a>
<a href="#sim">-simulate</a>
<a href="#shl">-shell</a>
<a href="#scr">-source=...</a>
<a href="#dmp">-dump=...</a>
<a href="#vd">-vdmp=...</a>
<a href="#sv">-save=...</a>
<a href="#sz">-sbsz=...</a>
<a href="#sp">-sdfp=...</a>
<a href="#px">-portmode=...</a>
<a href="#pt">-pt=...</a>
<a href="#mk">-make=...</a>
<a href="#vrb">-verbose=...</a>
<a href="#ev">-err.verbose=...</a> 
<a href="#es">-err.dont_stop=...</a> 
<a href="#eu">-err.give_up=...</a> 
<a href="#do">-err.do_stop=...</a> 
<a href="#dbg">-debug=...</a> 
<a href="#s0">-stdin=...</a>
<a href="#s1">-stdout=...</a> 
<a href="#s2">-stderr=...</a>
<a href="#pm">-pm=...</a>
<a href="#xp">-explain=...</a>
<a href="#cd">-chdir=...</a>
<br clear=all>
<hr>
<h2>Script</h2>
The <i>v2k</i> executable is run by the <a href="../../../bin/v2k">v2k</a> script,
which identifies the platform and operating system and sets library and
executable path environment variables.
<ul>
<a name="cm"></a><h3><pre><i>-&lt;cmode&gt</i></pre></h3>
<ul>Select executable (e.g. -g,-O)</ul>
<a name="bug"></a><h3><pre>-dbg</pre></h3>
<ul>Runs command as "debug v2k ..." (needs 'debug' script)</ul>
<a name="doc"></a><h3><pre>-doc</pre></h3>
<ul>Start documentation browser ($BROWSER)</ul>
</ul>
<br>
<hr>
<h2>Executable</h2>
<ul>
<a name="v"></a><h3><pre>*.v *.V         </pre></h3>
<ul>Verilog Source</ul>
<a name="va"></a><h3><pre>*.va            </pre></h3>
<ul>Verilog-A Source</ul>
<a name="sdf"></a><h3><pre>*.sdf           </pre></h3>
<ul>Standard Delay Format</ul>
<a name="f"></a><h3><pre>*.f             </pre></h3>
<ul>File of arguments</ul>
<a name="lib"></a><h3><pre>-library        <name> </pre></h3>
<ul>Set library name for following objects.</ul>
<a name="elb"></a><h3><pre>-elaborate      </pre></h3>
<ul>Elaborate Design</ul>
<a name="cod"></a><h3><pre>-codegen      </pre></h3>
<ul>Generate C++ for simulation</ul>
<a name="sim"></a><h3><pre>-simulate      </pre></h3>
<ul>Start simulation</ul>
<a name="shl"></a><h3><pre>-shell          </pre></h3>
<ul>Go interactive</ul>
<a name="scr"></a><h3><pre>-script         &lt;file&gt;</pre></h3>
<ul>Run a (csh) script</ul>
<a name="dmp"></a><h3><pre>-dump           ndumov...[+,;&lt;file&gt;]</pre></h3>
<ul>Dump control</ul>
<ul><ul>
<li> + = append
<li> , = open
<li> ; = modify options on existing stream
</ul></ul>
<a name="vd"></a><h3><pre>-vdmp           ndumov...[+,;&lt;file&gt;]</pre></h3>
<ul>Dump verilog
<p><ul>
<li> n = nature
<li> d = discipline
<li> u = udp
<li> m = module
<li> o = obfiscate
<li> v = verbose
</ul></ul>
<a name="sv"></a><h3><pre>-save           [m]</pre></h3>
<ul>Database control
<p><ul>
<li> m = module
</ul></ul>
<a name="sz"></a><h3><pre>-sbsz           </pre></h3>
<ul>SDF read-ahead buffer size</ul>
<a name="sp"></a><h3><pre>-sdfp           <bool></pre></h3>
<ul>SDF Piped (no intermediate token file)</ul> 
<a name="pt"></a><h3><pre>-pt             [icd...]</pre></h3>
<ul>Default pool mode</ul> 
<a name="px"></a><h3><pre>-portmode       [xd...]</pre></h3>
<ul>Port-mode
<p><ul>
<li> x = No excess
<li> d = No dangling
</ul></ul>
<a name="mk"></a><h3><pre>-make           [fdei...]</pre></h3>
<ul>Make mode
<p><ul>
<li> f = files
<li> d = files
<li> e = environment
<li> i = includes
</ul></ul>
<a name="vrb"></a><h3><pre>-verbose        [amdeix+]</pre></h3>
<ul>Verbosity
<p><ul>
<li> a = all
<li> m = make
<li> d = dump
<li> e = elaboration
<li> i = information
<li> x = exit
<li> + = misc
</ul></ul>
<a name="ev"></a><h3><pre>-err.verbose    [n]</pre></h3>
<ul>Error verbosity level</ul>
<a name="es"></a><h3><pre>-err.dont_stop  [n]</pre></h3>
<ul>Ignore fatal errors</ul>
<a name="eu"></a><h3><pre>-err.give_up    [n]</pre></h3>
<ul>Give up after too many errors</ul>
<a name="do"></a><h3><pre>-err.do_stop    [n]</pre></h3>
<ul>Stop at first error</ul>
<a name="dbg"></a><h3><pre>-debug          [n]</pre></h3>
<ul>Debug level</ul>
<a name="s0"></a><h3><pre>-stdin          &lt;stream&gt;</pre></h3>
<a name="s1"></a><h3><pre>-stdout         &lt;stream&gt;</pre></h3>
<a name="s2"></a><h3><pre>-stderr         &lt;stream&gt;</pre></h3>
<a name="pm"></a><h3><pre>-pm             [crw...]</pre></h3>
<ul>Pool creation/access mode</ul>
<a name="xp"></a><h3><pre>-explain        [n]</pre></h3>
<ul>Explain a return code</ul> 
<a name="hlp"></a><h3><pre>-help           </pre></h3>  
<ul>Print help</ul> 
</ul>

<br clear=all><br><hr>$Id: command-htm.c,v 1.7 2005/04/14 23:27:26 dkc Exp $
<pre>



































</pre>
</html>


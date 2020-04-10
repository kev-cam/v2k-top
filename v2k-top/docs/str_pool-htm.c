<html>
#include "color-htm.c"
<br>
<h1>String <a href="pool.html">Pools</a></h1>

String pools are basically arrays of <a href="../../html/structplStr__s.html"
 target=_top>plStr</a> structures (which vary in size).

<h2>Predefined String Pools</h2>

Keywords and operators for various languages are divided into groups (*.tdf
files, e.g <a href="../../src/languages/verilog.tdf">verilog.tdf</a>), and
saved in pools which have a <i>pool index</i> less than 128 (which can be
stored in a byte - see <a href="../../src/include/html/tokpool.h.html"
 target=_top> eFixedPool</a>). This allows the <a href="tokeniz.html">
tokenizer</a> to generate keyword tokens using a single
<a href="../../src/include/html/tokpool.h.html#tokExt" target=_top>tokExt</a> struct (32 bits).
<p>
These pools are generated using code generated by the <a href="../../bin/mktkc">
mktkc</a> Perl script - see <a href="../../config/token.gmk">token.gmk</a> and
(e.g.) <a href="../../src/languages/html/verilog-t.cpp.html" target=_top>
verilog-t.cpp</a>.
<p>
The same string can appear in mutiple predefined pools, as this allows
different tokenizers to identify keywords according to context - i.e. "if" in
a VHDL file can be different from "if" in a Verilog file. This is not
recommended in general as strings may not then have a unique pool-id/index.

<h2>String DataBase</h2>

A general purpose string pool is usually created and allocated pool index 128.
This and the predefined pools are accessed through the <b>C</b> module
<a href="../../src/common/html/strdb.c.html" target=_top>strdb.c</a>.

<br clear=all><br><hr>$Id: str_pool-htm.c,v 1.8 2005/04/11 20:52:09 dkc Exp $
</html>
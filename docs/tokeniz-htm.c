<html>
#include "color-htm.c"
<br>
<h1>Tokenizing</h1>

The program <a href="../../src/languages/html/genparse.c.html">genparse</a>
is used to create a block of code that reads the source stream (e.g. <a href="../../src/languages/genparse-verilog.c">genparse-verilog</a>) and collects
characters into tokens, the token strings are defined in "tdf" files (e.g. <a href="../../src/languages/verilog.tdf">verilog.tdf</a>). The support routines in <a href="../../src/languages/html/tokfunc.c.html" target=_top>tokfunc.c</a> perform non language-specific
tasks, and language-specific functions are placed in the file that includes
the generated tokenizing code (e.g. <a href="../../src/languages/html/veri_pars-y.c.html" target=_top>veri_pars-y.c</a> generated from <a href="../../src/languages/veri_pars.c" target=_top>veri_pars.c</a>).
<p>
The output of tokenizing is a stream of 32bit tokens. Tokenizing a string
occasionally requires more data than will fit in 32 bits;
the values in
<a href="../../src/include/html/tokpool.h.html#eTOK" target=_top>eTOK</a> are
used to indicate these special cases. The data in a token usually decodes
into a <a href="str_pool.html">string pool</a> id/index 2-tuple, and allows simple 2-level case statements in the
<a href="../../src/languages/html/verilog2.cpp.html#VMcontext-prsVMtok" target=_top>pre-processor</a> and recursive decent parsers (e.g. for Verilog <a href="../../src/languages/html/verilog3.cpp.html#Module-Module" target=_top>modules</a>). The token string pools can be cached and reused from run to run as long as the order of the tokens in the tdf files is not changed.


<br clear=all><br><hr>$Id: tokeniz-htm.c,v 1.9 2005/04/14 23:27:26 dkc Exp $
</html>

<html>
#include "color-htm.c"

<h1 align=center>Style</h1>

<h2>Code Layout</h2>
<ul>
My preference is to use a fairly dense coding style so that I get the
maximum amount of code on the screen. I find this helps with debugging.
Otherwise code is laid out according to the <i>emacs</i> default C-mode.
<p>
Comments within the code are limited, as the intention is to document the
project in HTML, with references back into the code. The script <a href="../../bin/c2html">c2html</a> is used to generate and tag HTML versions of source
files, and <a href="../../html/index.html" target=_top>Doxygen</A> is used
to document the class hierarchy.

</ul>

<h2>Techniques</h2>
<ul>
<h3>Included Definition Files</h3>
A number of <i>.inc</i> files are used around the project. These files
are generally used to bind data that will be used in different arrays, enums
or other structures - e.g. <a href="../../src/include/html/timing.inc.html"
target=_top>timing.inc</a> used in <a href="../../src/include/html/veri_enum.h.html#eCHK" target=_top>veri_enum.h.inc</a> and <a href="../../src/languages/html/sdf.cpp.html" target=_top>sdf.cpp</a>.

<h3>Code Generators & Preprocessors</h3>
One of the reasons for using Perl preprocessors (on *.pcc files ) is that the
<i>C++</i> template mechanism is not consistently implemented across different
compilers. The other reason is that debuggers have problems with debugging
into included files sometimes.
<p>
Perl code generators are used where larger amounts of self-consistent code
are required from simple input.

</ul>

<br clear=all><br><hr>$Id: style-htm.c,v 1.8 2005/04/14 23:27:26 dkc Exp $
</html>

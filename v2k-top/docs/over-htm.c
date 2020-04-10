<html>
#include "color-htm.c"

<br>
<h1 align=center>Overview</h1>

<h2>Introduction</h2>
The V2000 project is intended to provide a general purpose
HDL front-end, i.e. a parser, database, and elaborator. This
can then be used as a base for simulators or other tools.
<p>
An emphasis has been placed on dividing the process of
'compiling' HDL into parallizable tasks for better throughput
with large and detailed designs. 

<h2>Database</h2>
The 'database' is a collection of files representing data
in different forms. Data is in general represented in a form
(<a href="pool.html">pools</a>)
that can be handled directly by software in a time efficient
or space efficient manner (i.e. memory mapped or streamed).

<h2>Parsers</h2>
Language processing is generally performed by <a href="tokeniz.html">tokenising</a>
(and sometimes caching) the source then parsing the tokens. For a language
like Verilog where the source is 'pre-processed' and parsed
as a single stream, the data objects (modules etc.) are cut
from the token stream, cached, and can be parsed in parallel. Parsing is
generally performed by recursive descent since that is easier to debug
and handles context-sensitive oddities better than grammar driven parsing.
<p>
Parsers are implemented for Verilog (inc. AMS) and SDF. V2k can read compressed
files directly for handling large SDF data. The Verilog parser also tracks
inter-file dependencies, so that it only needs to reparse changes when recompiling
large designs.

<h2>Elaboration</h2>
The initial stages of elaboration are common to most electronic
design tools, and it was therefore viewed as the obvious end-point
for the initial development of the V2000 project. A CSH style interface
is available for browsing the elaborated design and other interpreters
(e.g. perl) can be linked in. The CSH interface can also be used to
drive v2k from scripts.

<h2>Simulation</h2>
Currently there is a code generator backend that can handle some simple
Verilog. It generates C++ which is compiled on-the-fly and dynamically
loaded back into v2k - the C++ code handles behavior, signal network
construction is handled by v2k and is not code-generated. Look in the
examples directory for test cases to try; generated code is put in the
directory v2k-csrc,one file is generated per-parameter-set and it includes
the common module code, so the object file loaded by the simulator is
something like: v2k-csrc/lib/work/&lt;module&gt;/param+&lt;signature&gt;+0.o


<br clear=all><br><hr>$Id: over-htm.c,v 1.9 2005/04/15 18:48:34 dkc Exp $
</html>

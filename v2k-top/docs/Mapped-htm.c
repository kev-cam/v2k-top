<html>
#include "color-htm.c"
<br>
<br>
<h1><u>Mapped</u> - Memory Mapped <a href="pool.html">Pool</a></h1>

Mapped pools are loaded using the <i>mmap</i> functions that map files on
disk into virtual memory. The pool uses indexed addressing because it can be
mapped to different addresses in different processes.

This is probably the most efficient way to use pools on symmetric parallel
processing hardware.

<br clear=all><br><hr>$Id: Mapped-htm.c,v 1.5 2005/04/12 23:46:44 dkc Exp $
</html>

<html>
#include "color-htm.c"
<br>
<br>
<h1><u>InMem</u> - In Memory <a href="pool.html">Pool</a></h1>

This mode can be used if the pool is not intended to be shared, data
is just allocated as necessary in memory and accessed by pointer. Since
it is not contiguous it cannot be block written to disk, however a smart
writer could use the symbolic information to relocate the data and fix
references so that the pol can be reloaded.

<br clear=all><br><hr>$Id: InMem-htm.c,v 1.5 2005/04/12 23:46:44 dkc Exp $
</html>

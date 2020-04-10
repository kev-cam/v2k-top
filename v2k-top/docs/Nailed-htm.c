<html>
#include "color-htm.c"
<br>
<br>
<h1><u>Nailed</u> - Pointer-mode <a href="pool.html">Pool</a></h1>

A "nailed" pool is one where the internal references are pointers, so
it cannot be moved around in memory.
<p>
The code to modify a indexed access pool to a nailed pool has not been 
implemented yet, but the information required to do so is available.
The load/unload time for this method will be longer due to the pointer 
conversion but code using the pool does not need to use derefrencing
functions to access the data. This approach is best for handling data
that needs to be traversed repeatedly.
<p>

<br clear=all><br><hr>$Id: Nailed-htm.c,v 1.4 2005/04/12 23:46:44 dkc Exp $
</html>

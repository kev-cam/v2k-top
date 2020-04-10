<html>
#include "color-htm.c"
<br>
<br>
<h1><u>Contig</u> - Contiguous Memory <a href="pool.html">Pool</a></h1>

In this mode the pool is allocated as a single block of memory. Access is
by indexing since extending the pool may require relocating it (realloc)
and pointer references would be broken. If/when reallocation occurs old
memory is kept active but read-only (if possible) and all blocks allocated
are recycled together when the pool is closed (maybe written to disk for
mapped access). 

<br clear=all><br><hr>$Id: Contig-htm.c,v 1.4 2005/04/12 23:46:44 dkc Exp $
</html>

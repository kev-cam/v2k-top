<html>
#include "color-htm.c"
<br>
<br>
<h1><u>OnDisk</u> - On Disk (Paged) <a href="pool.html">Pool</a></h1>

In this mode the pool is just left on disk and indexed addressing used to
get to data using seek. The data is cached when read so that repeat access
is faster, and all pools have a tag table indicating structures that overrun
single cache blocks.

<p>
This method works best for large pool access from processors with limited
memory, but care is required not to leave pointers dangling when the cache
is recycled.

<br clear=all><br><hr>$Id: OnDisk-htm.c,v 1.5 2005/04/12 23:46:44 dkc Exp $
</html>

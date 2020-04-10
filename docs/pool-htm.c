<html>
#include "color-htm.c"

<h1>Memory Pools</h1>

<h2>Overview</h2>

The concept of 'pool' used here is a single class object that contains
a collection of data objects. The data is arranged such that the entire
class object can be block copied from process to process or to disk
without having to modify the contents before accessing the data.

<p>
The data structures used within a pool are described externally to the C++
and processed into C++ (see <a href="../../config/pool.gmk" target=_top> 
pool.gmk</a> for rules). The fields for the base object are included in
the pool class (e.g. <a href="../../src/include/TemplatePoPool.fld"
target=_top>TemplatePoPool.fld</a> for a ported object like a module), and
can reference other data objects within the pool (described in pool-struct
files e.g. <a href="../../src/languages/veri_pool.pls" target=_top> veri_pool.pls</a>).
The generated C++ headers (e.g. <a href="../../src/include/veri_pool-dsc.h"
target=_top>veri_pool-dsc.h</a>) declare structs with the data aligned for
use with either 32 or 64 bit CPUs and symbolic information that can be used
for data relocation and endianness swapping.

<p>
References to data in pools is either by pointer (for <i>InMem</i> or
<i>Nailed</i>) or by a pool/index <a href="../../html/structpoolRef.html"
target=_top> 2-tuple</a> normally bundled into a union (e.g.
<a href="../../html/classOnDiskRef.html" target=_top>OnDiskRef</a>)
 - which is 64bits in size. The mode of a pool can be changed after it is
loaded, and <i>Mapped</i> pools are shareable between processes. Dereferencing
within a pool is handled by virtual functions (methods belonging to the pool
and will vary depending on its mode).

<p>
The maximum size of a pool is 2Gbyte since that is the maximum indexable size,
but in practice they are fairly small since only one 2G size pool would fit in
memory for a 32 bit processor. Pools can be "chained" to create objects larger
than a single pool - pool ids of -1,-2,-3... are used to indicate chained references
in a pool/index 2-tuple (the actual pool ids need not be consecutive).

<br>
<h2>Pool Modes</h2>
<pre>
<li><a href="Mapped.html">Mapped</a>^^^^^- Memory mapped from disk.
<li><a href="Contig.html">Contiguous</a>^- Single memory block.
<li><a href="InMem.html" >In Memory</a>^^- Non-contiguous/direct-access.
<li><a href="OnDisk.html">On Disk</a>^^^^- Paged & cached from disk.
<li><a href="Nailed.html">Nailed</a>^^^^^- Mapped using pointers.
</pre>

<h2>Pool Types</h2>
<pre>
<li><a href="str_pool.html">String</a>     - String database.
</pre>

<h2>Caveats</h2>
<h3>Programming with Pools</h3>

Accessing the data in pools via dereferenced 'pool/index' 2-tuples requires
care as the data may move as pools are paged and extended. Code which (creates
pool structures (e.g. plSave* routines in <a href="../../src/languages/po_pool.pcc" target=_top>po_pool.pcc</a>)
usually attempts to allocate the space for all the objects needed prior to filling them - any time more space may have been allocated pointers may become invalid.
<p> 
To make programming easier some routines in the pool code will do dereferencing and perform call-backs into
user routines with pointers to copies of pool data - e.g. <a href="../../src/languages/html/mod_pool-inm.cpp.html#InMemModulePool-forAllStmts">forAllStmts</a>,
                                                                       the function P(&lt;pool ref&gt;) returns a pointer from a pool reference. The copied data is on the stack, so is released when the call-backs are done.

<br clear=all><br><hr>$Id: pool-htm.c,v 1.9 2005/04/15 18:48:34 dkc Exp $
</html>

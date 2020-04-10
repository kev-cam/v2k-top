/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  pc_analysis_h_rcsid
#define pc_analysis_h_rcsid() {return "$Id: pc_analysis.h,v 1.1 2010/05/20 21:22:49 dkc Exp $";} /* RCS ID */

namespace parc {

struct BackRef {
  void_pipe  *pipe;
  int         cnt;
  Connected **conn;

  BackRef() : pipe(0), cnt(0), conn(0) {}
};

class RefTracker {
  friend class Module;

  Connected  *network;
  int         brefs,
              b_top;
  BackRef    *bref;
  int        *cref;

 public:
  BackRef   *addBackRef(void_pipe *,Module *);
  BackRef   *findBackRef(void_pipe *);
  Connected *findConn(Module *inst);


  RefTracker(Connected *net);
};

}

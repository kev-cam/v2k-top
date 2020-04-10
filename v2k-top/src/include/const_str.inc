/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: $Id: const_str.inc,v 1.13 2010/05/20 20:21:15 dkc Exp $ */

// CppConst::

CONST_STR(this_,        this,         0,0)
CONST_STR(void_,        void,         0,0)
CONST_STR(sizeof_,      sizeof,       0,0)
CONST_STR(delete_,      delete,       0,0)
CONST_STR(new_arr,      new[],        0,0)
CONST_STR(delete_arr,   delete[],     0,0)

CONST_STR(cpp_wait,     wait,         BUILTIN_POOL,BLT_WAIT)

CONST_STR(_W,           _W,           0,0)
CONST_STR(_i,           _i,           0,0)
CONST_STR(ba,           ba,           0,0)
CONST_STR(up,           up,           0,0)
CONST_STR(nm,           nm,           0,0)
CONST_STR(_sz,          _sz,          0,0)
CONST_STR(_wait,        _wait,        0,0)
CONST_STR(_MB,          _MB,          0,0)
CONST_STR(nba,          nba,          0,0)
CONST_STR(pos,          pos,          0,0)
CONST_STR(init,         init,         0,0)
CONST_STR(prc_init,     prc_init,     0,0)
CONST_STR(init_drv,     init_drv,     0,0)
CONST_STR(main,         main,         0,0)
CONST_STR(parc,         parc,         0,0)
CONST_STR(pipe,         pipe,         0,0)
CONST_STR(size,         size,         0,0)
CONST_STR(read,         read,         0,0)
CONST_STR(_csts,        _csts,        0,0)
CONST_STR(_ists,        _ists,        0,0)
CONST_STR(Clear,        Clear,        0,0)
CONST_STR(Value,        Value,        0,0)
CONST_STR(value,        value,        0,0)
CONST_STR(write,        write,        0,0)
CONST_STR(start,        start,        0,0)
CONST_STR(driver,       driver,       0,0)
CONST_STR(Module,       Module,       0,0)
CONST_STR(unWait,       unWait,       0,0)
CONST_STR(isRoot,       isRoot,       0,0)
CONST_STR(suspend,      suspend,      0,0)
CONST_STR(migrate,      migrate,      0,0)
CONST_STR(waitSig,      waitSig,      0,0)
CONST_STR(receiver,     receiver,     0,0)
CONST_STR(unWaitSig,    unWaitSig,    0,0)
CONST_STR(waitSigPre,   waitSigPre,   0,0)
CONST_STR(pipe_client,  pipe_client,  0,0)
CONST_STR(bind_children,bind_children,0,0)

CONST_STR(sc_thread,    sc_thread,    0,0)

CONST_STR(RCVR_POSEDGE_,RCVR_POSEDGE, 0,0)
CONST_STR(RCVR_NEGEDGE_,RCVR_NEGEDGE, 0,0)
CONST_STR(RCVR_ARRAY_,  RCVR_ARRAY,   0,0)
CONST_STR(SIG_BLCK_,    SIG_BLCK,     0,0)
CONST_STR(CHNS_BLOCK_,  CHNS_BLOCK,   0,0)
CONST_STR(CUSG_NONE,    CUSG_NONE,    0,0)
CONST_STR(CUSG_READ,    CUSG_READ,    0,0)
CONST_STR(CUSG_WRITE,   CUSG_WRITE,   0,0)


#undef CONST_STR

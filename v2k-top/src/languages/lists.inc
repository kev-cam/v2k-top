/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: $Id: lists.inc,v 1.10 2007/02/01 06:49:08 dkc Exp $ */
  


#if !defined(LST_LOCAL)

  /* Global lists */

  LST(Nature,      Nature,  nature,  NATURE)
  LST(Disc,        Disc,    disc,    DISC)
  LST(InstRef,     InstRef, instLst, INST)
  LST(ModRef,      ModRef,  topLst,  ROOT)
  LST(Library,     Library, libLst,  LIB)
  LST(VerCntxtObj, Prim,    prim,    PRIM)
# ifndef LST_TYPES
  LST(VerCntxtObj, Module,  module,  MODULE)
  LST(VerCntxtObj, Task,    glbTask, TASK)
  LST(VerCntxtObj, Func,    glbFunc, FUNC)
# endif
#endif

#if !defined(LST_GLOBAL)

  /* Context specific lists */

  LST(Port,        Port,    port,    PORT)
  LST(Attr,        Attr,    attr,    ATTR)
# ifndef LST_NO_SPECIAL
  LST(Parm,        Attr,    parm,    PARM)
# endif
  LST(Branch,      Branch,  branch,  BRANCH)
  LST(Scope,       Scope,   scope,   SCOPE)
  LST(Unknown,     Unknown, unknown, UNKNOWN)
  LST(Usage,       Usage,   usage,   USAGE)
  LST(refList,     refList, file,    FILE)
# ifndef LST_TYPES
  LST(VerCntxtObj, Task,    task,    TASK)
  LST(VerCntxtObj, Func,    func,    FUNC)
# endif
#endif

#undef LST_NO_SPECIAL
#undef LST_GLOBAL
#undef LST_LOCAL
#undef LST_TYPES
#undef LST




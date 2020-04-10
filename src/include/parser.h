/* Copyright (c) 1998,1999,2001,2002,2003 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  parser_h_rcsid
#define parser_h_rcsid() {return "$id";} /* RCS ID */

 
class VParser {
public:
  virtual const Token *NextTok(const Token *,int) = 0;
};

class PrsrCntxt : public VParser {
public:
  int          line;
  int          rescan;

  MappedFile  *pools;
  const Token *file_T0,
              *file_TL;
  const Token *Tnl;

  poolRef      curr_file;
  Filename     cache;
  const char  *fmode;

  inline void initCntxt(int sz) {
    if (sz > 0) BZERO(&line,sz-OffsetOf(PrsrCntxt,line));
    else        BZERO(&line,(intptr_t)&fmode - (intptr_t)&line);
  }

  static const char RplcDir;
  static const char RplcDot;

  static char *encode(char *);

  Token         *rgstrPool(tokPoolRef *);
  void           CmnInit();
  bool           setCache(const char *);
  const char    *source(Filename &);
  const poolRef  srcRef();
  const char    *cch2src(const char *,Filename &);
  const char    *tok2src(const char *,Filename &);
  const Token   *findClsBr(const Token *,const Token *,int *,const Token **);
  const Token   *tokenize(const Token *,const Token *,int *,const Token **);
  const Token   *tokGetRef(const Token *,poolRef *,int,int,
                           const Token *TL = 0);

  inline int   inc_line(const Token *T) {
# if DBGLVL > 1
                                         assert(rescan || T != Tnl);
# endif
                                         if (rescan) {
                                           if (T <= Tnl) goto count; 
                                           rescan = 0;
					 }
                                         Tnl = T;
                                         count: return ++line;};
  inline int   line_no()                {return line;};

  inline const Token *nextTok(const Token *T0,int needed,int nl = 1) {
    const Token *T = T0;
    if (T && ++T >= file_TL) {
      T = NextTok(T,needed);
    }
    if (WHITESPACE_POOL == T->pi.pool
              && WHT_NL == T->pi.index) {
      if (nl >= 0) {
        int l = inc_line(T);
        if (nl) {
          T = nextPostNL(T,l);
        }
      }
    }
    return T;
  };

  virtual const Token *advanceTo(const Token *T0,const Token *T) {return T;}
  virtual const Token *nextPostNL(const Token *T,int l = 0)      {return T;}

  virtual const Token *rescanning(const Token *T = 0,int back_only = 1);

};


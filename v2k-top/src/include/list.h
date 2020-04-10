/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
#undef  list_h_rcsid
#define list_h_rcsid() {return "$Id: list.h,v 1.26 2012/10/16 22:30:00 cvs Exp $";} /* RCS ID */
 
#ifndef LIST_H
#define LIST_H

#include "v2k_mem.h"

#ifdef __cplusplus

class ListItem {
public:
  ListItem *next;

  inline ListItem(ListItem *_nxt = 0) : next(_nxt) {}

  inline void recycle() {delete this;}
};

template<typename t>
class ListItemT {
public:
  t *next;
  inline ListItemT(t *_nxt = 0) : next(_nxt) {}

  inline       t *Next()       { return next; }
  inline const t *Next() const { return next; }
};

typedef enum {
  DIFF_LESS = -1,
  DIFF_SAME =  0,
  DIFF_GRTR =  1,
  DIFF_HSHD =  2
} eDIFF;

typedef void       *(*listCB)(void *,void *);
typedef eDIFF       (*listCmp)(void *,void *);
typedef const char *(*listStringItem)(void *);
typedef int         (*listHsh)(void *);

template<typename t>
class ListBuilder {
  ListBuilder &operator = (ListBuilder &); // Not available
public:
  t        *start,
          **next;
};

typedef enum {
  LIST_LEAVE   = 0,
  LIST_USE_DEL = 1,
  LIST_RECYCLE = 2
} eListMode;

#define MAP_INCR 400
template<typename t,int del>
class sortedList;

template<typename t,int del>
class ListMap {
  friend class sortedList<t,del>;
  int       max,
            last,
            hshd;
  listHsh   hsh;
  t       **tbl;

  ListMap &operator = (ListMap &); // Not available

public:
  inline void xtnd()      {REALLOC2(tbl,max += MAP_INCR,t *);};
  inline int  add(t *p)   {if (this) {if (last >= max) xtnd();
                                      tbl[last++] = p;
                                      return last-1;} else return -1;};
  inline int  mode()      {return hshd + (0 != hsh);};
  inline int  dirty()     {return hshd = 1;};
  inline int  count()     {return last;};
  inline void clear()     {if (this && tbl) {BZERO(tbl,last * sizeof(void *));
                                             last = 0;}};
  inline t **Tbl() const  {return tbl;};
  inline t *operator[](int i) const
                          {return i >= 0 && i < last ? tbl[i] : 0;};
  inline ListMap()        {max = last = 0; tbl = 0; hshd = 0; hsh = 0;};
  inline ~ListMap()       {FREE(tbl)};
};

template<typename t,int del>
class List;

template<typename t,int del>
class sortedList : public ListBuilder<t> {
  friend class List<t,del>;
 protected:
  ListMap<t,del> *map;

 public:
  inline sortedList(ListMap<t,del> *m = 0)  {map = m;}
  inline ~sortedList()                      {DELETE(map);}

  inline ListMap<t,del> *Map() {return map;}

  void testMap(listCmp cmp,listStringItem prnt) {
    if (map && map->last > 0) {
      t **tbl = map->Tbl();
      fprintf(stderr,"-- %s\n",(*prnt)(tbl[0]));
      int n = 0;
      while (++n < map->last) {
        fprintf(stderr,"%2d %s\n",(*cmp)(tbl[n-1],tbl[n]),
		                 (*prnt)(tbl[n]));
      }
    }
  }

  INLINE eDIFF lsearch(t *tp,listCmp cmp,int *top,t **ret)
  {
    eDIFF   df;
    t     **scn = map->Tbl();
    int     n   = *top;

    for (; n-- > 0 ; scn++) {
      if (DIFF_SAME == (*cmp)(tp,*scn)) {
        *ret = *scn;
        *top = scn - map->Tbl();
        return DIFF_SAME;
      }
    }

    return DIFF_HSHD;
  }

  INLINE eDIFF bsearch(t *tp,listCmp cmp,int *pTop,t **pRet)
  {
    int    bot  = 0,
           ci   = 0,
           idx  = 0,
           top  = *pTop;
    eDIFF  df   = DIFF_LESS;
    t     *ret  = 0;

    while (top >= bot) {
      ret = map->Tbl()[idx = ci = (top + bot)/2];
      switch (df = (*cmp)(tp,ret)) {
      case DIFF_SAME: goto done;
      case DIFF_GRTR: if (++ci > top) goto done;
                      bot  = ci;
                      break;
      case DIFF_LESS: if (--ci < bot) goto done;
                      top = ci;
                      break;
      case DIFF_HSHD: map->hshd = 1;
                      return lsearch(tp,cmp,pTop,pRet);
      default:        assert(0);
      }
    }

    done:
    *pRet = ret;
    *pTop = idx;
    return df;
  }

  void fixEntry(int idx,t *item,listCmp cmp,int nxt_fld)
  {
    if (map->hshd) {
    } else {
      if (map->last > 1) {
        t     *near = 0;
        int    nidx = map->last-2;
        eDIFF  diff = bsearch(item,cmp,&nidx,&near);

        switch (diff) {
        case DIFF_GRTR: near = map->tbl[++nidx];
        case DIFF_LESS: assert(near && nidx < map->last);
                        for (;;) {
                          map->tbl[nidx++] = item;
			  if (nidx >= map->last) break;
                            item           = near;
                            near           = map->tbl[nidx];
                        }
        case DIFF_HSHD: break;
        default:        assert(0);
        }
      }
    }
  }

  t *findEntry(t *like,listCmp cmp)
  {
    t     *ret;
    int    idx  = Map()->last-1;
    eDIFF  diff = bsearch(like,cmp,&idx,&ret);

    if (DIFF_SAME != diff) ret = 0;

    return ret;
  }

};

template<int del>
  class ListArrayItem;

template<typename t,int del>
class List { 
  sortedList<t,del> l;

  static inline void remove(t *item) {switch (del) {case LIST_USE_DEL: delete item;
                                                    case LIST_LEAVE:   return;
                                                    case LIST_RECYCLE: item->recycle();}}

public:
  inline void set_map(ListMap<t,del> *m) {l.map = m;}
  inline void chk_map(ListMap<t,del> *m) {assert(l.map = m);}
  inline void chk_init()                 {assert(l.start == 0 
                                                 && l.next == &l.start);}
  inline void init()                     {l.start = 0; l.next = &l.start;
                                          l.map->clear();}
  inline t *first() const                {return l.start;};
  inline t **map() const                 {return l.map->Tbl();};
  inline int add(t *p)                   {*l.next = p; l.next = &(p->next);
                                          return l.map->add(p);};
  inline int count()                     {return l.map->count();};
  inline void *for_all(listCB cll,void *dp)
                                         {void *ret; t *p = l.start;
                                          for (;p;p = p->next) {
			                   if ((ret = (*cll)(p,dp))) return ret;}
                                          return 0;};
  inline void clear()                    {t *nxt,*dp = l.start; l.start = 0;
                                          if (del) {
                                            for (; dp ; dp = nxt) {
                                              nxt = dp->next; remove(dp); }}
                                          l.next = &l.start; l.map->clear(); };
  inline sortedList<t,del> *srtlst()     {return (sortedList<t,del> *)&l.start;};
  inline void sort(int i,t *p,listCmp cmp,void *nxt_fld)
                                         {srtlst()->fixEntry(i,p,cmp,((intptr_t)nxt_fld - (intptr_t)p));};
  inline int  mode()                     {return l.map->mode();}
  inline int  dirty()                    {return l.map->dirty();}
  inline void addord(t *p,listCmp cmp)   {int i; if ((i = add(p)) >= 0)
                                                  sort(i,p,cmp,&p->next);};
  inline t *findord(t *p,listCmp cmp)
                                         {return (t *)(srtlst()->findEntry(p,cmp));};

  inline List(bool mppd = false) : l(mppd ? new ListMap<t,del>
				          : 0)
                                         {init();}
  inline ~List()                         {clear();}

  void init(ListArrayItem<del> *l);
};

template<int del>
class ListArrayItem {
 public:
  ListMap<ListItem,del> map;
  List<ListItem,del>    list;

  ListArrayItem() : list(&map) {}
  ~ListArrayItem() { list.clear(); list.set_map(0); }
};

template<int del>
void InitLists(ListArrayItem<del> *lists,int max)
{
  for (;max-- > 0;lists++) {
    List<ListItem,del> *lst = (List<ListItem,del> *)&lists->list;
    lst->init(lists);
  }
}

template<typename t,int del>
void List<t,del>::init(ListArrayItem<del> *l)
{
  chk_map(&l->map);
  chk_init();
}

template<typename t>
class simpleList {
  ListBuilder<t> l;

public:
  inline t *Next()         {return l.next;}
  inline void init()       {l.start = 0; l.next = &l.start;};
  inline t *first() const  {return l.start;};
  inline int add(t *p)     {*l.next = p; l.next  = &(p->next);};
  inline simpleList()      {init();};
  inline ~simpleList()     {t *s,*nxt = l.start; l.start = 0;
                            while ((s=nxt)) { nxt = s->next; s->next = 0;
			                    delete s; }}
};

#endif // __cplusplus

#endif

/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: $Id: dump_value.inc,v 1.15 2009/05/22 21:24:40 dkc Exp $ */
  


  int         i,
              u    =  0,
              base =  10,
              wdth = -1,
              shft;
  U64         mask;

  const char *frmt = "%d";
  Qzx         qzx,
             *pq   = &qzx;

  qzx.next = 0;

  switch (typ) {
  case VT_EXPR:    DUMP(VFN_EXPR(vfn),strm);
                   break;
  case VT_UINT:    u        = 1;
  case VT_INT:     qzx.n    = vfn->val()->i;
                   goto do_int;
  case VT_U64:     u        = 1;
  case VT_I64:     qzx.n    = vfn->val()->i64;
                   goto do_int;
  case VT_LUINT:   u        = 1;
                   base     = vfn->val()->lgc.b;
                   wdth     = vfn->val()->lgc.w;
                   qzx.n    = vfn->val()->lgc.i;
                   qzx.x    = qzx.z = qzx.q = 0;
                   qzx.next = 0;
                   goto sized;
  case VT_LOGIC:   pq       = (Qzx *)xtnd;
                   qzx.n    = vfn->val()->i64;
                   goto do_logic;
  do_int:          qzx.x    = qzx.z = qzx.q = 0;
  do_logic:        if (!xdf) goto plain;
                   base = IBASE(xdf);
		   wdth = IWIDTH(xdf);
  sized:           if (wdth > 0) {
                     Strm()->printf("%d'",wdth);
                   }
                   switch (base) {
                   case 16: if (pq) {
                              printQzx(pq,wdth,Strm(),"h",16,-1,0);
                              break;
	  	            }
                            frmt = "h%x";
                            goto plain;
                   case 8:  if (pq) {
                              printQzx(pq,wdth,Strm(),"o",8,-1,0);
                              break;
                            }
                            frmt = "o%x";
                            goto plain;
                   case 2:  if (pq) {
                              printQzx(pq,wdth,Strm(),"b",2,-1,0);
                              break;
                            }
                            frmt = 0;
                            goto plain;
                   default: if (wdth > 0) frmt = "d%d";
                            goto plain;
                   }
                   break;
                 plain:
                   u  =  qzx.n;
                   if ((i = qzx.n >> 32) || !frmt) {
                     switch(base) {
                     case 16: Strm()->printf(frmt,i);
                              Strm()->printf("%08x",u); break;
                     case 8:  pq = &qzx; qzx.x = qzx.z = qzx.q = 0;
                              printQzx(pq,wdth,Strm(),"o",8,-1,0); break;
                     case 2:  pq = &qzx; qzx.x = qzx.z = qzx.q = 0;
                              printQzx(pq,wdth,Strm(),"b",2,-1,0); break;
                     default: { char  asc[40],
                                     *pa = &asc[40];
                                U64   d10;
                                for (*--pa = '\0'; qzx.n ; qzx.n = d10) {
                                  d10   =  qzx.n/10;
                                  *pa-- = '0' + (qzx.n - 10 * d10);
                                }
                                Strm()->printf(++pa);
                              }
                     }
                   } else {
                     Strm()->printf(frmt,u);
                   }
                   break;
  case VT_REF:     Strm()->printf("%s",strDump(vfn->val()->ref));       break;
  case VT_STRING: {const char *cp = strDeref(vfn->val()->ref);
                   TMPARR(char,buff,4 * strlen(cp));
                   char *bp = buff;
                   int   ch;
		   do switch (ch = *cp++) {
                   case '\\': ch = '\\'; goto esc1;
                   case '\n': ch = 'n';  goto esc1;
                   case '\f': ch = 'f';  goto esc1;
                   case '\a': ch = 'a';  goto esc1;
                   case '\b': ch = 'b';  goto esc1;
                   case '\r': ch = 'r';  goto esc1;
                   case '\t': ch = 't';  goto esc1;
                   case '\v': ch = 'v';  goto esc1;
                   esc1:      *bp++ = '\\';
                              *bp++ = ch;
                              break;
                   default:   if (ch && !isprint(ch)) {
                                sprintf(bp,"\\0%o",ch & 0xFF);
                                bp += strlen(bp);
		              } else {
                                *bp++ = ch;
                              }
		   } while (ch);
                   Strm()->printf("\"%s\"",buff);
                   break;}
  case VT_DOUBLE:  Strm()->printf("%g",vfn->val()->d);
                   break;
  case VT_NATURE:  Strm()->printf("%s",
                           strDump(MAPS nature_map(vfn->val()->i)->name));
                   break;
  case VT_DISC:    Strm()->printf("%s",
                           strDump(MAPS disc_map(vfn->val()->i)->name));
                   break;
  default:         assert(0);
  }

#ifdef KEEP_DEFS
# undef KEEP_DEFS
#else
# undef ROOT
# undef MAPS
# undef DUMP
# undef VFN_EXPR
#endif

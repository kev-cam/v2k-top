/* Copyright (c) 1998,1999,2001,2002,2003 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ const char * libv2kprl_i_rcsid() {return "$Id: libv2kprl.i,v 1.3 2003/01/14 02:26:22 dkc Exp $";}
 


%module libv2kprl

extern int PerlV2Kinit(const char *exe);
extern int PerlV2Kdo  (int e,int s,const char *command);



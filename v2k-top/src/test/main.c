/* Copyright (c) 1998-2007 Kevin Cameron */
/* Distributed under the GNU Lesser General Public License */
/* RCS ID: */ const char * main_c_rcsid() {return "$Id: main.c,v 1.7 2007/02/01 06:52:42 dkc Exp $";}
 



#include "system.h"
#include "args.h"

int main(int argc,char **argv)
{ 
  InitLang();
  return processArgs(&argc,&argv,0);
}

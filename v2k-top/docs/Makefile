default: all

HTC=$(shell ls *-htm.c)
HTM=$(patsubst %-htm.c,../export/html/%.html,$(HTC))

OVERRIDE_CLEAN=TRUE

include $(CONFIG_DIR)/common.gmk

html/%-dep::%
	if [ ! -d html ] ; then mkdir html ; fi ;\
	($(ECHON) html/;\
	 gcc -M -DDEPS $(patsubst html/%-dep,%,$@)) | sed s/-htm.o/.html/> $@

html/deps: Makefile
	if [ ! -d html ] ; then mkdir html ; fi ;\
	echo "# Auto-gen" > $@
	for f in $(HTC) ; do\
		echo include html/$$f-dep >> $@;\
	done

include html/deps

clean:
	rm html/deps $(HTM)

../export/html/%.html::%-htm.c
	if [ ! -d ../export/html ] ; then mkdir -p ../export/html ; fi ;\
	gcc -E -D_$(patsubst ../export/html/%.html,%,$@)\
	    $(patsubst ../export/html/%.html,%-htm.c,$@) | grep -v "^#" | tr "^" ' ' >$@

../html/index.html:
	if [ ! -d ../html ] ; then mkdir -p ../html ; fi ;\
	cp doxygen.html $@
	cp doxygen.gif ../export/html
	mk -cd .. doxy

../export/html/main.html: left-htm.c
	if [ ! -d ../export/html ] ; then mkdir -p ../export/html ; fi ;\
	gcc -E left-htm.c -DMAIN | grep -v "^#" >$@

../config/$(SYS_OS)-$(SYS_CPU)/uname:
	uname -a > $@
	if [ -d CVS ] ; then cvs add $@ ; echo "Created $@"; fi

uname-x: ../config/$(SYS_OS)-$(SYS_CPU)/uname
	@echo $<

html/os.lst: uname-x Makefile 
	@if [ ! -d html ] ; then mkdir html ; fi ;\
	(echo "<table border>";\
	 echo "<thead><tr>";\
	 echo "<th><pre> ENV: SYS_OS </pre></th><th><pre>  SYS_CPU  </pre></th><th><pre>  OBJ_DIR  </pre><th><pre>  SYS_LP   </pre></th>";\
	 echo "</thead></tr><tbody>")> $@
	 set set ; cat ../config/*-*/uname |\
		(read uname ; while [ 0 = $$? ] ; do\
		  UNAMEA=$$uname ; export UNAMEA ; mk -shmc;\
		 read uname;\
		 done) | sort -u |\
		(read id ; while [ 0 = $$? ] ; do\
		  eval $$id;\
		  echo "<tr><td><pre> $$SYS_OS</pre></td><td><pre> $$SYS_CPU</pre></td><td><pre> $$OBJ_DIR</pre></td><td><pre> $$SYS_LP  </pre></td></tr>" >> $@;\
		 read id;\
		 done);\
	echo "</tbody></table>" >> $@;

dir-list.txt:
	mk -cd .. all_html

../export/html/source.html: dir-list.txt
	echo > $@;\
	for d in `cat dir-list.txt` ; do\
	  if [ -d ../$$d/html ] ; then\
	    mk -cd ../$$d html/files.html DOCUMENTING=TRUE;\
	    echo '<h3><a href="'../../$$d/html/files.html'">'$$d'</a></h3>' >> $@;\
	    if [ -f ../$$d/dir.html ] ; then cat ../$$d/dir.html >> $@; fi;\
	  fi ;\
	done

clean:
	rm -f html/* ../doxy.cfg

ALL_HTM = $(HTM) ../export/html/main.html ../export/html/source.html

all: $(ALL_HTM) ../html/index.html html/os.lst

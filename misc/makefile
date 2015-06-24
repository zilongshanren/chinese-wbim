###################################
# user customized variables
EMACS = emacs
PREFIX = $$HOME/local
###################################

###################################
ELISPDIR = $(PREFIX)/share/emacs/site-lisp/eim
EL = eim.el chinese-wbim-extra.el chinese-wbim-py.el \
            chinese-wbim-wb-gb2312.el chinese-wbim-wb-gbk.el chinese-wbim-wb.el chinese-wbim-cj.el \
     chinese-wbim-cj-chars.el chinese-wbim-eb.el chinese-wbim-eb-map.el chinese-wbim-table.el
ELC = eim.elc chinese-wbim-extra.elc chinese-wbim-py.elc \
      chinese-wbim-wb-gb2312.elc chinese-wbim-wb-gbk.elc chinese-wbim-wb.elc chinese-wbim-cj.elc \
      chinese-wbim-eb.elc chinese-wbim-eb-map.elc chinese-wbim-cj-chars.elc chinese-wbim-table.elc
EXTRAFILES = wb.txt mywb.txt wbpy.txt otherpy.txt py.txt cj.txt \
          cjeb.txt ebpy.txt
DISTFILES = ChangeLog ChangeLog.1 Readme makefile \
            $(EL) $(EXTRAFILES) \
            pychr.txt sanguo.txt charpy.st pyword2tbl.pl mergepy.pl \
           table2chartbl.pl

all: $(ELC)

install: all
	install -d $(ELISPDIR)
	install -m 0644 $(EL) $(ELC) $(ELISPDIR)
	install -m 0666 $(EXTRAFILES) $(ELISPDIR)
	cd $(ELISPDIR) && gzip -qf *.el

uninstall:
	if [ -d "$(ELISPDIR)" ]; then rm -rf $(ELISPDIR); fi

dist: $(DISTFILES)
	version=`grep "chinese-wbim-version" eim.el`; \
	version=`echo $$version | perl -ne '/[0-9.]+/; print $$&'`; \
	rm -rf "chinese-wbim-$$version"; \
	mkdir "chinese-wbim-$$version"; \
	cp $(DISTFILES) "chinese-wbim-$$version"; \
	tar -zcvf "chinese-wbim-$$version.tar.gz" "chinese-wbim-$$version"; \
	rm -rf "chinese-wbim-$$version"

%.elc: %.el
	$(EMACS) -q -batch --no-site-file --eval="(add-to-list 'load-path \".\")" \
	-f batch-byte-compile $<

clean:
	-rm -f *.elc *~

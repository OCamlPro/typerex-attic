
include ../../autoconf/Makefile.config

OBUILD=../../_obuild

all:
	ocp-build ocp-manager

ocp-manager.1: $(OBUILD)/ocp-manager/ocp-manager.asm
	$(OBUILD)/ocp-manager/ocp-manager.asm -man > ocp-manager.1

install: ocp-manager.1
	cp $(OBUILD)/ocp-manager/ocp-manager.asm $(bindir)/ocp-manager
	mkdir -p $(mandir)/man1
	cp ocp-manager.1 $(mandir)/man1/ocp-manager.1
	ocp-manager -list
	@echo
	@echo "Don't forget to update your configuration files with"
	@echo
	@echo '    eval `ocp-manager -config`'
	@echo

install.user:
	$(OBUILD)/ocp-manager/ocp-manager.asm

install.opam: ocp-manager.1
	mkdir -p $(MANDIR)/man1
	cp $(OBUILD)/ocp-manager/ocp-manager.asm $(BINDIR)/ocp-manager
	cp ocp-manager.1 $(MANDIR)/man1/ocp-manager.1

OPAMER=ocp-opamer
TOOL=ocp-manager
OPAM_PACKAGE=distrib/opam

force_tag:
	git tag -f $(TOOL).$(VERSION)
	git push -f origin $(TOOL).$(VERSION)


opam.man:
	@echo 'In Typerex:'
	@echo '(1) commit all your changes: git commit .'
	@echo '(2) push them to github: ocp-pubgit -e ocp-manager'
	@echo 'In a public checkout of OCamlPro/ocp-manager:'
	@echo '(3) update: git pull'
	@echo '(4) tag: make force_tag'
	@echo '(5) create the corresponding opam: make opamize'

opamize:
	$(OPAMER) \
	 	-descr $(OPAM_PACKAGE)/$(TOOL).descr \
		-opam $(OPAM_PACKAGE)/$(TOOL).opam  \
		$(TOOL) $(VERSION) \
		https://github.com/OCamlPro/$(TOOL)/tarball/$(TOOL).$(VERSION)

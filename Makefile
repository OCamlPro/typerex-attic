

include autoconf/Makefile.config

all: ocp-build-build

# install: ocp-build-install
install:
	for tool in tools/*; do $(MAKE) -C $$tool install; done

clean: ocp-build-clean

distclean: clean ocp-distclean
	find . -name '*~' -exec rm -f {} \;

include autoconf/Makefile.rules

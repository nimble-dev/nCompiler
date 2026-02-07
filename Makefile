.PHONY: clean build check

VERSION=$(shell grep Version: nCompiler/DESCRIPTION | grep -o '[^ ]*$$')

nCompiler_$(VERSION).tar.gz:
	R CMD build nCompiler

build: 
	R CMD build nCompiler

## e.g., `make install -l /tmp/nc-branch`
install: nCompiler_$(VERSION).tar.gz
	R CMD INSTALL $(LIB) --install-tests nCompiler_$(VERSION).tar.gz

check: nCompiler_$(VERSION).tar.gz
	R CMD check --as-cran nCompiler_$(VERSION).tar.gz

clean:
	rm -rf nCompiler_$(VERSION).tar.gz

.PHONY: clean build check

VERSION=$(shell grep Version: nCompiler/DESCRIPTION | grep -o '[^ ]*$$')

nCompiler_$(VERSION).tar.gz:
	R CMD build nCompiler

build: 
	R CMD build nCompiler

install: nCompiler_$(VERSION).tar.gz
	R CMD INSTALL --install-tests ${NCDIR} nCompiler_$(VERSION).tar.gz

check: nCompiler_$(VERSION).tar.gz
	R CMD check --as-cran nCompiler_$(VERSION).tar.gz

clean:
	rm -rf nCompiler_$(VERSION).tar.gz

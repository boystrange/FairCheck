
TESTS  = $(wildcard examples/*.pi)
ERRORS = $(wildcard errors/*.pi)
DEST   = padovani@pianeta.di.unito.it:public_html/Software/FairCheck/

all:
	@stack build

watch:
	@stack build --file-watch

install:
	@stack install

dist:
	@cabal sdist

sync:
	@make -C html
	@scp html/*.* $(DEST)
	@scp dist/*.tar.gz $(DEST)

info:
	@stack exec happy -- -i src/Parser.y

%.check_ok:
	@faircheck --log $(@:%.check_ok=%) || echo

check_examples:
	@echo
	@echo "SCRIPTS THAT MUST PASS"
	@echo "–————————————————————–"
	@for i in $(TESTS); do make -s $$i.check_ok; done

%.check:
	@faircheck --log $(@:%.check=%) || echo

check_errors:
	@echo
	@echo "SCRIPTS THAT MUST FAIL"
	@echo "–————————————————————–"
	@for i in $(ERRORS); do make -s $$i.check; done

update_licence:
	for hs in `find src -name "*.hs"`; do \
		TEMP=`mktemp`; \
		cp LICENSE.hs $$TEMP; \
		tail -n +17 <$$hs >>$$TEMP; \
		mv $$TEMP $$hs; \
	done

check: check_examples check_errors
	@echo

.PHONY: dist clean check check_examples check_errors

clean:
	@stack clean
	@rm -f `stack path --local-bin`/faircheck

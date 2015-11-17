EMACS ?= emacs
EMACSFLAGS =
CASK = cask

default: test

elpa:
	$(CASK) install
	$(CASK) update
	touch $@

org-github.elc: elpa
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		--directory "."                          \
		$(EMACSFLAGS)                            \
		--eval "(progn                           \
			(setq byte-compile-error-on-warn t)  \
			(batch-byte-compile))" org-github.el

.PHONY: test
test: org-github.elc
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		$(EMACSFLAGS) \
		-l org-github.elc \
		-l test/org-github-test.el \
		--eval "(ert-run-tests-batch-and-exit t)"

.PHONY: clean
clean:
	rm -f org-github.elc
	rm -f elpa
	rm -rf .cask

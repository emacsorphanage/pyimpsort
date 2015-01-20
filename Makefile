TOX    ?= tox
CASK   ?= cask
WGET   ?= wget
EMACS  ?= emacs
PYTEST ?= py.test
BATCH   = $(EMACS) --batch -Q -L .
BATCHC  = $(BATCH) -f batch-byte-compile

ELS     = pyimpsort.el
ELCS    = $(ELS:.el=.elc)
ELSTEST = test_pyimpsort.el

.PHONY: all
all: compile README.md

.PHONY: compile
compile: elpa $(ELCS)

elpa: Cask
	$(CASK) install
	touch $@

%.elc: %.el
	$(CASK) exec $(BATCHC) $<

README.md: make-readme-markdown.el $(ELS)
	$(CASK) exec $(BATCH) --script $< <$(ELS) >$@ 2>/dev/null

make-readme-markdown.el:
	$(WGET) -q -O $@ "https://raw.github.com/mgalgs/make-readme-markdown/master/make-readme-markdown.el"

.INTERMEDIATE: make-readme-markdown.el

.PHONY: test
test: test-py test-el

test-py:
	$(PYTEST)

test-el:
	$(CASK) exec $(BATCH) -l ert -l $(ELSTEST) -f ert-run-tests-batch-and-exit

clean:
	$(RM) $(ELCS)

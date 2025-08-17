export EMACS ?= $(shell command -v emacs 2>/dev/null)
CASK_DIR := $(shell cask package-directory)

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: compile
compile: cask
	cask emacs -batch -L . -L test \
	  -f batch-byte-compile $$(cask files); \
	  (ret=$$? ; cask clean-elc && exit $$ret)

generics-tests: compile
	cask emacs --batch -L . -L test -l quickcheck-generics-tests -f ert-run-tests-batch

general-utilities-tests: compile
	cask emacs --batch -L . -L test -l quickcheck-general-utilities-tests -f ert-run-tests-batch    

testing-utilities-tests: compile
	cask emacs --batch -L . -L test -l quickcheck-testing-utilities-tests -f ert-run-tests-batch

seq-extras-tests: compile
	cask emacs --batch -L . -L test -l seq-extras-tests -f ert-run-tests-batch

EMACS ?= emacs
SELECTOR ?= (tag :org)
ERT = "(ert-run-tests-batch-and-exit)"
ERTWITHSELECTOR = "(ert-run-tests-batch-and-exit '$(SELECTOR))"
QUICKCHECK = ~/Documents/quickcheck/quickcheck
QUICKCHECKTESTS = ~/Documents/quickcheck/quickcheck-tests

# dependencies
DASH = ~/.emacs.d/straight/repos/dash.el/dash.el
S = ~/.emacs.d/straight/repos/s.el/s.el



test-quickcheck: ## test-runner for quickcheck
test-quickcheck:
	$(EMACS) --batch -l $(DASH) \
		 -l $(S) \
		 -l $(QUICKCHECK) \
		 -l $(QUICKCHECKTESTS) \
		 --eval $(ERT); \

EMACS ?= emacs
SELECTOR ?= (tag :org)
ERT = "(ert-run-tests-batch-and-exit)"
ERTWITHSELECTOR = "(ert-run-tests-batch-and-exit '$(SELECTOR))"
QUICKCHECK = ~/.emacs.d/personal-packages/quickcheck/quickcheck

# dependencies
DASH = ~/.emacs.d/straight/repos/dash.el/dash.el



test-quickcheck: ## test-runner for quickcheck
test-quickcheck:
	$(EMACS) --batch -l $(DASH) \
		 -l $(QUICKCHECK) \
		 -l $(QUICKCHECKTESTS) \
		 --eval $(ERT); \

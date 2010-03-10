ROOT:=$(dir $(lastword $(MAKEFILE_LIST)))
BUILD_DIR=$(ROOT)/build
SRC_DIR=$(ROOT)/../..
TEST_SRC_DIR=$(ROOT)
DIST_DIR=$(ROOT)dist
GHC_OPTS=-fhpc -i$(SRC_DIR) -i$(TEST_SRC_DIR) -odir $(BUILD_DIR) -hidir $(BUILD_DIR)

test: Main
	@$(DIST_DIR)/Main

Main: clean
	@rm -rf $(DIST_DIR)
	mkdir $(DIST_DIR)
	ghc $(GHC_OPTS) --make $(TEST_SRC_DIR)/Main.hs -o $(DIST_DIR)/Main

hpc-report: test
	hpc report Main --exclude=Main

hpc-markup: test
	hpc markup Main --exclude=Main --exclude QuickCheckUtils --destdir=$(DIST_DIR)
	@echo Find the report at $(DIST_DIR)/hpc_index.html

clean:
	rm -fr $(BUILD_DIR)
	rm -fr $(DIST_DIR)
	rm -fr .hpc
	rm -f Main.tix

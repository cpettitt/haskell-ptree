BUILD_DIR=build
SRC_DIR=../..
DIST_DIR=dist
GHC_OPTS=-i${SRC_DIR} -odir ${BUILD_DIR} -hidir ${BUILD_DIR}

Main: clean
	@rm -rf ${DIST_DIR}
	mkdir ${DIST_DIR}
	ghc ${GHC_OPTS} -fhpc --make Main.hs -o ${DIST_DIR}/Main

test: Main
	@${DIST_DIR}/Main

hpc-report: test
	hpc report Main --exclude=Main

hpc-markup: test
	hpc markup Main --exclude=Main --exclude QuickCheckUtils --destdir=${DIST_DIR}
	@echo Find the report at ${DIST_DIR}/hpc_index.html

clean:
	rm -fr ${BUILD_DIR}
	rm -fr ${DIST_DIR}
	rm -fr .hpc
	rm -f Main.tix

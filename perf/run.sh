#!/bin/sh

GHC_OPTS=-O
SRC_DIR=`pwd`/..
BUILD_DIR=`pwd`/build
RESULTS_DIR=`pwd`/results
TESTS="PTree Map Trie"

graph() {
R -q --vanilla >/dev/null <<EOF

r <- read.table("$1", row.names=1, header=TRUE)
xl <- length(row.names(r))
yl <- length(names(r))

png("$1.png", width=700, height=400)
par(xpd=T, mar=par()\$mar+c(0,0,0,5))
matplot(r, type='p', pch='x', xaxt='n', ylab="Mean Time (s)", log="y")
axis(1, at=1:xl, labels=row.names(r))
legend(xl * 1.05 , max(r), names(r), pch='x', col=1:yl )
dev.off()
EOF
}

if test "$NO_BENCH" == ""
then
    # Cleanup results
    rm -rf $BUILD_DIR
    mkdir $BUILD_DIR

    rm -rf $RESULTS_DIR
    mkdir $RESULTS_DIR

    # Run benchmarks
    for t in $TESTS
    do
        echo Running benchmarks for: $t
        ghc ${GHC_OPTS} --make Bench$t.hs -i${SRC_DIR} -odir ${BUILD_DIR} -hidir ${BUILD_DIR} -o ${BUILD_DIR}/Bench$t
        mkdir $RESULTS_DIR/$t
        pushd $RESULTS_DIR/$t
        ln -s ../../data
        $BUILD_DIR/Bench$t -k png -t png -u results.csv
        popd
    done
fi

# Consolidate results
TMP_RESULTS_DIR=$RESULTS_DIR/tmp
mkdir $TMP_RESULTS_DIR
for g in $($BUILD_DIR/Bench$(echo $TESTS | cut -d' ' -f1) -l | tail -n +2 | sed -E 's|([a-z]+)/.*|\1|' | sort | uniq)
do
    echo Consolidating benchmarks for group: $g
    for t in $TESTS
    do
        grep $g $RESULTS_DIR/$t/results.csv | cut -d',' -f2 > $TMP_RESULTS_DIR/$g-$t
    done
    grep $g $RESULTS_DIR/$t/results.csv | cut -d',' -f1 > $TMP_RESULTS_DIR/$g-rows
    GRP_RESULTS=$(echo $TESTS | sed -E "s|([A-Za-z]+)|$TMP_RESULTS_DIR/$g-\1|g")
    echo "name\t$(echo $TESTS | tr ' ' '\t')" > $RESULTS_DIR/$g-results
    paste $TMP_RESULTS_DIR/$g-rows $GRP_RESULTS >> $RESULTS_DIR/$g-results
    graph $RESULTS_DIR/$g-results
done
rm -rf $TMP_RESULTS_DIR

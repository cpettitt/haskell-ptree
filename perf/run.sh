#!/bin/sh

GHC_OPTS=-O
SRC_DIR=`pwd`/..
BUILD_DIR=`pwd`/build
RESULTS_DIR=`pwd`/results

if test "${TESTS}" == ""
then
    TESTS="PTree Map Trie"
fi

graph() {
R -q --vanilla >/dev/null <<EOF

r <- read.table("$1", row.names=1, header=TRUE)
xl <- length(row.names(r))
yl <- length(names(r))

png("$1.png", width=700, height=400)
par(xpd=T, mar=par()\$mar+c(0,0,0,5))
matplot(r, type='p', pch=23:25, xaxt='n', ylab="Mean Time (s)", log="y")
axis(1, at=1:xl, labels=row.names(r))
legend(xl * 1.05 , max(r), names(r), pch=23:25, col=1:yl )
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
        $BUILD_DIR/Bench$t -g -v -k png -t png -u results.csv
        popd
    done
fi

# Consolidate results
TMP_RESULTS_DIR=$RESULTS_DIR/tmp
rm -rf $TMP_RESULTS_DIR
mkdir $TMP_RESULTS_DIR
for g in $($BUILD_DIR/Bench$(echo $TESTS | cut -d' ' -f1) -l | tail -n +2 | sed -E 's|([a-z]+)/.*|\1|' | sort | uniq)
do
    echo Consolidating benchmarks for group: $g
    for t in $TESTS
    do
        grep $g $RESULTS_DIR/$t/results.csv | cut -d',' -f2 > $TMP_RESULTS_DIR/$g-$t
    done
    for b in $(grep $g $RESULTS_DIR/$t/results.csv | cut -d',' -f1)
    do
        printf "%20s\n" $b >> $TMP_RESULTS_DIR/$g-rows
    done
    GRP_RESULTS=$(echo $TESTS | sed -E "s|([A-Za-z]+)|$TMP_RESULTS_DIR/$g-\1|g")
    echo -e "Name\t$(echo $TESTS | tr ' ' '\t')" > $RESULTS_DIR/$g-results
    paste $TMP_RESULTS_DIR/$g-rows $GRP_RESULTS >> $RESULTS_DIR/$g-results
    graph $RESULTS_DIR/$g-results
done
rm -rf $TMP_RESULTS_DIR

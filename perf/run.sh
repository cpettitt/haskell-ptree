#!/bin/sh

rm -rf results
mkdir results

for t in PTree Map Trie
do
    echo Running benchmarks for: $t
    ghc -O --make Bench$t.hs
    mkdir results/$t
    pushd results/$t
    ln -s ../../data
    ../../Bench$t -k png -t png -u results.csv
    popd
done

# Cleanup
rm *.o *.hi

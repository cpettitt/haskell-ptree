#!/bin/sh

ghc -fforce-recomp -O2 -Wall --make Main.hs >/dev/null 2>&1

for t in ptree datamap trie
do
    echo $t
    time ./Main $t < testdata 2>&1 >/dev/null
    echo
    echo
done 

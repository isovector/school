#!/usr/bin/env bash

mkdir crunch-results/randTSP
for LENGTH in randTSP/*; do
    print "processing ${LENGTH}"
    for FILE in $LENGTH/*; do
        cat $FILE | java -jar tsp.jar >> crunch-results/${LENGTH}.txt
    done
done

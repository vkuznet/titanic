#!/usr/bin/env bash
# set path to Weka
export CLASSPATH=/Applications/weka-3-6-9.app/Contents/Resources/Java/weka.jar
data=./model.arff
seed=1
opts="-S 1 -M 2 -N 5 -C 1.0 -P POSTPRUNED"

if [ $# -eq 0 ]; then
java weka.classifiers.trees.BFTree
echo "Usage: weka.j48.sh <file.arff>"
else
java weka.classifiers.trees.BFTree -t $data $opts
fi

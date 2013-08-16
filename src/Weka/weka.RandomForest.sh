#!/usr/bin/env bash
# set path to Weka
export CLASSPATH=/Applications/weka-3-6-9.app/Contents/Resources/Java/weka.jar
data=./model.arff
seed=1
opts="-I 10 -K 0 -S 1"

if [ $# -eq 0 ]; then
java weka.classifiers.trees.RandomForest
echo "Usage: weka.RandomForest.sh <file.arff>"
else
java weka.classifiers.trees.RandomForest -t $data $opts
fi

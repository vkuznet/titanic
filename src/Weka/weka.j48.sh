#!/usr/bin/env bash
# set path to Weka
export CLASSPATH=/Applications/weka-3-6-9.app/Contents/Resources/Java/weka.jar
data=./model.arff
seed=1
opts="-C 0.25 -M 2"

if [ $# -eq 0 ]; then
java weka.classifiers.trees.J48
echo "Usage: weka.j48.sh <file.arff>"
else
data=$1
java weka.classifiers.trees.J48 -t $data $opts
fi

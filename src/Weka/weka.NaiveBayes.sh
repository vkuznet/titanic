#!/usr/bin/env bash
# set path to Weka
export CLASSPATH=/Applications/weka-3-6-9.app/Contents/Resources/Java/weka.jar
data=./model.arff
seed=1

if [ $# -eq 0 ]; then
java weka.classifiers.bayes.NaiveBayes
echo "Usage: weka.j48.sh <file.arff>"
else
data=$1
java weka.classifiers.bayes.NaiveBayes -t $data
fi

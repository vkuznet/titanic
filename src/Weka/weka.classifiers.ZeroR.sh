#!/usr/bin/env bash
# set path to Weka
export CLASSPATH=/Applications/weka-3-7-10-apple-jvm.app/Contents/Resources/Java/weka.jar
data=./model.arff
seed=1

if [ $# -eq 0 ]; then
java weka.classifiers.rules.ZeroR
echo "Usage: weka.attributeSelection.CfsSubsetEval.sh <file.arff>"
else
data=$1 

		java weka.classifiers.rules.ZeroR -t $data -i -x 10 

fi

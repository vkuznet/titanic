#!/usr/bin/env bash
# set path to Weka
export CLASSPATH=/Applications/weka-3-7-10-apple-jvm.app/Contents/Resources/Java/weka.jar
data=./model.arff
seed=1

#Attribute Evaluator  weka.attributeSelection.CorrelationAttributeEval

SearchMethod="weka.attributeSelection.BestFirst -D 1 -N 5"

if [ $# -eq 0 ]; then
java weka.attributeSelection.CfsSubsetEval
echo "Usage: weka.attributeSelection.CfsSubsetEval.sh <file.arff>"
else
data=$1
java weka.attributeSelection.CfsSubsetEval -P 1 -E 1 -i $data -s "$SearchMethod"  
fi

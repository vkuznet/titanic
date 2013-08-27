#!/usr/bin/env bash
# set path to Weka
export CLASSPATH=/Applications/weka-3-7-10-apple-jvm.app/Contents/Resources/Java/weka.jar
data=./model.arff
seed=1

#Attribute Evaluator  weka.attributeSelection.CorrelationAttributeEval

SearchMethod="weka.attributeSelection.Ranker -T -1.7976931348623157E308 -N -1 "

if [ $# -eq 0 ]; then
java weka.attributeSelection.CorrelationAttributeEval
echo "Usage: weka.attributeSelection.CorrelationAttributeEval.sh <file.arff>"
else
data=$1
java weka.attributeSelection.CorrelationAttributeEval -D -i $data -s "$SearchMethod" 
fi

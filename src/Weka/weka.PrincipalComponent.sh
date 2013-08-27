#!/usr/bin/env bash
# set path to Weka
export CLASSPATH=/Applications/weka-3-7-10-apple-jvm.app/Contents/Resources/Java/weka.jar
data=./model.arff
seed=1

if [ $# -eq 0 ]; then
java weka.filters.unsupervised.attribute.PrincipalComponents
echo "Usage: weka.filters.unsupervised.attribute.PrincipalComponents.sh <file.arff>"
else
data=$1 

		java weka.filters.unsupervised.attribute.PrincipalComponents -R 0.95 -A 5 -M -1 -i $data -o model_PrincipalComponents.arff  

fi

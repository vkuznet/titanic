#!/usr/bin/env bash
# set path to Weka
export CLASSPATH=/Applications/weka-3-6-9.app/Contents/Resources/Java/weka.jar
data=./model.arff
seed=1

kernel="weka.classifiers.functions.supportVector.PolyKernel -C 250007 -E 1.0"
alg1="weka.classifiers.functions.SMO -C 1.0 -L 0.0010 -P 1.0E-12 -N 0 -V -1 -W 1 -K \"$kernel\""
alg2="weka.classifiers.trees.RandomForest -I 10 -K 0 -S 1"

opts="-S 1 -B \"$alg1\" -B \"$alg2\" -R AVG"

if [ $# -eq 0 ]; then
java weka.classifiers.meta.Vote
echo "Usage: weka.Vote.sh <file.arff>"
else
data=$1
java weka.classifiers.meta.Vote -t $data -S 1 -B "$alg1" -B "$alg2" -R AVG
fi

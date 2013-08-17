#!/usr/bin/env bash
# set path to Weka
export CLASSPATH=/Applications/weka-3-6-9.app/Contents/Resources/Java/weka.jar
data=./model.arff
seed=1
cmd="java weka.classifiers.meta.AdaBoostM1"
opts="-P 100 -S 1 -I 10 -W weka.classifiers.trees.RandomForest -- -I 10 -K 0 -S 1"

if [ $# -eq 0 ]; then
$cmd
echo "Usage: weka.AdaBoostM1.sh <file.arff>"
else
$cmd -t $data $opts
fi

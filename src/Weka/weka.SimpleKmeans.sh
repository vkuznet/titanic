#!/usr/bin/env bash
# set path to Weka
export CLASSPATH=/Applications/weka-3-6-9.app/Contents/Resources/Java/weka.jar
data=./model.arff
seed=1
cmd="java weka.clusterers.SimpleKMeans"
opts="-N 10 -A weka.core.EuclideanDistance -I 500 -S 10"

if [ $# -eq 0 ]; then
$cmd
echo "Usage: weka.SimpleKMeans.sh <file.arff>"
else
$cmd -t $data $opts
fi

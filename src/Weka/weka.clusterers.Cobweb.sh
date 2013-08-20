#!/usr/bin/env bash
# set path to Weka
export CLASSPATH=/Applications/weka-3-6-9.app/Contents/Resources/Java/weka.jar
data=./model.arff
seed=1
cmd="java weka.clusterers.Cobweb"
opts="-A 1.0 -C 0.0028 -S 42"

if [ $# -eq 0 ]; then
$cmd
echo "Usage: weka.SimpleKMeans.sh <file.arff>"
else
$cmd -t $data $opts
fi

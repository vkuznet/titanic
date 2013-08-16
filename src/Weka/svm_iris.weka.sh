#!/usr/bin/env bash
# set path to Weka
export CLASSPATH=/Applications/weka-3-6-9.app/Contents/Resources/Java/weka.jar
data=./iris.arff
kernel="weka.classifiers.functions.supportVector.RBFKernel -C 250007 -G 0.01"
c=1.0
t=0.001
# -V The number of folds for the internal cross-validation. (default -1, use training data)
# -N Whether to 0=normalize/1=standardize/2=neither. (default 0=normalize)
# -W The random number seed. (default 1)
#opts="-C $c -L $t -N 2 -V -1 -W 1"
opts="-C $c -L $t -N 2"
cmd="java weka.classifiers.functions.SMO"
if [ "$1" == "help" ]; then
    $cmd
    exit 0
fi
$cmd $opts -K "$kernel" -t $data

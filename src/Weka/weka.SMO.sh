#!/usr/bin/env bash
# set path to Weka
export CLASSPATH=/Applications/weka-3-6-9.app/Contents/Resources/Java/weka.jar
data=./model.arff
seed=1
kernel1="weka.classifiers.functions.supportVector.PolyKernel -C 250007 -E 1.0"
kernel2="weka.classifiers.functions.supportVector.RBFKernel -C 250007 -G 1.0"
#kernel3="weka.classifiers.functions.supportVector.PolyKernel -C 250007 -E 2.0"
#kernel4="weka.classifiers.functions.supportVector.PolyKernel -C 250007 -E 4.0"
#kernel5="weka.classifiers.functions.supportVector.RBFKernel -C 250007 -G 0.01"
# -V The number of folds for the internal cross-validation. (default -1, usre
# training data)
opts="-C 1.0 -L 0.001 -N 0 -V 10 -W 1"
cmd="java weka.classifiers.functions.SMO"

if [ $# -eq 0 ]; then
java weka.classifiers.functions.SMO
echo "Usage: weka.SMO.sh <file.arff> <kernel-id>"
else
data=$1
if [ $# -eq 2 ]; then
    id=$2
    if [ $id == 1 ]; then
        mykernel=$kernel1
    elif [ $id == 2 ]; then
        mykernel=$kernel2
    elif [ $id == 3 ]; then
        mykernel=$kernel3
    elif [ $id == 4 ]; then
        mykernel=$kernel4
    elif [ $id == 5 ]; then
        mykernel=$kernel5
    fi
else
    mykernel=$kernel1
fi
$cmd $opts -K "$mykernel" -t $data
fi

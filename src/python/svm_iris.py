#!/usr/bin/env python
#-*- coding: utf-8 -*-
#pylint: disable-msg=
"""
File       : cleanup.py
Author     : Valentin Kuznetsov <vkuznet AT gmail dot com>
Description: 
"""

# system modules
import os
import sys
import csv
import numpy as np
import pylab as pl
from sklearn import svm
from sklearn import svm, datasets
from sklearn.metrics import confusion_matrix
from sklearn.metrics import classification_report

def report(clf, x_test, y_test):
    y_pred = clf.predict(x_test)
    print clf
    print(classification_report(y_test, y_pred))
    print(confusion_matrix(y_test, y_pred))

def classifier():
    # import some data to play with
    iris = datasets.load_iris()
#    x_train = iris.data[:100]
#    y_train = iris.target[:100]
    x_train = iris.data
    y_train = iris.target
    regC = 1.0  # SVM regularization parameter
    clf = svm.SVC(kernel='rbf', gamma=0.01, C=regC).fit(x_train, y_train)
#    x_test = iris.data[100:]
#    y_test = iris.target[100:]
#    report(clf, x_test, y_test)
    report(clf, x_train, y_train)

if __name__ == '__main__':
    classifier()

#!/usr/bin/env python
#-*- coding: utf-8 -*-
#pylint: disable-msg=
"""
File       : merge.py
Author     : Valentin Kuznetsov <vkuznet AT gmail dot com>
Description: 
"""

import sys
import csv

def read(fname):
    "Read given CSV file and yeild its rows"
    with open(fname, 'r') as csvfile:
        reader = csv.reader(csvfile, delimiter=' ', quotechar='|')
        for row in reader:
            yield ', '.join(row)

def read_pred(fname):
    "Read prediction file and create output dict of ids"
    ids = {}
    for row in read(fname):
        idx, val = row.split(',')
        ids[idx] = val.strip()
    return ids

def main(tfile, pfile, sfile='submission.csv'):
    """
    Main function which reads test data CSV file, prediciotn file and create
    final submission CSV file
    """
    gen1 = read(tfile)
    ids = read_pred(pfile)
    with open(sfile, 'w') as ostream:
        ostream.write(gen1.next()) # dump header values
        ostream.write(',Survived\n')
        for row in gen1:
            values = row.split(',')
            values.append(ids[values[0]])
            ostream.write(','.join(values) + '\n')

if __name__ == '__main__':
    usage = 'Usage: merge.py <testdata.csv> <prediction.txt> <submission.csv>'
    if  len(sys.argv) != 4:
        print usage
    main(sys.argv[1], sys.argv[2], sys.argv[3])

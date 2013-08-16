titanic
=======

This package is designed to run different algorithm for Titanic data. Here are
steps we need:

1. Run R-script, ./src/R/titanic.R, it will do data pre-processing and write
   out model.arff, model.csv files into current directory
2. Run bin/run_weka_alg model.arff, this script will invoke multiple Weka
   algorithm and write out their results into log directly. It is designed to
   parse output logs and print out results on stdout. The only stuff we print
   out is Correctly identified results (from training set and cross-validation)
   along with their kappa statistics.


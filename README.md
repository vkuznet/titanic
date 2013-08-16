titanic
=======

This package is designed to run different algorithms for Titanic data. Here are
the steps we need:

- Run R-script, **./src/R/titanic.R**

   - it will do data pre-processing
   - will printout correlation matrix and make correlation plot
   - will create mosaic plot
   - write out model.arff and model.csv for further processing

- Run **bin/run_weka_alg model.arff**

   - it will invoke multiple Weka algorithms, see src/Weka directory and
     will store results into the log area
   - it will also print on stdout Correctly identified results and kappa
     statistics for traninig set nad cross-validation steps

Refernces:
1) Stacking Multiple Classifiers, http://www.youtube.com/watch?v=Nje8mblA7bs

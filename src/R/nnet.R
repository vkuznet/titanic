#!/usr/bin/env Rscript
# clean-up session parameters
#rm(list=ls())

do.nnet <- function(tdf, testdata, keeps=NULL, fname="nnet", 
                        formula=NULL, testindex=NULL, printModel=FALSE) {
    # keep requested attributes
    if(!is.null(keeps))
        tdf <- keep(tdf, keeps)

    train.df <- tdf
    test.df <- train.df
    # use 70/30 splitting
    if (is.null(testindex)) {
        print(sprintf("Run nnet, use full training set"))
        survived <- train.df$Survived
        pids <- train.df$PassengerId
    } else {
        testset <- train.df[testindex,]
        trainset <- train.df[-testindex,]
        train.df <- trainset
        test.df <- testset
        survived <- testset$Survived
        pids <- testset$PassengerId
        print(sprintf("Run nnet, train %d, test %d", nrow(trainset), nrow(testset)))
    }

    # exclude id/PassengerId columns to work with ML
    test.df.copy <- test.df
    train.df <- drop(train.df, c("id", "PassengerId"))
    test.df <- drop(test.df, c("id", "PassengerId", "Survived"))

    # run svm algorithm
    if (is.null(formula)) formula <- as.formula("Survived~.")
    print(formula)
    nnet.model <- nnet(formula, data=train.df,
                       size = 2, rang = 0.1, decay = 5e-4, maxit = 200)
    nnet.pred <- predict(nnet.model, test.df)
    print(nnet.model)

    # print confugtion matrix
    if(!is.null(testindex)) {
        conf.matrix(survived, nnet.pred, printTable=T)
#        mdf <- misclassified(test.df.copy, nnet.pred)
    } else {
        # write out prediction
        pfile <- sprintf("%s_prediction.csv", fname)
        write.prediction(nnet.model, testdata, pfile)

        conf.matrix(survived, nnet.pred, printTable=F)
    }

    return(int.pred(nnet.pred))
}

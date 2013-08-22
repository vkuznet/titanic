#!/usr/bin/env Rscript
# clean-up session parameters
#rm(list=ls())

do.nnet <- function(tdf, testdata, fname="nnet") {
    # exclude id columne to work with ML
    train.df <- drop(tdf, c("id", "PassengerId"))
    # during training we use the same dataset, but exclude classification var
    test.df <- drop(train.df, c("Survived"))

    # run RandomForest, make sure that the variable used for classification is a
    # factor. For prediction use the same dataset but exclude classification var.
    nnet.model <- nnet(Survived~., data=train.df,
                       size = 2, rang = 0.1, decay = 5e-4, maxit = 200)
    nnet.pred <- predict(nnet.model, test.df)
    print(nnet.model)

    # write out prediction
    pfile <- sprintf("%s_prediction.csv", fname)
    write.prediction(nnet.model, testdata, pfile)

    # print confugtion matrix
    conf.matrix(train.df$Survived, nnet.pred)

}

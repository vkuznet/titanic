#!/usr/bin/env Rscript
# clean-up session parameters
#rm(list=ls())

library(MASS)

do.lda <- function(tdf, testdata, fname="lda") {
    # exclude id columne to work with ML
    train.df <- drop(df, c("id", "PassengerId"))
    # during training we use the same dataset, but exclude classification var
    test.df <- drop(train.df, c("Survived"))

    # this is an example on how to split data into train/test datasets
    #index <- 1:nrow(df)
    #testindex <- sample(index, trunc(length(index)/3))
    #testset <- df[testindex,]
    #trainset <- df[-testindex,]

    ###### kernels

    # run lda algorithm
    lda.model <- lda(Survived~., data=train.df)
    print(lda.model)

    # the last column of this dataset is what we'll predict, so we'll exclude it
    lda.pred <- predict(lda.model, test.df)

    # write out prediction
    pfile <- sprintf("%s_prediction.csv", fname)
    write.prediction(lda.model, testdata, pfile)

    # print confugtion matrix
    conf.matrix(train.df$Survived, lda.pred)
}

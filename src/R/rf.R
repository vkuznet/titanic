#!/usr/bin/env Rscript
# clean-up session parameters
#rm(list=ls())

# load libraries, helper functions, set seed.
source("src/R/helper.R")

# load data
my.path <- paste0(getwd(), "/")
file.name <- "model.csv"
df <- read.csv(paste0(my.path, file.name), header=T)

# exclude id columne to work with ML
train.df <- df[2:ncol(df)]
# during training we use the same dataset, but exclude classification var (last one)
test.df <- train.df[2:ncol(train.df)-1]

# run RandomForest, make sure that the variable used for classification is a
# factor. For prediction use the same dataset but exclude classification var.
rf <- randomForest(as.factor(Survived) ~ ., data=train.df, importance=T, proximity=T)
rf.pred <- predict(rf, test.df)

# convert rf.pred into integers
rf.pred <- sapply(rf.pred, function(x) {as.integer(as.character(x))})

# print confugtion matrix
conf.matrix(train.df$Survived, rf.pred)

# make RF plots
fig.name <- paste0("rf1", ext)
start.plot(fig.name)
par(mfrow=c(1,1))
plot(rf)
dev.off()
fig.name <- paste0("rf2", ext)
start.plot(fig.name)
par(mfrow=c(1,2))
varImpPlot(rf)
dev.off()

# print important variables used in RF
imp <- importance(rf)
print(imp)

#fig.name <- paste0("partial_rf", ext)
#start.plot(fig.name)
#impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
#op <- par(mfrow=c(2, 3))
#for (i in seq_along(impvar)) {
#    partialPlot(rf, train.df[-ncol(train.df)], impvar[i], xlab=impvar[i],
#                main=paste("Partial Dependence on", impvar[i]))
#}
#par(op)
#dev.off()


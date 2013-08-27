#!/usr/bin/env Rscript
# clean-up session parameters
rm(list=ls())

# set Age threshold to divide Adults and Children
#age.thr <- 8
#age.thr <- 10
age.thr <- 12

# load helper functions, libraries, set seeds, etc.
source("src/R/helper.R")
source("src/R/aux.R")

# read aux data
print(sprintf("Read TITANICA data"))
tit.data <- aux.data()

# reset previous graphics settings and close all plot windows
graph.reset()

# load data
my.path <- paste0(getwd(), "/data/")
file.name <- "train.csv"
train.data <- read.csv(paste0(my.path, file.name), header=T)
orig.train.df <- train.data
orig.train.df$sname <- sapply(orig.train.df$Name, assign.sname)
orig.train.df$T <- sapply(orig.train.df$Ticket, assign.ticket)
file.name <- "test.csv"
test.data <- read.csv(paste0(my.path, file.name), header=T)
orig.test.df <- test.data
orig.test.df$sname <- sapply(orig.test.df$Name, assign.sname)
orig.test.df$T <- sapply(orig.test.df$Ticket, assign.ticket)

# adjust our train/test data wrt aux.data
merged.train.df <- merge.with.aux(tit.data, orig.train.df)
merged.test.df <- merge.with.aux(tit.data, orig.test.df)

# re-assign train/test data to merged ones
train.data <- merged.train.df
test.data <- merged.test.df

# get all names form both train and test sets to form unique set
names.1 <- sapply(train.data$Name, function(x) {tolower(unlist(strsplit(as.character(x), ","))[1])})
names.2 <- sapply(test.data$Name, function(x) {tolower(unlist(strsplit(as.character(x), ","))[1])})
f.names <- unique(c(names.1, names.2))

# get all tickets from both train and test sets to form unique set
tickets.1 <- sapply(train.data$Ticket, function(x) {as.character(x)})
tickets.2 <- sapply(test.data$Ticket, function(x) {as.character(x)})
tickets <- unique(c(tickets.1, tickets.2))

# get all cabins from both train and test sets to form unique set
cabins.1 <- sapply(train.data$Cabin, function(x) {as.character(x)})
cabins.2 <- sapply(test.data$Cabin, function(x) {as.character(x)})
cabins <- unique(c(cabins.1, cabins.2))

# always set a seed
set.seed(12345)

# adjust our input data
adjust.data <- function(x, debug=F) {
    ndf <- data.frame()
#    adult.age <- mean(x$Age, na.rm=T)
#    kid.age <- age.thr-1
    adult.age <- runif(1, age.thr+1, max(x$Age, na.rm=T))
    kid.age <- runif(1, 0, age.thr-1)
    for(i in 1:nrow(x)) {
        row <- x[i,]
        row$Parch <- adjust.Parch(row)
        row$SibSp <- adjust.SibSp(row)
        row$SP <- row$SibSp+row$Parch
        row$Age <- row$Age #will be replaced in second loop
        row$Child <- 0 # placeholder
        row$W <- 0
#        row$Age <- assign.age(row, adult.age, kid.age)
#        row$Child <- assign.child(row, age.thr) # run after Parch/SibSp/Age
        row$Family <- assign.family(row)
        row$fname<- assign.fname(row)
        row$Title <- assign.title(row)
        row$tid <- assign.tid(row, tickets)
        row$Embarked <- assign.eid(row)
        row$ccat <- cabin.category(row) # run after assigned tid
        row$cid <- assign.cid(row, cabins)
        row$Fare <- assign.fare(row)
        row$Gender <- assign.gender(row)
        row$fid <- assign.fid(row, f.names)
#        row$Fbin <- assign.bin(row, "Fare", c(0, 10, 30, 100))
        row$Fbin <- assign.bin(row, "Fare", seq(0,100,10))
        if (debug == T) {
            print(row)
        }
        ndf <- rbind(ndf, row)
    }
    for(i in 1:nrow(ndf)) {
        row <- ndf[i,]
        row$Age <- assign.age(ndf, row, adult.age, kid.age)
        row$Child <- assign.child(row, age.thr) # run after Parch/SibSp/Age
        row$W <- assign.weigth(row, age.thr) # run after we assign age
        ndf[i,] <- row
    }
#    ndf$Fare <- scale(ndf$Fare)
#    ndf$Age <- scale(ndf$Age)
    ndf$fname <- as.integer(as.factor(ndf$fname))
    return(ndf)
}

# pre-process input data
preprocess <- function(df.orig, bin.attrs=NULL, survived=T) {

    # Take all numeric attributes from original df and put it into new dataframe
    df.new <- df.orig

    # adjust cabin category attribute based on ticket ids in given dataset
    df.new <- adjust.cabin(df.new)

    # adjust family attribute based on ticket ids in given dataset
    df.new <- adjust.family(df.new)

    # drop requested attributes
#    if(!is.null(drop.attrs))
#        df.new <- drop(df.new, drop.attrs)

    # break down attributes into binary form
    df.new <- to.binary(df.new, bin.attrs)

    # classifier attribute
    if (survived==T) {
        sur <- as.factor(df.new$Survived)
    } else {
        sur <- rep("?", nrow(df.orig))
    }

    # make sure that Survived is a last column
    df.new <- drop(df.new, c("Survived"))
    df.new$Survived <- sur

    # return our data frame
    return(df.new)
}

# Perform data adjustments, make some plots and write out our data for ML algorithms

print(sprintf("Fix Age attribute of train data"))
print(sprintf("Prior correction: # of missing Age: %d", nrow(subset(train.data, is.na(train.data$Age)))))
print(subset(train.data, is.na(train.data$Age))[,c("PassengerId", "Name")])
train.data <- adjust.data(train.data)
print(sprintf("After correction: # of missing Age: %d", nrow(subset(train.data, is.na(train.data$Age)))))
# make some plots for our data
make.plots(train.data)

# Fix Age attribute of test data
print(sprintf("Fix Age attribute on test data"))
print(sprintf("Prior correction: # of missing Age: %d", nrow(subset(test.data, is.na(test.data$Age)))))
print(subset(test.data, is.na(test.data$Age))[,c("PassengerId", "Name")])
test.data <- adjust.data(test.data)
print(sprintf("After correction: # of missing Age: %d", nrow(subset(test.data, is.na(test.data$Age)))))

# prepare data for ML algorithms
#drop.attrs <- c("id", "Ticket", "Name", "Sex", "Cabin","Age","Embarked", "ccat", "Fbin", "SibSp", "Parch")
#drop.attrs <- c("id", "Ticket", "Name", "Sex", "Cabin", "Fbin", "SibSp", "Parch", "Title", "tid")
#drop.attrs <- c("id", "Ticket", "Name", "Sex", "Cabin", "Fbin", "tid", "SibSp", "Parch", "Age", "Fare")
#drop.attrs <- c("id", "Ticket", "Name", "Sex", "Cabin", "SibSp", "Parch", "Fbin", "W", "sname")
#drop.attrs <- c("id", "Ticket", "Name", "Sex", "Cabin", "Fbin", "W", "sname", "Title", "SP")
#drop.attrs <- c("id", "Ticket", "Name", "Sex", "Cabin", "Fbin", "W", "sname", "SibSp", "Parch", "tid")
drop.attrs <- c("id", "Ticket", "Name", "Sex", "Cabin", "Fbin", "W", "sname", "tid", "SP", "fname", "Family")
print(sprintf("Drop attributes"))
print(drop.attrs)

# binary attributes
bin.attrs <- NULL # list of attributes to convert to binary form
#bin.attrs <- c("Title", "SibSp", "Parch", "Pclass", "Embarked", "Fbin", "SP")
#bin.attrs <- c("Title", "SP", "ccat", "Pclass", "Embarked")
print(sprintf("Binary attributes"))
print(bin.attrs)

print(sprintf("Preprocess train data"))
real.train.df <- preprocess(train.data, bin.attrs)
train.df <- real.train.df # working copy
# prepare test.data for ML, we do not write survived attribute
print(sprintf("Preprocess test data"))
real.test.df <- preprocess(test.data, bin.attrs, survived=F)
test.df <- real.test.df # working copy

# drop Survived attribute from real test dataset
real.test.df <- drop(test.df, c("Survived"))

# ready to go, print train/test data to stdout
print(sprintf("Train data"))
print(head(train.df))
print(sprintf("Test data"))
print(head(test.df))


# make correlation plots for train/test data
#make.cor.plot(train.df, "train_cor", drop.attrs)
#make.cor.plot(test.df, "test_cor", drop.attrs)

# make two subset datasets
print(sprintf("Make sdf/ndf"))
sdf <- subset(real.train.df, real.train.df$Survived==1)
ndf <- subset(real.train.df, real.train.df$Survived==0)

# write data
write.model(train.df, drop.attrs)

# Run R ML algorithms
source("src/R/ksvm.R")
source("src/R/rf.R")
source("src/R/nnet.R")
source("src/R/KMeansCluster.R")
source("src/R/caret.R")

# make a sample index
index <- 1:nrow(train.df)
testindex <- sample(index, trunc(length(index)/3))
formula <- NULL

print(sprintf("##### Benchmark KSVM #####"))
#formula <- as.formula("Survived~SP*Fare*tid+ccat*cid+Gender")
#formula <- as.formula("Survived~tid+SibSp+Parch+Age+Fare")
ksvm.fit <- do.ksvm(train.df, test.df, drop.attrs, formula=formula)
ksvm.fit <- do.ksvm(train.df, test.df, drop.attrs, formula=formula, testindex=testindex)

print(sprintf("##### Benchmark RandomForest #####"))
#formula <- as.formula("Survived~Pclass+Age*Family+Title+Gender+SP*Fare+tid+ccat*cid+Embarked")
#formula <- as.formula("Survived~Pclass+Embarked+Child+W+Family+Title+ccat+cid+Gender")
rf.fit <- do.rf(train.df, test.df, drop.attrs, formula=formula)
rf.fit <- do.rf(train.df, test.df, drop.attrs, formula=formula, testindex=testindex)

#par(mfrow=c(2,1))
#hist(sdf$W, breaks=seq(0,1,0.1))
#hist(ndf$W, breaks=seq(0,1,0.1))

#fits <- data.frame(pid=train.df[testindex,]$PassengerId,
#                   Survived=train.df[testindex,]$Survived,
#                   ksvm=ksvm.fit, rf=rf.fit)
#merge.pred(fits)

#print(sprintf("Run NNET"))
#do.nnet(df, test.df)

# Load caret stuff, here how it should be used (default alg is "rf"):
#      run.caret(train.df)
#      run.caret(train.df, "ksvm")
# uncomment loop below when you want to test multiple MLs via caret
#for(m in c("rf", "svmRadial", "svmLinear", "svmPoly")) {
#    print(sprintf("Run caret with %s", m))
#    run.caret(train.df, m)
#}

# Test clustering
#test.clustering(train.df, test.df, c(7,10,12))

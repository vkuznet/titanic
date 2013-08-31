#!/usr/bin/env Rscript
# clean-up session parameters
rm(list=ls())

# set Age threshold to divide Adults and Children
#age.thr <- 8
age.thr <- 10
#age.thr <- 12

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
write.csv(merged.train.df, "merged.train.csv")
write.csv(merged.test.df, "merged.test.csv")

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

# get all tickets from both train and test sets to form unique set
digi.tickets <- c(train.data$T, test.data$T)

# get all cabins from both train and test sets to form unique set
cabins.1 <- sapply(train.data$Cabin, function(x) {as.character(x)})
cabins.2 <- sapply(test.data$Cabin, function(x) {as.character(x)})
cabins <- unique(c(cabins.1, cabins.2))

# survival cabins
sdf <- subset(train.data, train.data$Survived==1)
sur.cabins <- unlist(sapply(unique(sdf$Cabin), function(x) {if(as.character(x)!="") return(as.character(x))}))
#print(sprintf("Survival cabins"))
#print(sort(sur.cabins))

# survival jid
sur.jids <- unique(sdf$jid)
#print(sprintf("Survival jid"))
#print(sort(sur.jids))

# survival snames
sur.fnames <- unique(sdf$fname)
#print(sprintf("Survival names"))
#print(sort(sur.fnames))

# survival tickets
sur.tickets <- unique(sdf$T)
#print(sprintf("Survival tickets"))
#print(sort(sur.tickets))

# always set a seed
set.seed(12345)

# adjust our input data
adjust.data <- function(x, debug=F) {
    ndf <- data.frame()
    adult.age <- runif(1, age.thr+1, max(x$Age, na.rm=T))
    kid.age <- runif(1, 0, age.thr-1)
    for(i in 1:nrow(x)) {
        row <- x[i,]
        row$Parch <- adjust.Parch(row)
        row$SibSp <- adjust.SibSp(row)
        row$SP <- row$SibSp+row$Parch
        row$Age <- row$Age #will be replaced in second loop
        row$Child <- 0 # placeholder
        row$W <- 0 # placeholder
        row$SW <- 0 # placeholder
        row$Family <- assign.family(row)
        row$fname<- assign.fname(row)
        row$Title <- assign.title(row)
        row$tid <- assign.tid(row, tickets)
        row$Embarked <- assign.eid(row)
        row$ccat <- cabin.category(row) # run after assigned tid
        row$cid <- assign.cid(row, cabins)
        row$jid.full <- row$jid
        row$scid <- assign.scid(row, sur.cabins)
        row$Fare <- assign.fare(row)
        row$Gender <- assign.gender(row)
        row$fid <- assign.fid(row, f.names)
        row$Fbin <- assign.bin(row, "Fare", c(0, 10, 30, 100))
        row$sn <- assign.sfname(row, sur.fnames)
        row$st <- assign.sticket(row, sur.tickets)
        if (debug == T) {
            print(row)
        }
        ndf <- rbind(ndf, row)
    }
    for(i in 1:nrow(ndf)) {
        row <- ndf[i,]
        adult.age <- runif(1, age.thr+1, max(ndf$Age, na.rm=T))
        kid.age <- runif(1, 0, age.thr-1)
        row$Age <- assign.age(ndf, row, adult.age, kid.age)
        row$Child <- assign.child(row, age.thr) # run after Parch/SibSp/Age
        row$W <- assign.weigth(row, age.thr) # run after we assign age
        row$SW <- assign.SW(row, digi.tickets)
        ndf[i,] <- row
    }
#    ndf$Fare <- scale(ndf$Fare)
#    ndf$Age <- scale(ndf$Age)
    ndf$fname <- as.integer(as.factor(ndf$fname))
    ndf$jid <- sapply(ndf$jid, function(x) {if(length(which(sur.jids==x))>0) return(1) else return(0)})
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
drop.attrs <- c("id", "Ticket", "Name", "Sex", "Cabin", "Fbin", "W", "sname", "tid", "SP", "fname", "Family")
keep0 <- c("PassengerId", "Survived", "Pclass", "Age", "SibSp", "Parch", "Fare",
            "Embarked", "T", "jid", "Class", "Servant.1", "Servant.2", "Child", "SW",
            "Title", "ccat", "cid", "fid")
keep0 <- c("PassengerId", "Survived", "Pclass", "Age", "SibSp", "Parch", "Fare", "Embarked", "T", "jid", "Class", "Servant.1", "Servant.2", "Title", "scid")
keep0 <- c("PassengerId", "Survived", "Pclass", "Age", "SP", "Fare", "Embarked", "jid", "Class", "Title", "scid")
keep0 <- c("PassengerId", "Survived", "Age", "Title", "Gender", "Pclass", "T", "SP", "Fare", "st")
print(sprintf("Keep attributes"))
print(keep0)

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
source("src/R/svm.R")
source("src/R/ksvm.R")
source("src/R/rf.R")
source("src/R/nnet.R")
source("src/R/KMeansCluster.R")
source("src/R/caret.R")

# make a sample index
index <- 1:nrow(train.df)
testindex <- sample(index, trunc(length(index)/3))
formula <- NULL

# split data into first class and the rest
dd1 <- subset(train.df, train.df$Pclass==1)
ddt1 <- subset(test.df, test.df$Pclass==1)
index1 <- 1:nrow(dd1)
testindex1 <- sample(index1, trunc(length(index1)/3))

dd2 <- subset(train.df, train.df$Pclass==2)
ddt2 <- subset(test.df, test.df$Pclass==2)
index2 <- 2:nrow(dd2)
testindex2 <- sample(index2, trunc(length(index2)/3))

dd3 <- subset(train.df, train.df$Pclass==3)
ddt3 <- subset(test.df, test.df$Pclass==3)
index3 <- 3:nrow(dd3)
testindex3 <- sample(index3, trunc(length(index3)/3))

keep1 <- c("PassengerId", "Survived", "Age", "Title", "Gender", "scid", "sn")
keep1 <- c("PassengerId", "Survived", "Title", "Gender", "scid", "sn")
keep2 <- c("PassengerId", "Survived", "Age", "Title", "Gender", "scid", "jid", "sn")
keep3 <- c("PassengerId", "Survived", "Age", "Title", "Gender", "SP", "Embarked", "Fare", "scid", "T", "jid", "sn")

#drop.common <- c("id", "Ticket", "Name", "Sex", "Cabin", "Fbin", "W", "sname")
#drops1 <- c(drop.common, c("tid", "fname", "Parch", "SibSp", "fid", "Family", "Pclass", "Servant.2", "Servant.1", "Embarked", "T", "Class", "SW", "ccat", "Fare", "ccat", "SP", "Class", "jid", "Fare", "Child", "cid"))
#drops2 <- c(drop.common, c("tid", "fname", "Parch", "SibSp", "fid", "Family", "Pclass", "Servant.1", "Embarked"))

print(sprintf("##### Benchmark KSVM #####"))
print(keep0)
#formula <- as.formula("Survived~SP*Fare*tid+ccat*cid+Gender")
#formula <- as.formula("Survived~tid+SibSp+Parch+Age+Fare")
ksvm.fit <- do.ksvm(train.df, test.df, keep0, formula=formula)
ksvm.fit <- do.ksvm(train.df, test.df, keep0, formula=formula, testindex=testindex, printModel=T)

print(sprintf("##### Benchmark RandomForest #####"))
print(keep0)
#formula <- as.formula("Survived~Pclass+Age*Family+Title+Gender+SP*Fare+tid+ccat*cid+Embarked")
#formula <- as.formula("Survived~Pclass+Embarked+Child+W+Family+Title+ccat+cid+Gender")
rf.fit <- do.rf(train.df, test.df, keep0, formula=formula)
rf.fit <- do.rf(train.df, test.df, keep0, formula=formula, testindex=testindex, printModel=T)

#print(sprintf("##### Benchmark RandomForest, dd1 #####"))
#print(keep1)
#rf.fit1 <- do.rf(dd1, ddt1, keep1, formula=formula, fname="rf1")
#rf.fit1 <- do.rf(dd1, ddt1, keep1, formula=formula, fname="rf1", testindex=testindex1, printModel=T)
#print(sprintf("##### Benchmark RandomForest, dd2 #####"))
#print(keep2)
#rf.fit2 <- do.rf(dd2, ddt2, keep2, formula=formula, fname="rf2")
#rf.fit2 <- do.rf(dd2, ddt2, keep2, formula=formula, fname="rf2", testindex=testindex2, printModel=T)
#print(sprintf("##### Benchmark RandomForest, dd3 #####"))
#print(keep3)
#rf.fit3 <- do.rf(dd3, ddt3, keep3, formula=formula, fname="rf3")
#rf.fit3 <- do.rf(dd3, ddt3, keep3, formula=formula, fname="rf3", testindex=testindex3, printModel=T)
#cmd <- "cat rf[1-3]_prediction.csv | sort -u -n > rf_merged.csv"
#system(cmd)
#cmd <- "cat rf[1-3]_prediction_test.csv | sort -u -n > rf_merged_test.csv"
#system(cmd)

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

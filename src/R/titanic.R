#!/usr/bin/env Rscript
# clean-up session parameters
rm(list=ls())

# set Age threshold to divide Adults and Children
age.thr <- 10
#age.thr <- 18

# load helper functions, libraries, set seeds, etc.
source("src/R/helper.R")

# reset previous graphics settings and close all plot windows
graph.reset()

# load data
my.path <- paste0(getwd(), "/data/")
file.name <- "train.csv"
train.data <- read.csv(paste0(my.path, file.name), header=T)
file.name <- "test.csv"
test.data <- read.csv(paste0(my.path, file.name), header=T)

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
    adult.age <- mean(x$Age, na.rm=T)
    kid.age <- age.thr-1
    for(i in 1:nrow(x)) {
        row <- x[i,]
        row$Parch <- adjust.Parch(row)
        row$SibSp <- adjust.SibSp(row)
        row$Age <- assign.age(row, adult.age, kid.age)
        row$W <- assign.weigth(row, age.thr) # run after we assign age
        row$Title <- assign.title(row)
        row$CabinCat <- cabin.category(row$Cabin)
        row$CabinId <- assign.cid(row$Cabin, cabins)
        row$TicketId <- assign.tid(row$Ticket, tickets)
        row$Fbin <- assign.fbin(row, step=10, nbins=10)
        if (debug == T) {
            print(row)
        }
        ndf <- rbind(ndf, row)
    }
    return(ndf)
}

# pre-process input data
preprocess <- function(df.orig, survived=T) {

    # Take all numeric attributes from original df and put it into new dataframe
    df.new <- data.frame(PassengerId=df.orig$PassengerId)

    # W attribute
    df.new$W <- df.orig$W

    # Title attribute
    df.new$Title <- df.orig$Title

    # SibSp attribute
    df.new$SibSp <- df.orig$SibSp

    # Parch attribute
    df.new$Parch <- df.orig$Parch

    # Cabin attribute
#    df.new$CabinCat <- df.orig$CabinCat
    df.new$CabinId <- df.orig$CabinId

    # Ticket attribute
    df.new$TicketId <- df.orig$TicketId
#    df.new$Ticket <- df.orig$Ticket

    # Age attribute
    df.new$Age <- df.orig$Age
#    df.new$Age <- sapply(df.orig$Age, function(x) {
#        if(x<=20) return(0)
#        else if(x>20&x<=40) return(1)
#        else if(x>40&x<=60) return(2)
#        else return(3)
#    })
    # binary age categories
#    df.new$Child <- sapply(df.orig$Age, function(x) {if(!is.na(x) & x<age.thr) return(1) else return(0)})
#    df.new$Adult <- sapply(df.orig$Age, function(x) {if(!is.na(x) & x>=age.thr) return(1) else return(0)})
#    df.new$Age.NA <- sapply(df.orig$Age, function(x) {if(is.na(x)) return(1) else return(0)})

    # Class attribute
    df.new$Pclass <- df.orig$Pclass

    # Gender attribute, convert Gender Male to 1, Female to 0
    df.new$Gender <- sapply(df.orig$Sex, function(x) {if(x=="male") return(1) else return(0)})

    # Fare attribute, put Fare in bins
    fare <- sapply(df.orig$Fare, function(x) {if(is.na(x)) return(50) else return(x)})
    df.new$Fare <- scale(fare) # or use scale(fare)
#    df.new$Fbin <- df.orig$Fbin

    # Embarked attribute
    embarked <- sapply(df.orig$Embarked,
        function(x) {
            if(as.character(x)=="C") return(1)
            else if(as.character(x)=="S") return(2)
            else if(as.character(x)=="Q") return(3)
            else return(3) # missing value
        })
    df.new$Embarked <- unlist(embarked)

    # add classifier as last column
    if (survived==T) {
        df.new$Survived <- as.factor(df.orig$Survived)
    } else {
        df.new$Survived <- rep("?", nrow(df.orig))
    }

    # break down attributes into binary form
#    attrs <- c("Title", "SibSp", "Parch", "Pclass", "Embarked", "Fbin")
#    for(a in attrs) {
#        df.new <- break.down(df.new, a)
#    }

    # make sure that Survived is a last column
    sur <- df.new$Survived
    df.new <- drop(df.new, c("Survived"))
    df.new$Survived <- sur

    # return our data frame
    return(df.new)
}

# Perform data adjustments, make some plots and write out our data for ML algorithms

print(sprintf("Fix Age attribute of train data"))
print(sprintf("Prior correction: # of missing Age: %d", nrow(subset(train.data, is.na(train.data$Age)))))
train.data <- adjust.data(train.data)
train.data <- assign.cabin(train.data)
print(sprintf("After correction: # of missing Age: %d", nrow(subset(train.data, is.na(train.data$Age)))))
# make some plots for our data
make.plots(train.data)

# Fix Age attribute of test data
print(sprintf("Fix Age attribute on test data"))
print(sprintf("Prior correction: # of missing Age: %d", nrow(subset(test.data, is.na(test.data$Age)))))
test.data <- adjust.data(test.data)
test.data <- assign.cabin(test.data)
print(sprintf("After correction: # of missing Age: %d", nrow(subset(test.data, is.na(test.data$Age)))))

# prepare data for ML algorithms
print(sprintf("Preprocess train data"))
real.train.df <- preprocess(train.data)
train.df <- real.train.df # working copy
# prepare test.data for ML, we do not write survived attribute
print(sprintf("Preprocess test data"))
real.test.df <- preprocess(test.data, survived=F)
test.df <- real.test.df # working copy

# drop Survived attribute from real test dataset
real.test.df <- drop(test.df, c("Survived"))

# drop some attribute before writing
train.df <- drop(train.df, c("id", "Ticket"))
test.df <- drop(test.df, c("id", "Ticket"))

# make correlation plots for train/test data
make.cor.plot(train.df, "train_cor")
make.cor.plot(test.df, "test_cor")

# make two subset datasets
print(sprintf("Make sdf/ndf"))
sdf <- subset(real.train.df, real.train.df$Survived==1)
ndf <- subset(real.train.df, real.train.df$Survived==0)

# write data out for SVM
print(sprintf("Write model.csv"))
write.csv(train.df, file="model.csv")
cmd="cat model.csv | sed 's/\"\"/\"id\"/g' > t.csv; mv -f t.csv model.csv"
system(cmd)

# write data in arff format for Weka
print(sprintf("Write model.arff"))
write.arff(train.df, file="model.arff")
cmd="cat model.arff  | sed \"s/Survived numeric/Survived {0,1}/g\" > t.arff; mv -f t.arff model.arff"
system(cmd)
# write test data in arff format
write.arff(test.df, file="test.arff")
cmd="cat test.arff  | sed -e \"s/Survived string/Survived {0,1}/g\" -e \"s/'//g\" > t.arff; mv -f t.arff test.arff"
system(cmd)

# Run R ML algorithms
source("src/R/ksvm.R")
source("src/R/rf.R")
source("src/R/nnet.R")
source("src/R/KMeansCluster.R")
print(sprintf("Run KSVM"))
do.ksvm(train.df, real.test.df)
print(sprintf("Run RandomForest"))
do.rf(train.df, real.test.df)
#print(sprintf("Run NNET"))
#do.nnet(df, real.test.df)

# Load caret stuff, here how it should be used (default alg is "rf"):
#      run.caret(train.df)
#      run.caret(train.df, "ksvm")
source("src/R/caret.R")
# uncomment loop below when you want to test multiple MLs via caret
#for(m in c("rf", "svmRadial", "svmLinear", "svmPoly")) {
#    print(sprintf("Run caret with %s", m))
#    run.caret(train.df, m)
#}

# re-run ML with additional cluster info
print(sprintf("Run Clustering"))
#clusters <- do.clustering(df, 6:10)
#nclusters <- c(3,4,5,6,7,8,9,10)
nclusters <- c(7)
train.dd <- train.df
test.dd <- real.test.df
train.dd <- drop(train.dd, c("id", "Ticket"))
test.dd <- drop(test.dd, c("id", "Ticket"))
for(nc in nclusters) {
    fname <- sprintf("rf%i", nc)
    train.dd <- add.cluster(train.dd, nc, TRUE)
    test.dd <- add.cluster(test.dd, nc, FALSE)
    print(sprintf("Run RandomForest with cluster %i", nc))
    do.rf(train.dd, test.dd, fname)
}

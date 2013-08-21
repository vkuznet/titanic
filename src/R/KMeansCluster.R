#!/usr/bin/env Rscript
# clean-up session parameters
#rm(list=ls())
# load libraries, helper functions, set seed.
source("src/R/helper.R")
require(graphics)
require(cluster)
require(fpc)


RunClusters<-function(x,NumCluster=2){
  #Run cluster
  Cluster<-kmeans(x,NumCluster)
  #print Results
  print (Cluster)
  #Plot Clusters 
  # Make Plot Clusters
  fig.name <- paste0("plots/KMeansClusterOf",NumCluster,"nodesType1", ext)
  start.plot(fig.name)
  par(mfrow=c(1,1))
  plot(x,col=Cluster$cluster)
  #plot Centers
  points(Cluster$centers,col=1:2,pch=8)
  dev.off()
  #makePlot 2 
  fig.name <- paste0("plots/KMeansClusterOf",NumCluster,"nodesType2", ext)
  start.plot(fig.name)
  par(mfrow=c(1,1))
  plotcluster(x,Cluster$cluster)
  dev.off()
  #makeplot 3
  fig.name <- paste0("plots/KMeansClusterOf",NumCluster,"nodesType3", ext)
  start.plot(fig.name)
  par(mfrow=c(1,1))
  clusplot(train.df,Cluster$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)
  dev.off()
  return(Cluster)
}

# load data
my.path <- paste0(getwd(), "/")
file.name <- "model.csv"
df <- read.csv(paste0(my.path, file.name), header=T)
#set the number of cluster to run 
nc=2

# exclude id columne to work with ML
train.df <- drop(df, c("id", "PassengerId"))
# during training we use the same dataset, but exclude classification var
test.df <- drop(df, c("id", "PassengerId", "Survived"))

for(i in 2:10){
  RunClusters(train.df,i)
}

#!/usr/bin/env Rscript
# clean-up session parameters
#rm(list=ls())

# load libraries, helper functions, set seed.
source("src/R/helper.R")
require(graphics)
require(cluster)
require(fpc)

# helper function to create clusters
RunClusters<-function(x,NumCluster=2,printCluster=F){
  print(sprintf("Run Clustering with nc=%i", NumCluster))
  system("mkdir -p plots")
  system("rm plots/*.png")
  #Run cluster
  Cluster<-kmeans(x,NumCluster)
  #print Results
  if (printCluster) {
    print (Cluster)
  }
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
  clusplot(x,Cluster$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)
  dev.off()
  return(Cluster)
}

do.clustering <- function(tdf, clusters=seq(2:10)) {
    # exclude id columne to work with ML
    train.df <- drop(tdf, c("id", "PassengerId", "Survived"))
    # create new data frame and add clusters to it
    df <- data.frame(id=seq(1:nrow(tdf)))

    for(i in 6:10){
      cls <- RunClusters(train.df,i)
      df <- cbind(df, cls$cluster)
      df.names <- names(df)
      df.names[length(df.names)] <- sprintf("cls.%d", i)
      names(df) <- df.names
    }
    return(df)
}

# helper function to add cluster with given nc to given dataframe
add.cluster <- function(x, nc, survived) {
    if (survived) {
        sur <- x$Survived
        d <- drop(x, c("Survived"))
    } else {
        d <- x
    }
    cls <- kmeans(x, nc)
    d$cls <- cls$cluster
    if (survived) {
        d$Survived <- sur
    }
    return (d)
}

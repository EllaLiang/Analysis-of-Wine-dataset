# ------------------------------------ #
#             Question 1               #
# ------------------------------------ #
# ---------- Data Splitting ---------- #
setwd('/Users/ella/Downloads/DataMining')
library(readxl)
wine<-read_excel('wine_data.xlsx')
"%w/o%" <- function(x,y) x[!x %in% y]
get.train <- function (data.sz, train.sz) {
  # Take subsets of data for training/test samples
  # Return the indices
  train.ind <- sample(data.sz, train.sz)
  test.ind <- (1:data.sz) %w/o% train.ind
  list(train=train.ind, test=test.ind)
}
Train.sz<-119
(tt.ind <- get.train(dim(wine)[1], Train.sz))
wine_train<-wine[tt.ind$train,1:14]
wine_test<-wine[tt.ind$test,1:14]
Class1<-c((sum(wine$Cultivar==1))/178,(sum(wine_train$Cultivar==1))/119,(sum(wine_test$Cultivar==1))/59) 
Class2<-c((sum(wine$Cultivar==2))/178,(sum(wine_train$Cultivar==2))/119,(sum(wine_test$Cultivar==2))/59) 
Class3<-c((sum(wine$Cultivar==3))/178,(sum(wine_train$Cultivar==3))/119,(sum(wine_test$Cultivar==3))/59)
Class<- data.frame(Class1,Class2,Class3)
Class<-Class*100
barplot(as.matrix(Class),beside=TRUE,col=c("purple","brown","yellow"),main="Distribution of Classes in Full Data, Training Data and Test Data",ylab="Samples' Proportion  of Three Given Class")
legend(8.5,40,c("Original full Data ","Training Set","Testing Set "),bty="n",fill=c("purple","brown","yellow"))
# run the above several times until we get the best split, then save the files
write.csv(wine_train,"wine_train.csv",row.names=FALSE)
write.csv(wine_test,"wine_test.csv",row.names=FALSE)

# --------------------------------------------------- #
#             Question 2 and Question 4               #
# --------------------------------------------------- #
# ---------------- Kmeans clustering ---------------- #
# For Question 4, we need to change the distance function in the code.
# Besides, the code used to construct the plot for training and test set is also different.
# ----- Kmeans standardized data ----- #
results<-matrix(nrow=50,ncol=3)
wine<-read_excel('wine_data.xlsx')
wine<-wine[sample(nrow(wine)),]
wine_norm<-scale(wine)
wine_norm<-as.data.frame(wine_norm)
wine_norm$Cultivar<-wine$Cultivar 
for (i in 1:50) {
  set.seed(i)
  # select 3 initial centroids randomly
  k1<-wine_norm[sample(length(wine_norm[,1]),1),] 
  k2<-wine_norm[sample(length(wine_norm[,1]),1),] 
  k3<-wine_norm[sample(length(wine_norm[,1]),1),]
  k1[2:14]<-sapply(k1[2:14], function(x) rnorm(1,0.0,1.0))
  k2[2:14]<-sapply(k2[2:14], function(x) rnorm(1,0.0,1.0))
  k3[2:14]<-sapply(k3[2:14], function(x) rnorm(1,0.0,1.0))
  # calculate the Euclidean distance
  k1.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k1[2:14])^2))) 
  k2.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k2[2:14])^2))) 
  k3.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k3[2:14])^2)))
  distances<-cbind(k1.euc.dist,k2.euc.dist,k3.euc.dist)
  # calculate the Manhattan distance
  #k1.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k1[2:14]))) 
  #k2.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k2[2:14]))) 
  #k3.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k3[2:14])))
  #distances<-cbind(k1.manh.dist,k2.manh.dist,k3.manh.dist)
  
  # then compare the distances, adding a new column "cluster" to wine_norm
  wine_norm$cluster<-apply(distances,1,function(x) which(x==min(x)))
  k1old<-k1
  k2old<-k2
  k3old<-k3
  # 3 clusters we got
  cluster1<-subset(wine_norm,wine_norm$cluster==1)
  cluster2<-subset(wine_norm,wine_norm$cluster==2)
  cluster3<-subset(wine_norm,wine_norm$cluster==3)
  # compute new centroids for each cluster
  k1[2:14]<-apply(cluster1[2:14],2,function(col) mean(col))
  k2[2:14]<-apply(cluster2[2:14],2,function(col) mean(col))
  k3[2:14]<-apply(cluster3[2:14],2,function(col) mean(col))
  
  # if we have empty clusters, we need to make its centroid the point with the highest SSE
  # so we have to find the SSE of each sample first
  cluster1$sse<-apply(cluster1[,2:14],1, function(x) sum( (x-k1[2:14])^2 )) 
  cluster2$sse<-apply(cluster2[,2:14],1, function(x) sum( (x-k2[2:14])^2 )) 
  cluster3$sse<-apply(cluster3[,2:14],1, function(x) sum( (x-k3[2:14])^2 )) 
  sse<-rbind(cluster1,cluster2,cluster3)
  sse<-sse[order(sse$sse),]
  # when we have a null centroid, make it the point with the highest SSE
  # then remove null centroids from our sse list so we won't reuse it
  if(any(is.na(k1))) {
    # make it the point with the highest SSE #
    k1<-sse[(length(sse[,1])),] 
    k1<-k1[,-16]
    # remove it #
    sse<-sse[-length(sse[,1]),] 
  }
  if(any(is.na(k2))) {
    k2<-sse[(length(sse[,1])),]
    k2<-k2[,-16]
    sse<-sse[-length(sse[,1]),] 
  }
  if(any(is.na(k3))) {
    k3<-sse[(length(sse[,1])),] 
    k3<-k3[,-16]
    sse<-sse[-length(sse[,1]),] 
  }
  final.missclassified.samples<-11111 
  
  # now we loop until the previous centroids do not change
  while(!(sum(k1^2)==sum(k1old^2) && sum(k2^2)==sum(k2old^2) && sum(k3^2)==sum(k3old^2))){
    # new Euclidean distances 
    k1.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k1[2:14])^2)))
    k2.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k2[2:14])^2)))
    k3.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k3[2:14])^2)))
    distances<-cbind(k1.euc.dist,k2.euc.dist,k3.euc.dist)
    # new Manhattan distances 
    #k1.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k1[2:14]))) 
    #k2.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k2[2:14]))) 
    #k3.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k3[2:14])))
    #distances<-cbind(k1.manh.dist,k2.manh.dist,k3.manh.dist)
    
    # assign points to a cluster
    wine_norm$cluster<-apply(distances,1, function(x) which(x == min(x)))
    # make new clusters
    k1old<-k1
    k2old<-k2
    k3old<-k3
    # 3 new clusters we got
    cluster1<-subset(wine_norm,wine_norm$cluster==1)
    cluster2<-subset(wine_norm,wine_norm$cluster==2)
    cluster3<-subset(wine_norm,wine_norm$cluster==3)
    # compute new centroids for each cluster
    k1[2:14]<-apply(cluster1[2:14],2,function(col) mean(col))
    k2[2:14]<-apply(cluster2[2:14],2,function(col) mean(col))
    k3[2:14]<-apply(cluster3[2:14],2,function(col) mean(col))
    # find every sample's SSE
    cluster1$sse<-apply(cluster1[,2:14],1, function(x) sum( (x-k1[2:14])^2 )) 
    cluster2$sse<-apply(cluster2[,2:14],1, function(x) sum( (x-k2[2:14])^2 )) 
    cluster3$sse<-apply(cluster3[,2:14],1, function(x) sum( (x-k3[2:14])^2 )) 
    sse<-rbind(cluster1,cluster2,cluster3)
    sse<-sse[order(sse$sse),]
    # if a cluster is empty, make that centroid the point with the greatest SSE 
    # then remove null centroids from our sse list so we won't reuse it
    if(any(is.na(k1))) {
      k1<-sse[(length(sse[,1])),] 
      k1<-k1[,-16]
      sse<-sse[-length(sse[,1]),] 
    }
    if(any(is.na(k2))) {
      k2<-sse[(length(sse[,1])),] 
      k2<-k2[,-16]
      sse<-sse[-length(sse[,1]),]
    }
    if(any(is.na(k3))) {
      k3<-sse[(length(sse[,1])),] 
      k3<-k3[,-16]
      sse<-sse[-length(sse[,1]),] 
    }
    # we can use the total number of misclasified examples to measure the quality of our clustering
    subset1<-subset(wine_norm, wine_norm$cluster == 1)
    subset2<-subset(wine_norm, wine_norm$cluster == 2)
    subset3<-subset(wine_norm, wine_norm$cluster == 3)
    # we take the mean of each cluster's class and then rounding this, 
    # comparing each sample's class with it,
    # then we count up the number of misclassified samples
    misclassified.samples<-0
    misclassified.samples<-misclassified.samples + length(which(subset1$Cultivar != round(mean(subset1$Cultivar)))) 
    misclassified.samples<-misclassified.samples + length(which(subset2$Cultivar != round(mean(subset2$Cultivar)))) 
    misclassified.samples<-misclassified.samples + length(which(subset3$Cultivar != round(mean(subset3$Cultivar)))) 
    final.misclassified.samples<-misclassified.samples 
  }
  # choose the seed which gave us the best clustering results, then run the code "print(k1)", "print(k2)", "print(k3)",we will get centroids of clusters
  #print(k1)
  #print(k2)
  #print(k3)
  # we can also use SSE to measure the quality of our clustering, so we print the Final SSE
  print(c("Final SSE = ",sum(sse$sse)))
  print(c("Final Misclassified Samples = ", final.misclassified.samples))
  #store the results, so we can use that table to find a good seed later
  results[i,1]<-i
  results[i,2]<-sum(sse$sse)
  results[i,3]<-final.misclassified.samples 
}

#----- Kmeans raw data -----#
results<-matrix(nrow=50,ncol=3)
wine<-read_excel('wine_data.xlsx')
wine<-wine[sample(nrow(wine)),]
# raw data #
wine_norm<-wine
wine_norm<-as.data.frame(wine_norm)
wine_norm$Cultivar<-wine$Cultivar 
for (i in 1:50) {
  set.seed(i)
  # select 3 initial centroids randomly
  k1<-wine_norm[sample(length(wine_norm[,1]),1),] 
  k2<-wine_norm[sample(length(wine_norm[,1]),1),] 
  k3<-wine_norm[sample(length(wine_norm[,1]),1),]
  k1[2:14]<-sapply(k1[2:14], function(x) runif(1,0.0,1.0))
  k2[2:14]<-sapply(k2[2:14], function(x) runif(1,0.0,1.0))
  k3[2:14]<-sapply(k3[2:14], function(x) runif(1,0.0,1.0))
  # calculate the Euclidean distance
  k1.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k1[2:14])^2))) 
  k2.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k2[2:14])^2))) 
  k3.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k3[2:14])^2)))
  distances<-cbind(k1.euc.dist,k2.euc.dist,k3.euc.dist)
  # calculate the Manhattan distances 
  #k1.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k1[2:14]))) 
  #k2.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k2[2:14]))) 
  #k3.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k3[2:14])))
  #distances<-cbind(k1.manh.dist,k2.manh.dist,k3.manh.dist)
  
  # then compare the distances, adding a new column "cluster" to wine_norm
  wine_norm$cluster<-apply(distances,1,function(x) which(x==min(x)))
  k1old<-k1
  k2old<-k2
  k3old<-k3
  # 3 clusters we got
  cluster1<-subset(wine_norm,wine_norm$cluster==1)
  cluster2<-subset(wine_norm,wine_norm$cluster==2)
  cluster3<-subset(wine_norm,wine_norm$cluster==3)
  # compute new centroids for each cluster
  k1[2:14]<-apply(cluster1[2:14],2,function(col) mean(col))
  k2[2:14]<-apply(cluster2[2:14],2,function(col) mean(col))
  k3[2:14]<-apply(cluster3[2:14],2,function(col) mean(col))
  
  # if we have empty clusters, we need to make its centroid the point with the highest SSE
  cluster1$sse<-apply(cluster1[,2:14],1, function(x) sum( (x-k1[2:14])^2 )) 
  cluster2$sse<-apply(cluster2[,2:14],1, function(x) sum( (x-k2[2:14])^2 )) 
  cluster3$sse<-apply(cluster3[,2:14],1, function(x) sum( (x-k3[2:14])^2 )) 
  sse<-rbind(cluster1,cluster2,cluster3)
  sse<-sse[order(sse$sse),]
  if(any(is.na(k1))) {
    k1<-sse[(length(sse[,1])),] 
    k1<-k1[,-16]
    sse<-sse[-length(sse[,1]),] 
  }
  if(any(is.na(k2))) {
    k2<-sse[(length(sse[,1])),]
    k2<-k2[,-16]
    sse<-sse[-length(sse[,1]),] 
  }
  if(any(is.na(k3))) {
    k3<-sse[(length(sse[,1])),] 
    k3<-k3[,-16]
    sse<-sse[-length(sse[,1]),] 
  }
  final.missclassified.samples<-11111 
  
  # now we loop until the previous centroids do not change
  while(!(sum(k1^2)==sum(k1old^2) && sum(k2^2)==sum(k2old^2) && sum(k3^2)==sum(k3old^2))){
    # new Euclidean distances 
    k1.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k1[2:14])^2)))
    k2.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k2[2:14])^2)))
    k3.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k3[2:14])^2)))
    distances<-cbind(k1.euc.dist,k2.euc.dist,k3.euc.dist)
    # new Manhattan distances 
    #k1.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k1[2:14]))) 
    #k2.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k2[2:14]))) 
    #k3.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k3[2:14])))
    #distances<-cbind(k1.manh.dist,k2.manh.dist,k3.manh.dist)
    
    # assign points to a cluster
    wine_norm$cluster<-apply(distances,1, function(x) which(x == min(x)))
    # make new clusters
    k1old<-k1
    k2old<-k2
    k3old<-k3
    # 3 new clusters we got
    cluster1<-subset(wine_norm,wine_norm$cluster==1)
    cluster2<-subset(wine_norm,wine_norm$cluster==2)
    cluster3<-subset(wine_norm,wine_norm$cluster==3)
    # compute new centroids for each cluster
    k1[2:14]<-apply(cluster1[2:14],2,function(col) mean(col))
    k2[2:14]<-apply(cluster2[2:14],2,function(col) mean(col))
    k3[2:14]<-apply(cluster3[2:14],2,function(col) mean(col))
    # find every sample's SSE
    cluster1$sse<-apply(cluster1[,2:14],1, function(x) sum( (x-k1[2:14])^2 )) 
    cluster2$sse<-apply(cluster2[,2:14],1, function(x) sum( (x-k2[2:14])^2 )) 
    cluster3$sse<-apply(cluster3[,2:14],1, function(x) sum( (x-k3[2:14])^2 )) 
    sse<-rbind(cluster1,cluster2,cluster3)
    sse<-sse[order(sse$sse),]
    # if a cluster is empty, make that centroid the point with the greatest SSE 
    # then remove null centroids from our sse list so we won't reuse it
    if(any(is.na(k1))) {
      k1<-sse[(length(sse[,1])),] 
      k1<-k1[,-16]
      sse<-sse[-length(sse[,1]),] 
    }
    if(any(is.na(k2))) {
      k2<-sse[(length(sse[,1])),] 
      k2<-k2[,-16]
      sse<-sse[-length(sse[,1]),]
    }
    if(any(is.na(k3))) {
      k3<-sse[(length(sse[,1])),] 
      k3<-k3[,-16]
      sse<-sse[-length(sse[,1]),] 
    }
    # we can use the total number of misclasified examples to measure the quality of our clustering
    subset1<-subset(wine_norm, wine_norm$cluster == 1)
    subset2<-subset(wine_norm, wine_norm$cluster == 2)
    subset3<-subset(wine_norm, wine_norm$cluster == 3)
    # we take the mean of each cluster's class and then rounding this, 
    # comparing each sample's class with it,
    # then we count up the number of misclassified samples
    misclassified.samples<-0
    misclassified.samples<-misclassified.samples + length(which(subset1$Cultivar != round(mean(subset1$Cultivar)))) 
    misclassified.samples<-misclassified.samples + length(which(subset2$Cultivar != round(mean(subset2$Cultivar)))) 
    misclassified.samples<-misclassified.samples + length(which(subset3$Cultivar != round(mean(subset3$Cultivar)))) 
    final.misclassified.samples<-misclassified.samples 
  }
  # run the code "print(k1)", "print(k2)", "print(k3)" we will get centroids of clusters
  #print(k1)
  #print(k2)
  #print(k3)
  # we can also use SSE to measure the quality of our clustering, so we print the Final SSE
  print(c("Final SSE = ",sum(sse$sse)))
  print(c("Final Misclassified Samples = ", final.misclassified.samples))
  #store the results, so we can use that table to find a good seed later
  results[i,1]<-i
  results[i,2]<-sum(sse$sse)
  results[i,3]<-final.misclassified.samples 
}

#----- Kmeans whitened data -----#
results<-matrix(nrow=50,ncol=3)
wine<-read_excel('wine_data.xlsx')
wine<-wine[sample(nrow(wine)),]
# use the function below to whiten the data #
Sphere.data<- function(data) {
  data <- as.matrix(data)
  data <- t(t(data) - apply(data, 2, mean))
  data.svd <- svd(var(data))
  sphere.mat <- t(data.svd$v %*% (t(data.svd$u) * (1/sqrt(data.svd$d))))
  return(data %*% sphere.mat)
}
wine_norm<-Sphere.data(wine)
colnames(wine_norm)<-c("Cultivar","Alcohol","Malicacid","Ash","Alcalinity.of.ash","Magnesium","Total.phenols",
                       "Flavanoid","Nonflavanoid.phenols","Proanthocyanins","Color.intensity","Hue",
                       "OD280.OD315.of.diluted.wines","Proline")
wine_norm<-apply(wine_norm,2,function (x) (x-min(x)) / (max(x) - min(x) ) )
wine_norm<-as.data.frame(wine_norm)
wine_norm$Cultivar<-wine$Cultivar 
for (i in 1:50) {
  set.seed(i)
  # select 3 initial centroids randomly
  k1<-wine_norm[sample(length(wine_norm[,1]),1),] 
  k2<-wine_norm[sample(length(wine_norm[,1]),1),] 
  k3<-wine_norm[sample(length(wine_norm[,1]),1),]
  k1[2:14]<-sapply(k1[2:14], function(x) runif(1,0.0,1.0))
  k2[2:14]<-sapply(k2[2:14], function(x) runif(1,0.0,1.0))
  k3[2:14]<-sapply(k3[2:14], function(x) runif(1,0.0,1.0))
  # calculate the Euclidean distance
  k1.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k1[2:14])^2))) 
  k2.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k2[2:14])^2))) 
  k3.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k3[2:14])^2)))
  distances<-cbind(k1.euc.dist,k2.euc.dist,k3.euc.dist)
  # calculate the Manhattan distances 
  #k1.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k1[2:14]))) 
  #k2.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k2[2:14]))) 
  #k3.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k3[2:14])))
  #distances<-cbind(k1.manh.dist,k2.manh.dist,k3.manh.dist)
  
  # then compare the distances, adding a new column "cluster" to wine_norm
  wine_norm$cluster<-apply(distances,1,function(x) which(x==min(x)))
  k1old<-k1
  k2old<-k2
  k3old<-k3
  # 3 clusters we got
  cluster1<-subset(wine_norm,wine_norm$cluster==1)
  cluster2<-subset(wine_norm,wine_norm$cluster==2)
  cluster3<-subset(wine_norm,wine_norm$cluster==3)
  # compute new centroids for each cluster
  k1[2:14]<-apply(cluster1[2:14],2,function(col) mean(col))
  k2[2:14]<-apply(cluster2[2:14],2,function(col) mean(col))
  k3[2:14]<-apply(cluster3[2:14],2,function(col) mean(col))
  
  # if we have empty clusters, we need to make its centroid the point with the highest SSE
  cluster1$sse<-apply(cluster1[,2:14],1, function(x) sum( (x-k1[2:14])^2 )) 
  cluster2$sse<-apply(cluster2[,2:14],1, function(x) sum( (x-k2[2:14])^2 )) 
  cluster3$sse<-apply(cluster3[,2:14],1, function(x) sum( (x-k3[2:14])^2 )) 
  sse<-rbind(cluster1,cluster2,cluster3)
  sse<-sse[order(sse$sse),]
  if(any(is.na(k1))) {
    k1<-sse[(length(sse[,1])),] 
    k1<-k1[,-16]
    sse<-sse[-length(sse[,1]),] 
  }
  if(any(is.na(k2))) {
    k2<-sse[(length(sse[,1])),]
    k2<-k2[,-16]
    sse<-sse[-length(sse[,1]),] 
  }
  if(any(is.na(k3))) {
    k3<-sse[(length(sse[,1])),] 
    k3<-k3[,-16]
    sse<-sse[-length(sse[,1]),] 
  }
  final.missclassified.samples<-11111 
  
  # now we loop until the previous centroids do not change
  while(!(sum(k1^2)==sum(k1old^2) && sum(k2^2)==sum(k2old^2) && sum(k3^2)==sum(k3old^2))){
    # new Euclidean distances 
    k1.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k1[2:14])^2)))
    k2.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k2[2:14])^2)))
    k3.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k3[2:14])^2)))
    distances<-cbind(k1.euc.dist,k2.euc.dist,k3.euc.dist)
    # new Manhattan distances 
    #k1.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k1[2:14]))) 
    #k2.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k2[2:14]))) 
    #k3.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k3[2:14])))
    #distances<-cbind(k1.manh.dist,k2.manh.dist,k3.manh.dist)
    
    # assign points to a cluster
    wine_norm$cluster<-apply(distances,1, function(x) which(x == min(x)))
    # make new clusters
    k1old<-k1
    k2old<-k2
    k3old<-k3
    # 3 new clusters we got
    cluster1<-subset(wine_norm,wine_norm$cluster==1)
    cluster2<-subset(wine_norm,wine_norm$cluster==2)
    cluster3<-subset(wine_norm,wine_norm$cluster==3)
    # compute new centroids for each cluster
    k1[2:14]<-apply(cluster1[2:14],2,function(col) mean(col))
    k2[2:14]<-apply(cluster2[2:14],2,function(col) mean(col))
    k3[2:14]<-apply(cluster3[2:14],2,function(col) mean(col))
    # find every sample's SSE
    cluster1$sse<-apply(cluster1[,2:14],1, function(x) sum( (x-k1[2:14])^2 )) 
    cluster2$sse<-apply(cluster2[,2:14],1, function(x) sum( (x-k2[2:14])^2 )) 
    cluster3$sse<-apply(cluster3[,2:14],1, function(x) sum( (x-k3[2:14])^2 )) 
    sse<-rbind(cluster1,cluster2,cluster3)
    sse<-sse[order(sse$sse),]
    # if a cluster is empty, make that centroid the point with the greatest SSE 
    # then remove null centroids from our sse list so we won't reuse it
    if(any(is.na(k1))) {
      k1<-sse[(length(sse[,1])),] 
      k1<-k1[,-16]
      sse<-sse[-length(sse[,1]),] 
    }
    if(any(is.na(k2))) {
      k2<-sse[(length(sse[,1])),] 
      k2<-k2[,-16]
      sse<-sse[-length(sse[,1]),]
    }
    if(any(is.na(k3))) {
      k3<-sse[(length(sse[,1])),] 
      k3<-k3[,-16]
      sse<-sse[-length(sse[,1]),] 
    }
    # we can use the total number of misclasified examples to measure the quality of our clustering
    subset1<-subset(wine_norm, wine_norm$cluster == 1)
    subset2<-subset(wine_norm, wine_norm$cluster == 2)
    subset3<-subset(wine_norm, wine_norm$cluster == 3)
    # we take the mean of each cluster's class and then rounding this, 
    # comparing each sample's class with it,
    # then we count up the number of misclassified samples
    misclassified.samples<-0
    misclassified.samples<-misclassified.samples + length(which(subset1$Cultivar != round(mean(subset1$Cultivar)))) 
    misclassified.samples<-misclassified.samples + length(which(subset2$Cultivar != round(mean(subset2$Cultivar)))) 
    misclassified.samples<-misclassified.samples + length(which(subset3$Cultivar != round(mean(subset3$Cultivar)))) 
    final.misclassified.samples<-misclassified.samples 
  }
  # run the code "print(k1)", "print(k2)", "print(k3)" we will get centroids of clusters
  #print(k1)
  #print(k2)
  #print(k3)
  # we can also use SSE to measure the quality of our clustering, so we print the Final SSE
  print(c("Final SSE = ",sum(sse$sse)))
  print(c("Final Misclassified Samples = ", final.misclassified.samples))
  #store the results, so we can use that table to find a good seed later
  results[i,1]<-i
  results[i,2]<-sum(sse$sse)
  results[i,3]<-final.misclassified.samples 
}

#---- Examing our clustering results for Training set and Test set----#
# Examing our clustering for Training set #
results<-matrix(nrow=50,ncol=3)
wine<-read.csv('wine_train.csv',head=TRUE)
wine<-wine[sample(nrow(wine)),]
wine_norm<-scale(wine)
wine_norm<-as.data.frame(wine_norm)
wine_norm$Cultivar<-wine$Cultivar 
for (i in 1:50) {
  set.seed(i)
  # select 3 initial centroids randomly
  k1<-wine_norm[sample(length(wine_norm[,1]),1),] 
  k2<-wine_norm[sample(length(wine_norm[,1]),1),] 
  k3<-wine_norm[sample(length(wine_norm[,1]),1),]
  k1[2:14]<-sapply(k1[2:14], function(x) rnorm(1,0.0,1.0))
  k2[2:14]<-sapply(k2[2:14], function(x) rnorm(1,0.0,1.0))
  k3[2:14]<-sapply(k3[2:14], function(x) rnorm(1,0.0,1.0))
  # calculate the Euclidean distance
  k1.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k1[2:14])^2))) 
  k2.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k2[2:14])^2))) 
  k3.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k3[2:14])^2)))
  distances<-cbind(k1.euc.dist,k2.euc.dist,k3.euc.dist)
  # calculate the Manhattan distances 
  #k1.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k1[2:14]))) 
  #k2.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k2[2:14]))) 
  #k3.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k3[2:14])))
  #distances<-cbind(k1.manh.dist,k2.manh.dist,k3.manh.dist)
  
  # then compare the distances, adding a new column "cluster" to wine_norm
  wine_norm$cluster<-apply(distances,1,function(x) which(x==min(x)))
  k1old<-k1
  k2old<-k2
  k3old<-k3
  # 3 clusters we got
  cluster1<-subset(wine_norm,wine_norm$cluster==1)
  cluster2<-subset(wine_norm,wine_norm$cluster==2)
  cluster3<-subset(wine_norm,wine_norm$cluster==3)
  # compute new centroids for each cluster
  k1[2:14]<-apply(cluster1[2:14],2,function(col) mean(col))
  k2[2:14]<-apply(cluster2[2:14],2,function(col) mean(col))
  k3[2:14]<-apply(cluster3[2:14],2,function(col) mean(col))
  
  # if we have empty clusters, we need to make its centroid the point with the highest SSE
  cluster1$sse<-apply(cluster1[,2:14],1, function(x) sum( (x-k1[2:14])^2 )) 
  cluster2$sse<-apply(cluster2[,2:14],1, function(x) sum( (x-k2[2:14])^2 )) 
  cluster3$sse<-apply(cluster3[,2:14],1, function(x) sum( (x-k3[2:14])^2 )) 
  sse<-rbind(cluster1,cluster2,cluster3)
  sse<-sse[order(sse$sse),]
  if(any(is.na(k1))) {
    k1<-sse[(length(sse[,1])),] 
    k1<-k1[,-16]
    sse<-sse[-length(sse[,1]),] 
  }
  if(any(is.na(k2))) {
    k2<-sse[(length(sse[,1])),]
    k2<-k2[,-16]
    sse<-sse[-length(sse[,1]),] 
  }
  if(any(is.na(k3))) {
    k3<-sse[(length(sse[,1])),] 
    k3<-k3[,-16]
    sse<-sse[-length(sse[,1]),] 
  }
  final.missclassified.samples<-11111 
  
  # now we loop until the previous centroids do not change
  while(!(sum(k1^2)==sum(k1old^2) && sum(k2^2)==sum(k2old^2) && sum(k3^2)==sum(k3old^2))){
    # new Euclidean distances 
    k1.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k1[2:14])^2)))
    k2.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k2[2:14])^2)))
    k3.euc.dist<-apply(wine_norm[,2:14],1, function(x) sqrt(sum((x - k3[2:14])^2)))
    distances<-cbind(k1.euc.dist,k2.euc.dist,k3.euc.dist)
    # new Manhattan distances 
    #k1.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k1[2:14]))) 
    #k2.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k2[2:14]))) 
    #k3.manh.dist<-apply(wine_norm[,2:14],1, function(x) sum(abs(x-k3[2:14])))
    #distances<-cbind(k1.manh.dist,k2.manh.dist,k3.manh.dist)
    
    # assign points to a cluster
    wine_norm$cluster<-apply(distances,1, function(x) which(x == min(x)))
    # make new clusters
    k1old<-k1
    k2old<-k2
    k3old<-k3
    # 3 new clusters we got
    cluster1<-subset(wine_norm,wine_norm$cluster==1)
    cluster2<-subset(wine_norm,wine_norm$cluster==2)
    cluster3<-subset(wine_norm,wine_norm$cluster==3)
    # compute new centroids for each cluster
    k1[2:14]<-apply(cluster1[2:14],2,function(col) mean(col))
    k2[2:14]<-apply(cluster2[2:14],2,function(col) mean(col))
    k3[2:14]<-apply(cluster3[2:14],2,function(col) mean(col))
    # find every sample's SSE
    cluster1$sse<-apply(cluster1[,2:14],1, function(x) sum( (x-k1[2:14])^2 )) 
    cluster2$sse<-apply(cluster2[,2:14],1, function(x) sum( (x-k2[2:14])^2 )) 
    cluster3$sse<-apply(cluster3[,2:14],1, function(x) sum( (x-k3[2:14])^2 )) 
    sse<-rbind(cluster1,cluster2,cluster3)
    sse<-sse[order(sse$sse),]
    # if a cluster is empty, make that centroid the point with the greatest SSE 
    # then remove null centroids from our sse list so we won't reuse it
    if(any(is.na(k1))) {
      k1<-sse[(length(sse[,1])),] 
      k1<-k1[,-16]
      sse<-sse[-length(sse[,1]),] 
    }
    if(any(is.na(k2))) {
      k2<-sse[(length(sse[,1])),] 
      k2<-k2[,-16]
      sse<-sse[-length(sse[,1]),]
    }
    if(any(is.na(k3))) {
      k3<-sse[(length(sse[,1])),] 
      k3<-k3[,-16]
      sse<-sse[-length(sse[,1]),] 
    }
    # we can use the total number of misclasified examples to measure the quality of our clustering
    subset1<-subset(wine_norm, wine_norm$cluster == 1)
    subset2<-subset(wine_norm, wine_norm$cluster == 2)
    subset3<-subset(wine_norm, wine_norm$cluster == 3)
    # we take the mean of each cluster's class and then rounding this, 
    # comparing each sample's class with it,
    # then we count up the number of misclassified samples
    misclassified.samples<-0
    misclassified.samples<-misclassified.samples + length(which(subset1$Cultivar != round(mean(subset1$Cultivar)))) 
    misclassified.samples<-misclassified.samples + length(which(subset2$Cultivar != round(mean(subset2$Cultivar)))) 
    misclassified.samples<-misclassified.samples + length(which(subset3$Cultivar != round(mean(subset3$Cultivar)))) 
    final.misclassified.samples<-misclassified.samples 
  }
  # run the code "print(k1)", "print(k2)", "print(k3)" we will get centroids of clusters
  #print(k1)
  #print(k2)
  #print(k3)
  # we can also use SSE to measure the quality of our clustering, so we print the Final SSE
  print(c("Final SSE = ",sum(sse$sse)))
  print(c("Final Misclassified Samples = ", final.misclassified.samples))
  #store the results, so we can use that table to find a good seed later
  results[i,1]<-i
  results[i,2]<-sum(sse$sse)
  results[i,3]<-final.misclassified.samples 
}

#------ Euclidean distance ------#
# when i=1 we will get the best clustering results, so before running the code below, let i=1 first (set.seed(1))
# produce the table to show misclassification/classification results #
table(sse$cluster,sse$Cultivar)
# plot for training set #
library(ggplot2)
sse_train<-sse
sse_train$cluster<-jitter(sse_train$cluster)
sse_train$Cultivar<-factor(sse_train$Cultivar)
qplot(cluster, sse, data = sse_train, color=Cultivar,size=I(2.5),ylab=("Euclidean Distance"),xlab="Cluster",main="Training Set Clustering Results, Colored by Cultivar") 
#------ Manhattan distance ------#
 #when i=1 we will get the best clustering results, so before running the code below, let i=1 first (set.seed(1))
 #produce the table to show misclassification/classification results #
#table(sse$cluster,sse$Cultivar)
 #plot for training set #
#distances_train<-distances
#distances_train<-apply(distances_train,1,min)
#distances_train<-as.data.frame(distances_train)
#distances_train$cluster<-jitter(wine_norm$cluster)
#distances_train$Cultivar<-factor(wine_norm$Cultivar)
#qplot(cluster, distances_train, data = distances_train, color=Cultivar,size=I(2.5),ylab=("Manhattan Distance"),xlab="Cluster",main="Training Set Clustering Results, Colored by Cultivar") 


# ------- Evaluating our model by using the Test set ------- #
# Again, before running the code below, making sure i=1, since we need the best centroids 
wine_train<-read.csv('wine_train.csv',head=TRUE)
wine_test<-read.csv('wine_test.csv',head=TRUE)
wine_test_s<-wine_test
# standardize test samples #
wine_test_s$Alcohol<-(wine_test_s$Alcohol-mean(wine_train$Alcohol))/sd(wine_train$Alcohol)
wine_test_s$Malicacid<-(wine_test_s$Malicacid-mean(wine_train$Malicacid))/sd(wine_train$Malicacid)
wine_test_s$Ash<-(wine_test_s$Ash-mean(wine_train$Ash))/sd(wine_train$Ash)
wine_test_s$Alcalinity.of.ash<-(wine_test_s$Alcalinity.of.ash-mean(wine_train$Alcalinity.of.ash))/sd(wine_train$Alcalinity.of.ash)
wine_test_s$Magnesium<-(wine_test_s$Magnesium-mean(wine_train$Magnesium))/sd(wine_train$Magnesium)
wine_test_s$Total.phenols<-(wine_test_s$Total.phenols-mean(wine_train$Total.phenols))/sd(wine_train$Total.phenols)
wine_test_s$Flavanoid<-(wine_test_s$Flavanoid-mean(wine_train$Flavanoid))/sd(wine_train$Flavanoid)
wine_test_s$Nonflavanoid.phenols<-(wine_test_s$Nonflavanoid.phenols-mean(wine_train$Nonflavanoid.phenols))/sd(wine_train$Nonflavanoid.phenols)
wine_test_s$Proanthocyanins<-(wine_test_s$Proanthocyanins-mean(wine_train$Proanthocyanins))/sd(wine_train$Proanthocyanins)
wine_test_s$Color.intensity<-(wine_test_s$Color.intensity-mean(wine_train$Color.intensity))/sd(wine_train$Color.intensity)
wine_test_s$Hue<-(wine_test_s$Hue-mean(wine_train$Hue))/sd(wine_train$Hue)
wine_test_s$OD280.OD315.of.diluted.wines<-(wine_test_s$OD280.OD315.of.diluted.wine-mean(wine_train$OD280.OD315.of.diluted.wine))/sd(wine_train$OD280.OD315.of.diluted.wine)
wine_test_s$Proline<-(wine_test_s$Proline-mean(wine_train$Proline))/sd(wine_train$Proline)
# here our centroids are from our run of the clustering with the training set, 
# then calculate the distance between samples and centroids.
# Euclidean distance #
k1.euc.dist<-apply(wine_test_s[,2:14],1, function(x) sqrt(sum((x - k1[2:14])^2))) 
k2.euc.dist<-apply(wine_test_s[,2:14],1, function(x) sqrt(sum((x - k2[2:14])^2))) 
k3.euc.dist<-apply(wine_test_s[,2:14],1, function(x) sqrt(sum((x - k3[2:14])^2)))
distances<-cbind(k1.euc.dist,k2.euc.dist,k3.euc.dist)
# Manhattan distance #
#k1.manh.dist<-apply(wine_test_s[,2:14],1, function(x) sum(abs(x-k1[2:14]))) 
#k2.manh.dist<-apply(wine_test_s[,2:14],1, function(x) sum(abs(x-k2[2:14]))) 
#k3.manh.dist<-apply(wine_test_s[,2:14],1, function(x) sum(abs(x-k3[2:14])))
#distances<-cbind(k1.manh.dist,k2.manh.dist,k3.manh.dist)
wine_test_s$cluster<-apply(distances,1, function(x) which(x == min(x)))
# find 3 clusters then calculate SSE #
cluster1<-subset(wine_test_s, wine_test_s$cluster == 1) 
cluster2<-subset(wine_test_s, wine_test_s$cluster == 2) 
cluster3<-subset(wine_test_s, wine_test_s$cluster == 3)
cluster1$sse<-apply(cluster1[,2:14],1, function(x) sum( (x-k1[2:14])^2 )) 
cluster2$sse<-apply(cluster2[,2:14],1, function(x) sum( (x-k2[2:14])^2 )) 
cluster3$sse<-apply(cluster3[,2:14],1, function(x) sum( (x-k3[2:14])^2 )) 
sse<-rbind(cluster1,cluster2,cluster3)
print(c('Test set sse=',sum(sse$sse)))
# count misclassified samples #
k1subset<-subset(wine_test_s, wine_test_s$cluster == 1)
k2subset<-subset(wine_test_s, wine_test_s$cluster == 2)
k3subset<-subset(wine_test_s, wine_test_s$cluster == 3)
misclassified.samples<-0
misclassified.samples<-misclassified.samples + length(which(k1subset$Cultivar != round(mean(k1subset$Cultivar)))) 
misclassified.samples<-misclassified.samples + length(which(k2subset$Cultivar != round(mean(k2subset$Cultivar)))) 
misclassified.samples<-misclassified.samples + length(which(k3subset$Cultivar != round(mean(k3subset$Cultivar))))
print(c('Test set misclassified samples=',misclassified.samples))

#------ Euclidean distance ------#
# plot for test set #
# produce the table to show misclassification/classification results #
table(sse$cluster,sse$Cultivar)
# plot for testing set #
sse_test<-sse
sse_test$cluster<-jitter(sse_test$cluster)
sse_test$Cultivar<-factor(sse_test$Cultivar)
qplot(cluster, sse, data = sse_test, color=Cultivar,size=I(2.5),ylab=("Euclidean Distance"),xlab="Cluster",main="Test Set Clustering Results, Colored by Cultivar")  

#------ Manhattan distance ------#
# produce the table to show misclassification/classification results #
#table(sse$cluster,sse$Cultivar)
# plot for testing set #
#distances_test<-distances
#distances_test<-apply(distances_test,1,min)
#distances_test<-as.data.frame(distances_test)
#distances_test$cluster<-jitter(wine_test_s$cluster)
#distances_test$Cultivar<-factor(wine_test_s$Cultivar)
#qplot(cluster, distances_test, data = distances_test, color=Cultivar,size=I(2.5),ylab=("Manhattan Distance"),xlab="Cluster",main="Test Set Clustering Results, Colored by Cultivar") 

# ------------------------------------ #
#             Question 3               #
# ------------------------------------ #
# --------------- PCA ---------------- #
library(stats)
wine<-read_excel('wine_data.xlsx')
wine_s<-scale(wine[,2:14])
pc<-prcomp(wine_s)
summary(pc)
pc$rotation
plot(pc,col=heat.colors(13))
plot(pc,type="lines")
library(rgl)
options(rgl.printRglwidget = TRUE)
attach(wine)
wine$Cultivar<-factor(wine$Cultivar)
wine.col<-Cultivar
# use the first three PCs #
plot3d(pc$x[,1:3],col=rainbow(3)[wine.col])

# use the first two PCs #
library(ChemometricsWithR)
wine.pca<-PCA(wine_s)
scoreplot(wine.pca,col=rainbow(3)[wine.col])
loadingplot(wine.pca,show.names=TRUE)

# ------------------------------------ #
#             Question 5               #
# ------------------------------------ #
# -------------- ICA ----------------- #
wine<-read_excel('wine_data.xlsx')
library(fastICA)
ica3<-fastICA(wine, n.comp=3)
ica5<-fastICA(wine, n.comp=5)
ica8<-fastICA(wine, n.comp=8)
attach(wine)
wine$Cultivar<- factor(wine$Cultivar)
wine.col<-Cultivar
pairs(ica3$S, col=rainbow(3)[wine.col])
pairs(ica5$S, col=rainbow(3)[wine.col])
pairs(ica8$S, col=rainbow(3)[wine.col])





setwd("~/Desktop/ChengyiXu/SpringA/GBA424/Assignment/3")

#A
# Running individual regression and replace NA with predicted values
regsum = data.frame()
conjointdata1 = data.frame()
for (i in 1:200) {
  dt = conjointData[conjointData$ID==i,]
  reg = lm(ratings~price+size+motion+style,data = dt)
  dt$ratings[3]=predict(reg,dt[dt$profile==3,])
  dt$ratings[6]=predict(reg,dt[dt$profile==6,])
  dt$ratings[10]=predict(reg,dt[dt$profile==10,])
  dt$ratings[16]=predict(reg,dt[dt$profile==16,])
  co = reg$coefficients
  regsum=rbind(regsum,co)
  conjointdata1 = rbind(conjointdata1,dt)
}
conjointData= conjointdata1
names(regsum) = c('intercept','price','size','motion','style')


#B
library(cluster)
library(fpc)
library(factoextra)
library(gridExtra)

clustTest = function(toClust,print=TRUE,scale=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100){
  if(scale){ toClust = scale(toClust);}
  set.seed(seed);   # set random number seed before doing cluster analysis
  wss <- (nrow(toClust)-1)*sum(apply(toClust,2,var))
  for (i in 2:maxClusts) wss[i] <- sum(kmeans(toClust,centers=i,nstart=nstart,iter.max=iter.max)$withinss)
  ##gpw essentially does the following plot using wss above. 
  #plot(1:maxClusts, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
  gpw = fviz_nbclust(toClust,kmeans,method="wss",iter.max=iter.max,nstart=nstart,k.max=maxClusts) #alternative way to get wss elbow chart.
  pm1 = pamk(toClust,scaling=TRUE)
  ## pm1$nc indicates the optimal number of clusters based on 
  ## lowest average silhoutte score (a measure of quality of clustering)
  #alternative way that presents it visually as well.
  gps = fviz_nbclust(toClust,kmeans,method="silhouette",iter.max=iter.max,nstart=nstart,k.max=maxClusts) 
  if(print){
    grid.arrange(gpw,gps, nrow = 1)
  }
  list(wss=wss,pm1=pm1$nc,gpw=gpw,gps=gps)
}
##Runs a set of clusters as kmeans
##Arguments:
##  toClust, data.frame with data to cluster
##  nClusts, vector of number of clusters, each run as separate kmeans 
##  ... some additional arguments to be passed to clusters
##Return:
##  list of 
##    kms, kmeans cluster output with length of nClusts
##    ps, list of plots of the clusters against first 2 principle components
runClusts = function(toClust,nClusts,print=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100){
  if(length(nClusts)>4){
    warning("Using only first 4 elements of nClusts.")
  }
  kms=list(); ps=list();
  for(i in 1:length(nClusts)){
    kms[[i]] = kmeans(toClust,nClusts[i],iter.max = iter.max, nstart=nstart)
    ps[[i]] = fviz_cluster(kms[[i]], geom = "point", data = toClust) + ggtitle(paste("k =",nClusts[i]))
    
  }
  library(gridExtra)
  if(print){
    tmp = marrangeGrob(ps, nrow = 2,ncol=2)
    print(tmp)
  }
  list(kms=kms,ps=ps)
}

##Plots a kmeans cluster as three plot report
##  pie chart with membership percentages
##  ellipse plot that indicates cluster definitions against principle components
##  barplot of the cluster means
plotClust = function(km,toClust,discPlot=FALSE){
  nc = length(km$size)
  if(discPlot){par(mfrow=c(2,2))}
  else {par(mfrow=c(3,1))}
  percsize = paste(1:nc," = ",format(km$size/sum(km$size)*100,digits=2),"%",sep="")
  pie(km$size,labels=percsize,col=1:nc)
  
  clusplot(toClust, km$cluster, color=TRUE, shade=TRUE,
           labels=2, lines=0,col.clus=1:nc); #plot clusters against principal components
  
  if(discPlot){
    plotcluster(toClust, km$cluster,col=km$cluster); #plot against discriminant functions ()
  }
  rng = range(km$centers)
  dist = rng[2]-rng[1]
  locs = km$centers+.05*dist*ifelse(km$centers>0,1,-1)
  bm = barplot(km$centers,beside=TRUE,col=1:nc,main="Cluster Means",ylim=rng+dist*c(-.1,.1))
  text(bm,locs,formatC(km$centers,format="f",digits=1))
}


checks = clustTest(regsum,print=TRUE,scale=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100)
##Notice this is again hard to determine which is best. . .
##elbow rule suggests around 3 clusters, pam around 9 

clusts = runClusts(regsum,c(3,4,9,14),print=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100)
##Pick one or plot them all. . .
for(i in 1) plotClust(clusts[[1]][[i]],regsum) #choose the first cluster 3, so i in 1



#C
NewData = merge(conjointData,respondentData,by='ID')
summary(lm(ratings~price+size+motion+style+price*age+price*gender+size*age+size*gender+motion*age+motion*gender+style*age+style*gender, data = NewData))
#NewData$segment1=0
#NewData$segment2=0
#NewData$segment3=0
#NewData$segment4=0
#if(NewData$age==0&&NewData$gender==0){NewData$segment1=1}
#if(NewData$age==0&&NewData$gender==1){NewData$segment2=1}
#if(NewData$age==1&&NewData$gender==0){NewData$segment3=1}
#if(NewData$age==1&&NewData$gender==1){NewData$segment4=1}
Segment1 = NewData[NewData$age==0&NewData$gender==0,]
Segment2 = NewData[NewData$age==0&NewData$gender==1,]
Segment3 = NewData[NewData$age==1&NewData$gender==0,]
Segment4 = NewData[NewData$age==1&NewData$gender==1,]
Segment1$segment1=1
Segment1$segment2=0
Segment1$segment3=0
Segment1$segment4=0
Segment2$segment1=0
Segment2$segment2=1
Segment2$segment3=0
Segment2$segment4=0
Segment3$segment1=0
Segment3$segment2=0
Segment3$segment3=1
Segment3$segment4=0
Segment4$segment1=0
Segment4$segment2=0
Segment4$segment3=0
Segment4$segment4=1
NewData=rbind(Segment1,Segment2,Segment3,Segment4)
NewData$segment1=as.factor(NewData$segment1)
NewData$segment2=as.factor(NewData$segment2)
NewData$segment3=as.factor(NewData$segment3)
NewData$segment4=as.factor(NewData$segment4)
levels(NewData$segment4)
S1<-lm(ratings~price+size+motion+style,data = Segment1)
summary(S1)
S2<-lm(ratings~price+size+motion+style,data = Segment2)
summary(S2)
S3<-lm(ratings~price+size+motion+style,data = Segment3)
summary(S3)
S4<-lm(ratings~price+size+motion+style,data = Segment4)
summary(S4)
#reg_seg=summary(lm(ratings~price+size+motion+style+price*segment1+size*segment1+motion*segment1+style*segment1+price*segment2+size*segment2+motion*segment2+style*segment2+price*segment3+size*segment3+style*segment3+price*segment4+size*segment4+motion*segment4+style*segment4,data = NewData))
#reg_seg=summary(lm(ratings~price+size+motion+style+price*factor(segment1)+size*factor(segment1)+motion*factor(segment1)+style*factor(segment1)+price*factor(segment2)+size*factor(segment2)+motion*factor(segment2)+style*factor(segment2)+price*factor(segment3)+size*factor(segment3)+motion*factor(segment3)+style*factor(segment3)+price*factor(segment4)+size*factor(segment4)+motion*factor(segment4)+style*factor(segment4), data = NewData))

result<-data.frame()
result<-rbind(S1$coefficients,S2$coefficients,S3$coefficients,S4$coefficients)
result<-data.frame(result,'age'=c(0,0,1,1),'gender'=c(0,1,0,1))

#D
library(reshape)

# use kmean to separate customers into three segments (same result as Part B)
regsum$ID=seq(1:200)
demo_cluster = kmeans(x=regsum[, 1:5], centers = 3, nstart = 1000)
cluster_id = data.frame(ID = regsum$ID)
cluster_id$cluster = demo_cluster$cluster
newdata=conjointData[,1:3]

# cast the dataset to pass into market simulation function
newdata.wide=cast(newdata,formula=ID~profile)
newdata=merge(newdata.wide, cluster_id, by='ID')






########## K-means ##########
source("kclust.R")
set.seed(0)

x = rbind(matrix(rnorm(2*100,sd=0.2),ncol=2),
scale(matrix(rnorm(2*100,sd=0.3),ncol=2),center=-c(1,1),scale=F),
scale(matrix(rnorm(2*100,sd=0.2),ncol=2),center=-c(0,1),scale=F))

k = 4
cent.init = rbind(c(0.5,1),c(1,0),c(0,0.5),c(1,1))

cols = c("red","green","blue","purple")
plot(x)
points(cent.init,pch=19,cex=2,col=cols)

km1 = kclust(x,centers=cent.init,alg="kmeans")
km2 = kmeans(x,centers=cent.init,alg="Lloyd")
sum(km1$cluster!=km2$cluster)

plot(x,col=cols[km1$cluster])
points(km1$centers,pch=19,cex=2,col=cols)

cent.old = cent.init
plot(x)
points(cent.old,pch=19,cex=2,col=cols)

par(ask=TRUE)

for (i in 1:km1$iter) {
  # Plot the new clusters
  plot(x,col=cols[km1$cluster.history[i,]],main=paste("Iteration",i))
  points(cent.old,pch=19,cex=2,col=cols) 

  # Plot the new centers
  plot(x,col=cols[km1$cluster.history[i,]],main=paste("Iteration",i))
  cent.new = centfromclust(x,km1$cluster.history[i,],k,alg="kmeans")
  points(cent.new,pch=19,cex=2,col=cols)

  cent.old = cent.new
}

par(ask=FALSE)

########## Hierarchical ##########
set.seed(0)
x = rbind(scale(matrix(rnorm(2*20),ncol=2),cent=c(1,1),scale=F),
scale(matrix(rnorm(2*30),ncol=2),cent=-c(1,1),scale=F))
x = rbind(x,matrix(runif(2*10,min(x),max(x)),ncol=2))
d = dist(x)

plot(x)

tree.sing = hclust(d,method="single")
tree.comp = hclust(d,method="complete")
tree.avg = hclust(d,method="average")

par(mfrow=c(2,3))
plot(tree.sing,labels=F,hang=-1e-10)
plot(tree.comp,labels=F,hang=-1e-10)
plot(tree.avg,labels=F,hang=-1e-10)

labs.sing = cutree(tree.sing,k=3)
labs.comp = cutree(tree.comp,k=3)
labs.avg = cutree(tree.avg,k=3)

cols = c("red","green","blue")
plot(x,col=cols[labs.sing])
plot(x,col=cols[labs.comp])
plot(x,col=cols[labs.avg])

# Compare clustering with 3 and 4 groups
cols = c("red","green","blue","purple")
labs.avg2 = cutree(tree.avg,k=4)

par(mfrow=c(1,2))
plot(x,col=cols[labs.avg],main="3 clusters")
plot(x,col=cols[labs.avg2],main="4 clusters")

table(labs.avg,labs.avg2)

########## Choosing K ##########
# The CH index
set.seed(0)
x = rbind(matrix(rnorm(50*2,sd=0.2),ncol=2),
  scale(matrix(rnorm(50*2,sd=0.2),ncol=2),cent=-c(1,0),scale=F),
  scale(matrix(rnorm(50*2,sd=0.2),ncol=2),cent=-c(1,1),scale=F),
  scale(matrix(rnorm(50*2,sd=0.2),ncol=2),cent=-c(1,1),scale=F),
  scale(matrix(rnorm(50*2,sd=0.2),ncol=2),cent=-c(0.2,0.6),scale=F))

nk = 10
wcv = bcv = numeric(nk)
cent = vector(mode="list",length=nk)
for (k in 1:nk) {
  km = kmeans(x,k,algorithm="Lloyd",nstart=10,iter.max=50)
  wcv[k] = km$tot.withinss
  bcv[k] = km$betweenss
  cent[[k]] = km$centers
}

ylim = range(c(wcv,bcv))
plot(1:nk,wcv,type="b",xlab="K",ylab="Variation measures",col="red",ylim=ylim)
points(1:nk,bcv,type="b",col="blue")
legend(8,60,legend=c("Within","Between"),col=c("red","blue"),pch=21)

wcv+bcv

n = nrow(x)
ch = (bcv/(1:nk-1)) / (wcv/(n-1:nk))
plot(1:nk,ch,type="b",xlab="K",ylab="CH index")

## Donut data example
library(scatterplot3d)
set.seed(0)
n = 2000
tt = seq(0,2*pi,length=n)
x = cos(tt)+rnorm(n,sd=0.1)
y = sin(tt)+rnorm(n,sd=0.1)
z = rnorm(n,sd=0.1)
lim = range(c(x,y,z))

mat = cbind(x,y,z)
a = princomp(mat)
v = a$loadings # directions
s = a$scores   # scores

par(mfrow=c(1,3))

s3d = scatterplot3d(x,y,z,xlim=lim,ylim=lim,zlim=lim,
  xlab="",ylab="",zlab="",grid=FALSE,
  main="Principal component directions")
s3d$points3d(rbind(c(0,0,0),v[,1]),type="l",lwd=3,col="blue")
s3d$points3d(rbind(c(0,0,0),v[,2]),type="l",lwd=3,col="red")
s3d$points3d(rbind(c(0,0,0),v[,3]),type="l",lwd=3,col=3)
legend("topleft", col=c("blue","red",3), lty=1, lwd=3,
       legend=c("v1","v2","v3"))

plot(s[,1],s[,2],main="First 2 principal component scores",
     xlab="Score 1",ylab="Score 2")

v2 = v[,1:2]
mat2 = mat %*% v2 %*% t(v2)

scatterplot3d(mat2[,1],mat2[,2],mat2[,3],
              xlim=lim,ylim=lim,zlim=lim,
              xlab="",ylab="",zlab="",grid=FALSE,
              main="Reconstruction from first 2 directions")

rho = cumsum(a$sdev^2)/sum(a$sdev^2)
plot(1:ncol(mat),rho,type="b",ylim=c(0,1),
xlab="Number of component directions",
ylab="Proportion of variance explained")

## Golf data example
load("playerstats.Rdata")
x = as.matrix(playerstats[,-c(1:5,11)])
a = prcomp(x,center=TRUE,scale=TRUE)
dirs = a$rotation # directions
scrs = a$x        # scores

round(dirs[,1:2],3)
namefinish = paste(playerstats[,3],playerstats[,5])

plot(scrs[,1],scrs[,2],xlim=range(scrs[,1])*1.5,
     ylim=range(scrs[,2])*1.5)
identify(scrs[,1],scrs[,2],labels=namefinish,n=15)

rho = cumsum(a$sdev^2)/sum(a$sdev^2)
plot(1:ncol(x),rho,type="b",ylim=c(0,1),
xlab="Number of component directions",
ylab="Proportion of variance explained")



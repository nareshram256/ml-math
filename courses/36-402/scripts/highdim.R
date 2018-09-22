source("bvsim.R")

set.seed(0)
n = 50
p = 30
x = matrix(rnorm(n*p),nrow=n)

m = cor(x)
m[lower.tri(m,diag=T)] = 0
max(abs(m))

R = 100

### 
# 2/3 small coefficients
bstar = c(runif(10,0.5,1),runif(20,0,0.3))
mu = as.numeric(x %*% bstar)

hist(bstar,breaks=30,col="gray",main="",xlab="True coefficients")

ridlam = seq(0,25,length=60)
laslam = seq(0,10,length=60)

a = bvsim(n,p,x,mu,R,ridlam,laslam,seed=1,bss=FALSE)
a.small = a

plotbv(a,"rid",loc="topleft")
plotbv(a,"las",loc="topleft")
plotbv(a,"las2",loc="bottomright")
plotbv(a,"bss",loc="topright")

# misc plots

par(mfrow=c(1,2))

plot(ridlam,a$testerr.rid,type="l",
xlab=expression(paste(lambda)),ylab="Expected test error")
abline(h=a$testerr.ls,lty=2)
text(c(1,24),c(1.48,1.48),c("Low shrinkage","High shrinkage"),cex=0.75)
legend("topleft",lty=c(2,1),
legend=c("Linear regression","Ridge regression"))

plot(ridlam,a$bias.rid,ylim=c(0,max(a$bias.rid,a$var.rid)),
xlab=expression(paste(lambda)),ylab="",type="l",col="red")
lines(ridlam,a$var.rid,col="blue")
legend("bottomright",lty=1,legend=c("Bias^2","Var"),
col=c("red","blue"))

plot(laslam,a$testerr.las,type="l",
xlab=expression(paste(lambda)),ylab="Expected test error")
abline(h=a$testerr.ls,lty=2)
text(c(1,24),c(1.48,1.48),c("Low shrinkage","High shrinkage"),cex=0.75)
legend("topleft",lty=c(2,1),
legend=c("Linear regression","Lasso regression"))

plot(laslam,a$bias.las,ylim=c(0,max(a$bias.las,a$var.las)),
xlab=expression(paste(lambda)),ylab="",type="l",col="red")
lines(laslam,a$var.las,col="blue")
legend("bottomright",lty=1,legend=c("Bias^2","Var"),
col=c("red","blue"))

dfs.lasso = a$avenzs.las
dfs.ridge = numeric(length(ridlam))
for (i in 1:length(ridlam)) {
  dfs.ridge[i] = sum(diag(x%*%solve(crossprod(x)+diag(ridlam[i],p))%*%t(x)))
}

plot(dfs.ridge,a$testerr.rid,type="l",col="blue",
xlab="df",ylab="Expected test error",xlim=range(c(dfs.lasso)))
lines(dfs.lasso,a$testerr.las,col="purple")
abline(h=a$testerr.ls,lty=2)
legend("topright",lty=c(2,1,1),col=c("black","blue","purple"),
legend=c("Linear regression","Ridge regression","Lasso regression"))

set.seed(11)
y = mu + rnorm(n)
y = y-mean(y)
x = scale(x,center=TRUE,scale=FALSE)
a = lm(y~x+0)
bls = coef(a)
lam = 50
aa = lm.ridge(y~x+0,lambda=lam)
brid = coef(aa)
sum(diag(x%*%solve(t(x)%*%x+lam*diag(1,p))%*%t(x)))

aaa = lars(x,y,normalize=F,intercept=F)
blas = coef(aaa,s=12.5)

plot(c(),c(),xlim=c(-0.5,2.5),ylim=range(c(bstar,bls,brid))*1.1,
     xlab="",ylab="Coefficients")
points(rep(0,p),bstar,col="red")
points(rep(1,p),bls)
points(rep(2,p),brid)
segments(rep(0,p),bstar,rep(1,p),bls,lty=3)
segments(rep(1,p),bls,rep(2,p),brid,lty=3)
text(0,1.175,"True")
text(1,1.175,"Linear")
text(2,1.175,"Ridge")

plot(c(),c(),xlim=c(-0.5,2.5),ylim=range(c(bstar,bls,blas))*1.1,
     xlab="",ylab="Coefficients")
points(rep(0,p),bstar,col="red")
points(rep(1,p),bls)
points(rep(2,p),blas)
segments(rep(0,p),bstar,rep(1,p),bls,lty=3)
segments(rep(1,p),bls,rep(2,p),blas,lty=3)
text(0,1.175,"True")
text(1,1.175,"Linear")
text(2,1.175,"Lasso")

plot(c(),c(),xlim=c(-0.5,2.5),ylim=range(c(bstar,bls,brid))*1.1,
     xlab="",ylab="Coefficients")
points(rep(0,p),bstar,col="red")
points(rep(1,p),brid)
points(rep(2,p),blas)
segments(rep(0,p),bstar,rep(1,p),brid,lty=3)
segments(rep(1,p),brid,rep(2,p),blas,lty=3)
text(0,1.175,"True")
text(1,1.175,"Ridge")
text(2,1.175,"Lasso")

###
# no small coefficients
set.seed(0)
bstar = runif(30,0.5,1)
mu = as.numeric(x%*%bstar)

hist(bstar,breaks=30,col="gray",main="",xlim=c(0,1),xlab="True coefficients")

ridlam = seq(0,30,length=60)
laslam = seq(0,10,length=60)
a = bvsim(n,p,x,mu,R,ridlam,laslam,seed=1,bss=FALSE)
a.nosmall = a

plotbv(a,"rid",loc="topleft")
plotbv(a,"las",loc="topleft")
plotbv(a,"las2",loc="bottomright")
plotbv(a,"bss",loc="topright")

###
# zero coefficients
set.seed(0)
bstar = c(runif(10,0.5,1),rep(0,20))
mu = as.numeric(x%*%bstar)

hist(bstar,breaks=30,col="gray",main="",xlab="True coefficients")

ridlam = seq(0,25,length=60)
laslam = seq(0,12,length=60)
a = bvsim(n,p,x,mu,R,ridlam,laslam,seed=1,bss=FALSE)
a.zero = a

plotbv(a,"rid",loc="topleft")
plotbv(a,"las",loc="topleft")
plotbv(a,"las2",loc="bottomright")
plotbv(a,"bss",loc="topright")

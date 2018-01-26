library(MASS)
library(lars)
library(leaps)

bvsim = function(n,p,x,mu,R,ridlam=NULL,laslam=NULL,
  rid=TRUE,las=TRUE,bss=TRUE,seed=NULL) {

  if (rid & is.null(ridlam)) stop("Must pass lambda values for ridge regression.")
  if (las & is.null(laslam)) stop("Must pass lambda values for lasso.")
  if (!is.null(seed)) set.seed(seed)

  fit.ls = matrix(0,R,n)
  fit.rid = array(0,dim=c(R,length(ridlam),n))
  fit.las = array(0,dim=c(R,length(laslam),n))
  fit.las2 = array(0,dim=c(R,length(laslam),n))
  fit.bss = array(0,dim=c(R,p,n))

  nzs.las = matrix(0,R,length(laslam))
  
  err.ls = numeric(R)
  err.rid = matrix(0,R,length(ridlam))
  err.las = matrix(0,R,length(laslam))
  err.las2 = matrix(0,R,length(laslam))
  err.bss = matrix(0,R,p)

  for (i in 1:R) {
    cat(c(i,", "))
    y = mu + rnorm(n)
    ynew = mu + rnorm(n)
    
    a = lm(y~x+0)
    bls = coef(a)
    fit.ls[i,] = x%*%bls
    err.ls[i] = mean((ynew-fit.ls[i,])^2)
    
    if (rid) {
      a = lm.ridge(y~x+0,lambda=ridlam)
      brid = coef(a)
      fit.rid[i,,] = brid%*%t(x)
      err.rid[i,] = rowMeans(scale(fit.rid[i,,],center=ynew,scale=F)^2)
    }

    if (las) {
      a = lars(x,y,int=F,norm=F)
      blas = predict.lars(a,type="coef",s=laslam,mode="lambda")$coef
      fit.las[i,,] = blas%*%t(x)
      err.las[i,] = rowMeans(scale(fit.las[i,,],center=ynew,scale=F)^2)
      nzs.las[i,] = rowSums(blas!=0)
      
      for (j in 1:length(laslam)) {
        s = which(blas[j,]!=0)
        fit.las2[i,j,] = x[,s]%*%coef(lm(y~x[,s]+0))
      }
      err.las2[i,] = rowMeans(scale(fit.las2[i,,],center=ynew,scale=F)^2)
    }
    
    if (bss) {
      a = leaps(x,y,int=F,nbest=1)
      for (k in 1:p) {
        ii = a$which[k,]
        bbss = lm(y~x[,ii]+0)$coef
        fit.bss[i,k,] = bbss%*%t(x[,ii])
      }
      err.bss[i,] = rowMeans(scale(fit.bss[i,,],center=ynew,scale=F)^2)
    }
  }
  
  testerr.ls = mean(err.ls)
  testerr.rid = colMeans(err.rid)
  testerr.las = colMeans(err.las)
  testerr.las2 = colMeans(err.las2)
  testerr.bss = colMeans(err.bss)
  
  avenzs.las = colMeans(nzs.las)
  
  bias.ls = sum((colMeans(fit.ls)-mu)^2)/n
  var.ls = sum(apply(fit.ls,2,var))/n
  
  bias.rid = rowSums(scale(apply(fit.rid,2:3,mean),center=mu,scale=F)^2)/n
  var.rid = rowSums(apply(fit.rid,2:3,var))/n
  
  bias.las = rowSums(scale(apply(fit.las,2:3,mean),center=mu,scale=F)^2)/n
  var.las = rowSums(apply(fit.las,2:3,var))/n
  
  bias.las2 = rowSums(scale(apply(fit.las2,2:3,mean),center=mu,scale=F)^2)/n
  var.las2 = rowSums(apply(fit.las2,2:3,var))/n
  
  bias.bss = rowSums(scale(apply(fit.bss,2:3,mean),center=mu,scale=F)^2)/n
  var.bss = rowSums(apply(fit.bss,2:3,var))/n
  
  mse.ls = bias.ls + var.ls
  mse.rid = bias.rid + var.rid
  mse.las = bias.las + var.las
  mse.las2 = bias.las2 + var.las2
  mse.bss = bias.bss + var.bss
  
  return(list(n=n,p=p,x=x,mu=mu,ridlam=ridlam,laslam=laslam,seed=seed,
              testerr.ls=testerr.ls,testerr.rid=testerr.rid,
              testerr.las=testerr.las,avenzs.las=avenzs.las,
              testerr.las2=testerr.las2,testerr.bss=testerr.bss,
              mse.ls=mse.ls,mse.rid=mse.rid,
              mse.las=mse.las,mse.las2=mse.las2,
              mse.bss=mse.bss,
              bias.ls=bias.ls,bias.rid=bias.rid,
              bias.las=bias.las,bias.las2=bias.las2,
              bias.bss=bias.bss,
              var.ls=var.ls,var.rid=var.rid,
              var.las=var.las,var.las2=var.las2,
              var.bss=var.bss))
}

plotbv = function(a,method="rid",loc="bottomright",ylim=NULL) {
  if (method=="rid") {
    mse = a$mse.rid
    bias = a$bias.rid
    var = a$var.rid
    x = a$ridlam
    xlab = expression(paste(lambda))
    name = "Ridge"
  }
  else if (method=="las") {
    mse = a$mse.las
    bias = a$bias.las
    var = a$var.las
    x = a$laslam
    xlab = expression(paste(lambda))
    name = "Lasso"
  }
  else if (method=="las2") {
    mse = a$mse.las2
    bias = a$bias.las2
    var = a$var.las2
    x = a$laslam
    xlab = expression(paste(lambda))
    name = "Relaxed Lasso"
  }
  else if (method=="lasboth") {
    mse = a$mse.las
    bias = a$bias.las
    var = a$var.las
    mse2 = a$mse.las2
    bias2 = a$bias.las2
    var2 = a$var.las2
    x = a$laslam
    xlab = expression(paste(lambda))
    name = "Lasso"
    name2 = "Relaxed Lasso"
  }
  else {
    mse = a$mse.bss
    bias = a$bias.bss
    var = a$var.bss
    x = 1:a$p
    xlab = "t"
    name = "BSS"
  }

  par(mar=c(4.5,4.5,0.5,0.5))
  if (is.null(ylim)) ylim=c(0,max(mse,a$mse.ls))
  plot(x,mse,type="l",ylim=ylim,xlab=xlab,ylab="")
  lines(x,bias,col="red")
  lines(x,var,col="blue")
  if (method=="lasboth") {
    lines(x,mse2,col="darkgray")
    lines(x,bias2,col="darkgreen")
    lines(x,var2,col="purple")
  }
  abline(h=a$mse.ls,lty=2)
  
  if (method!="lasboth") {
    legend(loc,lty=c(2,1,1,1),
           legend=c("Linear MSE",paste(name,c("MSE","Bias^2","Var"))),
           col=c("black","black","red","blue"))
  }
  else {
    legend(loc,lty=c(2,1,1,1,1,1,1),
           legend=c("Linear MSE",paste(name,c("MSE","Bias^2","Var")),
             paste(name2,c("MSE","Bias^2","Var"))),
           col=c("black","black","red","blue","darkgray","darkgreen","purple"))
  }
}

plotbv2 = function(a,method="rid",loc="bottomright",ylim=NULL) {
  if (method=="rid") {
    mse = a$mse.rid
    bias = a$bias.rid
    var = a$var.rid
    x = a$ridlam
    xlab = expression(paste(lambda))
    name = "Ridge"
  }
  else if (method=="las") {
    mse = a$mse.las
    bias = a$bias.las
    var = a$var.las
    x = a$laslam
    xlab = expression(paste(lambda))
    name = "Lasso"
  }
  else if (method=="las2") {
    mse = a$mse.las2
    bias = a$bias.las2
    var = a$var.las2
    x = a$laslam
    xlab = expression(paste(lambda))
    name = "Relaxed Lasso"
  }
  else if (method=="lasboth") {
    mse = a$mse.las
    bias = a$bias.las
    var = a$var.las
    mse2 = a$mse.las2
    bias2 = a$bias.las2
    var2 = a$var.las2
    x = a$laslam
    xlab = expression(paste(lambda))
    name = "Lasso"
    name2 = "Relaxed Lasso"
  }
  else {
    mse = a$mse.bss
    bias = a$bias.bss
    var = a$var.bss
    x = 1:a$p
    xlab = "t"
    name = "BSS"
  }
  
  par(mar=c(4.5,4.5,0.5,0.5))
  if (is.null(ylim)) ylim=c(0,1+max(mse,a$mse.ls))
  plot(x,1+mse,type="l",ylim=ylim,xlab=xlab,ylab="")
  lines(x,bias,col="red")
  lines(x,var,col="blue")
  if (method=="lasboth") {
    lines(x,1+mse2,col="darkgray")
    lines(x,bias2,col="darkgreen")
    lines(x,var2,col="purple")
  }
  abline(h=1+a$mse.ls,lty=2)
  
  if (method!="lasboth") {
    legend(loc,lty=c(2,1,1,1),
           legend=c("Linear Test Err",paste(name,c("Test Err","Bias^2","Var"))),
           col=c("black","black","red","blue"))
  }
  else {
    legend(loc,lty=c(2,1,1,1,1,1,1),
           legend=c("Linear Test Err",paste(name,c("Test Err","Bias^2","Var")),
             paste(name2,c("Test Err","Bias^2","Var"))),
           col=c("black","black","red","blue","darkgray","darkgreen","purple"))
  }
}



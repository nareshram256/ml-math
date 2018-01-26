##### Training and test evaluations #####

# Training data
r = function(x) {
  return(7*x^2 - 0.5*x)
}

set.seed(0)
n = 50
x = sort(rnorm(n))
err = rnorm(n)
y = r(x) + err

plot(x,y)
curve(7*x^2-0.5*x,add=TRUE)

# Fit polynomials 
maxdeg = 7
x0 = seq(-4,4,length=1000)
fits = matrix(0,length(x0),maxdeg+1)
err.tr = numeric(maxdeg+1)

for (d in 0:maxdeg) {
  # Fit degree d polynomial
  if (d==0) linmod = lm(y~1)
  else linmod = lm(y~poly(x,d))

  # Fitted values at x0
  fits[,d+1] = predict(linmod,data.frame(x=x0))

  # Training error
  err.tr[d+1] = mean(linmod$residuals^2)
}

# Plot fitted values
xlim = range(x)
plot(x,y,xlim=xlim)
for (d in 0:maxdeg) {
  lines(x0,fits[,d+1],col=d+2)
}
legend(0,max(y),lty=1,col=0:maxdeg+2,
       legend=paste("Degree",0:maxdeg))

# Plot training error
plot(0:maxdeg,err.tr,type="b",
     xlab="Poly degree",ylab="Training error")

# Test data
m = 10000
x.new = sort(rnorm(m))
y.new = r(x.new) + rnorm(m)

plot(x.new,y.new,pch=".")
for (d in 0:maxdeg) {
  lines(x0,fits[,d+1],col=d+2)
}
legend(0,max(y.new),lty=1,col=0:maxdeg+2,
       legend=paste("Degree",0:maxdeg))

err.te = numeric(maxdeg+1)
for (d in 0:maxdeg) {
  # Fit degree d polynomial
  if (d==0) linmod = lm(y~1)
  else linmod = lm(y~poly(x,d))
  
  fits.new = predict(linmod,data.frame(x=x.new))
  err.te[d+1] = mean((y.new-fits.new)^2)
}

# Plot training and test errors
ylim = range(c(err.tr,err.te))
plot(0:maxdeg,err.tr,type="b",ylim=ylim,log="y",
     xlab="Poly degree",ylab="Error")
points(0:maxdeg,err.te,type="b",col="red")

##### Cross-validation #####

# Create folds
set.seed(1)
K = 5
fold.assignments = rep(1:K,length=n)
fold.assignments = sample(fold.assignments)

# Perform cross-validation
err.cv = matrix(0,K,maxdeg+1)
for (k in 1:K) {
  # Print out progress
  cat("Fold",k,"... ")
  
  # Partition into internal training and test sets
  inds = which(fold.assignments==k)
  x.tr = x[-inds]
  y.tr = y[-inds]
  x.te = x[inds]
  y.te = y[inds]

  for (d in 0:maxdeg) {
    # Fit degree d polynomial
    if (d==0) linmod = lm(y.tr~1)
    else linmod = lm(y.tr~poly(x.tr,d))

    # Cross-validation error
    y.pred = predict(linmod,data.frame(x.tr=x.te))
    err.cv[k,d+1] = mean((y.te-y.pred)^2)
  }
}

# Plot the individual cross-validation error curves,
# i.e., one curve for each fold
matplot(0:maxdeg,t(err.cv),type="l",lty=1,log="y",
        xlab="Poly degree",ylab="Fold errors")

# Average across folds, and compute the standard errors
err.cv.ave = colMeans(err.cv)
err.cv.se = apply(err.cv,2,sd)/sqrt(K)

# Plot the cross-validation error curve with standard errors
ylim = range(c(err.cv.ave+err.cv.se,err.cv.ave-err.cv.se))
plot(0:maxdeg,err.cv.ave,type="b",log="y",ylim=ylim,
     xlab="Poly degree",ylab="Cross-validation error")
lines(0:maxdeg,err.cv.ave+err.cv.se,lty=2)
lines(0:maxdeg,err.cv.ave-err.cv.se,lty=2)

# Plot all three: training, cross-validation, test error curves
ylim = range(c(err.tr,err.te,err.cv.ave))
plot(0:maxdeg,err.tr,type="b",log="y",ylim=ylim,
     xlab="Poly degree",ylab="Error")
points(0:maxdeg,err.te,type="b",col="red")
points(0:maxdeg,err.cv.ave,type="b",col="blue")
legend("topleft",pch=21,col=c("black","red","blue"),
       legend=c("Train","Test","CV"))

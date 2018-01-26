library(ISLR)
colnames(Wage)

# Fit a smoothing spline of wage on age, fixing it
# at 5 degrees of freedom
set.seed(0)
n = 500
i = sample(1:nrow(Wage),n)
x = Wage$age[i] + rnorm(n,sd=0.1)
y = Wage$wage[i]

o = order(x)
x = x[o]
y = y[o]

df = 5
ssmod = smooth.spline(x,y,df=df)

plot(x,y,col="black")
lines(ssmod$x,ssmod$y,lwd=2,col="red")

# Write a function to get the smoother matrix S
getSmootherMat = function(x, df) {
  n = length(x)
  S = matrix(0,n,n)
  for (i in 1:n) {
    if (i%%100==0) cat(i,"... ")
    y = numeric(n)
    y[i] = 1
    S[,i] = predict(smooth.spline(x,y,df=df),x)$y
  }
  return(S)
}

# Get the smoother matrix for our example, and
# check it
S = getSmootherMat(x,df)
yhat = predict(ssmod,x)$y
max(abs(yhat - S%*%y))

# Construct 95% pointwise confidence intervals
sigmahat = sqrt(sum((y-yhat)^2)/(n-df))
syhat = sigmahat * sqrt(diag(S%*%t(S)))
alpha = 0.05
q1 = qt(alpha/2,n-df)
q2 = qt(1-alpha/2,n-df)
low.lims = yhat-syhat*q2
up.lims = yhat-syhat*q1

plot(x,y,col="black")
lines(ssmod$x,ssmod$y,col="red")
lines(x,low.lims,col="blue",lty=2)
lines(x,up.lims,col="blue",lty=2)

# An easier, but less accurate way that doesn't
# require the smoothing matrix S
sigmahat = sqrt(sum((y-yhat)^2)/(n-df))
syhat = sigmahat * sqrt(ssmod$lev)
q1 = qt(alpha/2,n-df)
q2 = qt(1-alpha/2,n-df)
low.lims = yhat-syhat*q2
up.lims = yhat-syhat*q1

plot(x,y,col="black")
lines(ssmod$x,ssmod$y,col="red")
lines(x,low.lims,col=3,lty=2)
lines(x,up.lims,col=3,lty=2)


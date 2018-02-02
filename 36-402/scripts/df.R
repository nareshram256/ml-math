# Create example data for linear regression
set.seed(0)
n = 100
p = 20
x = matrix(rnorm(n*p),n,p)
beta = 2*runif(p)
y = x %*% beta + rnorm(n)

# Use simulation to estimate degrees of freedom
R = 500
y.real = matrix(0,R,n)
yhat.real = matrix(0,R,n)

for (r in 1:R) {
  y.r = x %*% beta + rnorm(n)

  y.real[r,] = y.r
  yhat.real[r,] = lm(y.r~x+0)$fitted
}

# Function to compute trace of a matrix
tr = function(A) { return(sum(diag(A))) }

tr(cov(y.real,yhat.real)) # should be about p

# Use the bootstrap to estimate degrees of freedom
# Naive approach, pairs bootstrap
B = 500
y.boot = matrix(0,B,n)
yhat.boot = matrix(0,B,n)

for (b in 1:B) {
  index.b = sample(1:n,replace=TRUE)
  x.b = x[index.b,]
  y.b = y[index.b]

  y.boot[b,] = y.b
  yhat.boot[b,] = lm(y.b~x.b+0)$fitted
}

tr(cov(y.boot,yhat.boot)) # way too big!! why??

# Better approach, residual boostrap
yhat = lm(y~x+0)$fitted
ehat = y-yhat

B = 500
y.boot = matrix(0,B,n)
yhat.boot = matrix(0,B,n)

for (b in 1:B) {
  index.b = sample(1:n,replace=TRUE)
  y.b = yhat + ehat[index.b]
  
  y.boot[b,] = y.b
  yhat.boot[b,] = lm(y.b~x+0)$fitted
}

tr(cov(y.boot,yhat.boot)) # much better

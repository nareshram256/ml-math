# Function to generate an (X,Y) pair
gen.xy = function(n, rho=0.4, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  z1 = rnorm(n)
  z2 = rnorm(n)
  x = z1
  y = rho*z1 + sqrt(1-rho^2)*z2
  return(list(x=x,y=y))
}

# Function to estimate theta
theta.fn = function(data, index=1:length(data$x)) {
  x = data$x[index]
  y = data$y[index]
  return((var(y)-cov(x,y))/(var(x)+var(y)-2*cov(x,y)))
}

# "True" theta value
theta.true = theta.fn(gen.xy(1e7))

# Sample of n=100 points
n = 100
data = gen.xy(n,seed=1)
plot(data$x,data$y)
theta.hat = theta.fn(data)

# How variable is this theta.hat? Inspect over 1000
# simulations
R = 1000
theta.hat.real = numeric(R)
for (r in 1:R) {
  data.r = gen.xy(n)
  theta.hat.real[r] = theta.fn(data.r)
}
 
par(mfrow=c(1,2))
hist(theta.hat.real, col="orange",
     main="Real samples",
     xlab="theta.hat values")
abline(v=theta.true, lwd=3, lty=2)

# Simulated standard error estimate
sd(theta.hat.real)

##### Bootstrap #####
# Now use 1000 bootstrap simulations 
B = 1000
theta.hat.boot = numeric(B)
for (b in 1:B) {
  index.b = sample(1:n,replace=TRUE)
  theta.hat.boot[b] = theta.fn(data,index.b)
}

hist(theta.hat.boot, col="lightblue", 
     main="Bootstrap samples",
     xlab="theta.hat values")
abline(v=theta.true, lwd=3, lty=2)

# Bootstrap standard error estimate
sd(theta.hat.boot)

# Bootstrap bias estimate
mean(theta.hat.boot) - theta.hat

# Basic boostrap confidence interval
alpha = 0.05
q.lo = quantile(theta.hat.boot, prob=alpha/2)
q.hi = quantile(theta.hat.boot, prob=1-alpha/2)
basic.boot.int = c(2*theta.hat-q.hi, 2*theta.hat-q.lo)

# Plot comparison distributions
par(mfrow=c(1,2))
hist(theta.hat.real-theta.true, col="orange",
     main="", xlab="theta.hat - theta.true")
hist(theta.hat.boot-theta.hat, col="lightblue",
     main="", xlab="theta.hat.boot - theta.hat")
# Wow!

# Studentized bootstrap confidence interval
B = 500
M = 500
theta.hat.boot = numeric(B)
theta.hat.boot.se = numeric(B)
for (b in 1:B) {
  if (b %% 25==0) cat(b,"... ")
  
  index.b = sample(1:n,replace=TRUE)
  theta.hat.boot[b] = theta.fn(data,index.b)

  theta.hat.boot.inner = numeric(M)
  for (m in 1:M) {
    index.m = sample(index.b,replace=TRUE)
    theta.hat.boot.inner[m] = theta.fn(data,index.m)
  }
  theta.hat.boot.se[b] = sd(theta.hat.boot.inner)
}

t = (theta.hat.boot-theta.hat)/theta.hat.boot.se
s = sd(theta.hat.boot)

alpha = 0.05
q.lo = quantile(t, prob=alpha/2)
q.hi = quantile(t, prob=1-alpha/2)
student.boot.int = c(theta.hat-s*q.hi,theta.hat-s*q.lo)

# Plot comparison distributions
# First we actually need to compute a standard error for
# the theta.hat.real estimates
R = 500
theta.hat.real = numeric(R)
theta.hat.real.se = numeric(R)
for (r in 1:R) {
  if (r %% 25==0) cat(b,"... ")
  
  data.r = gen.xy(n)
  theta.hat.real[r] = theta.fn(data.r)
  
  theta.hat.boot.inner = numeric(M)
  for (m in 1:M) {
    index.m = sample(1:n,replace=TRUE)
    theta.hat.boot.inner[m] = theta.fn(data.r,index.m)
  }
  theta.hat.real.se[r] = sd(theta.hat.boot.inner)
}

par(mfrow=c(1,2))
hist((theta.hat.real-theta.true)/theta.hat.real.se, col="orange",
     main="", xlab="(theta.hat - theta.true) / theta.hat.se ")
hist((theta.hat.boot-theta.hat)/theta.hat.boot.se, col="lightblue",
     main="", xlab="(theta.hat.boot - theta.hat) / theta.hat.boot.se")
# Wow again!

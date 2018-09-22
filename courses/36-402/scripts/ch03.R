#### Figures for Section 3, "Over-Fitting and Model Selection"
# Examples of training data
# 20 standard-Gaussian X's
x = rnorm(20)
# Quadratic Y's
y = 7*x^2 - 0.5*x + rnorm(20)

# Initial plot of training data plus true regression curve
plot(x,y)
curve(7*x^2-0.5*x,col="grey",add=TRUE)

# Fit polynomials and add them to the plot
# Fit a constant (0th order polynomial), plot it
y.0 = lm(y ~ 1)
   # We'd get the same constant by just doing mean(y), but fitting it as a
   # regression model means functions like residuals() and predict() are
   # available for use later, the same as our other models
abline(h=y.0$coefficients[1])
# Get evenly spaced points for pretty plotting of other models
d = seq(min(x),max(x),length.out=200)
# Fit polynomials of order 1 to 9
  # It would be nicer if we let this run from 0 to 9, but R doesn't allow us
  # to do a polynomial of degree 0
for (degree in 1:9) {
	fm = lm(y ~ poly(x,degree))
	# Store the results in models called y.1, y.2, through y.9
	  # The assign/paste trick here is often useful
	assign(paste("y",degree,sep="."), fm)
	# Plot them, with different line types
	lines(d, predict(fm,data.frame(x=d)),lty=(degree+1))
}

# Calculate and plot in-sample errors
mse.q = vector(length=10)
for (degree in 0:9) {
	# The get() function is the inverse to assign()
	fm = get(paste("y",degree,sep="."))
	mse.q[degree+1] = mean(residuals(fm)^2)
}
plot(0:9,mse.q,type="b",xlab="polynomial degree",ylab="mean squared error",
     log="y")


# Plot the old curves with testing data
x.new = rnorm(2e4)
y.new = 7*x.new^2 - 0.5*x.new + rnorm(2e4)
plot(x.new,y.new,xlab="x",ylab="y",pch=24,cex=0.1,col="blue")
curve(7*x^2-0.5*x,col="grey",add=TRUE)
abline(h=y.0$coefficients[1])
d = seq(from=min(x.new),to=max(x.new),length.out=200)
for (degree in 1:9) {
	fm = get(paste("y",degree,sep="."))
	lines(d, predict(fm,data.frame(x=d)),lty=(degree+1))
}
points(x,y)


# Calculate and plot the out-of-sample errors
gmse.q = vector(length=10)
for (degree in 0:9) {
	# The get() function is the inverse to assign()
	fm = get(paste("y",degree,sep="."))
	predictions = predict(fm,data.frame(x=x.new))
	resids = y.new - predictions
	gmse.q[degree+1] = mean(resids^2)
}
plot(0:9,mse.q,type="b",xlab="polynomial degree",
     ylab="mean squared error",log="y",ylim=c(min(mse.q),max(gmse.q)))
lines(0:9,gmse.q,lty=2,col="blue")
points(0:9,gmse.q,pch=24,col="blue")



# Additive model example on Wage data from the ISL book
library(ISLR)
colnames(Wage)

# Fit an additive model of wage on year, age, and education,
# with smoothing splines for year and age, and a step function
# for education 
library(gam)
am3 = gam(wage~s(year,4)+s(age,5)+education, data=Wage)

par(mfrow=c(1,3))
plot.gam(am3, se=TRUE, col="blue")

# Looks like the nonlinear effect of year may be suspect;
# let's test it
am1 = gam(wage~s(age,5)+education, data=Wage)
am2 = gam(wage~year+s(age,5)+education, data=Wage)

anova(am1,am2,am3,test="F")
summary(am3)

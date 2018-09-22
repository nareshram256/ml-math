# Poisson regression example taken from "Regression Models for Count Data in R"
# (see Section 3 of http://cran.r-project.org/web/packages/pscl/vignettes/countreg.pdf)
load("DebTrivedi.rda")

dt = DebTrivedi[, c(1, 6:8, 13, 15, 18)]
colnames(dt)

plot(table(dt$ofp))
plot(dt$numchron, dt$ofp)

poismod = glm(ofp ~ ., data=dt, family="poisson")
summary(poismod)

qpoismod = glm(ofp ~ ., data=dt, family="quasipoisson")
summary(qpoismod)

#####
# Additive logistic regression example on Wage data from the ISL book
library(ISLR)
library(gam)
colnames(Wage)

alogmod1 = gam(I(wage>250) ~ year + s(age,df=5) + education, data=Wage, family="binomial")
par(mfrow=c(1,3))
plot(alogmod1, se=TRUE, col=3)

# Note that there are no instances of wage>250 in the
# first education category
table(Wage$education, I(Wage$wage>250))

alogmod2 = gam(I(wage>250) ~ year + s(age,df=5) + education, data=Wage, family="binomial",
  subset=which(education != "1. < HS Grad"))

par(mfrow=c(1,3))
plot(alogmod2, se=TRUE, col=3)

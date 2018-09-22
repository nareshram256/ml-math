# Sigmoid
x = seq(-10,10,length=1000)
plot(x,y = exp(x)/(1+exp(x)),type="l")

# Log odds
p = seq(0,1,length=1000)
plot(p,log(p/(1-p)),type="l")

#####
# South African heart disease data from ESL book
heart = read.table("SAheart.data",sep=",",head=TRUE,row.names=1)

# Fit a logistic regression of chd (chronic heart disease)
# on other variables
col.inds = c(1,2,3,5,7,8,9,10)
logmod = glm(chd~., family="binomial", data=heart[,col.inds])

# Standard inferential tools
summary(logmod)
confint(logmod)

# Backwards variable selection
logmod = step(logmod,direction="backward")
exp(logmod$coef)

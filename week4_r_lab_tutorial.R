library(ISLR)

############ The Validation Set Approach #############
# set seed so results can be reproduced at later time
set.seed(1)               

# sample() splits the set of obs into two halves. here selects random subset of 196 obs out of 392
train=sample(392,196)  

# use subset() option to fit linear regression using only the obs corresponding to the train data
lm.fit = lm(mpg~horsepower, data=Auto, subset=train)

attach(Auto)
# Use predict function to estimate response for all 392 obs, using the mean func to calc the MSE
# of all 196 obs in the validation set.. '-train' index denotes only obs that are NOT in train set.
# Calculate MSE : 
mean((mpg-predict(lm.fit, Auto))[-train]^2)

# estimated test MSE for linear regression is 26.14. We can use poly() func to estimate the
# test error for the polynomial and cubic regressions
lm.fit2 = lm(mpg~poly(horsepower,2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)  # MSE 19.82

lm.fit3 = lm(mpg~poly(horsepower,3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)   # MSE 19.78


# note: if use diff training set then, we will get diff errors on vali test set.
set.seed(2)
train=sample(392,196)
lm.fit = lm(mpg~horsepower, subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower ,2),data=Auto , subset=train)
mean((mpg -predict (lm.fit2 ,Auto ))[- train]^2)

lm.fit3=lm(mpg~poly(horsepower ,3),data=Auto , subset=train)
mean((mpg -predict (lm.fit3 ,Auto ))[- train]^2)
# here the quadratic model performs better than a linear model! :-)

# LEAVE-ONE-OUT CROSS-VALIDATION
# Note that glm() without the family='binomial' argument will work the same way 
# that the logistic regression does in lm(). 
glm.fit = glm(mpg~horsepower, data=Auto)
coef(glm.fit)

# and the below yield identical linear regression models. 
lm.fit = lm(mpg~horsepower, data=Auto)
coef(lm.fit)

# here we use glm() rather than lm() because the glm() can be used with cv.glm()
# via the boot library.
library(boot)
glm.fit=glm(mpg~horsepower, data=Auto)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta

# Note: the cv.glm() function produces a list with several components. The two number
# in the delta vector contain the cross validation results. In this case the 
# numbers are identical up to two decimal places and correspond to the LOOCV statistic.

# initialize a vector
cv.error = rep(0,5)
cv.error

for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

# > cv.error
# [1] 24.23151 19.24821 19.33498 19.42443 19.03321

# as can be seen there is a sharp drop in the estimated test MSE between the linear
# and quadratic fits, but then no clear improvement from using higher-order
# polynomials.


# K-FOLD CROSS VALIDATION
# cv.glm() function can also be used to implement k-fold CV.
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i), data=Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}


plot(cv.error.10)

# computation time for K-fold is much shorter than that of LOOCV for a least
# squares linear model (usually). Still see little evidence here that using 
# cubic or higher-order poly terms leads to lower test error than using the 
# quadratic fit (^2).



# BOOTSTRAP 
# lets create a function alpha.fn() which takes inputs(X,Y) as well as a vector
# indicating which obs should be used to estimate alpha for the portfolio.
alpha.fn = function(data, index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

str(Portfolio)
# the func returns an estimate for alpha based on applying func indexed by the 
# argument index. E.g. this will tell R to estimate alpha based upon all 100 obs.
alpha.fn(Portfolio, 1:100)

# sample() randomly selects 100 obs from the range 1 to 100, with replacement.
# equiv to using bootstrap data set and recomputing alpha based on the new data set
set.seed(1)
alpha.fn(Portfolio, sample(100,100,replace=T))

# bootstrap analysis performs this command many times recording all the estimates
# for alpha, and computing the resulting standard deviation. boot() func 
# automates this approach. R=1000 bootstrap estimates for alpha
boot(Portfolio, alpha.fn, R=1000)

# > boot(Portfolio, alpha.fn, R=1000)
# 
# ORDINARY NONPARAMETRIC BOOTSTRAP
# 
# 
# Call:
#   boot(data = Portfolio, statistic = alpha.fn, R = 1000)
# 
# 
# Bootstrap Statistics :
#   original        bias    std. error
# t1* 0.5758321 -7.315422e-05  0.08861826


# Estimating accuracy of linear regression model
# Bootstrap approach can be used to assess variability of the coefficient
# estimates and predictions from the statistical learning method. Here we use
# the bootstrap approach in order to assess the variability of the estimates
# for beta0 and beta1, the intercept and clope terms for the linear reg model
# that uses horsepower to predict mpg.

# first create function boot.fn() which takes in the Auto data set
# as well as the indices for the obs, and returns intercept and slope estimates
# for linear reg model. apply func to full set of 392 obs in order to compute
# estimates on beta0 and beta1. note: do not need { and } at the beginning
# and end because the func is ONLY 1 line long. 
boot.fn = function(data, index)
  return(coef(lm(mpg~horsepower, data=data, subset=index)))
boot.fn(Auto, 1:392)

# can also use boot.fn function to create bootstrap estimates for the intercept
# and slope terms by randomly sampling from among the obs with replacement. e.g.
set.seed(1)
boot.fn(Auto, sample(392,392,replace=T))

boot.fn(Auto, sample(392,392, replace=T))


# Next use boot() func to compute the SE's for the 1K bootstrap estimates
# for the intercept and slope terms
boot(Auto, boot.fn, 1000)

# > boot(Auto, boot.fn, 1000)
# 
# ORDINARY NONPARAMETRIC BOOTSTRAP
# 
# 
# Call:
#   boot(data = Auto, statistic = boot.fn, R = 1000)
# 
# 
# Bootstrap Statistics :
#   original      bias    std. error
# t1* 39.9358610  0.02972191 0.860007896
# t2* -0.1578447 -0.00030823 0.007404467


# this indicates that the bootstrap estimate for SE(B0) is 0.86 and that
# the bootstrap estimate for SE(B1) is 0.0074. Can also just use summary()

summary(lm(mpg~horsepower, data=Auto))$coef
# > summary(lm(mpg~horsepower, data=Auto))$coef
# Estimate  Std. Error   t value      Pr(>|t|)
# (Intercept) 39.9358610 0.717498656  55.65984 1.220362e-187
# horsepower  -0.1578447 0.006445501 -24.48914  7.031989e-81

# SE's obtained are 0.717 for intercept and 0.0064 for the slope. These are 
#  somewhat different from bootstrap estimates. 
# Bootstrap likely giving more accurate estimates of SEs than is summary() 
# for this non-linear data set.


#  finally compare both using quadratic model
boot.fn = function(data, index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn, 1000)

summary (lm(mpg~horsepower +I(horsepower ^2),data=Auto))$coef
library(MASS)
library(ISLR)

### Simple linear regression
names(Boston)

# note that we can run help on a data frame as below
?Boston

plot(medv~lstat, Boston)

# medv here is the response variable
fit1=lm(medv~lstat, data=Boston)

# provides the intercept and lstat coefficient
fit1

# provides more detail around the std. error and t- and p-values
summary(fit1)
abline(fit1, col='red')
names(fit1)

# find the confidence interval for fit1: default is the 5%-95% 
confint(fit1)

# predict with 3 values for lstat the prediction of the value as well as the lower
# confidence and upper confidence interval
predict(fit1, data.frame(lstat=c(5,10,15)), interval='confidence')

# Multiple Linear Regression
fit2 = lm(medv~lstat+age, data=Boston)
summary(fit2)

# use all other regressors by using the '.'
fit3 = lm(medv~.,Boston)
summary(fit3)  # age is now no longer predictive. 

par(mfrow=c(2,2))
plot(fit3)

# update is a useful function that updates a model fit3 in this case,
# removing variables age and indus by using the -'- sign.
fit4=update(fit3,~.-age-indus)
summary(fit4)

### Nonlinear terms and Interactions
fit5 = lm(medv~lstat*age, Boston)

# note that in the output the Interaction can be seen by lstat:age. it is significant
# in this case. 
summary(fit5)

# 'I' is an identity function -- further reading on this but it looks as though 
# the identity function ensures that the quadratic in this case is inserted 
# directly into the model
fit6 = lm(medv~lstat + I(lstat^2),Boston); summary(fit6)

attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat, fitted(fit6), col='red', pch=20)
fit7 = lm(medv~poly(lstat,4))
point(lstat, fitted(fit7), col='blue', pch=20)
plot(1:20, 1:20, pch=1:20, cex=2)



### Qualitative Predictors
fix(Carseats)
names(Carseats)
summary(Carseats)
fit1 = lm(Sales~. +Income:Advertising+Age:Price, Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc)

### Writing R Functions
regplot = function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col='red')
}

attach(Carseats)
regplot(Price,Sales)


# notice how ... works below
regplot = function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col='red')
}

regplot(Price, Sales, xlab='Price', ylab='Sales', col='blue', pch=20)

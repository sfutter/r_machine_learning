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
# removing variables age and indus
fit4=update(fit3,~.-age-indus)
summary(fit4)

# Exercise 5 - section 5.4 (ISLR)
# Set up data source and review file columns
set.seed(1)      # set seed to ensure replication is possible
str(Default)     # review columns
dim(Default)     # 10,000 rows and 4 columns
attach(Default)  # set up variables so do not need $ expansion

?Default
head(Default)

# the above was a little tedious as there is no predict() method for regsubsets(). Let's write out own predict() method.
predict.regsubsets=function(object, newdata, id, ...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object, id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}


# (a) Fit a logistic regression model that uses income and balance to predict default. 
glm.fit = glm(default~income+balance, data=Default, family=binomial) 
# glm.fit
# summary(glm.fit)

# (b) use the validation set approach to estimate the test error of the model. 
# i. Split the sample set into a training set and a validation set. 
#    sample() picks 5000 numbers from 10000
train=sample(10000,5000)
train

# ii. Fit a multiple logistic regression model using only the training observations. 
glm.fit = glm(default~income+balance, data=Default, family=binomial, subset=train) 
summary(glm.fit)

# iii. Obtain a prediction of default status for each individual in the validation set by 
#      computing the posterior probability of default for that individual, and classifying 
#      the individual to the default category if the posterior probability is greater than 0.5.
glm.probs = predict(glm.fit, newdata=Default[-train,], type='response')
glm.pred=ifelse(glm.probs > 0.5, 'Yes', 'No')

# iv. Compute the validation set error, which is the fraction of the observations in the validation 
# set that are misclassified. 
default.test = default[-train]
table(glm.pred,default.test)
mean(glm.pred==default.test) 
test.error = 1 - mean(glm.pred==default[-train]) 
test.error * 100


# (c) Repeat the process three more times, using three different splits of the observations, as below
# Split 2: train = 7500, test = 5000
# Split 3: train = 9000, test= 1000 
# Split 4: train = 9900, test= 100
set.seed(1)
train2=sample(10000,7500)
train3=sample(10000,9000)
train4=sample(10000,9900)

# fit the multiple logistic regression models using only the training observations. 
glm.fit2 = glm(default~income+balance, data=Default, family=binomial, subset=train2)
glm.fit3 = glm(default~income+balance, data=Default, family=binomial, subset=train3)
glm.fit4 = glm(default~income+balance, data=Default, family=binomial, subset=train4)

summary(glm.fit2)
summary(glm.fit3)
summary(glm.fit4)

# Obtain a prediction of default status for each individual in the validation set by 
# computing the posterior probability of default for that individual, and classifying 
# the individual to the default category if the posterior probability is greater than 0.5.
glm.probs = predict(glm.fit2, newdata=Default[-train2,], type='response')
glm.pred=ifelse(glm.probs > 0.5, 'Yes', 'No')
default.test = default[-train2]
table(glm.pred,default.test)
mean(glm.pred==default.test) 
test.error = 1 - mean(glm.pred==default[-train2]) 
test.error * 100

glm.probs = predict(glm.fit3, newdata=Default[-train3,], type='response')
glm.pred=ifelse(glm.probs > 0.5, 'Yes', 'No')
default.test = default[-train3]
table(glm.pred,default.test)
mean(glm.pred==default.test) 
test.error = 1 - mean(glm.pred==default[-train3]) 
test.error * 100


glm.probs = predict(glm.fit4, newdata=Default[-train4,], type='response')
glm.pred=ifelse(glm.probs > 0.5, 'Yes', 'No')
default.test = default[-train4]
table(glm.pred,default.test)
mean(glm.pred==default.test) 
test.error = 1 - mean(glm.pred==default[-train4]) 
test.error * 100





# d. Logistic Regression with Dummy Variable for 'student'
# sample() picks 9000 numbers from 10000
set.seed(1)
train=sample(10000,9000)

glm.fit5 = glm(default~income+balance+student, data=Default, family=binomial, subset=train) 
summary(glm.fit5)

glm.probs = predict(glm.fit5, newdata=Default[-train,], type='response')
glm.pred=ifelse(glm.probs > 0.5, 'Yes', 'No')

# evaluate performance of logistic regression model + dummy on test data
default.test = default[-train]
table(glm.pred,default.test)
mean(glm.pred==default.test) 
test.error = 1 - mean(glm.pred==default[-train]) 
test.error * 100






#######    Exercise 8 of Section 5.4
# We will now perform cross-validation on a simulated data set.
# (a) Generate a simulated data set as follows:
set.seed(1)
y=rnorm(100)
y
x=rnorm(100)
x
#?rnorm

y=x-2*x^2+rnorm (100)
#y
# In this data set, what is n and what is p? Write out the model
# used to generate the data in equation form.


# (b) Create a scatterplot of X against Y . Comment on what you find.
plot(x,y, main='Simulated Scatterplot of X vs Y taken from Standardized Normal Distribution')



# (c) Set a random seed, and then compute the LOOCV errors that
# result from fitting the following four models using least squares:

#   i. Y = β0 + β1X +
#   ii. Y = β0 + β1X + β2X2 +
#   iii. Y = β0 + β1X + β2X2 + β3X3 +
#   iv. Y = β0 + β1X + β2X2 + β3X3 + β4X4 + .
# Note you may find it helpful to use the data.frame() function
# to create a single data set containing both X and Y .

# first create the data.frame (df) using the simulated x and y values
set.seed(1)
x = rnorm(100)
y=x-2*x^2+rnorm (100)
df = data.frame(y,x)

# To automate the process, we use the for() function to initiate a for loop
# which iteratively fits polynomial regressions for polynomials of order i = 1 to 4, 
# computes the associated cross-validation error, and stores it in
# the ith element of the vector cv.error. 
cv.error = rep(0,4)                         # initialize the vector.
for (i in 1:4){
  glm.fit=glm(y~poly(x, i), data=df)
  cv.error[i]=cv.glm(df, glm.fit)$delta[1]  # delta[1] provides cv test error
}
cv.error                                    
plot(cv.error, main='4th order Polynomial MSEs')


# (d) Repeat (c) using another random seed, and report your results.
# Are your results the same as what you got in (c)? Why?
# first create the data.frame (df) using the simulated x and y values
set.seed(1000)
x = rnorm(100)
y=x-2*x^2+rnorm (100)
df = data.frame(y,x)

# the ith element of the vector cv.error. 
cv.error = rep(0,4)                         # initialize the vector.
for (i in 1:4){
  glm.fit=glm(y~poly(x, i), data=df)
  cv.error[i]=cv.glm(df, glm.fit)$delta[1]  # delta[1] provides cv test error
}
cv.error                                    
plot(cv.error, main='4th order Polynomial MSEs - Seed(1000)')


# (e) Which of the models in (c) had the smallest LOOCV error? Is
# this what you expected? Explain your answer.
cv.error


# (f) Comment on the statistical significance of the coefficient estimates
# that results from fitting each of the models in (c) using
# least squares. Do these results agree with the conclusions drawn
# based on the cross-validation results?
summary(glm.fit)





#### Exercise 8 of Section 6.8
# (a) Use the rnorm() function to generate a predictor X of length n = 100, as well as a noise vector of length n = 100.
x = rnorm(100)
e = rnorm(100)

# (b) Generate a response vector Y of length n = 100 according to the model Y = β0 + β1X + β2X^2 + β3X^3 + Error, 
#     where β0, β1, β2, and β3 are constants of your choice. 
y = 0.5 + x + 1.5*x^2 + 2*x^3 + e

# (c) Use the regsubsets() function to perform best subset selection in order to choose the best model containing 
# the predictors X, X2,...,X10. What is the best model obtained according to Cp, BIC, and adjusted R2? Show some 
# plots to provide evidence for your answer, and report the coefficients of the best model obtained. Note you will 
# need to use the data.frame() function to create a single data set containing both X and Y .
# create data.frame() with y and X polynomials 

x  = x
x2 = x^2
x3 = x^3
x4 = x^4
x5 = x^5
x6 = x^6
x7 = x^7
x8 = x^8
x9 = x^9
x10= x^10

df = data.frame(y,x,x2,x3,x4,x5,x6,x7,x8,x9,x10)
head(df)

library(leaps)
regfit.full=regsubsets(y~., data=df, nvmax=10)
reg.summary = summary(regfit.full)
reg.summary

par(mfrow=c(3,1))
# adjusted R-squared
plot(reg.summary$adjr2 ,xlab="Number of Variables ",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(4,reg.summary$adjr2[4],col='red', cex=2,pch=20) #plot red doc next to model with highest adjr2.

# similar way we can plot the Cp and BIC stats, indicating which.min()
plot(reg.summary$cp, xlab='Number of variables', ylab='Cp', type='l')
which.min(reg.summary$cp)
points(4,reg.summary$cp[4], col="red",cex=2,pch =20)

which.min(reg.summary$bic)
plot(reg.summary$bic, xlab='number of variables', ylab='BIC', type='l')
points(3, reg.summary$bic[3], col='red', cex=2, pch=20)

par(mfrow=c(3,1))
plot(regfit.full, scale='adjr2')
plot(regfit.full, scale='Cp')
plot(regfit.full, scale='bic')


# For the 6 variable model identified above we can see the coefficient estimates associated w model:
coef(regfit.full, 4) # AIC & Cp optimal model
coef(regfit.full, 3) # BIC optimal model

# (d) Repeat (c), using forward stepwise selection and also using backwards stepwise selection. 
# How does your answer compare to the results in (c)? 
# We can also use the regsubsets() func to perform forward and backward variable selection
regfit.fwd = regsubsets(y~., data=df, nvmax=10, method='forward')
summary(regfit.fwd)
regfit.bwd = regsubsets(y~., data=df, nvmax=10, method='backward')
summary(regfit.bwd)


# (e) Now fit a lasso model to the simulated data, again using X, X2, ...,X10 as predictors. 
#     Use cross-validation to select the optimal value of λ. Create plots of the cross-validation error 
#     as a function of λ. Report the resulting coefficient estimates, and discuss the results obtained. 
x = model.matrix(y~., df)
y

set.seed(1)
train=sample(1:nrow(df), nrow(df)/2)
test=(-train)
y.test=y[test]

# here we fit a lasso to the simulated data using x, x^2, ... x^10 as predictors. 
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=grid)

# from the plot() we can see that depending on the choice of tuning parameter, some coefs will be 
# exactly equal to zero.
par(mfrow=c(1,1))
plot(lasso.mod)

# Let's now use cross-validation to select the optimal value of λ.  
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)

# Let's look at the plots of cross-validation error and a function of lambda. 
plot(cv.out)

# using the lambda.min value from cv.out we can find the min value of lambda to be # 0.1071008 log (0.1071008) = -2.233985
bestlam=cv.out$lambda.min
bestlam # 0.1071008 log (0.1071008) = -2.233985
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)     # 0.9127635  = Test MSE

# Reporting resulting coefficient estimates
out = glmnet(x,y,alpha=1, lambda=grid)
lasso.coef=predict(out,type='coefficients',s=bestlam)
lasso.coef   # 3 out of 10 coef's are non-zero
lasso.coef[lasso.coef!=0]

# x, x2, x3, all have non-zero coefficients. The lasso has substantial advantages therefore over ridge regression in that the 
# resulting coefficient estimates are sparse. Here we see that 3 of the 10 coefficient estimates are exactly zero. The lasso model 
# with lambda chosen by cross-validation contains only 3 variables. 


# (f) Now generate a response vector Y according to the model Y = β0 + β7X^7 + error, and perform best 
#     subset selection and the lasso. Discuss the results obtained.

set.seed(1)
x = rnorm(100)
e = rnorm(100)

x  = x
x2 = x^2
x3 = x^3
x4 = x^4
x5 = x^5
x6 = x^6
x7 = x^7
x8 = x^8
x9 = x^9
x10= x^10



# generate a response vector Y according to the model Y = β0 + β7X^7 + error, and perform best 
y = 0.5 + 2*x^7 + e  # note: using β0=0.5 + β7=2. These numbers are chosen arbitrarily. The error term is model by norm dist with mean=0 and sd=1. 
df.f = data.frame(y,x,x2,x3,x4,x5,x6,x7,x8,x9,x10)
head(df.f)
summary(df.f)

# let's start by performing the best subset selection, as below
library(leaps)
regfit.full=regsubsets(y~., data=df.f, nvmax=10)
reg.summary = summary(regfit.full)
reg.summary

par(mfrow=c(3,1))
# adjusted R-squared
plot(reg.summary$adjr2 ,xlab="Number of Variables ",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(4,reg.summary$adjr2[4],col='red', cex=2,pch=20) #plot red doc next to model with highest adjr2.

# similar way we can plot the Cp and BIC stats, indicating which.min()
plot(reg.summary$cp, xlab='Number of variables', ylab='Cp', type='l')
which.min(reg.summary$cp)
points(2,reg.summary$cp[2], col="red",cex=2,pch =20)

plot(reg.summary$bic, xlab='number of variables', ylab='BIC', type='l')
which.min(reg.summary$bic)
points(1, reg.summary$bic[1], col='red', cex=2, pch=20)

par(mfrow=c(3,1))
plot(regfit.full, scale='adjr2')
plot(regfit.full, scale='Cp')
plot(regfit.full, scale='bic')

# For the 8 AND 5 variable models identified above we can see the coefficient estimates associated w model:
coef(regfit.full, 4) # adjr2 optimal model
coef(regfit.full, 2) # Cp optimal
coef(regfit.full, 1) # BIC optimal 


# COMPARISON WITH LASSO: Let's compare now the lasso optimal model.
# We fit a lasso model to the simulated data, again using X, X2, ...,X10 as predictors. 
# We use cross-validation to select the optimal value of λ and create plots of the cross-validation errors
# as a function of λ. We then report on the resulting coefficient estimates.
x = model.matrix(y~., df.f)
y=df.f$y

set.seed(1)
train=sample(1:nrow(df.f), nrow(df.f)/2)
test=(-train)
y.test=y[test]

# here we fit a lasso to the simulated data using x, x^2, ... x^10 as predictors. 
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=grid)
# from the plot() we can see that depending on the choice of tuning parameter, some coefs will be 
# exactly equal to zero.
par(mfrow=c(1,1))
plot(lasso.mod)


# Let's now use cross-validation to select the optimal value of λ.  
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)

# Let's look at the plots of cross-validation error and a function of lambda. 
plot(cv.out)

# using the lambda.min value from cv.out we can find the min value of lambda to be # 0.03126787. log (0.03126787) = -3.465164
bestlam=cv.out$lambda.min
bestlam # 4.635003 log (4.635003) = 1.533637
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)     # 9.444506  = Test MSE

# Reporting resulting coefficient estimates, and discuss the results obtained. 
out = glmnet(x,y,alpha=1, lambda=grid)
lasso.coef=predict(out,type='coefficients',s=bestlam)
lasso.coef   # exactly 9 zero coefs and ONLY 1 variable selected X^7
lasso.coef[lasso.coef!=0]

# x - x6 and x8-10 all have zero coefficients. The lasso has substantial advantages therefore over ridge regression in that 
# the resulting coefficient estimates are sparse. Here we see that 9 of the 10 coefficient estimates are exactly zero. 
# The lasso model with lambda chosen by cross-validation contains only 1 variable. 




##### Exercise 9 of Section 6.8
#  (a) split the data into a train and test set. 
head(College)
dim(College)
#?College
attach(College)

set.seed(1)
train=sample(777,600)    #approx 75% of the data set
test=(-train)
x = model.matrix(Apps~., College)
y = College$Apps
y.test=y[test]



# (b) Fit a linear model using least squares on the training set, and report the 
#     test error obtained. 

library(glmnet)
grid = 10^seq(10,-2,length=100)

ridge.mod = glmnet(x,y,alpha=0, lambda=grid)
dim(coef(ridge.mod))  # 19 100, 18 vars + 1 intercept = 19 rows. 100 cols for each lambda.

# least squares is a ridge regression with lambda = 0. 
ridge.pred = predict(ridge.mod, s=0, newx=x[test,], exact=T)
mean((ridge.pred-y.test)^2)   # MSE=1,060,712


# (c) ridge regression model with lambda chosen by cross-validation. Reporting test MSE obtained.
# it is a good idea to chose lambda via automated process which will minimize the MSE.
set.seed(1)
cv.out = cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam   #421.1785 lambda val

# therefore, we can see that the 
ridge.pred=predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2)    # MSE=1,179,980


# (d) Fit a lasso model on the training set, with λ chosen by cross-validation. Report the test error 
# obtained, along with the number of non-zero coefficient estimates. 
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out = cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)     # MSE=1,149,275

# number of non-zero test coef's
out=glmnet(x,y,alpha=1, lambda = grid)
lasso.coef=predict(out,type='coefficients',s=bestlam)[1:19,]
lasso.coef
names(lasso.coef[lasso.coef!=0])   #14 variables produced by lasso regression with non-zero coeffs

# (e) Fit a PCR model on the training set, with M chosen by cross-validation. Report the test error obtained, 
#     along with the value of M selected by cross-validation. 
# Principal components regression (PCR) can be performed using the pcr() function. Again, ensure that 
# the missing values have been removed from the data. 
# install.packages('pls')
library(pls)
set.seed(1)
pcr.fit = pcr(Apps~., data=College, scale=TRUE, validation='CV')

# The syntax for pcr() func is similar to that of lm(), with some additl options. 
# Set scale=TRUE to standardize each predictor, prior to generating PCs, 
# so that the scale on which each variable is measured will not have an effect. 
# Setting validation='CV' causes pcr() to compute the ten-fold cross-vali error for each
# possible value of M, the number of PCs used.
summary(pcr.fit)

# The CV score is provided for each possible number of components, raning from M=0 onwards. 
# Note that pcs() reports the RMSE in order to obtain the usual MSE, we must square this quantity.
# For e.g. RMSE of 352.8 corresponds to an MSE of 352.8^2 = 124,468. One can also plot the cross-vali
# scores using the validationplot() function. Using val.types='MSEP' will cause the cross-vali MSE
# to be plotted.
validationplot(pcr.fit,val.type = 'MSEP')

# we see that the smallest cross-validation error occurs when M=16 components are used. This is not
# much of a reduction from M = 19 (i.e. all components)
# From the plot we see that the cross-vali error is roughly the same when only one component is included 
# in the model. This suggests that a model that uses just a small number of components might suffice.

# The summary() func also provides the 'percentage of variance explained' in the predictors
# and in the response using different numbers of components. This is the amount of info about the predictors
# or response that is captured using M principal components. E.g. M = 1 only captures 38.31% of all 
# variance, or informatin, in the predictors. In contrast, M=6 increases the value to 88.63%. 
set.seed(1)
pcr.fit=pcr(Apps~., data=College, subset=train, scale=TRUE,validation='CV')
validationplot(pcr.fit, val.type = 'MSEP')

# Now we find the lowest cross-validatoin error occurs when M = 17 components are used. We compute the
# test MSE as follows:

head(x)
pcr.pred=predict(pcr.fit,x[test,],ncomp=17)
mean((pcr.pred-y.test)^2)   # ?????

# I was getting an erro of:
#   Error in predict.mvr(pcr.fit, x[test, ], ncomp = 17) : 
#   'newdata' does not have the correct number of columns
# Therefore -- re-do the creation of the test and train data set as follows

set.seed(1)
train=sample(c(TRUE,FALSE), nrow(College), rep=TRUE)
test=(!train)
College.train=College[train,,drop=F]
College.test = College[test,,drop=F]

set.seed(1)
pcr.fit = pcr(Apps~., data=College.train, scale=TRUE, validation='CV')
summary(pcr.fit)  # M = 17 for min 
pcr.pred=predict(pcr.fit, College.test, ncomp=17)
mean((pcr.pred-College.test$Apps)^2)   # MSE 1,520,331


# Finally we fit PCR on the full data set, using M=17, the number of components
# identified by cross-validation.
pcr.fit=pcr(y~x, scale=TRUE, ncomp=17)
summary(pcr.fit)


# (f) Fit a PLS model on the training set, with M chosen by cross-validation. Report the test error obtained, 
#     along with the value of M selected by cross-validation. 
set.seed(1)
pls.fit=plsr(Apps~., data=College, subset=train, scale=TRUE, validation='CV')
summary(pls.fit)
validationplot(pls.fit, val.type = 'MSEP')

# The lowest cross-validation error occurs when only M=17 partial least squares directions
# are used. Now eval the corresponding test set MSE
pls.pred = predict(pls.fit, College.test,ncomp=17)
mean((pls.pred-College.test$Apps)^2)   # 1,520,331

# The test MSE is comparable to, but slightly higher than, the test MSE obtained
# using ridge regression, the lasso, and PCR. 
# Finally we perform PLS using the full data set, using M=2, the number of components
# identified by cross -validation.
pls.fit=plsr(Apps~., data=College, scale=TRUE, ncomp=17)
summary(pls.fit)

# Notice that the percentage of variance in Salary that the two-component PLS fit
# explains 46.40%, is almost as much as that explained using the final seven-component
# PCR fit, 46.69%. This is because PCR only attemps to max the amount of variance
# explained in the predictors, while PLS search for directions that explain variance in 
# in both the predictors and the response. 

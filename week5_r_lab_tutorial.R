# ISLR - Chapter 6
# http://www-bcf.usc.edu/~gareth/ISL/ISLR%20Sixth%20Printing.pdf

# Section 6.5.1 - Best Subset Selection
library(ISLR)
#fix(Hitters)
names(Hitters)
dim(Hitters)

# is.na() function used to find missing obs. Returns true if any element is 
# missing, false otherwise. sum() of this counts all the missing elements.
sum(is.na(Hitters$Salary)) # 59 players have missing salary

# na.omit() removes all rows that have missing values in ANY variable
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary)) # 0 players found

# regsubsets() function performs best subset selection by identifying the 
# best model that contains a given number of predictors, where 'best' is
# quantified using RSS. Syntax is same as for lm()
# install.packages('leaps')
library(leaps)
regfit.full=regsubsets(Salary~., Hitters)
summary(regfit.full)

# asterix indicates that the best two-variable model contains only Hits and CRBI.
# By default regsubsets() only reports results up to the best eight-variable
# model. But the nvmax option can be used to return more vars. e.g.
regfit.full=regsubsets(Salary~., data=Hitters, nvmax=19)
reg.summary = summary(regfit.full)

# summary() func also returns R^2, RSS, adjusted R^2, Cp and BIC
names(reg.summary)

# e.g. reg.summary$rsq
# R^2 stat increase from 32% to almost 55% when all vars are included. 
reg.summary$rsq

# note that 'l' type tells R to connect the plotted points with lines
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab='Number of Variables', ylab='RSS', type='l')
plot(reg.summary$adjr2 ,xlab="Number of Variables ",ylab="Adjusted RSq",type="l")

# points() works like plot(), except it puts points on a plot that has already
# been created, instead of creating a new plot. 
# which.max function can be used to identify the location of the max point
# of a vector.
which.max(reg.summary$adjr2)

# can now plot a red dot to indicate the model with the largest R^2 stat
points(11,reg.summary$adjr2[11],col='red', cex=2,pch=20)

# similar way we can plot the Cp and BIC stats, indicating which.min()
plot(reg.summary$cp, xlab='Number of variables', ylab='Cp', type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10], col="red",cex=2,pch =20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab='number of variables', ylab='BIC', type='l')
points(6, reg.summary$bic[6], col='red', cex=2, pch=20)

# regsubsets() has a built-in plot() to dislpay selected vars for the best model
# with a given number of predictors, ranked according to BIC, Cp, adjusted R^2, 
# or AIC.  ?plot.regsubsets has more info.
?plot.regsubsets
par(mfrow=c(1,1))

# The black squares in the output below can be used to figure out which are the best model variables.
# e.g. for bic the 'best' (i.e. lowest bic value model) can be found with AtBat, Hits, Walkes, CRBI, 
# DivisionW, and PutOuts. This is a 6 Variable model. 
plot(regfit.full, scale='r2')
plot(regfit.full, scale='adjr2')
plot(regfit.full, scale='Cp')
plot(regfit.full, scale='bic')

# For the 6 variable model identified above we can see the coefficient estimates associated w model:
coef(regfit.full, 6)

# We can also use the regsubsets() func to perform forward and backward variable selection
regfit.fwd = regsubsets(Salary~., data=Hitters, nvmax=19, method='forward')
summary(regfit.fwd)
regfit.bwd = regsubsets(Salary~., data=Hitters, nvmax=19, method='backward')
summary(regfit.bwd)

# We see using the forward stepwise selection, the best one variable model contains only CRBI. THe best 2-var model adds Hits. 
# where possible use 'best', but if not computationally possible then use the forward/backward selection algos. 
coef(regfit.full, 6)
coef(regfit.fwd, 6)
# mdoels the same up to 7 vars
coef(regfit.full, 7)
coef(regfit.fwd, 7)

# CHOOSING AMONG MODELS USING THE VALIDATION SET APPROACH AND CROSS-VALIDATION
set.seed(1)
train = sample(c(TRUE,FALSE), nrow(Hitters), rep=TRUE)
test = (!train)

# now we apply regsubsets() to the training set in order to perform best subset selection
regfit.best = regsubsets(Salary~., data=Hitters[train,], nvmax=19)
test.mat = model.matrix(Salary~., data=Hitters[test,])

# model.matrix() function is used in many regression packages for building an X matrix from data.
val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best, id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}

# we find the best model is the one that contains 10 variables
val.errors
which.min(val.errors)
coef(regfit.best,10)

# the above was a little tedious as there is no predict() method for regsubsets(). Let's write out own predict() method.
predict.regsubsets=function(object, newdata, id, ...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object, id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

# we can show that the best 10 var model on the full data set differs from best-10 for the training data set. 
regfit.best = regsubsets(Salary~., data=Hitters, nvmax=19)
coef(regfit.best, 10)

# let's choose among models of diff sizes using cross-validation. k-folds
k = 10
set.seed(1)
folds=sample(1:k, nrow(Hitters), replace=TRUE)
cv.errors=matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))

# now write a for loop that performs the cross-validation. In the jth fold, the elements of folds that equal j are in the test set,
# and the remainder are in the training set.
for(j in 1:k){
  best.fit=regsubsets (Salary~.,data=Hitters [folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict (best.fit ,Hitters [folds ==j,],id=i)
    cv.errors[j,i]= mean( ( Hitters$Salary[ folds==j]-pred)^2)
  }
}

# this has given us a 10x19 matrix fo which the (i,j)th element corresponds to the test MSE
mean.cv.errors=apply(cv.errors, 2, mean)  # 2 in apply() indicates that you want to apply 'mean' for each columns
mean.cv.errors
plot(mean.cv.errors, type='b')

# from the output we can see that cross-validation selects an 11-variable model. 
reg.best = regsubsets(Salary~., data=Hitters, nvmax=19)
coef(reg.best, 11)



## LAB 2: RIDGE REGRESSION and the LASSO
x = model.matrix(Salary~., Hitters)[,-1]
y=Hitters$Salary
head(Hitters)
head(x)

# model.matrix() function is useful in that it automatically transforms qualitative variables into dummy variables.
# glmnet() ONLY takes quantitative inputs.

# RIDGE REGRESSION
# alpha = 0, ridge regression model is fit. 
# alpha = 1, lasso model is fit.
#install.packages('glmnet')
library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0, lambda=grid)  # alpha = 0, then ridge regression model is fit. alpha=1: lasso
# Note that by default the glmnet() function standardizes the variables so that they are on the same scale.

# associated with each value of lamdba is a vector of ridge regression coefficients, stored in a matrix that
# can be accessed by coef(). In this case a 20x100 matrix. 20 rows (1 for each predictor, plus an intercept)
# and 100 columns (one for each value of lambda)
dim(coef(ridge.mod))  # 20 100
ridge.mod$lambda[50]  # lambda = 11498

# we expect the coef estimates to be much smaller, in terms of l2 norm, when a large value of lambda
# these are the coeficients when lambda=11,498, along with their l2 norm:
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))  # 6.36

# by comparison here are the coefficients associated with smaller values of lambda.
ridge.mod$lambda[60]   # lambda = 705
coef(ridge.mod)[,60]   # by comparison the coefficients are larger with the smaller value of lambda.
sqrt(sum(coef(ridge.mod)[-1,60]^2))  # 57.1

# we can use the predict() func for a number of purposes. E.g. obtain the ridge regression coef's 
# for a new value of lambda, say 50. 
predict(ridge.mod, s=50, type='coefficients')[1:20,]

# Let's split the training and test set in order to estimate the test error of ridge regression and the 
# lasso. There are two common ways to randomly split a data set. First is to produce a random vector of 
# TRUE, FALSE elements and select the observations corresponding to TRUE for the training set. 2nd is to 
# randomly choose a subset of numbers between 1 and n; these can then be used as the indices for the train
# obs. Here let's do the latter. 

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

# fit the ridge regression model on the training set, evaluate the MSE on the test set, using lambda=4
ridge.mod=glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred-y.test)^2)  # 101037
# the test MSE is 101037. 

# Note that if we had instead fit a model with JUST an INTERCEPT, we would have predicted each test obs
# using the mean of the training obs, as follows:
mean((mean(y[train])-y.test)^2)  # 193253

# we could also get the same result by fitting a ridge regression model with a very large value of lambda.
# note that 1e10 means 10^10 (10 to the power of 10)
ridge.pred=predict(ridge.mod, s=1e10, newx=x[test,])
mean((ridge.pred-y.test)^2)  #193253 SAME as above..

# not surprisingly, fitting a ridge model with lambda=4 leads to a much lower test MSE than fitting a model 
# with just an intercept. Now let's check to see if there is any benefit to performing a ridge regression
# with a lambda = 4 vs just performing a least squares regression. 
# LEAST SQUARES IS A RIDGE REGRESSION WITH LAMBDA = 0.
ridge.pred = predict(ridge.mod, s=0, newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)   # 114783.1 MSE on test data set

# you will see the output of the below is the same! 
lm(y~x, subset=train)
predict(ridge.mod, s=0, exact=T, type='coefficients')[1:20,]

# general: we should fit a (unpenalized) least squares model, then we should use the lm() function, 
# since that func provides more useful outputs, such as standard errors and p-values for the coefficients.
# general: instead of arbitrarily choosing a tuning parameter, lambda, we use cross-validation to choose
# a tuning parameter lambda. As below:
set.seed(1)
cv.out = cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam  # 212

# Therefore, value of lambda that results in the smallest cross validation error is MSE = 212. 
# What is the test MSE associated with this value of lambda?
ridge.pred=predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2)   # 96015.51 is lower than test MSE when lambda=4 (test MSE = 101037)

# finally, we refit the ridge regression model on the full data set, using the value of lambda chosen 
# by the cross-validation, and examine the coefficient estimates.
out = glmnet(x,y,alpha=0)
predict(out, type='coefficients',s=bestlam)[1:20,]
# as expected, none of the coefs are zero. Ridge reg does NOT perform variable selection! 


### THE LASSO
# With Ridge regression a wise choice of lamdba can outperform least squares as well as the null model 
# Lasso can yeild either more accurate or more interpretable model than ridge since close to zero coef 
# vals can be set zero. Hers alpha = 1... 
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=grid)

# from the plot() we can see that depending on the choice of tuning parameter, some coefs will be 
# exactly equal to zero.
plot(lasso.mod)

# Perform cross-validation and compute the associated test error.
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)     # 100743

# this is substantially lower than the test set MSE of the null model and of least squares, and 
# very similar to the test MSE of ridge regression with lambda chosen by cross-validation.
# However, lasso has a substantialy advantage over ridge regression in that the resulting coef
# estimates are sparse. Here we see that 12 of the 19 coef estimates are excatly zero. The lasso model
# with lamdba chosen by cross-validation contains ONLY 7 vars.
out = glmnet(x,y,alpha=1, lambda=grid)
lasso.coef=predict(out,type='coefficients',s=bestlam)[1:20,]
lasso.coef # 12 exactly zero coefs
lasso.coef[lasso.coef!=0]






##### PCR and PLR Regression
##### PRINCIPAL COMPONENTS REGRESSION

# Principal components regression (PCR) can be performed using the pcr() function. Again, ensure that 
# the missing values have been removed from the data. 
# install.packages('pls')
library(pls)
set.seed(2)
pcr.fit = pcr(Salary~., data=Hitters, scale=TRUE, validation='CV')

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
pcr.fit=pcr(Salary~., data=Hitters, subset=train, scale=TRUE,validation='CV')
validationplot(pcr.fit, val.type = 'MSEP')

# Now we find the lowest cross-validatoin error occurs when M = 7 components are used. We compute the
# test MSE as follows:
pcr.pred=predict(pcr.fit, x[test,], ncomp=7)
mean((pcr.pred-y.test)^2)   # 96556  

# this test set MSE is competitive with the results obtained using ridge regression
# and the lasso. However, the way PCR is implemented, the final model is more difficult
# to interpret because it does not perform any kind of variable selection or even
# directly produce coef estimates. 

# Finally we fit PCR on the full data set, using M=7, the number of components
# identified by cross-validation.
pcr.fit=pcr(y~x, scale=TRUE, ncomp=7)
summary(pcr.fit)



##### PARTIAL LEAST SQUARES
set.seed(1)
pls.fit=plsr(Salary~., data=Hitters, subset=train, scale=TRUE, validation='CV')
summary(pls.fit)
validationplot(pls.fit, val.type = 'MSEP')

# The lowest cross-validation error occurs when only M=2 partial least squares directions
# are used. Now eval the corresponding test set MSE
pls.pred = predict(pls.fit, x[test,],ncomp=2)
mean((pls.pred-y.test)^2)   # 101417

# The test MSE is comparable to, but slightly higher than, the test MSE obtained
# using ridge regression, the lasso, and PCR. 
# Finally we perform PLS using the full data set, using M=2, the number of components
# identified by cross -validation.
pls.fit=plsr(Salary~., data=Hitters, scale=TRUE, ncomp=2)
summary(pls.fit)

# Notice that the percentage of variance in Salary that the two-component PLS fit
# explains 46.40%, is almost as much as that explained using the final seven-component
# PCR fit, 46.69%. This is because PCR only attemps to max the amount of variance
# explained in the predictors, while PLS search for directions that explain variance in 
# in both the predictors and the response. 
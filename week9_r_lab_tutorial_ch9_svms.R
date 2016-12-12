# Notes copied from ISLR: http://www-bcf.usc.edu/~gareth/ISL/ISLR%20Sixth%20Printing.pdf
# Min time for alteration to my own notes for this chapter. Text below is 99% copy of ISLR.

##### ch 9: SUPPORT VECTOR MACHINES (SVM)

### SUPPORT VECTOR CLASSIFIER

# We use the svm() function to fit the support vector classifier for a
# given value of the cost parameter. Here we demonstrate the use of this
# function on a two-dimensional example so that we can plot the resulting
# decision boundary. We begin by generating the observations, which belong
# to two classes, and checking whether the classes are linearly separable.

set.seed(1)
x=matrix(rnorm (20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
plot(x, col=(3-y))


# They are not. Next, we fit the support vector classifier. Note that in order
# for the svm() function to perform classification (as opposed to SVM-based
# regression), we must encode the response as a factor variable. We now
# create a data frame with the response coded as a factor.

#install.packages('e1071')
dat=data.frame(x=x, y=as.factor(y))
library(e1071)
svmfit=svm(y~., data=dat, kernel ="linear", cost=10,scale=FALSE)

# The argument scale=FALSE tells the svm() function not to scale each feature
# to have mean zero or standard deviation one; depending on the application,
# one might prefer to use scale=TRUE.
# We can now plot the support vector classifier obtained:
plot(svmfit , dat)



# The decision boundary between the two classes is linear (because we
# used the argument kernel="linear"), though due to the way in which the
# plotting function is implemented in this library the decision boundary looks
# somewhat jagged in the plot. We see that in this case ONLY ONE OBSERVATION IS 
# MISCLASSIFIED. (Note that here the second feature is plotted on the x-axis
# and the first feature is plotted on the y-axis, in contrast to the behavior of
# the usual plot() function in R.) The support vectors are plotted as crosses
# and the remaining observations are plotted as circles; we see here that there
# are seven support vectors. We can determine their identities as follows:

svmfit$index

# 
# We can obtain some basic information about the support vector classifier
# fit using the summary() command:


summary(svmfit)

# below tells us that a linear kernel was used with cost=10, and
# that there were seven support vectors, four in one class and three in the
# other.

# > summary(svmfit)
# 
# Call:
#   svm(formula = y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  linear 
# cost:  10 
# gamma:  0.5 
# 
# Number of Support Vectors:  7
# 
# ( 4 3 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#   -1 1




# What if we instead used a smaller value of the cost parameter? Note:
# A cost argument allows us to specify the cost of
# a violation to the margin. When the cost argument is small, then the margins
# will be wide and many support vectors will be on the margin or will
# violate the margin. When the cost argument is large, then the margins will
# be narrow and there will be few support vectors on the margin or violating
# the margin.

svmfit=svm(y~., data=dat , kernel ="linear", cost =0.1, scale=FALSE)
plot(svmfit , dat)
svmfit$index

# > svmfit$index
# [1]  1  2  3  4  5  7  9 10 12 13 14 15 16 17 18 20

# Now that a smaller value of the cost parameter is being used, we obtain a
# larger number of support vectors, because the margin is now wider. Unfortunately,
# the svm() function does not explicitly output the coefficients of
# the linear decision boundary obtained when the support vector classifier is
# fit, nor does it output the width of the margin.

# The e1071 library includes a built-in function, tune(), to perform cross- tune() validation. By default, tune() performs ten-fold cross-validation on a set
# of models of interest. In order to use this function, we pass in relevant
# information about the set of models that are under consideration. The
# following command indicates that we want to compare SVMs with a linear
# kernel, using a range of values of the cost parameter.

set.seed(1)
tune.out=tune(svm ,y~.,data=dat ,kernel ="linear",
              ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100) ))


summary(tune.out)

# > summary(tune.out)
# 
# Parameter tuning of ‘svm’:
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost
# 0.1
# 
# - best performance: 0.1 
# 
# - Detailed performance results:
#   cost error dispersion
# 1 1e-03  0.70  0.4216370
# 2 1e-02  0.70  0.4216370
# 3 1e-01  0.10  0.2108185
# 4 1e+00  0.15  0.2415229
# 5 5e+00  0.15  0.2415229
# 6 1e+01  0.15  0.2415229
# 7 1e+02  0.15  0.2415229

# We see that cost=0.1 (i.e. 1e-01) results in the lowest cross-validation error rate. The
# tune() function stores the best model obtained, which can be accessed as
# follows:

bestmod=tune.out$best.model
summary(bestmod)


# The predict() function can be used to predict the class label on a set of
# test observations, at any given value of the cost parameter. We begin by
# generating a test data set.

xtest=matrix(rnorm (20*2) , ncol=2)
ytest=sample (c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]= xtest[ytest==1,] + 1
testdat=data.frame(x=xtest , y=as.factor(ytest))


# Now we predict the class labels of these test observations. Here we use the
# best model obtained through cross-validation in order to make predictions.
ypred=predict (bestmod ,testdat)
table(predict =ypred , truth=testdat$y )

# Thus, with this value of cost, 19 of the test observations are correctly
# classified. What if we had instead used cost=0.01?
svmfit=svm(y~., data=dat , kernel ="linear", cost =.01,scale=FALSE)
ypred=predict (svmfit ,testdat )
table(predict =ypred , truth=testdat$y )


# In this case one additional observation is misclassified.
# Now consider a situation in which the two classes are linearly separable.
# Then we can find a separating hyperplane using the svm() function. We
# first further separate the two classes in our simulated data so that they are
# linearly separable:


x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch =19)


# Now the observations are just barely linearly separable. We fit the support
# vector classifier and plot the resulting hyperplane, using a very large value
# of cost so that no observations are misclassified.

dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~., data=dat , kernel ="linear", cost=1e5)
summary(svmfit)

plot(svmfit , dat)

# No training errors were made and only three support vectors were used.
# However, we can see from the figure that the margin is very narrow (because
# the observations that are not support vectors, indicated as circles, are very
# close to the decision boundary). It seems likely that this model will perform
# poorly on test data. We now try a smaller value of cost:

svmfit=svm(y~., data=dat , kernel ="linear", cost=1)
summary(svmfit)
plot(svmfit ,dat)

# Using cost=1, we misclassify a training observation, but we also obtain
# a much wider margin and make use of seven support vectors. It seems
# likely that this model will perform better on test data than the model with
# cost=1e5.







# ### SUPPORT VECTOR MACHINE
# In order to fit an SVM using a non-linear kernel, we once again use the svm()
# function. However, now we use a different value of the parameter kernel.
# To fit an SVM with a polynomial kernel we use kernel="polynomial", and
# to fit an SVM with a radial kernel we use kernel="radial". In the former
# case we also use the degree argument to specify a degree for the polynomial
# kernel (this is d in (9.22)), and in the latter case we use gamma to specify a
# value of γ for the radial basis kernel (9.24).
# We first generate some data with a non-linear class boundary, as follows:

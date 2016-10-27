# http://www-bcf.usc.edu/~gareth/ISL/ISLR%20Sixth%20Printing.pdf
# Lab: Decision Trees

# install.packages('tree')
# The tree library is used to construct classification and regression trees.
library(tree)

# Let's use classification trees to analyze the Carseats data.
# Sales is a continuous variable, so needs recoding as binary variable.
library(ISLR)
str(Carseats)
?Carseats 
attach(Carseats)

unique(Sales)
boxplot(Sales)
median(Sales)

# use ifelse() func to create new binary variable
High = ifelse(Sales<=8, "No", "Yes")

# merge Carseats with High Column
Carseats=data.frame(Carseats,High)

# now we use the tree() func to fit a classification tree in order
# to predict High using all variables but Sales. Syntax similar to lm()
tree.carseats=tree(High~.-Sales, Carseats)

# by typing the tree object in R directly, R prints output for each 
# branch of the tree. E.g. the the split criterio Price<92.5, 
# the number of observations in that branch, the deviance, the overall
# prediction for the branch (Yes or No), and the fraction of observatns
# in that branch that take on values Yes and NO. *=terminal node.
tree.carseats

# to accurately 'test' the model we need to split out the data into 
# a training and test data set.
dim(Carseats)
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales, Carseats,subset=train)
tree.pred=predict(tree.carseats, Carseats.test, type='class')
table(tree.pred, High.test)
(86+57)/200  # 71.5% accuracy

# training error rate is 9%
# note: the smaller the deviance reported the better the fit to the
#       training data.
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty=0)

# Tree Pruning
# This can lead to improved results potentially. cv.tree() performs
# cross validation in order to determine the optimal level of tree
# complexity. FUN=prune.missclass is used to indicate we want 
# classification error rate to guide the cross-vali and pruning process,
# rather than the default for cv.tree(), which is deviance.
set.seed(3)
cv.carseats=cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats
# from the output
# dev corresponds to cross-validation error rate (not deviance!)
# the tree with 9 terminal nodes results in the lowest cross-vali error
# with 50 cross-validation errors. k corresponds to alpha (see 8.4 equatn)

# can now plot the error as a function of both size and k
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type='b')
plot(cv.carseats$k, cv.carseats$dev, type='b')

# can now apply prune.misclass() to prune the tree to obtain the 9
# nine-node tree identified earlier as the lowest error rate.
prune.carseats=prune.misclass(tree.carseats, best=9)
plot(prune.carseats)
text(prune.carseats, pretty=0)

# how well does the pruned tree perform on the test data set?
# once again, apply predict() func.
tree.pred = predict(prune.carseats, Carseats.test, type='class')
table(tree.pred, High.test)
(94+60)/200   # 77% accuracy

# therefore - not just more easily interpreted, but also has better
# classification accuracy. 
# if we increase the value of 'best', we obtain a larger pruned tree
# with a lower classification accuracy
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred=predict(prune.carseats, Carseats.test, type='class')
table(tree.pred,High.test)
(86+62)/200    # 74%




#####     FITTING REGRESSION TREES     #####
library(MASS)
?Boston
set.seed(1)
train=sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
# note that the output of summary() indicates that only three of the 
# variables have been used in constructing the tree. In the context
# of a regression tree, the deviance is simply the sum of squared error
# for the tree. Lets plot it:
par(mfrow=c(1,1))
plot(tree.boston)
text(tree.boston, pretty=0)

# The tree indicates that lower values of lstat correspond to more expensive
# houses. The tree predicts a median house price of $46,600 for larger homes
# in the suburbs in which residents have high socioeconomic status (rm>7.437 and
# lstat<9.715).

# Can now use cv.tree() func to see whether pruning will improve performance.
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type='b')

prune.boston=prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)

# IMPORTANT: we use the unprune tree to make predictions on the test set. 
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,'medv']
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)
# i.e. the test set MSE associated with the regression tree is 25.05. 
# The square root of MSE is therefore around 5.005, indicating that this model
# leads to test predictions that are within around $5,005 of the true median
# home value for the suburb.



# BAGGING and RANDOM FORESTS
# Here we apply bagging and random forests to Boston data
# Bagging is a special case of random forest with m = p. 
# THerefore, randomForest() func can be used to perform both random
# forests and bagging. 

# reminder of data set
?Boston

# BAGGING
#install.packages('randomForest')
library(randomForest)
set.seed(1)
bag.boston = randomForest(medv~., data=Boston, subset=train, mtry=13, 
                          importance=TRUE)
bag.boston


# THe argument mtry=13 indicates that all 13 predictors should be considered 
# for each split of the tree - i.e. bagging should be done. 
# How well does the bagged model perform on the test set?
yhat.bag = predict(bag.boston, newdata = Boston[-train,])
boston.test=Boston[-train,'medv']
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
# The test MSE associated with the bagged regression tree is 13.47, which 
# is almost half that obtained using an optimally-pruned single tree.


# We can change the number of trees grown by randomForest() using the 
# ntree argument:
bag.boston = randomForest(medv~., data=Boston, subset=train, mtry=13,
                          ntree=25)
yhat.bag = predict(bag.boston, newdata = Boston[-train,])
mean((yhat.bag-boston.test)^2)  # 13.43


# Growing a random forest proceeds in the same way, except that we 
# use a smaller value of the mtry argument. By default randomForest()
# uses p/3 variables when building a random forest of regression trees,
# p^0.5 vars when building a random forest of classification trees.
# Lets try mtry=6
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,  subset=train,
                       mtry=6, importance=TRUE)
yhat.rf = predict(rf.boston, newdata = Boston[-train,])
mean((yhat.rf-boston.test)^2)  #11.48 = MSE on test set rf.

# Using the importance() function, we can view the importance of each 
# variable. %IncMSE is based upon the mean decrease of accuracy in 
# predictions on the out of bag samples when a given variable is 
# excluded from the model. IncNodePurity is a measure of the total 
# decrease in node purity that results from splits over that variable, 
# averaged over all trees
importance(rf.boston)
# see plots of importance measures:
varImpPlot(rf.boston)
# result: lstat and rm are by far the two most important variables. 
# In the case of regression trees, the node purity is measured by the 
# training RSS, and for classification trees by the deviance. 


# BOOSTING
# gbm package
# run gbm() with option distribution='gaussian' since this is a regression
# problem; if it were binary classification problem then we would use
# distribution='bernoulli'. Argument n.trees=5000 indicates that we want
# 5000 trees, and the option interaction.depth=4 limits the depth of
# each tree.
install.packages('gbm')
library(gbm)
set.seed(1)
boost.boston=gbm(medv~., data=Boston[train,],distribution='gaussian',
                 n.trees=5000,interaction.depth=4)

# the summary function produces relative influence plot and also
# outputs the relative influences statistics
summary(boost.boston)
# we see that lstat and rm are by far the most important variables. 

# We can also produce partial dependance plots for these two vars. 
# These plots illustrate the marginal effect of the selected variables 
# on the response after integrating out the other variables. In this 
# case, as we might expect, median house prices are increasing with rm
# and decreasing with lstat.
par(mfrow=c(1,2))
plot(boost.boston, i='rm')
plot(boost.boston, i='lstat')

# we now use the boosted model to predict medv on the test set:
yhat.boost = predict(boost.boston, newdata = Boston[-train,], 
                     n.trees=5000)
mean((yhat.boost-boston.test)^2)  # 11.8 test MSE
# test MSE obtained is 11.8; similar to the test MSE for random forests
# and superior to that for bagging. If we wnt to we can perform boosting
# with a different value of the shrinkage parameter, lambda.
# Default lambda value is 0.001.

boost.boston = gbm(medv~., data=Boston[train,], distribution = 'gaussian',
                   n.trees=5000, interaction.depth=4, shrinkage=0.2,
                   verbose=F)
yhat.boost = predict(boost.boston, newdata=Boston[-train,],
                     n.trees=5000)
mean((yhat.boost-boston.test)^2)  # 10.89
# using lambda = 0.2 leads to slightly lower test MSE than lambda=0.001.



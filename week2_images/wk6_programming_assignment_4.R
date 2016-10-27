# http://www-bcf.usc.edu/~gareth/ISL/ISLR%20Sixth%20Printing.pdf
# Lab: Decision Trees

# install.packages('tree')
# The tree library is used to construct classification and regression trees.
library(tree)
library(ISLR)
?Carseats

# Note: below 4 lines not part of question. We carry forward the
# Lab work into the homework assignment. The 'High' variable that was created, 
# as below: use ifelse() func to create new binary variable
attach(Carseats)

#     Exercise 8 of Section 8.4 
# (a) Split data into train and test set
set.seed(1)
train=sample(1:nrow(Carseats), nrow(Carseats)/2)    #use sample() func: 50% train-50% test

# (b) Fit a regression tree to the training set. Plot the tree, and interpret 
# the results. What test MSE do you obtain? 
tree.carseats=tree(Sales~.,Carseats,subset=train)
summary(tree.carseats)
# The output of summary() indicates that only four of the 
# variables have been used in constructing the tree: High, Price, ShelveLoc, and Age. 
# In the context of a regression tree, the deviance is simply the sum of squared error
# for the tree. So here the mean squared error is 2.36. 
par(mfrow=c(1,1))
plot(tree.carseats)
text(tree.carseats, pretty=0)

# The tree indicates that the variable added in the Lab, 'High' is the most predictive variable.
# For sales less thatn 8,000 units, Sales can be best predicted by splitting at Price <132.5.
# lower values of lstat correspond to more expensive
# houses. The tree predicts a median house price of $46,600 for larger homes
# in the suburbs in which residents have high socioeconomic status (rm>7.437 and
# lstat<9.715).

# The tree indicates that lower values of lstat correspond to more expensive
# houses. The tree predicts a median house price of $46,600 for larger homes
# in the suburbs in which residents have high socioeconomic status (rm>7.437 and
# lstat<9.715).


# Can now use cv.tree() func to see whether pruning will improve performance.
cv.carseats=cv.tree(tree.carseats)
plot(cv.carseats$size, cv.carseats$dev, type='b')

# question is have is how is # the 2 selected &&&&&&&&&&&&&&&&&&&&&&&&&
prune.carseats=prune.tree(tree.carseats, best=2)
plot(prune.carseats)
text(prune.carseats, pretty=0)

# IMPORTANT: we use the unprune tree to make predictions on the test set. 
yhat=predict(tree.carseats,newdata=Carseats[-train,])
carseats.test=Carseats[-train,'Sales']
plot(yhat,carseats.test)
abline(0,1)
mean((yhat-carseats.test)^2)
# i.e. the test set MSE associated with the regression tree is 2.7932
# The square root of MSE is therefore around 1.67, indicating that this model
# leads to test predictions that are within around $1,671 of the true median
# Sales 

(mean((yhat-carseats.test)^2)^0.5) # 1.671294



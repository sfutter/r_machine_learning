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
dim(Carseats)
set.seed(1)
train=sample(1:nrow(Carseats), nrow(Carseats)/2)    #use sample() func: 50% train-50% test
carseats.train=Carseats[train,]
carseats.test=Carseats[-train,'Sales']

# (b) Fit a regression tree to the training set. Plot the tree, and interpret 
# the results. What test MSE do you obtain? 
tree.carseats=tree(Sales~.,Carseats,subset=train) # fitting regression tree to training set.
summary(tree.carseats)                            
par(mfrow=c(1,1))
plot(tree.carseats)
text(tree.carseats, pretty=0)
# Calculate the Test MSE
yhat=predict(tree.carseats,newdata=Carseats[-train,])
mean((yhat-carseats.test)^2)          # 4.148897

(mean((yhat-carseats.test)^2)^0.5)    # 2.036884
# i.e. the test set MSE associated with the regression tree is 4.148897
# The square root of MSE is therefore around 2.036884, indicating that this model
# leads to test predictions that are within around $2,037 of the true median
# Sales 



# (c) Use cv.tree() func to see whether pruning will improve performance.
set.seed(1)
cv.carseats=cv.tree(tree.carseats)
plot(cv.carseats$size, cv.carseats$dev, type='b')

# based on the plot of size vs deviance, it looks like 8 nodes the optimal tree size
prune.carseats=prune.tree(tree.carseats, best=8)
plot(prune.carseats)
text(prune.carseats, pretty=0)

# calculated predicted response using pruned tree as classifier for the Test data
yhat=predict(prune.carseats,newdata=Carseats[-train,])
carseats.test=Carseats[-train,'Sales']
plot(yhat,carseats.test)
abline(0,1)
mean((yhat-carseats.test)^2)         # MSE = 5.09085
(mean((yhat-carseats.test)^2)^0.5)   # MSE = 2.256291
# i.e. the pruned tree MSE associated with the regression tree is 5.09085
# The square root of MSE is therefore around 2.256291, indicating that this model
# leads to test predictions that are within around $2,256 of the true median
# Sales 


# (d) Here we use the bagging approach to analyze the data. We calculate the test MSE, 
#     then use the importance() function to determine which variables are the most important.
# BAGGING
#install.packages('randomForest')
library(randomForest)
set.seed(1)
dim(Carseats) # 400 x 11 

# THe argument mtry=10 indicates that all 10 predictors should be considered 
# for each split of the tree - i.e. bagging should be done. 
bag.carseats = randomForest(Sales~., data=Carseats, subset=train, mtry=10, importance=TRUE)
bag.carseats

# How well does the bagged model perform on the test set?
yhat.bag = predict(bag.carseats, newdata = Carseats[-train,])
carseats.test=Carseats[-train,'Sales']
plot(yhat.bag, carseats.test)
abline(0,1)
mean((yhat.bag-carseats.test)^2)          # MSE = 2.640977
(mean((yhat.bag-carseats.test)^2)^0.5)    #1.625108
# The test MSE associated with the bagged regression tree is 2.640977, which 
# is almost half that obtained using an optimally-pruned single tree.

# Use the importance() functon to determine which variables are most important
# %IncMSE is based upon the mean decrease of accuracy in 
# predictions on the out of bag samples when a given variable is 
# excluded from the model. IncNodePurity is a measure of the total 
# decrease in node purity that results from splits over that variable, 
# averaged over all trees
importance(bag.carseats)
# see plots of importance measures:
varImpPlot(bag.carseats)
# result: Price and ShelveLoc are by far the two most important variables. 
# In the case of regression trees, the node purity is measured by the 
# training RSS, and for classification trees by the deviance. 


# (e) Use Random Forests to analyze the data and calculate test MSE.
# Growing a random forest proceeds in the same way, except that we 
# use a smaller value of the mtry argument. By default randomForest()
# uses p/3 variables when building a random forest of regression trees,
# p^0.5 vars when building a random forest of classification trees.
# Lets try mtry=3 (i.e. 10/3 = 3.33)
set.seed(1)
rf.carseats=randomForest(Sales~.,data=Carseats,  subset=train,
                       mtry=3, importance=TRUE)
yhat.rf = predict(rf.carseats, newdata = Carseats[-train,])
mean((yhat.rf-carseats.test)^2)        # 3.307= MSE on test set RF when mtry=3
(mean((yhat.rf-carseats.test)^2)^0.5)  # 1.818689

# Using the importance() function, we can view the importance of each 
# variable. %IncMSE is based upon the mean decrease of accuracy in 
# predictions on the out of bag samples when a given variable is 
# excluded from the model. IncNodePurity is a measure of the total 
# decrease in node purity that results from splits over that variable, 
# averaged over all trees
importance(rf.carseats)
# see plots of importance measures:
varImpPlot(rf.carseats)
# result: lstat and rm are by far the two most important variables. 
# In the case of regression trees, the node purity is measured by the 
# training RSS, and for classification trees by the deviance. 



#### EXERCISE 9 of SECTION 8.4 ####
?OJ

# (a) Create a training set containing a random sample of 800 observations, and a 
#     test set containing the remaining observations. 
dim(OJ)   # 1070 x 8
set.seed(2)
train = sample(1:nrow(OJ),800)
oj.test = OJ[-train,]
oj.train = OJ[train,]


# (b) Fit a tree to the training data, with Purchase as the response and the other 
# variables as predictors. Use the summary() function to produce summary statistics 
# about the tree, and describe the results obtained. What is the training error rate? 
# How many terminal nodes does the tree have? 
attach(OJ)
# now we use the tree() func to fit a classification tree in order
# to predict Purchase using all variables. Syntax similar to lm():
tree.oj = tree(Purchase~.,OJ)
summary(tree.oj)

# (c) Type in the name of the tree object in order to get a detailed text output. 
#     Pick one of the terminal nodes, and interpret the information displayed. 
# By typing the tree object in R directly, R prints output for each 
# branch of the tree. E.g. the the split criterio Price<92.5, 
# the number of observations in that branch, the deviance, the overall
# prediction for the branch (Yes or No), and the fraction of observations
# in that branch that take on values Yes and NO. *=terminal node.
tree.oj

# (d) plot the tree
plot(tree.oj)
text(tree.oj, pretty=0)

# (e) Predict the response on the test data, and produce a confusion matrix 
#     comparing the test labels to the predicted test labels. What is the test 
#     error rate? 

set.seed(2)
tree.oj=tree(Purchase~., OJ, subset=train)
tree.pred=predict(tree.oj, oj.test, type='class')
table(tree.pred, oj.test$Purchase)
(161+69)/270  # 85.19% accuracy
(28+12)/270   # 14.81% error


# (f) apply cv.tree() function to determine optimal tree size. 
# This can lead to improved results potentially. cv.tree() performs
# cross validation in order to determine the optimal level of tree
# complexity. FUN=prune.missclass is used to indicate we want 
# classification error rate to guide the cross-validation and pruning process,
# rather than the default for cv.tree(), which is deviance.
set.seed(2)
cv.oj=cv.tree(tree.oj, FUN=prune.misclass)
names(cv.oj)
cv.oj
# from the output
# dev corresponds to cross-validation error rate (not deviance!)
# the tree with 8 & 7 terminal nodes results in the lowest cross-vali error
# with 145 cross-validation errors. k corresponds to alpha (see 8.4 equatn)

# can now plot the error as a function of size
par(mfrow=c(1,1))
plot(cv.oj$size, cv.oj$dev, type='b')

# (i) Produce a pruned tree corresponding to the optimal tree size obtained using 
# cross-validation.
# can now apply prune.misclass() to prune the tree to obtain the 7
# 7-node tree identified earlier as the lowest error rate.
prune.oj=prune.misclass(tree.oj, best=7)
plot(prune.oj)
text(prune.oj, pretty=0)

# (j)  Compare the training error rates between the pruned and unpruned trees. 
# how well does the pruned tree perform on the test data set?
# once again, apply predict() func.
tree.pred = predict(prune.oj, oj.train, type='class')
table(tree.pred, oj.train$Purchase)
(440+226)/800  # 83.25% accuracy
(40+94)/800    # 16.75% error

# in comparison to unprune training cross-validation error
tree.pred=predict(tree.oj, oj.train, type='class')
table(tree.pred, oj.train$Purchase)
(440+226)/800  # 83.25% accuracy
(40+94)/800    # 16.75% error


# (k) Compare the test error rates between the pruned and unpruned trees. 
tree.pred = predict(prune.oj, oj.test, type='class')
table(tree.pred, oj.test$Purchase)
(161+69)/270  # 85.19% accuracy
(28+12)/270   # 14.81% error

# in comparison to unpruned test cross-validation error
tree.pred=predict(tree.oj, oj.test, type='class')
table(tree.pred, oj.test$Purchase)
(161+69)/270  # 85.19% accuracy
(28+12)/270   # 14.81% error





### EXERCISE 9 of Section 10.7
# a. Using hierarchical clustering with complete linkage and Euclidean distance, 
#    cluster the states. 
head(USArrests)
hc.complete= hclust(dist(USArrests), method='complete')
?USArrests
# plot dendrogram
par(mfrow=c(1,1))
plot(hc.complete ,main="Complete Linkage ", xlab="", sub="",
     cex=.9)

# b.  Cut the dendrogram at a height that results in three distinct clusters. 
#     Which states belong to which clusters? 
# To determine cluster labels for each obs associated with cut of dendrogram
# we can use the cutree() func:
cutree(hc.complete, 3)


# c. Hierarchically cluster the states using complete linkage and Euclidean 
#    distance, after scaling the variables to have standard deviation one. 
xsc=scale(USArrests)
plot(hclust(dist(xsc), method ="complete"), main="Hierarchical 
     Clustering with Scaled Features")

# (d) What effect does scaling the variables have on the hierarchical 
#     clustering obtained? In your opinion, should the variables be scaled 
#     before the inter-observation dissimilarities are computed? Provide a 
#     justification for your answer.




##### Exercise 10 of Section 10.7
# (a) Generate a simulated data set with 20 observations in each of three classes 
#     (i.e. 60 observations total), and 50 variables. 
# Hint: There are a number of functions in R that you can use to generate data. 
# One example is the rnorm() function; runif() is another option. Be sure to add a mean 
# shift to the observations in each class so that there are three distinct classes. 
set.seed(1)
x=matrix(rnorm(20*3*50), ncol=50)
dim(x) # 60 x 50 matrix created. 

# add mean shift to observations 
x[1:20,  1]=x[1:20,  1] + 5
x[21:40, 2]=x[21:40, 2] 
x[41:60, 3]=x[41:60, 3] - 5
true.labels <- c(rep(1, 20), rep(2, 20), rep(3, 20))

#(b) Perform PCA on the 60 observations and plot the first two principal component score vectors. 
# Use a different color to indicate the observations in each of the three classes. If the three 
# classes appear separated in this plot, then continue on to part (c). If not, then return to part 
# (a) and modify the simulation so that there is greater separation between the three classes. 
# Do not continue to part (c) until the three classes show at least some separation in the 
# first two principal component score vectors. 

# perform PCA on x
pr.out=prcomp(x)

# create color coding for each class (i.e. for true.labels vector created above)
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

# plot first two PC score vectors
par(mfrow=c(1,1))
plot(pr.out$x[,1:2], col=Cols(true.labels), pch=19, xlab="Z1",ylab="Z2")


# (c) Perform K-means clustering of the observations with K = 3. How well do the 
#     clusters that you obtained in K-means clustering compare to the true class 
#     labels? Hint: You can use the table() function in R to compare the true 
#     class labels to the class labels obtained by clustering. Be careful how you 
#     interpret the results: K-means clustering will arbitrarily number the 
#     clusters, so you cannot simply check whether the true class labels and 
#     clustering labels are the same. 

# Perform K-means clustering with K=3
set.seed(1)
km.out=kmeans(x,3,nstart=20)
table(km.out$cluster,true.labels)

#(d) Perform K-means clustering with K = 2. Describe your results.
# Perform K-means clustering with K=2
set.seed(1)
km.out=kmeans(x,2,nstart=20)
table(km.out$cluster, true.labels)

#(e) Now perform K-means clustering with K = 4, and describe your results. 
set.seed(1)
km.out=kmeans(x,4,nstart=20)
table(km.out$cluster, true.labels)
plot(x, col=(km.out$cluster+1), main='K-Means clustering results K=4',
     xlab="", ylab="", pch=20, cex=2)


#(f) Now perform K-means clustering with K = 3 on the first two principal 
# component score vectors, rather than on the raw data. That is, perform K-means 
# clustering on the 60 × 2 matrix of which the first column is the first principal 
# component score vector, and the second column is the second principal component 
# score vector. Comment on the results. 
set.seed(1)
km.out=kmeans(pr.out$x[,1:2],3,nstart=20)
table(km.out$cluster, true.labels)


#(g) Using the scale() function, perform K-means clustering with K = 3 on the 
# data after scaling each variable to have standard deviation one. How do these 
# results compare to those obtained in (b)? Explain.
set.seed(1)
km.out=kmeans(scale(x),3,nstart=20)
table(km.out$cluster,true.labels)


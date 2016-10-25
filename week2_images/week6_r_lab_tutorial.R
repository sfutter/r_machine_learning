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
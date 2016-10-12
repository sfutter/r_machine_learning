library(ISLR)
data(Auto)
attach(Auto) # to save using $ expansion on vars

# Exercise 9 - 3.7
# a 
pairs(Auto)

# b. -9 excludes col 9 as this is categorical (qualitative)
cor(Auto[,-9])

# c
# use all other regressors by using the '.', then remove name. Print results with summary()
fit1 = lm(mpg~.-name,Auto)
summary(fit1)  

# d
# produce matrix of fit plots
par(mfrow=c(2,2))
plot(fit1)

# leverage plot identifies the index of the largest element of a vector. In this case it is 14.
par(mfrow=c(1,1))
plot(hatvalues (fit1))
which.max(hatvalues (fit1))

# e. Use the * and : symbols to fit linear regression models with interaction 
# effects. 
fit1 = lm(mpg~weight+horsepower+weight*horsepower)
summary(fit1)

fit1 = lm(mpg~displacement+acceleration+weight+horsepower+acceleration:displacement+weight:horsepower)
summary(fit1)


# f. Lets try taking the square of acceleration 
lm.fit1 = lm(mpg~weight+horsepower+acceleration+I(horsepower^2))
summary(lm.fit1)
plot(lm.fit1)

# and the log transformation of horsepower
lm.fit2 = lm(mpg~weight+horsepower+acceleration+log(horsepower))
summary(lm.fit2)
plot(lm.fit2)

# no transform
lm.fit3 = lm(mpg~weight+horsepower+acceleration)
summary(lm.fit3)
plot(lm.fit3)



# Exercise 15 - 3.7
# a 
library(MASS)
names(Boston)
str(Boston)
?Boston
attach(Boston)
lm.fit1 = lm(crim~zn)
lm.fit2 = lm(crim~indus)
lm.fit3 = lm(crim~chas)
lm.fit4 = lm(crim~nox)
lm.fit5 = lm(crim~rm)
lm.fit6 = lm(crim~age)
lm.fit7 = lm(crim~dis)
lm.fit8 = lm(crim~rad)
lm.fit9 = lm(crim~tax)
lm.fit10 = lm(crim~ptratio)
lm.fit11 = lm(crim~black)
lm.fit12 = lm(crim~lstat)
lm.fit13 = lm(crim~medv)

summary(lm.fit1)
summary(lm.fit2)
summary(lm.fit3)
summary(lm.fit4)
summary(lm.fit5)
summary(lm.fit6)
summary(lm.fit7)
summary(lm.fit8)
summary(lm.fit9)
summary(lm.fit10)
summary(lm.fit11)
summary(lm.fit12)
summary(lm.fit13)

# chas is the only insignificant variable here/ Let's create some plots to visualize
plot(zn,crim);  abline(lm.fit1)
plot(indus,crim);  abline(lm.fit2)
plot(tax,crim);  abline(lm.fit2)

# b - multiple linear regression
lm.fit=lm(crim~.,data=Boston)
summary (lm.fit)

# c plot of univariate coefficients (x-axis) against the multiple regression coefficients Yaxis
# extract coefficients from each univariate model using coef function
x = c(coef(lm.fit1)[[2]],coef(lm.fit2)[[2]],coef(lm.fit3)[[2]],coef(lm.fit4)[[2]],coef(lm.fit5)[[2]],
      coef(lm.fit6)[[2]],coef(lm.fit7)[[2]],coef(lm.fit8)[[2]],coef(lm.fit9)[[2]],coef(lm.fit10)[[2]],
      coef(lm.fit11)[[2]],coef(lm.fit12)[[2]],coef(lm.fit13)[[2]])
x

# extract coefficients from the regression model using coef function
y = c(coef(lm.fit)[[2]],coef(lm.fit)[[3]],coef(lm.fit)[[4]],coef(lm.fit)[[5]],coef(lm.fit)[[6]],
      coef(lm.fit)[[7]],coef(lm.fit)[[8]],coef(lm.fit)[[9]],coef(lm.fit)[[10]],coef(lm.fit)[[11]],
      coef(lm.fit)[[12]],coef(lm.fit)[[13]],coef(lm.fit)[[14]])
y

par(mfrow=c(1,1))
plot(x,y, xlab='Univariate regression coefficients from (a)', 
          ylab='Multiple regression coefficients from (b)', 
          main='Comparison between Univariate and Multiple Regression Coefficients')

# d - find evidence of non-linearity 
lm.fit1p  = lm(crim~poly(zn,4))       ; summary(lm.fit1p)
lm.fit2p  = lm(crim~poly(indus,4))    ; summary(lm.fit2p)
# not that chas only has 2 unique points (0 and 1). poly() degree must be less than #unique points.
#lm.fit3p  = lm(crim~poly(chas,4))     ; summary(lm.fit3p)
lm.fit4p  = lm(crim~poly(nox,4))      ; summary(lm.fit4p)
lm.fit5p  = lm(crim~poly(rm,4))       ; summary(lm.fit5p)
lm.fit6p  = lm(crim~poly(dis,4))      ; summary(lm.fit6p)
lm.fit7p  = lm(crim~poly(rad,4))      ; summary(lm.fit7p)
lm.fit8p  = lm(crim~poly(tax,4))      ; summary(lm.fit8p)
lm.fit9p  = lm(crim~poly(ptratio,4))  ; summary(lm.fit9p)
lm.fit10p = lm(crim~poly(black,4))    ; summary(lm.fit10p)
lm.fit11p = lm(crim~poly(lstat,4))    ; summary(lm.fit11p)
lm.fit12p = lm(crim~poly(medv,4))     ; summary(lm.fit12p)





# Exercise 11 - Section 4.7 ISLR
# a.  Create a binary variable, mpg01, that contains a 1 if mpg contains a value above its 
#     median, and a 0 if mpg contains a value below its median. Note you may find it helpful 
#     to use the data.frame() function to create a single data set containing both mpg01 and 
#     the other Auto variables. 
head(Auto)
Auto$mpg01 = ifelse(mpg > median(mpg), 1, 0)  
head(Auto,20)

# b. Which of the other features seem most likely to be useful in predicting mpg01? 
#    Scatterplots and boxplots may be useful tools to answer this question. 
pairs(Auto[,-3])
names(Auto)
?Auto

# Some knowledge of which input factors may be useful here. 
# Before running any analysis I would predict that the following variables
# may have some explanatory power over mpg01 binary variable. 
# cylinders, displacement, horsepower, weight, acceleration, year, origin

# Let's remove the vehicle name column and create a new data.frame var 'auto'. 
auto = Auto[,-9]

# re-attach the auto data set so that can call variables without $ expansion
attach(auto)

# sort by correlation of mpg and other vars (descending). Present as matrix for column view output.
as.matrix(cor.mpg[order(-cor.mpg)])

# let's see how 
par(mfrow=c(3,2))
boxplot(weight ~ mpg01, data=auto, xlab='mpg01', ylab='weight')
boxplot(displacement ~ mpg01, data=auto, xlab='mpg01', ylab='displacement')
boxplot(horsepower ~ mpg01, data=auto, xlab='mpg01', ylab='horsepower')
boxplot(cylinders ~ mpg01, data=auto, xlab='mpg01', ylab='cylinders')
boxplot(year ~ mpg01, data=auto, xlab='mpg01', ylab='year')
boxplot(origin ~ mpg01, data=auto, xlab='mpg01', ylab='origin')


# c. Lets now split the data into a training and test set. 
table(year)        # we can see that there are years 1970-82. 
train = (year<82)  # lets use all years prior to 1982 as the train set

# d. perform LDA on variables most correlated with mpg01 and calculate the test error
lda.fit=lda(mpg01~weight+displacement,data=auto ,subset=train)
lda.fit

# to evaluate accuracy of the model we need to run the fitted model above on the test data set. I.e. year=82
auto.1982=subset(auto, year==82)
lda.pred = predict(lda.fit, auto.1982)
data.frame(lda.pred)[1:5,]
table(lda.pred$class,auto.1982$mpg01) # is the confusion matrix
mean(lda.pred$class==auto.1982$mpg01) # 0.9667 mean accuracy
test.error = 1 - mean(lda.pred$class==auto.1982$mpg01) # 0.9667 mean accuracy
test.error*100 

# e. next perform QDA on the training data 
qda.fit = qda(mpg01~weight+displacement, data=auto, subset=train)
qda.fit

# create the test data set
auto.1982=subset(auto, year==82)

# create confusion matrix and evaluate mean accuracy
qda.class = predict(qda.fit, auto.1982)$class
table(qda.class, auto.1982$mpg01)
mean(qda.class==auto.1982$mpg01)
test.error = 1 - mean(qda.class==auto.1982$mpg01)
test.error * 100 # for percentage error


# f. Logistic Regression
train = (year<82) 
glm.fit = glm(mpg01~weight+displacement,
              data=auto, family=binomial, subset=train)
glm.probs = predict(glm.fit, newdata=auto[!train,], type='response')
glm.pred=ifelse(glm.probs > 0.5, 1, 0)
mpg01.1998=auto$mpg01[!train]
table(glm.pred,mpg01.1998)
mean(glm.pred==mpg01.1998) 


# g. KNN 
library(class)
?knn
auto.knn = auto[,c('weight','displacement')]
class(auto.knn)
head(auto.knn)

train=year<82
knn.pred = knn(auto.knn[train,], auto.knn[!train,], mpg01[train], k=1)
table(knn.pred, mpg01[!train])
mean(knn.pred==mpg01[!train])  # outcome is 0.833 -- the worst performer so far. 

knn.pred = knn(auto.knn[train,], auto.knn[!train,], mpg01[train], k=2)
table(knn.pred, mpg01[!train])
mean(knn.pred==mpg01[!train]) 

knn.pred = knn(auto.knn[train,], auto.knn[!train,], mpg01[train], k=3)
table(knn.pred, mpg01[!train])
mean(knn.pred==mpg01[!train]) 

knn.pred = knn(auto.knn[train,], auto.knn[!train,], mpg01[train], k=4)
table(knn.pred, mpg01[!train])
mean(knn.pred==mpg01[!train]) 







# Exercise 13 - Section 4.7
# use the Boston data set, fit classification models in order to predict whether a 
# given suburb has a crime rate above or below the median. Explore logistic 
# regression, LDA, and KNN models using various subsets of the predictors. 
# Describe your findings.
?Boston

# Let's explore the Boston data set
head(Boston)
str(Boston)
summary(Boston)
pairs(Boston)
attach(Boston)

# First see if the crime rate in an area is above the median (1) or below the median (0)
median(crim)
Boston$crim01 = ifelse(crim > median(crim), 1, 0)  
head(Boston[,c(1,15)],20)

# Let's recreate the Boston data set and rename as boston so we can manipulate without affecting the main df
boston = Boston
attach(boston)

# Let's see which of the features of the boston data seem most likely to be useful in predicting crim01? 
pairs(boston, col=boston$crim01)

# As there are a large number of variables it is hard to tell which are 
# most correlated to crim01. Let's look at the correlation between crim and 
# the other variables. 
# Sort by correlation of mpg and other vars (descending). Present as matrix for column view output.
# get the absolute value and display as column matrix descending order
cor.crim = abs(cor(boston)[,1])
as.matrix(cor.crim[order(-cor.crim)])
# four most correlated variables are rad, tax, lstat, and nox. 

# Let's repeat the same exercise now on the binary median crim variable crim01
cor.crim01 = abs(cor(boston)[,15])
as.matrix(cor.crim01[order(-cor.crim01)])
# the four most correlated variables with crim01 are not nox, rad, dis, & age

# Let's see the association via scatterplots of the most correlated variables
par(mfrow=c(3,2))
plot(crim,tax)
plot(crim,rad)
plot(crim,nox)
plot(crim,dis)
plot(crim,age)
plot(crim,lstat)

# let's see how the boxplots look when splitting out above/below median crim areas
par(mfrow=c(3,2))
boxplot(rad ~ crim01, data=boston, xlab='crim01', ylab='rad')
boxplot(tax ~ crim01, data=boston, xlab='crim01', ylab='tax')
boxplot(nox ~ crim01, data=boston, xlab='crim01', ylab='nox')
boxplot(dis ~ crim01, data=boston, xlab='crim01', ylab='dis')
boxplot(age ~ crim01, data=boston, xlab='crim01', ylab='age')
boxplot(lstat ~ crim01, data=boston, xlab='crim01', ylab='lstat')


# Lets now split the data into a training and test set. 
## 90% of the sample size
dim(boston)
smp.size = floor(0.90 * nrow(boston))
smp.size

## set the seed to make your partition reproductible
set.seed(123)
train = sample(seq_len(nrow(boston)), size = smp.size)
train

train.df = boston[train, ]
train.df
dim(train.df)

test.df  = boston[-train, ]
test.df
dim(test.df)

train = train.df
test  = test.df

dim(train)
dim(test)


# Perform LDA on variables most correlated with crim01 and calculate the test error
lda.fit=lda(crim01~nox+rad,data=train)
lda.fit

# to evaluate accuracy of the model we need to run the fitted model above on the test data set. I.e. year=82
lda.pred = predict(lda.fit, test)
data.frame(lda.pred)[1:5,]
table(lda.pred$class,test$crim01) # is the confusion matrix
mean(lda.pred$class==test$crim01)                 # 0.902 mean accuracy
test.error = 1 - mean(lda.pred$class==test$crim01) 
test.error*100                                    # 9.8% error 

# Next perform QDA on the training data 
qda.fit = qda(crim01~nox+rad, data=train)
qda.fit

# create confusion matrix and evaluate mean accuracy
qda.class = predict(qda.fit, test)$class
table(qda.class, test$crim01)
mean(qda.class==test$crim01)
test.error = 1 - mean(qda.class==test$crim01)    # 92.16% accuracy
test.error * 100 # for percentage error          # 7.84% error


# Logistic Regression
glm.fit = glm(crim01~nox+rad,
              data=train, family=binomial)
glm.probs = predict(glm.fit, newdata=test, type='response')
glm.pred=ifelse(glm.probs > 0.5, 1, 0)
table(glm.pred,test$crim01)   
mean(glm.pred==test$crim01)                     # 86.27% accuracy
test.error = 1 - mean(glm.pred==test$crim01)
test.error                                      #13.73% error 

# g. KNN 
library(class)
boston.knn = boston[,c('nox','rad')]
class(boston.knn)
head(boston.knn)

set.seed(123)
train = sample(seq_len(nrow(boston)), size = smp.size)
train 

# the first parameter is the training data points for the predictors of interest:
# nox and rad. However, currently 'train' is currently the actual data.frame for 
knn.pred = knn(boston.knn[train,], boston.knn[-train,], crim01[train], k=1)
table(knn.pred, crim01[-train])
mean(knn.pred==crim01[-train])      # outcome is 0.98 -- the best performer so far. 
test.error = 1 - mean(knn.pred==crim01[-train])
test.error*100                      # 1.96% error

knn.pred = knn(boston.knn[train,], boston.knn[-train,], crim01[train], k=2)
table(knn.pred, crim01[-train])
mean(knn.pred==crim01[-train])  
test.error = 1 - mean(knn.pred==crim01[-train])
test.error                      

knn.pred = knn(boston.knn[train,], boston.knn[-train,], crim01[train], k=3)
table(knn.pred, crim01[-train])
mean(knn.pred==crim01[-train])   
test.error = 1 - mean(knn.pred==crim01[-train])
test.error                      



#### Let's repeat the above on a different subset of predictors: add tax  
#### Logistic Regression
#### Set the seed to make your partition reproductible
set.seed(123)
train = sample(seq_len(nrow(boston)), size = smp.size)
train

train.df = boston[train, ]
train.df
dim(train.df)

test.df  = boston[-train, ]
test.df
dim(test.df)

train = train.df
test  = test.df

dim(train)
dim(test)



# Perform LDA on variables most correlated with crim01 and calculate the test error
lda.fit=lda(crim01~nox+rad+tax,data=train)
lda.fit

# to evaluate accuracy of the model we need to run the fitted model above on the test data set. I.e. year=82
lda.pred = predict(lda.fit, test)
data.frame(lda.pred)[1:5,]
table(lda.pred$class,test$crim01) # is the confusion matrix
mean(lda.pred$class==test$crim01)                 # 0.902 mean accuracy (SAME!)
test.error = 1 - mean(lda.pred$class==test$crim01) 
test.error*100                                    # 9.8% error (SAME!)

# Next perform QDA on the training data 
qda.fit = qda(crim01~nox+rad+tax, data=train)
qda.fit

# create confusion matrix and evaluate mean accuracy
qda.class = predict(qda.fit, test)$class
table(qda.class, test$crim01)
mean(qda.class==test$crim01)
test.error = 1 - mean(qda.class==test$crim01)    # 86.27% accuracy
test.error * 100 # for percentage error          # 13.73% error


# Logistic Regression
glm.fit = glm(crim01~nox+rad+tax,
              data=train, family=binomial)
glm.probs = predict(glm.fit, newdata=test, type='response')
glm.pred=ifelse(glm.probs > 0.5, 1, 0)
table(glm.pred,test$crim01)   
mean(glm.pred==test$crim01)                     # 86.27% accuracy (SAME)
test.error = 1 - mean(glm.pred==test$crim01)
test.error                                      #13.73% error (SAME)

# g. KNN 
library(class)
boston.knn = boston[,c('nox','rad', 'tax')]
class(boston.knn)
head(boston.knn)

set.seed(123)
train = sample(seq_len(nrow(boston)), size = smp.size)

knn.pred = knn(boston.knn[train,], boston.knn[-train,], crim01[train], k=1)
table(knn.pred, crim01[-train])
mean(knn.pred==crim01[-train])  
test.error = 1 - mean(knn.pred==crim01[-train])
test.error                      

knn.pred = knn(boston.knn[train,], boston.knn[-train,], crim01[train], k=2)
table(knn.pred, crim01[-train])
mean(knn.pred==crim01[-train])   
test.error = 1 - mean(knn.pred==crim01[-train])
test.error                      

knn.pred = knn(boston.knn[train,], boston.knn[-train,], crim01[train], k=3)
table(knn.pred, crim01[-train])
mean(knn.pred==crim01[-train])   
test.error = 1 - mean(knn.pred==crim01[-train])
test.error                      

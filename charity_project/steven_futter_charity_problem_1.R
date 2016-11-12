### 1. Read data from csv file
setwd('/Users/stevenfutter/Dropbox/NU/MACHINE_LEARNING/charity_project')
charity = read.csv('projectDataPart1.csv', header=T, na.strings="NA",
                   stringsAsFactors = TRUE,
                   colClasses=c("DONR"="factor", "HOME"="factor","HINC"="factor"))

# set up $ expansion
attach(charity)

# check that column types are set up correctly
str(charity)

# confirm size of data table
dim(charity) # 3684 21

# take a peek at the data
head(charity)


### 2. Data Quality Check
library(Hmisc)
Hmisc::describe(charity)

library(plyr)
library(psych)
multi.hist(charity[,sapply(charity, is.numeric)])

### 3. Exploratory Data Analysis (EDA)
df = charity[,-1]
names(df)

#b. # create new df (df.num) with numeric and integer values only
df.num = df[,sapply(df, class) %in% c("numeric", "integer") ]

# calculate the correlations between each variable and DAMT
apply(df.num,2, function(col)cor(col, df.num$DAMT))

# build scatterplots between numeric variables
par(mfrow=c(1,3))
plot(df.num$LASTGIFT, df.num$DAMT)
plot(df.num$MAXRAMNT, df.num$DAMT)
plot(df.num$RAMNTALL, df.num$DAMT)

par(mfrow=c(2,2))
boxplot(DAMT~HOME, data=charity, col='lightgray',main="Charity Data - HOME", 
xlab="Home Owner (HOME)", 
ylab="Donation Amount (in $)")

boxplot(DAMT~HINC, data=charity, col='lightgray',main="Charity Data - HINC", 
xlab="Household Income (HINC)", 
ylab="Donation Amount (in $)")

boxplot(DAMT~RFA_97, data=charity, col='lightgray',main="Charity Data - RFA_97",
xlab="Donor's RFA status as of 1997 Promo date (RFA_97)", 
ylab="Donation Amount (in $)")

boxplot(DAMT~RFA_96, data=charity, col='lightgray',main="Charity Data - RFA_96",
xlab="Donor's RFA status as of 1996 Promo date (RFA_96)", 
ylab="Donation Amount (in $)")


### 4. Data Preparation
#a. How did you handle missing values?

df1 = charity[,-6]
names(df1)

#b. Are there any derived or transformed variables that you added to the dataset?

par(mfrow=c(1,2))
df1$LOG_MAXRAMNT = log(df1$MAXRAMNT)
plot(df1$MAXRAMNT,df1$DAMT, main='MAXRAMNT')
plot(df1$LOG_MAXRAMNT,df1$DAMT, main='Log(MAXRAMNT)')

#c. Did you perform any re-categorization of categorical variables?
#df1$RECENCY_97   = substr(RFA_97,1,1)   #removed as only 1 factor found.
df1$FREQUENCY_97 = substr(RFA_97,2,2)
df1$AMOUNT_97    = substr(RFA_97,3,3)
df1$RECENCY_96   = substr(RFA_96,1,1)
df1$FREQUENCY_96 = substr(RFA_96,2,2)
df1$AMOUNT_96    = substr(RFA_96,3,3)

names(df1)

#d. Are there any variables that you have chosen to remove from the dataset?

df1 = df1[,c(-19,-20)]
names(df1)



### 5. Dataset Partitioning
#For this assignment, you will employ a hold-out test dataset for model validation and selection.

#a. Hold-Out Test Set
#The first step you should take is to sample 25% of the observations in the dataset to form a hold-out test set. This data will be referred to as the Regression Test Set (or simply the Test Set for the remainder of this document). Report the number of observations and the distribution of response values in the Test Set. The data in the Test Set should not be used until Exercise 7 of this assignment.
#b. Training Set
#The remaining 75% of the observations will be referred to as the Regression Training Set (or simply the Training Set for the remainder of this document). Report the number of observations and the distribution of response values in the Training Set.

dim(df1) # 3684 x 24
names(df1)

df1=df1[,-c(1,2,6)]  # removes ID and DONR
names(df1)

smp.size = floor(0.75 * nrow(df1))

set.seed(1)
train = sample(seq_len(nrow(df1)), size = smp.size)
test = -train

ch.train = df1[train,]
ch.test = df1[-train,]

dim(ch.train)
summary(ch.train)

dim(ch.test)
summary(ch.test)


### 6. Model Fitting
#Use R to develop various models for the response variable DAMT. The variables ID and DONR are not to be used as predictors. Fit at least one model from each of the following four categories. Each model should be fit to the Training Set data only.

#### Simple Linear Regression Model
#a. Simple linear regression model: 

#DAMT = B0 + B1*LASTGIFT + e

#For the first model we run a simple linear regression model using LASTGIFT as the single predictor since this was the variable that was most correlated to DAMT with an r-value = 0.72. The LASTGIFT variable is significant and has an R^2 value of 0.56
fit1 = lm(DAMT~LASTGIFT,ch.train)
summary(fit1)
plot(fit1)

# Calculate the MSE of train and test data
mean((DAMT-predict(fit1, df1))[train]^2)      # MSE Training Set = 65.18256
mean((DAMT-predict(fit1, df1))[test]^2)       # MSE Test Set     = 79.24383

#b. Multiple linear regression (ISLR Section 3.1) or multiple linear regression with subset selection (ISLR Section 6.1)

#### Multiple Linear Regression Model
#Let's first convert the newly added columns to factors as below:
  
df1$FREQUENCY_97 = factor(df1$FREQUENCY_97)
df1$AMOUNT_97    = factor(df1$AMOUNT_97)
df1$RECENCY_96   = factor(df1$RECENCY_96)
df1$FREQUENCY_96 = factor(df1$FREQUENCY_96)
df1$AMOUNT_96    = factor(df1$AMOUNT_96)

#MODEL 2:
  
#  DAMT= B0 + B1*HOME1 + B2*NUMPROM + B3*RAMNTALL + B4*NGIFTALL + B5*LASTGIFT + B6*LOG_MAXRAMNT + B7*AMOUNT_97F + B8*AMOUNT_97G + B9*FREQUENCY_961 + e

#The multiple linear regression model above was created by using the best subset selection algorithm. The regsubsets() function performs best subset selection by identifying the best model that contains a given number of predictors, where 'best' is quantified using RSS. The asterixes in the output below indicate that the best nine-variable model contains the variables provided in the equation above. 
library(leaps)
fit2=regsubsets(DAMT~., data=ch.train)
summary(fit2)

# repeat the fit using lm() so that we can use the predict() function for calculating MSE.
fit2=lm(DAMT~HOME+NUMPROM+RAMNTALL+NGIFTALL+LASTGIFT+LOG_MAXRAMNT+AMOUNT_97+AMOUNT_97+FREQUENCY_96, data=ch.train)
 
#Note that we do not use the nvmax option in the regsubsets() function. I made this decision for ease of interpretability of the final model. A model with fewer variables is more easily interpreted. Let's now move on to shrinkage models.

# Calculate the MSE of train and test data
mean((DAMT-predict(fit2, df1))[train]^2)      # MSE Training Set = 59.64491
mean((DAMT-predict(fit2, df1))[test]^2)       # MSE Test Set     = 73.80784


#c. Shinkage Models 
#MODEL 3: Lasso Model
#In comparison to ridge regression a Lasso model can yeild either more accurate or more interpretable model. 

library(glmnet)
x = model.matrix(DAMT~., df1)
y=df1$DAMT


grid=10^seq(10,-2,length=100) 
fit3 = glmnet(x[train,], y[train], alpha=1, lambda=grid)  # alpha=1 is the LASSO model

#As there is no predict() method for regsubsets(), we create our own predict() method, as below:
predict.regsubsets=function(object, newdata, id, ...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object, id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}


# Perform cross-validation and compute the associated test error.
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min                          # 0.6740771 lambda is the best lambda value
bestlam



# We used cv to find the lambda value that minimizes WHAT??
# NEED TO EXPLAIN THIS...


# Calculate the MSE of training and test sets
y.test=y[test]
y.train=y[train]
lasso.pred=predict(fit3,s=bestlam,newx=x[train,])
mean((lasso.pred-y.train)^2)                        # 61.35197 is train MSE

lasso.pred=predict(fit3,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)                         # 74.24213 is the MSE


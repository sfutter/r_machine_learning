########################################
## PREDICT 422
## Charity Project - Part 1 (The Regression Problem)
##
## SampleCodePart1.R
########################################

# Load packages required for this code.
# Remember, the first time you use a package, you will need to install the package 
# with the package installer.
library(leaps)
library(glmnet)
library(tree)
library(randomForest)

########################################
## Exercise 1
## Read Data from CSV File
########################################

# This path is specified wrt a Mac, the Windows path will start differently
inPath = file.path("/Users","JLW","Documents","Northwestern MSPA","PREDICT 422",
                   "Project","Project Data Files")

regData = read.csv(file.path(inPath,"projectDataPart1.csv"),na.strings=c("NA"," "))

# You can also try the following command for browsing to the csv file instead.
# regData = read.csv(file.choose(),na.strings=c("NA"," "))

# Convert categorical variables to factors
# This is highly recommended so that R treats the variables appropriately.
# The lm() method in R can handle a factor variable without us needing to convert 
# the factor to binary dummy variable(s).
regData$DONR = as.factor(regData$DONR)
regData$HOME = as.factor(regData$HOME)
regData$HINC = as.factor(regData$HINC)

########################################
## Exercise 2
## Data Quality Check
########################################

dim(regData)      # dimensions of data
names(regData)    # variable names
str(regData)      # one form of summary of data
summary(regData)  # another form of summary

## Check for Missing Values
which(sapply(regData,anyNA))

# Missing values identified in HINC, GENDER, and RFA_96
# Get counts of missing values for each variable
table(regData$HINC,useNA="ifany")
table(regData$GENDER,useNA="ifany")
table(regData$RFA_96,useNA="ifany")

########################################
## Exercise 3
## Exploratory Data Analysis
########################################

# Histogram of the response variable DAMT
hist(regData$DAMT,col="blue",breaks=20)

# Get counts for a categorical variable
table(regData$GENDER,useNA="ifany")

# Barplot of a categorical variable
barplot(table(regData$GENDER,useNA="ifany"),main="Gender")

# Boxplot of donation amount by gender
plot(regData$GENDER,regData$DAMT,xlab="Gender",ylab="Donation ($)")

# Plot response against a quantitative variable
plot(regData$AGE,regData$DAMT,xlab="Age",ylab="Donation ($)")
# add regression line (optional)
lm_age = lm(DAMT ~ AGE, data=regData)
abline(lm_age,col="red")

########################################
## Exercise 4
## Data Preparation
########################################

## Part A - Resolve Missing Values

# HINC - Make a level 0 and code missing values as 0
levels(regData$HINC) = c(levels(regData$HINC),"0")
regData$HINC[is.na(regData$HINC)] = "0"
table(regData$HINC,useNA="ifany")

# GENDER - Assign A, J, and NA to category U
idxMF = regData$GENDER %in% c("M","F")
regData$GENDER[!idxMF] = "U"
regData$GENDER = factor(regData$GENDER)
table(regData$GENDER)

# RFA_96 - Make a level XXX and code missing values as XXX
levels(regData$RFA_96) = c(levels(regData$RFA_96),"XXX")
regData$RFA_96[is.na(regData$RFA_96)] = "XXX"
table(regData$RFA_96,useNA="ifany")

## Part B - Derived or Transformed Variables

# Add your own code here (optional).
#
# Note: Applying a transform to the response variable DAMT is an all-or-none 
# proposition. Transforming the response changes the scale of the y (response) and 
# yhat (predicted) values. Therefore, you cannot compare the MSEs of a model fit to 
# DAMT and a model fit to f(DAMT) where "f" is some transformation function such as
# log or sqrt. If you make the mistake of comparing MSEs in such a way, one MSE value
# may be much smaller than the other. Yet, that will not be a sign that one model
# fits much better than the other; it will be an indication of the y and yhat values
# being on a different scale due to the transformation.
#
# The solution is to either use NO transformation of the response or to apply the 
# same transformation to the response for EVERY model that you fit.

## Part C - Re-categorize Variables

# Separate RFA Values (R = recency, F = frequency, A = amount)
# Note: I wrote a function (separateRFA) to perform these steps.
separateRFA = function(xData,varName)
{
  bytes = c("R","F","A")
  newVarNames = paste(varName,bytes, sep="_")
  
  for (ii in 1:length(bytes)) # Loop over 1 to 3 (corresponding to R, F, and A)
  {
    # Find the unique values for current byte
    byteVals = unique(substr(levels(xData[,varName]),ii,ii))
    
    for (jj in 1:length(byteVals)) # Loop over unique byte values
    {
      rowIdx = substr(xData[,varName],ii,ii) == byteVals[jj]
      xData[rowIdx,newVarNames[ii]] = byteVals[jj]
    }
    
    xData[,newVarNames[ii]] = factor(xData[,newVarNames[ii]])
  }
  
  return(xData)
}

# Apply separateRFA to the variables RFA_96 and RFA_97
regData = separateRFA(regData,"RFA_96")
regData = separateRFA(regData,"RFA_97")

# Check the results
table(regData$RFA_96,regData$RFA_96_R)
table(regData$RFA_96,regData$RFA_96_F)
table(regData$RFA_96,regData$RFA_96_A)
table(regData$RFA_97,regData$RFA_97_R)
table(regData$RFA_97,regData$RFA_97_F)
table(regData$RFA_97,regData$RFA_97_A)

# Note: Sometimes there is some iteration between steps (such as between data
# preparation and EDA). For example, you may want to include some tables or figures 
# showing the separated RFA categories in your EDA.

## Part D - Drop Variables

# This part is optional. However, there are several reasons one might want to drop
# variables from the dataset. A few reasons are listed here.
#
# - In EDA, you may find that some variables have no (or negligible) predictive value.
# Some variables that you have access to may prove to be irrelevant to the modeling
# problem at hand. You are permitted to eliminate these from consideration in your
# models. One way to do this is to drop them from the dataset.
# 
# - Transformed variables should replace the original variables. Typically, you 
# would not use both a variable and its transformed version.
#
# - Derived variables might need to replace base variables. For example, if you 
# compute a ratio between two variables, then you may run into problems including
# both the original variables and the ration in your model (due to multi-collinearity
# concerns).
#
# - In the case of RFA variables that we have broken down into separate R, F, and A
# variables, you should not include both the combined and the separated variables in
# your models. Make your choice between using the RFA variable and the separated
# variables and drop the unused one(s) from the dataset. My recommendation is to
# use the separated variables since there will be fewer dummy variables generated,
# and it might be the case that some of R, F, and A have less predictive value (and
# can be left out of your models).
#
# - Factor variables can cause problems with some of the R methods. Specifically,
# let's suppose that GENDER does not have much predictive ability and you do not plan
# to include GENDER in your models. You can write the model formula in such a way
# that GENDER is excluded. However, if your test set happens to be a sample that does
# not contain any observations in a particular category (GENDER = U, perhaps), then 
# you will run into trouble with R making predictions on the test set, despite the
# fact that GENDER is not included in your model. In my opinion, this is a weakness 
# in the way some methods are implemented in R. However, if you run into this problem,
# then the most direct solution is to remove the problem variable from your dataset.

# Index of variables to drop from dataset. You can identify the column numbers
# manually, or you can search by variable name as shown below.
# - Remove DONR since it only has one level in the regression problem. DONR is not
# meant to be used for the regression problem anyway.
# - Remove RFA_96 and RFA_97 in favor or keeping the separate R, F, and A variables.
# - Remove RFA_97_R since there is only one level expressed. No information is added
# and it may cause problems with the code.
dropIdx = which(names(regData) %in% c("DONR","RFA_96","RFA_97","RFA_97_R"))

# Drop the variables indicated by dropIdx.
regData2 = regData[,-dropIdx]
names(regData2)   # check that the result is as expected

########################################
## Exercise 5
## Dataset Partitioning
########################################

# There are two basic ways to sample training or test data: 1) logical, 2) numeric.
# You can choose which method/representation you prefer. Uncomment the version you
# want to use and comment out the version you will not use (you can't use both).
testFraction = 0.25   # specify the fraction of data to use in the hold-out test 
set.seed(123)

# 1) Logical 
# - the index vector has length equal to the number of observations 
# - the index values are boolean (TRUE and FALSE)
# - TRUE = use that row in the sample, FALSE = do not use that row in the sample
trainIdx = sample(c(TRUE,FALSE),size=nrow(regData2),replace=TRUE,
                  prob=c(1-testFraction,testFraction))
# Note that the order needs to match between (TRUE, FALSE) and (1-testFraction,testFraction).
# The TRUEs will be sampled at a rate of 0.75 to indicate use in the training set.
# The FALSEs will be sampled at a rate of 0.25 to indicate use in the test set 
# (FALSE = not to be included in the training set = included in the test set).

# 2) Numeric
# - the index vector has length equal to the size of the sampled set
# - the index values are integer, representing the row numbers to use for the sample
trainIdx = sample(nrow(regData2),size=(1-testFraction)*nrow(regData2),replace=FALSE)

########################################
## Exercise 6
## Model Fitting
########################################

# Note: In the following sub-sections I give one example of each kind of model (I
# illustrate a tree-based model instead of a non-linear model). The examples are 
# meant to illustrate the necessary coding for each model type. I intentionally 
# build models that are intended to be adequate and may be based on somewhat 
# arbitrary choices. Hence, there are plenty of better models left for you to build
# on your own.

## Part A - Simple Linear Regression
modelA1 = lm(DAMT ~ MAXRAMNT,data=regData2,subset=trainIdx)
summary(modelA1)
par(mfrow=c(2,2))
plot(modelA1)
# Note: Observation 715 appears to be a high-leverage point (or outlier) for this
# particular model. I will fit this model again excluding that observation.
# Version 1 indexing: 
tmpIdx = trainIdx; tmpIdx[715] = FALSE
# Version 2 indexing:
# tmpIdx = trainIdx[-which(trainIdx == 715)]
modelA2 = lm(DAMT ~ MAXRAMNT,data=regData2,subset=tmpIdx)
summary(modelA2)
par(mfrow=c(2,2))
plot(modelA2)
par(mfrow=c(1,1))

## Part B - Multiple Linear Regression

# Using my version of the dataset, I will fit the full model (ModelB1) and a 
# subset model (ModelB2) using forward stepwise selection. Your dataset may vary at 
# this point based on derived/transformed/dropped variables.

# IMPORTANT NOTE: The model notation "DAMT ~ .-ID" (namely using all variables minus
# a select few) is valid and should work. However, I have encountered some cases 
# where an error was caused using this notation. I have not figured out why the error
# shows up, but I have found that changing the model specification to the form
# "DAMT ~ AGE + HOME + HINC" and so on (adding all variables that you want using plus
# signs) makes the error go away. If you encounter an error in building your model,
# try making this change to see if the error goes away. If the error remains, then
# you have something else wrong with your model and should keep troubleshooting
# (including posting a question to the Q & A).

# Full Regression Model (minus ID which is not to be used as a predictor)
modelB1 = lm(DAMT ~ .-ID,data=regData2,subset=trainIdx)
summary(modelB1)

# Forward Stepwise Selection 
# Using k-fold cross-validation to select number of variables.

# Set up k folds.
k = 10
set.seed(36)
foldNum = sample(1:k,nrow(regData2[trainIdx,]),replace=TRUE)
barplot(table(foldNum),xlab="Fold #",ylab="Count")  # check results

# Specify set of variables eligible to use in model. This will be the starting point
# for the variable selection. You can use the full set or a smaller set of variables.
# Note: I have included all eligible predictors in my dataset (except the RFA_97 and
# RFA_96 variables), but I have broken them up into two groups. This notation is 
# artificial for my selection, but it does show you how you could select multiple 
# subsets of variables and leave others out.
varsToUse = names(regData2)[c(3:11,12:18)]
print(varsToUse)
maxVars = length(varsToUse)

# Create a matrix to hold the k-fold MSE. Initialize with all NAs
# One row for each fold (k columns), one column for each size model
kValErrors = matrix(NA,k,maxVars,
                    dimnames=list(paste("Fold",1:k),paste(1:maxVars,"Vars")))

# Prediction function from Section 6.5.3 of ISLR
predict.regsubsets = function(object,newdata,id,...)
{
  form = as.formula(object$call[[2]])
  mat = model.matrix(form,newdata)
  coefi = coef(object,id=id)
  xvars = names(coefi)
  result = mat[,xvars] %*% coefi
  return(result)
}

# Loop over the folds
for (kk in 1:k)
{
  # Define formula to include all of the variables in varsToUse
  myFormula = paste("DAMT ~ ",paste(varsToUse,collapse=" + "),sep="")
  
  # Train on all folds except fold kk.
  regfit_bestCV = regsubsets(as.formula(myFormula), 
                             data=regData2[trainIdx,][foldNum!=kk,],
                             nvmax=maxVars, method="forward")
  
  # Loop over number of variables (model size).
  for (jj in 1:maxVars)
  {
    # Predict on validation folds (Fold == kk) and calculate validation MSE
    kValPred = predict(regfit_bestCV,regData2[trainIdx,][foldNum==kk,],id=jj)
    kValErrors[kk,jj] = mean( (regData2$DAMT[trainIdx][foldNum==kk]-kValPred)^2 )
  }
}

# Average cv_errors down the columns using the apply() method
meanValError = apply(kValErrors,2,mean)
plot(meanValError,type='b',xlab='# Variables',ylab='MSE',main='Validation Data')
bestModel = which.min(meanValError)
points(bestModel,meanValError[bestModel],col="red",cex=2,pch=20)

# When I ran this code, the minimum error was with a single variable (your results
# may be different). This is not entirely surprising, but it does seem somewhat 
# anti-climactic. Looking beyond 1 variable, the MSE is much larger for the 2- and 
# 3-variable models. The MSE drops down at the 4-variable model and remains basically
# flat (although increasing slightly) as additional variables are added. For the 
# sake of illustrating the process, I am going to move forward with the 4-variable
# model.

# Re-fit the forward selection models to all folds of the training data.
regfit_best = regsubsets(as.formula(myFormula), data=regData2[trainIdx,],
                         nvmax=maxVars, method="forward")
summary(regfit_best)
coef(regfit_best,4)

# I am going to fit the best model (the 4 variables selected above) as an LM object. 
# This is for the purpose of model portability in the downstream code. Note that
# coefficients in my LM should match the coefficients from regfit_best above.
modelB2 = lm(DAMT ~ RAMNTALL + NGIFTALL + LASTGIFT + TDON,data=regData2,subset=trainIdx)
summary(modelB2)
coef(modelB2)

## Part C - Shrinkage Models
regX = model.matrix(DAMT ~ .-ID,data=regData2)[,-1]
regY = regData2$DAMT
cvLasso = cv.glmnet(regX[trainIdx,],regY[trainIdx],alpha=1)
plot(cvLasso)

# I will build models corresponding to both lambda.min (modelC1) and 
# lambda.1se (modelC2). Note that modelC2 has a single variable plus intercept.
modelC1 = glmnet(regX[trainIdx,],regY[trainIdx],alpha=1,lambda=cvLasso$lambda.min)
coef(modelC1)

modelC2 = glmnet(regX[trainIdx,],regY[trainIdx],alpha=1,lambda=cvLasso$lambda.1se)
coef(modelC2)

## Part D - Non-linear Models OR Tree/Forest Models

# Using my version of the dataset, I will fit a tree model (ModelD1) and a 
# forest model (ModelD2). Your dataset may vary at this point based on 
# derived/transformed/dropped variables.

# Tree Model
fullTree = tree(DAMT ~ .-ID,data=regData2,subset=trainIdx)
summary(fullTree)
plot(fullTree)
text(fullTree,pretty=0)

# Prune the tree
cvTree = cv.tree(fullTree)
plot(cvTree$size ,cvTree$dev ,type="b")

modelD1 = prune.tree(fullTree,best=4)
summary(modelD1)
par(mfrow=c(1,1))
plot(modelD1)
text(modelD1,pretty=0)

# Forest Model
# For a regression problem, recommend using mtry = nvars/3. Currently, I have 
# nvars = 21.
modelD2 = randomForest(DAMT ~ .-ID,data=regData2,subset=trainIdx,mtry=7,
                       importance =TRUE)
summary(modelD2)
varImpPlot(modelD2)

########################################
## Exercise 7
## Model Validation
########################################

# For each model, I will generate predictions for all data (Train and Test). I
# will then calculate the Train MSE and the Test MSE by subsetting the predictions
# accordingly. The following function will calculate both MSE values simultaneously.
calcMSE = function(model,modelLabel,dataSet,trainIdx,newX=NULL)
{
  # The predict method for glmnet will need to be called differently from the
  # other predict methods.
  if ("glmnet" %in% class(model)) {
    predVals = predict(model,newX,type="response")
  } else {
    predVals = predict(model,dataSet)
  }
  MSE = list(
    name = modelLabel,
    train = mean( (predVals[trainIdx] - dataSet$DAMT[trainIdx])^2 ),
    test = mean( (predVals[-trainIdx] - dataSet$DAMT[-trainIdx])^2 )
  )
  
  return(MSE)
}

modelMSEs = data.frame(Model = rep(NA,8),Train.MSE = rep(NA,8),Test.MSE = rep(NA,8))

modelMSEs[1,] = calcMSE(modelA1,"A1",regData2,trainIdx)
modelMSEs[2,] = calcMSE(modelA2,"A2",regData2,trainIdx)
modelMSEs[3,] = calcMSE(modelB1,"B1",regData2,trainIdx)
modelMSEs[4,] = calcMSE(modelB2,"B2",regData2,trainIdx)
modelMSEs[5,] = calcMSE(modelC1,"C1",regData2,trainIdx,newX=regX)
modelMSEs[6,] = calcMSE(modelC2,"C2",regData2,trainIdx,newX=regX)
modelMSEs[7,] = calcMSE(modelD1,"D1",regData2,trainIdx)
modelMSEs[8,] = calcMSE(modelD2,"D2",regData2,trainIdx)

print(modelMSEs)

# Note that in fitting modelB1 (full multiple linear regression model), some of the
# dummy variables corresponding to RFA_96 appear to be collinear. I leave it to you 
# to pursue this issue further.

# Note that for the results I have here, the Test MSE is lower than the Train MSE in
# almost every case. That can happen, although the opposite relation is more common.
# Usually when this happens it is due to the composition of the two sampled sets. 
# If I were to change the seed value I used prior to sampling the datasets, then I 
# would get a different set of MSE values. It is important to recall that the 
# prediction accuaracy you obtain varies depending on how the dataset is sampled. 
# You can sample and fit models many times over in order to generate a boxplot 
# showing the distribution of the errors. High variability in the errors is bad, 
# low variability in the errors is good.

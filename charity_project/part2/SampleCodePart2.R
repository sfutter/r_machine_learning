########################################
## PREDICT 422
## Charity Project - Part 2 (The Classification Problem)
##
## SampleCodePart2.R
########################################

# Load packages required for this code.
# install.packages('pROC')
library(pROC)
library(glmnet)
library(rpart)
library(e1071)

########################################
## Exercise 1
## Read Data from CSV File
########################################

# This path is specified wrt a Mac, the Windows path will start differently
inPath = file.path("/Users","JLW","Documents","Northwestern MSPA","PREDICT 422",
                   "Project","Project Data Files")

classData = read.csv(file.path(inPath,"projectDataPart2.csv"),na.strings=c("NA"," "))

# You can also try the following command for browsing to the csv file instead.
# classData = read.csv(file.choose(),na.strings=c("NA"," "))

# Convert categorical variables to factors
# This is highly recommended so that R treats the variables appropriately.
# The lm() method in R can handle a factor variable without us needing to convert 
# the factor to binary dummy variable(s).
classData$DONR = as.factor(classData$DONR)
classData$HOME = as.factor(classData$HOME)
classData$HINC = as.factor(classData$HINC)

########################################
## Exercise 2
## Data Quality Check
########################################

dim(classData)      # dimensions of data
names(classData)    # variable names
str(classData)      # one form of summary of data
summary(classData)  # another form of summary

## Check for Missing Values
which(sapply(classData,anyNA))

# Missing values identified in HINC, GENDER, and RFA_96
# Get counts of missing values for each variable
table(classData$HINC,useNA="ifany")
table(classData$GENDER,useNA="ifany")
table(classData$RFA_96,useNA="ifany")

########################################
## Exercise 3
## Exploratory Data Analysis
########################################

# Counts of the response variable DONR
table(classData$DONR)
barplot(table(classData$DONR),xlab="DONR")

# Counts for a categorical predictor variable
table(classData$GENDER,useNA="ifany")
barplot(table(classData$GENDER,useNA="ifany"),main="Gender")

# Boxplot of AGE by DONR status
# In order for R to make this into a boxplot, DONR needs to be a factor variable
# and DONR needs to be plotted on the horizontal axis.
plot(classData$DONR,classData$AGE,xlab="DONR",ylab="AGE")

# Plot response against a categorical variable
# "Wrong" Way
# The following barplot is an accurate presentation of the data. I call it the 
# "wrong" way because students have a tendency to draw the wrong conclusions from
# this graph.
barplot(table(classData$GENDER[classData$DONR == 1]),
        xlab="GENDER",main="Barplot of GENDER for DONR = 1")
# This graph shows that there are more female donors than male donors. Therefore, 
# it seems reasonable to conclude that a female is more likely to donate. However,
# if you were to look at the non-donors, you would see that there are more female 
# non-donors than male non-donors. What this graph is showing us is that females
# outnumber males in the dataset as a whole, not that females are more likely to 
# donate.

# "Right" Way
# A mosaic plot is obtained when we plot one factor variable against another. The
# mosaic plot represents the counts as proportions to the whole. A deviation in
# overall proportion of females donating compared to males donating is meaningful
# whereas the absolute count of females donating compared to males donating was not.
plot(classData$DONR,classData$GENDER,xlab="DONR",ylab="GENDER",main="Mosaic Plot")
# Or
plot(classData$GENDER,classData$DONR,xlab="GENDER",ylab="DONR",main="Mosaic Plot")
# These graphs show that M/F doesn't show any difference in DONR status.

########################################
## Exercise 4
## Data Preparation
########################################

## Part A - Resolve Missing Values

# HINC - Make a level 0 and code missing values as 0
levels(classData$HINC) = c(levels(classData$HINC),"0")
classData$HINC[is.na(classData$HINC)] = "0"
table(classData$HINC,useNA="ifany")

# GENDER - Assign A, J, and NA to category U
idxMF = classData$GENDER %in% c("M","F")
classData$GENDER[!idxMF] = "U"
classData$GENDER = factor(classData$GENDER)
table(classData$GENDER)

# RFA_96 - Make a level XXX and code missing values as XXX
levels(classData$RFA_96) = c(levels(classData$RFA_96),"XXX")
classData$RFA_96[is.na(classData$RFA_96)] = "XXX"
table(classData$RFA_96,useNA="ifany")

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
classData = separateRFA(classData,"RFA_96")
classData = separateRFA(classData,"RFA_97")

# Check the results
table(classData$RFA_96,classData$RFA_96_R)
table(classData$RFA_96,classData$RFA_96_F)
table(classData$RFA_96,classData$RFA_96_A)
table(classData$RFA_97,classData$RFA_97_R)
table(classData$RFA_97,classData$RFA_97_F)
table(classData$RFA_97,classData$RFA_97_A)

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
dropIdx = which(names(classData) %in% c("DAMT","RFA_96","RFA_97","RFA_97_R"))

# Drop the variables indicated by dropIdx.
classData2 = classData[,-dropIdx]
names(classData2)   # check that the result is as expected

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
trainIdx = sample(c(TRUE,FALSE),size=nrow(classData2),replace=TRUE,
                  prob=c(1-testFraction,testFraction))
# Note that the order needs to match between (TRUE, FALSE) and (1-testFraction,testFraction).
# The TRUEs will be sampled at a rate of 0.75 to indicate use in the training set.
# The FALSEs will be sampled at a rate of 0.25 to indicate use in the test set 
# (FALSE = not to be included in the training set = included in the test set).

# 2) Numeric
# - the index vector has length equal to the size of the sampled set
# - the index values are integer, representing the row numbers to use for the sample
trainIdx = sample(nrow(classData2),size=(1-testFraction)*nrow(classData2),replace=FALSE)

########################################
## Exercise 6
## Model Fitting
########################################

# Note: In the following sub-sections I give one example of each kind of model. 
# The examples are meant to illustrate the necessary coding for each model type. 
# I intentionally build models that are intended to be adequate and may be based on 
# somewhat arbitrary choices. Hence, there are plenty of better models left for you 
# to build on your own.

## Part A - Simple Logistic Regression
modelA1 = glm(DONR ~ MAXRAMNT,data=classData2,subset=trainIdx,family=binomial)
summary(modelA1)
par(mfrow=c(2,2))
plot(modelA1)
par(mfrow=c(1,1))

# Note that fitting the logistic model is only the first part of the classification
# task. The logistic model provides a score for the likelihood that an individual
# will donate. We must select a cutoff or threshold value to use for translating
# the score to a 0/1 classification result.

# In general, a default threshold of 0.5 can be used. However, there are two problems
# with this strategy.
#  1) A default value of 0.5 assumes that the 0-class and 1-class are represented
#  equally in the training dataset. If that assumption fails, then 0.5 will not work
#  well as a cutoff.
#  2) The default value of 0.5 is not necessarily optimal. We should generate a ROC
#  curve in order to assess the potential for a more optimal threshold and to 
#  evaluate the cost-benefit of a particular threshold in terms of FP-TP rates.

# As you may have observed in EDA, the DONR=1 class represents approximately 5%
# of the dataset. This explains why the logistic regression scores are on the scale
# of 0.05. As such, using the default cutoff of 0.5 would result in all individuals
# being classified as non-donors. While this would be a very unhelpful classifcation
# model, it would have 95% accuracy since the model would be incorrect only 5% of 
# the time.
trnProbsA1 = predict(modelA1,type="response")
hist(trnProbsA1,col="gray")   # Note that scores are distributed around 0.05.
hist(trnProbsA1,col="gray",xlim=c(0,1))   # Rescale to make obvious.

# ROC Curve for Model A1 - Use methods from pROC package.
rocA1 = roc(response=classData$DONR[trainIdx],predictor=trnProbsA1)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocA1,col="blue",
     main=paste("ROC curve for Model A1\nAUC = ",round(rocA1$auc,digits=3),sep=""))
par(pty="m")

# Determine "optimal" threshold.
# Note: There is no single rule to define an "optimal" threshold. We must apply
# our own judgment within the context of the classification problem.
# 
# One rule of thumb for determining an optimal point on the ROC curve is to select 
# the point closest to the upper-left corner (coordinates (0,1)). This rule gives 
# equal weight to TP and FP. We know that is not appropriate in some cases.
# Note: Since the ROC curve is plotted with Specificity on the horizontal axis
# (instead of FP, where FP = 1-Specificity) and the horizontal axis goes
# from 1 down to 0, I will be using the coordinates (1,1) in the distance formula.
#dist01 = sqrt((rocA1$specificities-1)^2 + (rocA1$sensitivities-1)^2)
dist01 = sqrt((0.68*(rocA1$specificities-1))^2 + (15.62*(rocA1$sensitivities-1)^2))
optIdxA1 = which.min(dist01)  # index corresponding to minimum distance
threshA1 = rocA1$thresholds[optIdxA1]  # threshold corresponding to min. distance
points(rocA1$specificities[optIdxA1],rocA1$sensitivities[optIdxA1],col="red",pch=7)



# A few notes on the ROC curve.
#  1) Given the ROC curve we get for the charity classification problem, there don't
#  appear to be a lot of great choices for the threshold. Recall that we are not
#  going to get a particularly good classification model on this problem (though
#  it won't matter so much when we get to Part 3 of the project).
#  2) A better ROC curve would go higher into the top-left corner. Therefore, you
#  could find a threshold that provides a much higher TP for much lower FP. We don't
#  see that scenario with this project.
#  3) Depending on the context of the classification problem, you may only be able
#  to tolerate a certain amount of FP or you may be able to tolerate quite a bit of
#  FP.
#  4) For the mailing problem, FPs are cheap (we mail to a non-donor at a cost of
#  $0.68) and FNs (or missed TPs) are comparably more expensive (we lose out on a 
#  donation that has an average value of $15.62). If you are interested in 
#  incorporating these relative weights into the classification model, then
#  you could replace the dist01 calculation above with the following:
#     sqrt((0.68*(rocA1$specificities-1))^2 + (15.62*(rocA1$sensitivities-1)^2))
#  Note that if you do so, you will end up with a tremendous number of FPs but you
#  should end up capturing most of the TPs.

## Part B - Multiple Logistic Regression

# Using my version of the dataset, I will fit the full model (ModelB1) and a 
# subset model (ModelB2) using forward stepwise selection. Your dataset may vary at 
# this point based on derived/transformed/dropped variables.

# IMPORTANT NOTE: The model notation "DONR ~ .-ID" (namely using all variables minus
# a select few) is valid and should work. However, I have encountered some cases 
# where an error was caused using this notation. I have not figured out why the error
# shows up, but I have found that changing the model specification to the form
# "DONR ~ AGE + HOME + HINC" and so on (adding all variables that you want using plus
# signs) makes the error go away. If you encounter an error in building your model,
# try making this change to see if the error goes away. If the error remains, then
# you have something else wrong with your model and should keep troubleshooting
# (including posting a question to the Q & A).

# Full Regression Model (minus ID which is not to be used as a predictor)
modelB1 = glm(DONR ~ .-ID,data=classData2,subset=trainIdx,family=binomial)
summary(modelB1)

# ROC Curve for Model A1 - Use methods from pROC package.
trnProbsB1 = predict(modelB1,type="response")
rocB1 = roc(response=classData$DONR[trainIdx],predictor=trnProbsB1)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
plot(rocB1,col="blue",
     main=paste("ROC curve for Model B1\nAUC = ",round(rocB1$auc,digits=3),sep=""))
par(pty="m")

# Determine "optimal" threshold.
dist01 = sqrt((rocB1$specificities-1)^2 + (rocB1$sensitivities-1)^2)
optIdxB1 = which.min(dist01)  # index corresponding to minimum distance
threshB1 = rocB1$thresholds[optIdxB1]  # threshold corresponding to min. distance
points(rocB1$specificities[optIdxB1],rocB1$sensitivities[optIdxB1],col="red",pch=7)



# Logistic Regression on a Subset of Variables 
# Note that the topic of automated variable selection (e.g. forward stepwise 
# selection) for logistic regression models is not fully fleshed out in ISLR.
# For that reason, it is acceptable to me if you select variables for multiple 
# logistic regression based on EDA that you performed.

## Part C - Tree-Based Models
# A few notes about fitting a classification tree to the charity data.
#  1) tree(DONR ~ .-ID,data=classData2,subset=trainIdx) gives you a single node tree
#  This is not desirable for a classification model. Basically, every individual is
#  labelled a non-donor (as with logistic regression and a cutoff of 0.5).
#  2) One thing we can do to result in the tree method building more
#  than a single node tree, is set split="gini". See pp. 311-12 of ISLR for 
#  discussion of the Gini index.
#  3) When we set split="gini", we next get an error that maximum tree depth has
#  been reached. We can ameliorate this error by selecting fewer variables in the
#  model formula (I started with DONR ~ .-ID). I believe this error results from
#  limitations in the way the tree method is implemented.
#  4) Having said all that, with the formula 
#     DONR ~  NGIFTALL + MAXRAMNT + LASTGIFT + TDON
#  I got a tree with 2185 nodes. When I tried to run cv.tree to look at pruning 
#  the tree, I got an error about a singlenode tree that I was unable to resolve.
#  5) The tree package is not the most widely used package for building trees; rpart
#  is much more common. Therefore, I am switching to using the rpart package.
#  6) The rpart method requires some parameter settings in order for it to build 
#  a tree of more than a single node. The parameters are: 
#     method="class" (should be the default setting when DONR is a factor variable, 
#       but at this point it doesn't hurt to be explicit)
#     split="gini" (same as with the tree method)
#     loss=matrix(0,15.62,0.68,0) (sets cost parameters: $0.68 for FP and $15.62 
#       for FN)
fullTree = rpart(DONR ~  NGIFTALL + MAXRAMNT + LASTGIFT + TDON,
                data=classData2,subset=trainIdx,method="class",
                parms=list(split="gini",loss=matrix(c(0,15.62,0.68,0),nrow=2,ncol=2)))
summary(fullTree)
plot(fullTree)
text(fullTree)

# Prune the tree
printcp(fullTree)
cpBest = fullTree$cptable[which.min(fullTree$cptable[,"xerror"]),"CP"]
modelC1 = prune(fullTree,cp=cpBest) # In this case, the optimal tree is the unpruned tree
summary(modelC1)
plot(modelC1)
text(modelC1,pretty=0)

# We do not need a ROC curve for a classification tree. The tree provides the
# classification results directly.

## Part D - SVM Model
svmRadial = svm(DONR ~  NGIFTALL + MAXRAMNT + LASTGIFT + TDON,data=classData2,
              subset=trainIdx,kernel="radial")
summary(svmRadial)

# Note: It will take a while to plot all data points. I recommend taking a random
# sample for the sake of speedier visualization.
if (class(trainIdx) %in% c("integer","numeric")) # trainIdx is numeric
{
  sampleIdx = sample(trainIdx,size=0.1*length(trainIdx))
} else # trainIdx is logical
{
  sampleIdx = sample(which(trainIdx),size=0.1*sum(trainIdx))
}
plot(svmRadial,data=classData2[sampleIdx,],NGIFTALL~TDON)

# Tune SVM model
# I'm getting excessively long run times in trying to tune the SVM model.
# I'm going to defer that step for now.
# tuneLinear=tune(svm,DONR ~ NGIFTALL + MAXRAMNT + LASTGIFT + TDON,
#                 data=classData2[trainIdx,],kernel="linear",
#                 ranges=list(cost=c(1,5)),sampling="fix")
# summary(tuneLinear)
modelD1 = svmRadial

########################################
## Exercise 7
## Model Validation
########################################

# For each model, I will generate predictions for all data (Train and Test). I
# will then calculate the Train and Test Classification Accuracy, Confusion Matrices,
# and TP and FP Rates by subsetting the predictions accordingly. The following 
# functions will perform those tasks for me.
assignClass = function(probVals,threshVal)
{
  predVals = rep(0,length(probVals))
  predVals[probVals > threshVal] = 1
  predVals = factor(predVals)
  
  return(predVals)
}

calcMetrics = function(targetVals,predVals)
{
  confMat = table(targetVals,predVals,dnn=c("Target","Predicted"))
  
  classResults = list(
    confMat = confMat,
    TPrate = round(confMat[2,2] / sum(confMat[2,]),digits=4),
    FPrate = round(confMat[1,2] / sum(confMat[1,]),digits=4),
    accuracy = round(mean(targetVals == predVals),digits=2)
  )
  
  return(classResults)
}

calcResults = function(model,modelLabel,dataSet,trainIdx,threshVal=NULL)
{
  if (!is.null(threshVal) & "glm" %in% class(model)) {
    # Predict for glm models
    probVals = predict(model,dataSet,type="response")
    predVals = assignClass(probVals,threshVal)
  } else if (length(intersect(class(model),c("tree","rpart","randomForest")) > 0)) {
    # Predict for tree, rpart, randomForest models
    predVals = predict(model,dataSet,type="class")
  } else if (length(intersect(class(model),c("lda")) > 0)) {
    # Predict for lda models
    predVals = predict(model,dataSet)$class
  } else if (length(intersect(class(model),c("svm")) > 0)) {
    # Predict for svm models
    predVals = predict(model,dataSet)
  }
    
  results = list(
    name = modelLabel,
    train = calcMetrics(classData2$DONR[trainIdx],predVals[trainIdx]),
    test = calcMetrics(classData2$DONR[-trainIdx],predVals[-trainIdx])
  )
  
  return(results)
}

nModels = 4 # Number of models you fit. I fit 4 models in this sample code.
naTmp = rep(NA,nModels) # Code short-hand.
nanTmp = rep(NaN,nModels)
modelMetrics = data.frame(
  Model = naTmp,
  Train.Accuracy = nanTmp, Train.TP = nanTmp, Train.FP = nanTmp,
  Test.Accuracy = nanTmp, Test.TP = nanTmp, Test.FP = nanTmp
  )

resultsA1 = calcResults(modelA1,"A1",classData2,trainIdx,threshA1)
print(resultsA1$test$confMat)
modelMetrics[1,] = c(resultsA1$name,
                     resultsA1$train$accuracy,resultsA1$train$TPrate,resultsA1$train$FPrate,
                     resultsA1$test$accuracy,resultsA1$test$TPrate,resultsA1$test$FPrate)

resultsB1 = calcResults(modelB1,"B1",classData2,trainIdx,threshB1)
print(resultsB1$test$confMat)
modelMetrics[2,] = c(resultsB1$name,
                     resultsB1$train$accuracy,resultsB1$train$TPrate,resultsB1$train$FPrate,
                     resultsB1$test$accuracy,resultsB1$test$TPrate,resultsB1$test$FPrate)

resultsC1 = calcResults(modelC1,"C1",classData2,trainIdx)
print(resultsC1$test$confMat)
modelMetrics[3,] = c(resultsC1$name,
                     resultsC1$train$accuracy,resultsC1$train$TPrate,resultsC1$train$FPrate,
                     resultsC1$test$accuracy,resultsC1$test$TPrate,resultsC1$test$FPrate)

resultsD1 = calcResults(modelD1,"D1",classData2,trainIdx)
print(resultsD1$test$confMat)
modelMetrics[4,] = c(resultsD1$name,
                     resultsD1$train$accuracy,resultsD1$train$TPrate,resultsD1$train$FPrate,
                     resultsD1$test$accuracy,resultsD1$test$TPrate,resultsD1$test$FPrate)

print(modelMetrics)

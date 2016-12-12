########################################
## PREDICT 422
## Charity Project - Part 3 (The Mailing List Problem)
##
## SampleCodePart3.R
########################################

# RECOMMENDATIONS: Before working with SampleCodePart3.R, perform the following steps.
#  PENDING 1. Alter the functions processPart1 and processPart2 in DataPreparation.R to match
#      the data preparation steps you applied in Exercise 4 of Parts 1 and 2 of the 
#      project.
#  DONE  2. Execute the examples in SaveYourModels.R. Start with a new R session (nothing 
#      in memory) for working with SampleCodePart3.R.

# Load packages required for this code.
library(leaps)
library(glmnet)
library(tree)
library(randomForest)
library(rpart)
library(e1071)

########################################
## Exercise 1
## Read Data from CSV File
########################################

# input source data file
inPath = file.path("~/Dropbox","NU","MACHINE_LEARNING","charity_project","part3")
valData = read.csv(file.path(inPath,"projectDataPart3.csv"),na.strings=c("NA"," "))

# Convert categorical variables to factors
# This is highly recommended so that R treats the variables appropriately.
# The lm() method in R can handle a factor variable without us needing to convert 
# the factor to binary dummy variable(s).
valData$DONR = as.factor(valData$DONR)
valData$HOME = as.factor(valData$HOME)
valData$HINC = as.factor(valData$HINC)

########################################
## Exercise 2
## Predictions on Validation Set
########################################

codePath = file.path("~/Dropbox","NU","MACHINE_LEARNING","charity_project","part3")
source(file.path(codePath,"steven_futter_data_preparation.R"))

## Part A - Apply the Part 1 data processing steps to valData
valDataPart1 = processPart1(valData)
head(valDataPart1,10)


## Part B - Predict DAMT for valData using your chosen model from Part 1
modelPath = file.path("~/Dropbox","NU","MACHINE_LEARNING","charity_project","part3")
load(file.path(modelPath,"modelPart3.RData"))

names(valDataPart1)

# Note that the model I am using is a glmnet model. I must call the predict function
# using the syntax for a glmnet model. Search for help on predict.glmnet for details.
# You can type class(modelPart1) on the command line (after you have loaded the model)
# to determine the class of the model.
x = model.matrix(DAMT~.,data=valDataPart1)[,-1]

# predict(fit3,type='coefficients',s=bestlam)   # from part 1 my notes
# valData$DAMT.Pred = as.numeric(predict(modelPart1,newx=x,type="response"))
# valData$DAMT.Pred = as.numeric(predict(modelPart1,s=modelPart1_bestlam,newx=x))

# Error message for the lasso model 
# > valData$DAMT.Pred = as.numeric(predict(modelPart1,s=modelPart1_bestlam,newx=x))
# Error in `$<-.data.frame`(`*tmp*`, "DAMT.Pred", value = c(19.6508745138337,  : 
#   replacement has 3684 rows, data has 14539

### This works with lm multiple regression instead of using the lasso. 
class(modelPart3) # lm model
str(valData)
valData$DAMT.Pred = predict(modelPart3,newdata=valDataPart1)

# Check the predictions as a sanity check
hist(valData$DAMT.Pred,xlab="DAMT",main="Validation Set",col="gray",breaks=50)
par(pty="s")
plot(valData$DAMT,valData$DAMT.Pred,xlab="Target DAMT",ylab="Predicted DAMT",
     main="Validation Set")
abline(0,1,col="red")
par(pty="m")



## Part C - Apply the Part 2 data processing steps to valData
valDataPart2 = processPart2(valData)

## Part D - Predict DONR and PDONR for valData using your chosen model from Part 2
# Recall DONR = 0 or 1 is the predicted class and PDONR in [0,1] is the predicted 
# probability.
load(file.path(modelPath,"modelPart2.RData"))

# Note that the model I am using is a rpart model. I must call the predict function
# using the syntax for a rpart model. Search for help on predict.rpart for details.
# You can type class(modelPart2) on the command line (after you have loaded the model)
# to determine the class of the model.
#
# Further note that for predict.rpart, we can obtain the predicted class using the
# argument type="class" and the predicted probability using the argument type="prob".
# Each predict method should have some means of obtaining the probabilities. You
# will have to check the documentation for the type of model you are using to 
# determine the appropriate syntax.
class(modelPart2)

?predict
valData$PDONR.Pred = predict(modelPart2,newdata=valDataPart2,type="response")
valData$DONR.Pred = ifelse(valData$PDONR.Pred > modelPart2_prob_threshold, 1, 0)
  
# Check the predictions as a sanity check
table(valData$DONR,valData$DONR.Pred,dnn=c("Target","Predicted"))
hist(valData$PDONR.Pred,xlab="P(DONR=1)",main="Validation Set",col="gray",breaks=50,
     xlim=c(0,1))
plot(valData$DONR,valData$PDONR.Pred,xlab="Target DONR Value",
     ylab="Predicted P(DONR=1)",main="Validation Set")

########################################
## Exercise 3
## Mailing List Selection
########################################

# "Source" the file RankedDonorOutput.R in order to put the function 
# outputForRankedDonors into memory. It is assumed here that all of your R code
# for this part of the project is located in one file directory.
source(file.path(codePath,"RankedDonorOutput.R"))

## Evaluate various mailing list strategies using function outputForRankedDonors

# Rank donors by PDONR.Pred
numBins = 10
out1 = outputForRankedDonors(numBins,rankVar="PDONR.Pred",dataToRank=valData)
print(out1$Donor.Table)
print(out1$Mailing.Table)

# Rank donors by EXAMT.Pred (expected donation amount)
# EXAMT.Pred = PDONR.Pred * DAMT.Pred 
# (likelihood of donation * predicted donation amount)

sum(is.na(valData$DAMT.Pred)) 
sum(is.na(valData$EXAMT.Pred)) 

valData$EXAMT.Pred = valData$PDONR.Pred * valData$DAMT.Pred
# sum(is.na(valData$EXAMT.Pred))  #141 missing

# including this step below because there were 141 is.na values included in EXAMT. 
valData$EXAMT.Pred = ifelse(is.na(valData$EXAMT.Pred),0,valData$EXAMT.Pred)

out2 = outputForRankedDonors(numBins,rankVar="EXAMT.Pred",dataToRank=valData)
print(out2$Donor.Table)
print(out2$Mailing.Table)

# Plot profit profiles
# Note, the following code is minorly complicated by the fact that there may be
# fewer than numBins unique PDONR values. I have worked out the complications, but
# it makes the x-axis for the plot a bit less intuitive.

# Calculate percentiles of breakVals for each profile using the empircal CDF function.
fn1 = ecdf(out1$breakVals)
fn2 = ecdf(out2$breakVals)
yLimits = c(0,500+1000*ceiling(max(c(out1$Mailing.Table$Total.Profit,out2$Mailing.Table$Total.Profit))/1000))
plot(fn1(out1$breakVals)[-1],out1$Mailing.Table$Total.Profit,type='b',col="blue",
     xlab="% Mailed",ylab="Profit ($)",main="Profit Profiles",xlim=c(0,1),ylim=yLimits)
lines(fn2(out2$breakVals)[-1],out2$Mailing.Table$Total.Profit,col="red")
points(fn2(out2$breakVals)[-1],out2$Mailing.Table$Total.Profit,col="red",pch=16)
legend(x="topleft",legend=c("PDONR","EXAMT"),col=c("blue","red"),lty=c(1,1),pch=c(1,16))

# Currently, I am seeing that the highest profit is obtained by mailing everyone.
# Note that the donation and profit values are measured from the known target data.
# However, the potential for profit is driven by the RANKING of individuals within
# the dataset. The ranking is determined by the choice of metric (e.g. EXAMT) and 
# by your models' predicted values. A different choice of models might re-order the
# individuals so that more individuals worth mailing work their way towards the top
# and a higher profit is obtained from mailing fewer individuals.
#
# For this coding example, suppose selecting the top 7 bins determined by EXAMT.Pred 
# is deemed to be the  optimal selection criterion (results using your models may 
# vary). Apply cutoff to valData.
cutOff = out2$breakVals[numBins+1-7]
valMailList = data.frame(ID=valData$ID[valData$EXAMT.Pred >= cutOff])
length(valMailList$ID)

########################################
## Exercise 4
## Predictions on Test Set
########################################

## Part A - Repeat Exercise 1 on projectDataTEST.csv
testData = read.csv(file.path(inPath,"projectDataTEST.csv"),na.strings=c("NA"," "))

testData$HOME = as.factor(testData$HOME)
testData$HINC = as.factor(testData$HINC)


## Part B - Repeat Exercise 2 on projectDataTEST.csv

# Note: The model.matrix method will not allow us to use a dataframe with "missing" 
# columns. Therefore, we add dummy DAMT and DONR columns to testData.
testData$DAMT = -1
testData$DONR = -1

## Apply the Part 1 data processing steps to testData
testDataPart1 = processPart1(testData)

## Predict DAMT for testData using your chosen model from Part 1
# Note that the model I am using is a glmnet model.
x = model.matrix(DAMT ~ .-ID,data=testDataPart1)[,-1]
testData$DAMT.Pred = predict(modelPart3,newdata=testDataPart1)

# Check the predictions as a sanity check
summary(testData$DAMT.Pred)  ### en needed - predicts a min of -7.621 !! needs to have min=0.

## Apply the Part 2 data processing steps to valData
testDataPart2 = processPart2(testData)

## Predict DONR and PDONR for valData using your chosen model from Part 2
testData$PDONR.Pred = predict(modelPart2,newdata=testDataPart2,type="response")
testData$DONR.Pred = ifelse(testData$PDONR.Pred > modelPart2_prob_threshold, 1, 0)


# Check the predictions as a sanity check
table(testData$DONR.Pred)
summary(testData$PDONR.Pred)

## Part C - Write Test Set Predictions to CSV File
# Name the columns in the CSV file ID, DONR, PDONR, DAMT
testPredOut = data.frame(ID = testData$ID,
                         DONR = testData$DONR.Pred,
                         PDONR = testData$PDONR.Pred,
                         DAMT = testData$DAMT.Pred)

outPath = file.path("~/Dropbox/NU/MACHINE_LEARNING/charity_project/part3")

write.csv(testPredOut,file=file.path(outPath,"projectPredictionsTEST.csv"),
          row.names=FALSE)

## Part D - Apply Mailing List Strategy to Test Data
# Use cutoff selected above.
testData$EXAMT.Pred = testData$PDONR.Pred * testData$DAMT.Pred
testMailList = data.frame(ID=testData$ID[testData$EXAMT.Pred >= cutOff])
length(valMailList$ID)

## Part E - Write Test Set Mailing List to CSV File
write.csv(testMailList,file=file.path(outPath,"projectListTEST.csv"),row.names=FALSE)


# figure out why there are a lot of NA values in the projectListTEST.csv file. 
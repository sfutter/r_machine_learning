# 1. Read Data from CSV File
# Read the data into R from the CSV file projectDataPart2.csv. 
# a. Here I use the na.strings="NA" argument to encode the missing values. 
# b. I set stringsAsFactors=TRUE 
# c. Finally, I use the colClasses argument to update DONR, HOME, and HINC from integers to factors. 
# Note that the additional colClasses option was added because DONR, HOME, and HINC were not automatically 
# set to factors. For the purpose of this analysis it is necessary for these variables to be factors.

setwd('/Users/stevenfutter/Dropbox/NU/MACHINE_LEARNING/charity_project/part2')
charity = read.csv('projectDataPart2.csv', header=T, na.strings=c("NA"," "),stringsAsFactors = TRUE,
                   colClasses=c("DONR"="factor", "HOME"="factor","HINC"="factor"))

# set up $ expansion
attach(charity)

# check that column types are set up correctly
str(charity)

# confirm size of data table
dim(charity) # 71700 21. This is much larger than the csv file in part1

# take a peek at the data
head(charity)



# 2. Data Quality Check
# The purpose of a data quality check is for the user to get to know the data. The data quality check is a quick
# summary of the values of the data. The summary can be tabular or graphical, but in general you want to know
# the value ranges, the shape of the distributions, and the number of missing values for each variable in the
# dataset.
# a. Use R to perform a data quality check on the dataset provided. Report your findings.
sapply(charity, function(x) sum(is.na(x)))
sapply(charity, function(x) sum(x==' '))

# b. Are there any missing values in the data?
# c. Does the data quality check indicate that there are any data anomalies or features in the data that might
# cause issues in a statistical analysis?

# report findings from data quality check
# missing values in the data
# data anomalies or features that might cause issues in statistical analysis?


#### THIS IS WHAT I DID BEFORE>>>> 

#a. We now look at a quick summary of the values of the data to get a feel for the value ranges, shape of the distributions, and the number of missing values for each variable in the dataset. To do this we use R's Hmisc library to describe the dataset, as below. 

#As can be seen in the output MEDHVAL is highly skewed since the mean and median numbers are quite different. Other variables have extreme high and low values. For example, RAMNTALL, the dollar amount of lifetime gifts to date, has 290.8 as the 95% percentile value, but the highest values are 1190, 1610, 1622, 1765, and 2200. Other variables are skewed, but this is better highlighted by the matrix of histograms. See part (c) below. 


library(Hmisc)
# describe(charity)
Hmisc::describe(charity


#b. As can be seen in the above output, Household Income (HINC) has 453 missing values. Depending on the analysis we choose we may need to impute some of these missing values before fitting any models. By including missing values into a training model, the model is likely to produce less accurate predictive accuracy.

#c. As mentioned in part (a), besides the missing values it appears that many of the variables in the data set are either positively or negatively skewed. This is best represented by a matrix of histograms, as below. The skewness or non-normality in some of the variables of the dataset may cause issues for some type of model fits which rely upon normality in the residuals. In addition, many of the variables have extreme values: RAMNTALL, MAXRAMNT, and LASTGIFT appear to have especially large extreme values. 

library(plyr)
library(psych)
multi.hist(charity[,sapply(charity, is.numeric)])



# EDA
df = charity[,-1]   # remove ID variable
names(df)
df.num = df[,sapply(df, class) %in% c("numeric", "integer") ]

# install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)
PerformanceAnalytics::chart.Correlation(df.num)




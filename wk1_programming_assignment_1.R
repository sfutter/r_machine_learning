# Exercise 8 of Section 2.4 of ISLR.

# 8. 

# Overview of college.csv data table
# • Private : Public/private indicator
# • Apps : Number of applications received
# • Accept : Number of applicants accepted
# • Enroll : Number of new students enrolled
# • Top10perc : New students from top 10 % of high school class
# • Top25perc : New students from top 25 % of high school class
# • F.Undergrad : Number of full-time undergraduates
# • P.Undergrad : Number of part-time undergraduates
# • Outstate : Out-of-state tuition
# • Room.Board : Room and board costs
# • Books : Estimated book costs
# • Personal : Estimated personal spending
# • PhD : Percent of faculty with Ph.D.’s
# • Terminal : Percent of faculty with terminal degree
# • S.F.Ratio : Student/faculty ratio
# • perc.alumni : Percent of alumni who donate
# • Expend : Instructional expenditure per student
# • Grad.Rate : Graduation rate

# (a) Use the read.csv() function to read the data into R. Call the
# loaded data college. Make sure that you have the directory set
# to the correct location for the data.
college = read.csv('/Users/stevenfutter/Dropbox/NU/MACHINE_LEARNING/College.csv')

# (b) Look at the data using the fix() function. You should notice
# that the first column is just the name of each university. We don’t
# really want R to treat this as data. However, it may be handy to
# have these names for later. 
rownames(college) = college[,1]
fix(college)

# You should see that there is now a row.names column with the
# name of each university recorded. This means that R has given
# each row a name corresponding to the appropriate university. R
# will not try to perform calculations on the row names. However,
# we still need to eliminate the first column in the data where the
# names are stored. Try
college = college[,-1]
fix(college)

# Now you should see that the first data column is Private. Note
# that another column labeled row.names now appears before the
# Private column. However, this is not a data column but rather
# the name that R is giving to each row.
# (c) i. Use the summary() function to produce a numerical summary
# of the variables in the data set.
summary(college)

# ii. Use the pairs() function to produce a scatterplot matrix of
# the first ten columns or variables of the data. Recall that
# you can reference the first ten columns of a matrix A using
# A[,1:10].
pairs(college[,1:10])


# iii. Use the plot() function to produce side-by-side boxplots of
attach(college)
# Create boxplot of Outstate by Private
boxplot(Outstate~Private,data=college, main="Out-of-state Tuition in Private vs Non-Private Colleges", xlab="Private", ylab="Out-of-state Tuition")

# iv. Create a new qualitative variable, called Elite, by binning
# the Top10perc variable. We are going to divide universities
# into two groups based on whether or not the proportion
# of students coming from the top 10 % of their high school
# classes exceeds 50 %.
Elite = rep('no', nrow(college))
Elite[college$Top10perc > 60]='Yes'
Elite = as.factor(Elite)
college = data.frame(college, Elite)

# Use the summary() function to see how many elite universities
# there are. Now use the plot() function to produce
# side-by-side boxplots of Outstate versus Elite.
summary(college)

# Boxplot of Outstate by Elite
boxplot(Outstate~Elite,data=college, main="Outstate versus Elite", 
        xlab="Elite", ylab="Out-of-state Tuition")

# v. Use the hist() function to produce some histograms with
# differing numbers of bins for a few of the quantitative variables.
# You may find the command par(mfrow=c(2,2)) useful:
#   it will divide the print window into four regions so that four
# plots can be made simultaneously. Modifying the arguments
# to this function will divide the screen in other ways.
# 4 figures arranged in 2 rows and 2 columns
par(mfrow=c(2,2))
hist(Outstate, main="Histogram of Out-of-state Tuition", breaks=20)
hist(Expend, main="Histogram of Instructional Expenditure per Student")
hist(Room.Board, main="Histogram of Room and Board Costs")
hist(Books, main="Histogram of Estimated Book Costs", breaks=25)


# vi. Continue exploring the data, and provide a brief summary
# of what you discover.
# Let's see where NU is ranked in terms of difficulty of being accepted.
pct.accept = Accept / Apps
college = data.frame(college, pct.accept)
rank.difficulty.accepted = rank(pct.accept)
college = data.frame(college, rank.difficulty.accepted)
columns = c('Apps', 'Accept', 'pct.accept', 'rank.difficulty.accepted')
head(college[order(college$rank, decreasing= F),columns], n = 30)


# Complete Exercise 9 of Section 2.4 of ISLR.
# 9. This exercise involves the Auto data set studied in the lab. Make sure
# that the missing values have been removed from the data.
# such as a question mark it should be treated as a missing element of the data matrix
auto = read.table('/Users/stevenfutter/Dropbox/NU/MACHINE_LEARNING/Auto.data.txt', header=T, na.strings='?')
dim(auto)
# only 5 rows contain missing obs
auto = na.omit(auto)
dim(auto)


# (a) Which of the predictors are quantitative, and which are qualitative?
# By running head(auto) I show that the predictor variables fall into two categories, as below:
attach(auto)
head(auto)
# Qualitative: cylinders, year, origin, name
# Quantitative: mpg, displacement, horsepower, weight, acceleration

# (b) What is the range of each quantitative predictor? You can answer
# this using the range() function. range()
range(mpg)
range(displacement)
range(horsepower)
range(weight)
range(acceleration)


# (c) What is the mean and standard deviation of each quantitative predictor?
auto.quant = auto[c(1,3,4,5,6)]         # selects the quantitative columns, only
sapply(auto.quant, mean, na.rm = TRUE)  # sapply on auto.quant data.frame to calculate the mean
sapply(auto.quant, sd, na.rm = TRUE)    # sapply on auto.quant data.frame to calculate the standard deviation


# (d) Now remove the 10th through 85th observations. What is the range, mean, and standard deviation of 
# each predictor in the subset of the data that remains?
# use of negative sign tells R to keep all rows/cols except those indicated with the -ve sign
# selects the rows/cols of importance. -ve values mean remove the rows 10-85. 
auto.quant.rm.10.85 = auto[-c(10,85),c(1,3,4,5,6)] 
sapply(auto.quant.rm.10.85, range, na.rm = TRUE)  # sapply to calculate the range
sapply(auto.quant.rm.10.85, mean, na.rm = TRUE)   # sapply to calculate the mean
sapply(auto.quant.rm.10.85, sd, na.rm = TRUE)     # sapply to calculate the standard deviation


# (e) Using the full data set, investigate the predictors graphically,
# using scatterplots or other tools of your choice. Create some plots
# highlighting the relationships among the predictors. Comment
# on your findings.
# Use pairs to see correlations between all variables in auto data set. 
pairs(auto)

# THere is a lot going on so lets try to reduce down the data set to the most correlated 
# variables
library(dplyr)
library(reshape2)
d.cor = as.matrix(cor(auto.quant))
d.cor.melt = arrange(melt(d.cor), -abs(value))
d.cor.melt[c(6,8,10,12,14,16,18,20,22,24),]

# Lets plot the four most correlated variables
par(mfrow=c(2,2))
pairs(~ mpg + displacement + horsepower + weight, auto)


# (f) Suppose that we wish to predict gas mileage (mpg) on the basis
# of the other variables. Do your plots suggest that any of the
# other variables might be useful in predicting mpg? Justify your
# answer.
par(mfrow=c(2,1))
boxplot(mpg~year,data=auto, main="Increasing MPG by Year (1970-1982)", xlab="Year", ylab="Miles Per Gallon (MPG)")
boxplot(mpg~cylinders,data=auto, main="MPG By Cylinder Count (1970-1982)", xlab="Cylinders", ylab="Miles Per Gallon (MPG)")

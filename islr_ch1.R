# Create a vector
x <- c(1,3,2)
x

y = c(1,3,4)
y

# check length of vector
length(x)
length(y)

# look at list of all objects created
ls()

# remove objects
rm(c,g)
ls()

# remove all objects at once
rm(list=ls())

# see the help function for the funcname (matrix)
?matrix

# create a matrix
x = matrix(data=c(1,2,3,4), nrow=2, ncol=2)
x

# is the same as -- can omit nrow and ncol
x = matrix(data=c(1,2,3,4), 2,2)
x

# or use the row=TRUE option and work within parameter ordering
x = matrix(c(1,2,3,4), 2,2, byrow=TRUE)
x

# square root and squared values
sqrt(x)
x^2

# rnorm() generates a vector of random normal variables with first arg 'n' the sample size
rnorm(x)

# cor() correlates two vectors of equal length
# note that rnorm() creates a random variable with a mean of 0 and sd of 1, but can be altered as below
x = rnorm(50)
y = x + rnorm(50, mean=50, sd=0.1)
cor(x,y)

# use set.seed(integer) to ensure that user can reproduce same results
set.seed(1303)
rnorm(50)

# calc mean, var, and sd
set.seed(3)
y=rnorm(100)
var(y)
sqrt(var(y))
sd(y)

# Graphics 2.3.2
x = rnorm(100)
y = rnorm(100)

# plot(x,y) produces a scatterplot
plot(x,y)

# additional args
plot(x,y, xlab='this is the x-axis', ylab='this is the y..', main='this is the header')

# to create a PDF
pdf('Figure.pdf')
plot(x,y, col='green')
dev.off() # indicates that we are done creating the plot

# create vector integers between 1 and 10
x = seq(1,10)
x

# create vector of integers equally spaced between 0 and 1
x = seq(0,1,length=10)
x

x = seq(3:13)
x

x = seq(3,13)
x

x = seq(-pi, pi, length=50)
x

# Contours ... fancier graphs
y = x
f = outer(x,y, function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f, nlevel=45, add=T)
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)

image(x,y,fa)
persp(x,y,fa)

# Indexing
A = matrix(seq(1,16),4,4)
A

# first is row then column
A[2,3]
A[1,2]

# select multiple rows, cols
A[1:3, 2:3]
A[1:2,]
A[,1:2]

# use of negative sign tells R to keep all rows/cols except those indicated with the -ve sign
A[-c(1,2),]

# dim() outputs the number of rows and cols of a given matrix
dim(A)

# Note that na.strings='?' tells R that any time it sees a particular char or set of chars
# such as a question mark it should be treated as a missing element of the data matrix
Auto = read.table('/Users/stevenfutter/Dropbox/NU/MACHINE_LEARNING/Auto.data.txt',
                  header=T, na.strings='?')
# open in spreadsheet-like window
fix(Auto)
dim(Auto)

# only 5 rows contain missing obs
Auto = na.omit(Auto)

# checks the variable names
names(Auto)

# can use attach(Auto) to make variables in the data table avialable by name
attach(Auto)
plot(cylinders, mpg)
plot(cylinders , mpg , col ="red", varwidth =T)

hist(mpg)
hist(mpg, col=2) #col=2 has the same effect as col='red'
hist(mpg, col=2, breaks=15)

# Create scatterplot matrix for either all pairs of variables in data table, or selection
pairs(Auto)
# selection only... REALLY USEFUL FUNCTION!!!! 
pairs(~ mpg + displacement + horsepower + weight + acceleration , Auto)

#allows user to click into each point in scatter plot to see the underlying values
identify(horsepower, mpg, name)

# quick summary of key statistics
summary(Auto)
summary(mpg)


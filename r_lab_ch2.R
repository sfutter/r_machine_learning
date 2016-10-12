 x = c(2,7,5)
 x
 
 y=seq(from=4, length=3, by=3)
 y
 
 ?seq
 
# r does element wise 
x+y
x/y
 
# accessing various elements -- use the [] braces
x[2]
x[2:3]

# remove the element 2 from x and return subsetted vector
x[-2] 

z = matrix(seq(1,12),4,3)
z 
z[3:4, 2:3]
z[,2:3]
z[,1,drop=FALSE] #drop keeps 1 col as matrix 
ls()

# generating random data, graphics
# random uniform
x = runif(50)
x

# random normal
y = rnorm(50)

plot(x,y)
plot(x,y,xlab='random uniform', ylab='random normal', pch='*', col='blue')

# create panel of plots with 2 rows and 1 column
par(mfrow=c(2,1))
plot(x,y)
hist(y)

# reading data 
auto = read.table('/Users/stevenfutter/Dropbox/NU/MACHINE_LEARNING/Auto.data.txt', header=T)
dim(auto)

class(auto)
plot(auto$cylinders, auto$mpg)

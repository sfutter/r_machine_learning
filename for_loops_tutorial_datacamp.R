separateRFA_TEST = function(df,colName){
  newColName = paste('RECENCY_',substr(colName,5,6),sep='')
  df[,newColName] = sapply(df[,colName],substring,1,1)
  return(df)
}

separateRFA_TEST(valData,'RFA_96')



# This path is specified wrt a Mac, the Windows path will start differently
inPath = file.path("~/Dropbox","NU","MACHINE_LEARNING","charity_project","part3")
valData = read.csv(file.path(inPath,"projectDataPart3.csv"),na.strings=c("NA"," "))
head(valData)
str(valData)

RECENCY_96      = sapply(df[,1],substring,char,char)
FREQUENCY_96    = sapply(df[,1],substring,char+1,char+1) 
AMOUNT96        = sapply(df[,1],substring,char+2,char+2)
df$RECENCY_96   = data.frame(RECENCY_96)
df$FREQUENCY_96 = data.frame(FREQUENCY_96)
df$AMOUNT96     = data.frame(AMOUNT96)

head(valDataPart1,10)











# https://www.datacamp.com/community/tutorials/tutorial-on-loops-in-r#gs.A6Jibzc

# Create a vector filled with random normal values
u1 <- rnorm(30)
print("This loop calculates the square of the first 10 elements of vector u1")

# Initialize `usq`
usq <- 0

for(i in 1:10) {
  # i-th element of `u1` squared into `i`-th position of `usq`
  usq[i] <- u1[i]*u1[i]
  print(usq[i])
}

print(i)



# NESTING FOR LOOPS
# Create a 30 x 30 matrix (of 30 rows and 30 columns)
mymat <- matrix(nrow=30, ncol=30)

# For each row and for each column, assign values based on position: product of two indexes
for(i in 1:dim(mymat)[1]) {
  for(j in 1:dim(mymat)[2]) {
    mymat[i,j] = i*j
  }
}

# Just show the upper left 10x10 chunk
mymat[1:10, 1:10]






# 3x3 matrix using array()
# Create your three-dimensional array
my_array <- array(1:20, dim=c(20, 20, 20))

for (i in 1:dim(my_array)[1]) {
  for (j in 1:dim(my_array)[2]) {
    for (k in 1:dim(my_array)[3]) {
      my_array[i,j,k] = i*j*k
    }
  }
}

# Show a 10x10x15 chunk of your array
my_array[1:10, 1:10, 1:15]






# Your User Defined Function
readinteger <- function(){
  n <- readline(prompt="Please, enter your ANSWER: ")
}


while (response!=42) {   
  print("Sorry, the answer to whatever the question MUST be 42");
  response <- as.integer(readinteger());
}



# use of next
m=20

for (k in 1:m){
  if (!k %% 2)      # modulus 2 ---- only numbers that have no remainder after divisible by 2.
    next
  print(k)
}




# vectorization - avoid the need for loops...
set.seed(42)
m=10
n=10
mymat <- replicate(m, rnorm(n)) 
mymat

mydframe <- data.frame(mymat)
mydframe <- mydframe + 10*sin(0.75*pi)
head(mydframe)




# apply() function... see the family of apply functions s, l, etc...
# define matrix `mymat` by replicating the sequence `1:5` for `4` times and transforming into a matrix
mymat<-matrix(rep(seq(5), 4), ncol = 5)

# `mymat` sum on rows
apply(mymat, 1, sum)

# `mymat` sum on columns
apply(mymat, 2, sum)


mymat
# With user defined function within the apply that adds any number `y` to the 
# sum of the row `y` is set at `4.5` 
apply(mymat, 1, function(x, y) sum(x) + y, y=4.5)

# Or produce a summary column wise for each column
apply(mymat, 2, function(x, y) summary(mymat))




# https://www.r-bloggers.com/the-r-apply-function-a-tutorial-with-examples/
# more examples of the apply function


# Create the matrix
m<-matrix(c(seq(from=-98,to=100,by=2)),nrow=10,ncol=10)
head(m)
tail(m)

# Return the product of each of the rows
apply(m,1,prod)

# Return the sum of each of the columns
apply(m,2,sum)





# examples of sapply
x = data.frame(c('ABCD', 'EFGH'), row.names=c('1A', '1B'))
x
sapply(x, substring, 2, 4)
substring(x[,1], 2, 4) 



d


recency97 = sapply(d[,1],substring,1,1)
data.frame(recency97)

recency97 = sapply(d[,d$rfa],substring,1,1)
recency97 = data.frame(recency97)
recency97

?sapply


d = d[,1:2]
d


splitter = function(df,colName){
  # create new columns in data frame 'df' 
  char = 1
  recency97      = sapply(df[,1],substring,char,char)
  frequency97    = sapply(df[,1],substring,char+1,char+1) 
  amount97       = sapply(df[,1],substring,char+2,char+2)
  df$recency97   = data.frame(recency97)
  df$frequency97 = data.frame(frequency97)
  df$amount97    = data.frame(amount97)
  
  return(df)
  
}

splitter(d,rfa)










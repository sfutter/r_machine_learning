# Notes from https://www.youtube.com/watch?v=TxvEVc8YNlU

# similar to library we can use require()
install.packages('ISLR')
require(ISLR)  
names(Smarket)
summary(Smarket)
?Smarket

# make plot of data scatterplots
# told pairs to use the binary response as the color indicator (useful for class variables)
pairs(Smarket, col=Smarket$Direction)

# Logistic Regression - family binomial
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
              data=Smarket, family=binomial) 

# seems like none of coefficients are significant here. 
summary(glm.fit)

# make predictions on the training data we use to fit the model
glm.probs = predict(glm.fit, type='response')
# values around 0.5 which is as expected for financial data.
glm.probs[1:5]
# ifelse takes a vector or true/false then calls up/down
glm.pred=ifelse(glm.probs>0.5, 'Up', 'Down')
attach(Smarket)
# check prediction vs actual
table(glm.pred, Direction)
# get the proportion which are accurately predicted
# here we get 0.5216. 
mean(glm.pred==Direction)

# May have slightly overfit on the training set. 
# Make training and test set
# Create vector of logicals
train = Year<2005


# note here we say that subset=train. Here is years<2005
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
              data=Smarket, family=binomial, subset=train)

# use predict function again but indexed by NOT train 
# i.e. year 2005 onwards
glm.probs = predict(glm.fit, newdata=Smarket[!train,],type='response')
glm.pred = ifelse(glm.probs > 0.5, 'Up', 'Down')
Direction.2005 = Smarket$Direction[!train]

# prediction rate is actually now slightly lower than 0.5! :-(
# we get a prediction accuracy on 'test' data of 0.48. 
# evidence of over fitting on training data. Lets use diff model below.
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

# Fit smaller model (on lag 1 and lag 2)
glm.fit = glm(Direction~Lag1+Lag2,
              data=Smarket, family=binomial, subset=train)

glm.probs = predict(glm.fit, newdata=Smarket[!train,],type='response')

glm.pred = ifelse(glm.probs > 0.5, 'Up','Down')

table(glm.pred, Direction.2005)

# correct classification is now about 56%. Decent.
mean(glm.pred==Direction.2005)

# Better predictions, but none of the vars are significant. But
# the performance for prediction has improved. 
summary(glm.fit)

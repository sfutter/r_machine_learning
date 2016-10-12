require(ISLR)
names(Smarket)
summary(Smarket)
?Smarket

# useful way to separate out class vars in scatterplot view
pairs(Smarket, col=Smarket$Direction)

# logistic regression --- family = binomial
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
              data=Smarket, family=binomial)
summary(glm.fit) #no coeffs signficant. not big surprise for 
# stock market data. doesnt necessarily mean we can not make 
# decent predictions

# make predictions from the fitted model - use type = response
glm.probs = predict(glm.fit, type='response')
# provides vector of fitted probs
glm.probs[1:5]

# turn probs into classifications by using 0.5 threshold
glm.pred = ifelse(glm.probs > 0.5, 'Up', 'Down')
attach(Smarket)
table(glm.pred, Direction)
mean(glm.pred==Direction)  # mean 0.52 (on training data.. i.e. 
# we have done slightly better than chance.)

# create the test data set
# create vector of logicals
train = Year<2005
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
              data=Smarket, family=binomial, subset=train)
glm.probs = predict(glm.fit, newdata=Smarket[!train,], type='response')
glm.pred=ifelse(glm.probs > 0.5, 'Up', 'Down')
Direction.2005=Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)  # 0.48 on the test data. not as good as 50%! 

# Fit smaller model
glm.fit=glm(Direction~Lag1+Lag2, 
            data=Smarket, family=binomial, subset=train)
glm.probs = predict(glm.fit, newdata=Smarket[!train,],type='response')
glm.pred = ifelse(glm.probs > 0.5, 'Up', 'Down')
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)  # correct classification is 0.56 56%.. better on smaller model.

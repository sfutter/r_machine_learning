# http://www-bcf.usc.edu/~gareth/ISL/ISLR%20Sixth%20Printing.pdf
# Lets use QDA model to the Smarket data. 
library(MASS)

train = (Year<2005)
qda.fit = qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit
# Output produced below 
# Call:
#   qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
# 
# Prior probabilities of groups:
#   Down       Up 
# 0.491984 0.508016 
# 
# Group means:
#   Lag1        Lag2
# Down  0.04279022  0.03389409
# Up   -0.03954635 -0.03132544

# The output contains the group means. But it does not contain the coefficients of the linear discriminants, because the QDA 
# classifier involves a quadratif, rather than a linear, function of the predictors. The predict() function works in 
# exactly the same fashio as for LDA. 

# get the test data set... i.e. year = 2005
Smarket.2005= Smarket [!train ,]
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)
# the QDA predictions are accurate almost 60% of the time, even though the 2005 data was not used to fit the model. This level of 
# accuracy is quite impressive for the stock market data, which is known to be hard to model accurately. This suggests that the 
# quadratic form assumed by QDA may capture the true relationship more accurately than the linear forms assumed by LDA. 

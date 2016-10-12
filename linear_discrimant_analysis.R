# https://www.youtube.com/watch?v=2cl7JiPzkBY
require(ISLR)
require(MASS)

# Linear Discriminant Analysis
lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
lda.fit
plot(lda.fit)
# lets make a subset of the data frame for year = 2005
Smarket.2005=subset(Smarket, Year==2005)
lda.pred = predict(lda.fit, Smarket.2005)
lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class,Smarket.2005$Direction) # is the confusion matrix
mean(lda.pred$class==Smarket.2005$Direction) # 0.5595 - taking the mean is 0.56 correct classification of ups/downs



# http://www-bcf.usc.edu/~gareth/ISL/ISLR%20Sixth%20Printing.pdf reference.
library(MASS)

# fit the lda model using observations before 2005
train = (Year<2005)
lda.fit=lda(Direction~Lag1+Lag2 ,data=Smarket,subset=train)
lda.fit
# LDA output indicates PieHat1 = 0.492 dnd PieHat2 = 0.508; in other words, 49.2% of the training obs correspond
# to days during which the market went down. 
 
# Group means:
#   Lag1        Lag2
# Down  0.04279022  0.03389409
# Up   -0.03954635 -0.03132544

# The above suggests that there is a tendancy for the previous two days to be negative when the market went up. 
# and a tendancy for the previous two days to be positive when the market went down. 

# Coefficients of linear discriminants:
#   LD1
# Lag1 -0.6420190
# Lag2 -0.5135293

# The coefficients of linear discrimants ouptut provides the linear combination of Lag1 and Lag2 that are used
# to form the LDA decision rule. if -0.642 x Lag1 -0.514 x Lag2 is large, then the LDA classifier will predict 
# a market increase, and if it is small, then the lDA classifier will predict a market decline. 


plot(lda.fit)
# The plot function produces plots of the linear discriminants, obtained by computing -0.642 x Lag1 -0.514 x Lag2
# for each of the training obs. 

# the predict() function returns a list with three elements. The first element, class, contains LDAs predictions
# about the movement of the market. The second element, posterior, is a matrix whose kth column contains the 
# posterior probability that the corresponding observation belongs to the kth class. Finally, x contains the 
# linear disciminants.
Smarket.2005= Smarket [!train ,]
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class=lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class==Direction.2005)

# Applying a 50% threshold to the posterior probabilities allows us to recreate the predictions contained in
# lda.pred$class.
sum(lda.pred$posterior[,1] > .5)  #70
sum(lda.pred$posterior[,1] < .5)  #182

# notice that the posterior probability output by the model corresponds to the probability that the market will 
# decrease: 
lda.pred$posterior[1:20,1]
lda.class[1:20]              # in output you can see that Down corresponds to posterior probs >0.5 only. 

# if we wanted to use a posterior prob threhold other than 50% in order to make predictions, we could easily do so. E.g. suppose
# that we wish to predict a market decrease ONLY if we are very certain that the market will decrease on that day. I.e. if the posterior
# probability is at least 90%. 
sum(lda.pred$posterior[,1] > .9) # note: no days in 2005 meet that threshold!. In fact the greatest posterior probability of decrease
# in all of 2005 was 52.02%. 


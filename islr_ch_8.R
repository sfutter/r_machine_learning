# Decision trees


require(ISLR)
require(tree)
attach(Carseats)
hist(Sales)
High=ifelse(Sales<=8,'No','Yes')
Carseats=data.frame(Carseats,High)

tree.carseats=tree(High~.-Sales, data=Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty=0) # terminal nodes are Yes, No. 

# tree above is quite big. 
# to see each node printed out in the tree:
tree.carseats

# split train/test
set.seed(1011)
train=sample(1:nrow(Carseats),250) # 400 obs total
tree.carseats=tree(High~.-Sales, Carseats, subset=train)
plot(tree.carseats);text(tree.carseats, pretty=0)
tree.pred=predict(tree.carseats, Carseats[-train,],type='class')
with(Carseats[-train,],table(tree.pred, High))
(72+33)/150   #0.7 error

cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
cv.carseats
plot(cv.carseats) # see size is 13 where missclass is lowest
prune.carseats=prune.misclass(tree.carseats, best=13)
plot(prune.carseats);text(prune.carseats, pretty=0)
tree.pred=predict(prune.carseats, Carseats[-train,],type='class')
with(Carseats[-train,],table(tree.pred, High))
(72+33)/150  #0.7 error

# Therefore, pruning did not us wrt missclassification errors, 
# but we now have a simpler tree. 

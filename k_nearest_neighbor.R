# https://www.youtube.com/watch?v=9TVVF7CS3F4
library(class)
?knn
attach(Smarket)
Xlag=cbind(Lag1, Lag2)
head(Xlag)
train=Year<2005

knn.pred = knn(Xlag[train,], Xlag[!train,], Direction[train], k=1)
table(knn.pred, Direction[!train])
mean(knn.pred==Direction[!train])  # outcome is 0.5 -- i.e. no better than flip of coin.

knn.pred = knn(Xlag[train,], Xlag[!train,], Direction[train], k=2)
table(knn.pred, Direction[!train])
mean(knn.pred==Direction[!train])  # k =2 increase to 0.5357

knn.pred = knn(Xlag[train,], Xlag[!train,], Direction[train], k=3)
table(knn.pred, Direction[!train])
mean(knn.pred==Direction[!train])  # k =3 decreases the value again to 0.5278


Xlag=cbind(Lag1, Lag2)
head(Xlag)
train=Year<2005
knn.pred = knn(Xlag[train,], Xlag[!train,], Direction[train], k=1)
table(knn.pred, Direction[!train])
mean(knn.pred==Direction[!train]) 

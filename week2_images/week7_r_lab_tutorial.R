# ISLR Chapter 10 notes
# http://www-bcf.usc.edu/~gareth/ISL/ISLR%20Sixth%20Printing.pdf


# LAB 1: PRINCIPAL COMPONENTS ANALYSIS

# 50 states in data set
states = row.names(USArrests)
states

# columns contain 4 vars
names(USArrests)

# the variables have vastly different means
# apply() func -- 1 = rows; 2 = columns
# x3 as many rapes as murders..etc.
apply(USArrests, 2, mean)

# sames as above but for variance
# all variables have vastly different variances too.
apply(USArrests, 2, var)

# Given that the variables have different scales it is necessary to 
# standardize the variables to have mean of zero and standard deviation one
# before performing PCA. 
# prcomp() function centers the variables to have mean zero. 
# scale=TRUE scales the variables to have standard deviation of one.
pr.out = prcomp(USArrests, scale=TRUE)

# center and scale correspond to the means and standard deviations of the 
# variables that were used for scaling prior to implementing PCA.
names(pr.out)
pr.out$center
pr.out$scale

# the rotation matrix provides the PC loadings; each column of pr.out$rotation
# contains the corresponding principal component loading vector.
# We see that there are 4 principal comps.
pr.out$rotation 

# with prcomp() we do not need to explicitly multiply the data by the princ
# component loading vectors in order to obtain the prin comp score vectors.
# Rather the 50 x 4 matrix 'x' has as its columns the PC score vectors. o.e., the 
# kth col is the kth PC score vector.
dim(pr.out$x)

# Can plot the first two PCs as follows:
# scale=0 ensures the arrows are scaled to represent
# the loadings.
biplot(pr.out, scale=0)

# create mirror image
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)

# prcomp() also outputs standard deviation of each PC.
pr.out$sdev

# the variance explained by each PC is obtained by squaring the st. dev.
pr.var = pr.out$sdev^2
pr.var

# to compute the proportion of variance explained by each
# PC, we simply divide the variance explained by each PC
# by all four PCs.
pve = pr.var/sum(pr.var)
pve

# We see that the first PC explains 62% of the variance 
# in the data, the next PC 24.7%, and so on.
# Can plot the PVE explained by each component, as well
# as the cumulative PVE, as follows:
par(mfrow=c(1,2))
plot(pve, xlab='Principal Component', ylab='Proportion of
     Variance Explained', ylim=c(0,1), type='b')
plot(cumsum(pve), xlab='Principal Component', ylab='
     Cumulative Proportion of Variance Explained',
     ylim=c(0,1), type='b')

# Note that function cumsum() computes the cumulative
# sum of the elements of a numeric vector.
# e.g. 
# a = c(1,2,8,-3)
a = c(1,2,8,-3)
cumsum(a)


# LAB 2: K-MEANS CLUSTERING
set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
x[1:25, 1]=x[1:25, 1]+3
x[1:25, 2]=x[1:25, 2]-4


# K-means clustering with K=2
km.out=kmeans(x,2,nstart=20)

# the cluster assignments of the 50 obs are contained in km.out$cluster
km.out$cluster

# we can plot the data, with each obs colored ac to its cluster assignment
plot(x, col=(km.out$cluster+1), main='K-Means clustering results K=2',
     xlab="", ylab="", pch=20, cex=2)

# K-means clustering with K=3
set.seed(4)
km.out=kmeans(x, 3, nstart=20)
km.out
plot(x, col=(km.out$cluster+1), main='K-Means clustering results K=3',
     xlab="", ylab="", pch=20, cex=2)


# nstart argument: is the multiple random assignment part of the algo. 
# nstart=20 means that 20 random assignments of clusters are made to initiate
# the clustering. kmeans() function then reports the best result from each.
# suggestion by author is to use nstart=20 or 20 or higher.e.g.
# note: tot.withinss = total within-cluster sum of squares, which we are seeking
# to minimize by performing K-means clustering.
set.seed(3)
km.out=kmeans (x,3, nstart =1)
km.out$tot.withinss # nstart=1 has 104.3319 which is higher than (see below)
km.out=kmeans (x,3, nstart =20)
km.out$tot.withinss # nstart=20 has 97.97927 sum-of-squares.


###  HIERARCHICAL CLUSTERING
hc.complete= hclust(dist(x), method='complete')
hc.average= hclust(dist(x), method='average')
hc.single= hclust(dist(x), method='single')

# plot dendrograms
par(mfrow=c(3,1))
plot(hc.complete ,main="Complete Linkage ", xlab="", sub="",
     cex=.9)
plot(hc.average ,main="Averate Linkage ", xlab="", sub="",
     cex=.9)
plot(hc.single ,main="Single Linkage ", xlab="", sub="",
     cex=.9)

# To determine cluster labels for each obs associated with cut of dendrogram
# we can use the cutree() func:
# Complete and average linkage generally separate obs into correct groups.
# Single linkage identifies one point as belonging to its own cluster.
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)

# A more sensible answer is obtained when four clusters are selected,
# although there are still two singletons.
cutree(hc.single, 4)

# To scale variables before performing hierarchical clustering of obs:
xsc=scale(x)
plot(hclust(dist(xsc), method ="complete"), main="Hierarchical 
     Clustering with Scaled Features")

# correlation-based distance can be computed using as.dist() func.
# This only makes sense for data with at least 3 features since the
# absolute correlation between any two observations with measurements 
# on two features is always 1. 
x=matrix(rnorm(30*3), ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method ="complete"), main="Complete Linkage
     with Correlation-Based Distance", xlab="", sub ="")




# LAB 3: NCI60 Data Example
# Unsupervised techniques are often used in analysis of genomic data. 
# PCA and hierarchical clustering are popular tools. 
library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
dim(nci.data)

?NCI60
head(NCI60)
# take a look at the data
nci.labs[1:4]
table(nci.labs)

# PCA on the NCI60 Data
# perform PCA on the data after scaling the variables (genes) to have a 
# standard deviation one, although one could reasonably argue that it is 
# better not to scale the genes.
pr.out=prcomp(nci.data, scale=TRUE)

# plot first few PC score vectors so we can visualize the data.
# the observations (cell lines) corresponding to a given cancer type will
# be plotted in the same color. 
# 1. create simple func that assigns distinct color to each element of a
#    numeric vector. I.e. to each of the 64 cell lines, based on the 
#    cancer type to which it corresponds.
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}


# on the whole, cell lines corresponding to a single cancer type do 'tend'
# to have similar values on the first few principal component score vectors.
# this indicates that cell lines from the same cancer type tend to have 
# similar gene expression levels. 
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19,
     xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19,
       xlab="Z1",ylab="Z3")

# obtain a summary of the proportion of variance explained (PVE) of the 
# the first few principal components:
summary(pr.out)

# can also plot the variance exaplained by the first few prin comps.
# The height of each bar in the plot is given by squaring pr.out$sdev.
plot(pr.out)

# it is more informative to plot the PVE of each PC (i.e. scree plot) 
# and the cumulative PVE of each principal component. 
# We see that the first 7 PCs explain around 40% of the variance in the data.
# After the 'elbow' each additional PC adds little benefit to the examination.
pve =100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve, type="o", ylab="PVE", xlab="Principal Component",
       col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="
       Principal Component ", col="brown3")





# Clustering the Obsersavations of the NCI60 Data
# optional step to standardize mean=0 and st dev=1
# Performing Hierarchical Clustering
sd.data=scale(nci.data)

par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist), labels=nci.labs , main="Complete
       Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist , method ="average"), labels=nci.labs ,
       main="Average Linkage ", xlab="", sub="",ylab="")
plot(hclust(data.dist , method ="single"), labels=nci.labs ,
       main="Single Linkage ", xlab="", sub="",ylab="")


# OUTPUT
# we see choice of linkage certainly does not affect the results obtained.
# Usually, single linkage will tend to yield training clusters: very large
# clusters onto which individual observations attach one-by-one.
# Complete and average tend to yield more balance/attractive clusters and 
# generally preferred to single linkage. Cell lines within a single cancer
# type do tend to cluster together, although the clustering is not perfect.


# Cut dendrogram at the height that will yield a particular number of clusters
# e.g. 4. Let's use complete linkage hierarchical clustering ...
hc.out=hclust(dist(sd.data))
hc.clusters =cutree(hc.out,4)
table(hc.clusters,nci.labs)

# OUTPUT
# all leukemia cell lines fall in cluster 3, breast cancer cell lines
# are spread out over three different clusters.
par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs)
abline(h=139, col='red')

# printing output of hclust gives:
hc.out


# how do hclust results compare to k-means with K=4?
set.seed(2)
km.out=kmeans(sd.data, 4, nstart=20)
km.clusters=km.out$cluster
table(km.clusters,hc.clusters)

# OUTPUT
# we see that the four clusters obtained using hierarchical clustering
# and K-means clustering are somewhat different. Cluster 4 in kmeans 
# contains a portion of the obs assigned to cluster 1 by hierarchical
# clustering, as well as all the obs assigned to cluster 2 by 
# hierarchical clustering. 


# Rather than performing hierarchical clustering on the entire data matrix,
# we can perform hierarchical clustering on the first few PC score vectors:
hc.out=hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels=nci.labs, main="Hier. Clust. on First
       Five Score Vectors")
table(cutree(hc.out,4), nci.labs)

# OUTPUT
# Results obtained are different to when we performed hierarchical clustering
# on the full data set. Taking the first few PC score vectors can give 
# better results than performing clustering on the full data. "Denoising" 
# the data... Same applies to kmeans clustering on the first few PC score vecs.






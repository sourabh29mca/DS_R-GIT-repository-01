#=========KMEANS ON IRIS==========
install.packages("mltools")
A = data.frame(iris)
str(A)
library(data.table)
library(mltools)
A = one_hot(as.data.table(A))
#----k = 2----
m1 = kmeans(A,2)
m1$cluster
par(mfrow=c(3,2))
plot(A$Sepal.Length,A$Sepal.Width,col = m1$cluster)
plot(A$Sepal.Length,A$Petal.Length,col = m1$cluster)
plot(A$Sepal.Length,A$Petal.Width,col = m1$cluster)
plot(A$Sepal.Width,A$Petal.Length,col = m1$cluster)
plot(A$Sepal.Width,A$Petal.Width,col = m1$cluster)
plot(A$Petal.Length,A$Petal.Width,col = m1$cluster)

#----k = 3----

m3 = kmeans(A,3)
par(mfrow=c(3,2))
plot(A$Sepal.Length,A$Sepal.Width,col = m3$cluster)
plot(A$Sepal.Length,A$Petal.Length,col = m3$cluster)
plot(A$Sepal.Length,A$Petal.Width,col = m3$cluster)
plot(A$Sepal.Width,A$Petal.Length,col = m3$cluster)
plot(A$Sepal.Width,A$Petal.Width,col = m3$cluster)
plot(A$Petal.Length,A$Petal.Width,col = m3$cluster)

cbind(A$Species,m3$cluster)

#----k = 4----
m4 = kmeans(A,4)
par(mfrow=c(3,2))
plot(A$Sepal.Length,A$Sepal.Width,col = m4$cluster)
plot(A$Sepal.Length,A$Petal.Length,col = m4$cluster)
plot(A$Sepal.Length,A$Petal.Width,col = m4$cluster)
plot(A$Sepal.Width,A$Petal.Length,col = m4$cluster)
plot(A$Sepal.Width,A$Petal.Width,col = m4$cluster)
plot(A$Petal.Length,A$Petal.Width,col = m4$cluster)


#----k = 5-----

m5 = kmeans(A,5)
par(mfrow=c(3,2))
plot(A$Sepal.Length,A$Sepal.Width,col = m5$cluster)
plot(A$Sepal.Length,A$Petal.Length,col = m5$cluster)
plot(A$Sepal.Length,A$Petal.Width,col = m5$cluster)
plot(A$Sepal.Width,A$Petal.Length,col = m5$cluster)
plot(A$Sepal.Width,A$Petal.Width,col = m5$cluster)
plot(A$Petal.Length,A$Petal.Width,col = m5$cluster)
levels(A$Species)
plot(A$Sepal.Length,A$Species,col = m2$cluster)
#===================

#---------ELBOW CURVE---------

A = data.frame(iris)
scaled_data = as.matrix(scale(A[,1:4]))
# Compute and plot wss for k = 2 to k = 15.
k.max = 20
data = scaled_data
wss = sapply(2:k.max, 
             function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(2:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#--===09 June===---
  
library(class)
library(data.table)
A = data.frame(iris)
library(mltools)
B = one_hot(as.data.table(A))
#---------KMEANS-----------
model_kmeans = kmeans(B,4)
model_kmeans$cluster
C = data.frame(B,CV_K= model_kmeans$cluster)
#---------HIERARCHICAL-----------
model_hc = hclust(dist(B),method = "complete")
#cutree(model_hc,h=3)
CV_hc = cutree(model_hc,k=4)
C = data.frame(C,CV_hc)
plot(model_hc)  

#=====Associatioin Mining

#---------------ASSOCIATION RULE MINING---------------------------------
#R. Agrawal and R. Srikant

install.packages("arules")

#library(amodel1)
library(amodel1Viz)
library (datasets)
library(arules)
A = data.frame(iris)
psych::pairs.panels(A[,1:4])
# Load the data set
data(Groceries) 
head(Groceries)
summary(Groceries)
inspect(Groceries[1:10])
itemFrequencyPlot(Groceries,support = 0.15)
itemFrequencyPlot(Groceries,topN = 20)
# Implement apriori
model = apriori(Groceries)
inspect(model)

# Implement apriori with support=0.01, confidence=0.5
model1 <- apriori(Groceries, parameter=list(support=0.01, confidence=0.5))
?apriori
inspect(model1[])
summary(model1)
# Sort model1
model2=arules::sort(model1,by="confidence")
inspect(model2[])
# plot model1
plot(model2) # default shading = "lift" 
yog = subset(model2, items %in% "yogurt")
inspect(yog)
#---/Association Rule Mining----------------
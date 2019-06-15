install.packages("rpart")
install.packages("tree")

library(tree)
library(naivebayes)
library(rpart)
A= data.frame(iris)
psych::pairs.panels(A[,1:4])
sf= sample(2,nrow(A), replace = TRUE, prob = c(0.7,0.3))
trd = A[sf==1,]
tsd = A[sf==2,]
Model_tree= rpart (Species ~ .,data=trd)
model_nb = naive_bayes (Species ~ .,data = trd)

plot (Model_tree)
text (Model_tree)
printcp(model_tree)

pred_tree = predict(Model_tree, tsd)
pred_t= ifelse (pred_tree[,1] >0.5, "setosa", ifelse (pred_tree [,2] > 0.5, "versicolor", "virginica"))

pred_nb= predict(model_nb, tsd)

cf_tree= table (pred_t, tsd$Species)
cf_nb= table (pred_nb, tsd$Species)

#_______26 May

install.packages("randomForest")
library(randomForest)
A = data.frame(ISLR::Hitters)
str(A)
B = na.omit(A)
sf = sample(2,nrow(B),replace = TRUE,prob = c(0.7,0.3))
trd= B[sf == 1,]
tsd= B[sf == 2,]

#---------FOREST---------

rf = randomForest(League ~ .,data = trd,na.action = na.pass)
pred = predict(rf,tsd)
cbind(pred,tsd$League)
table(pred,Actual = tsd$League)
misclassification_rf= (1-(sum(diag(cm))/nrow(tsd))) * 100

#----TREE-----
tr = tree::tree(League ~ .,data = trd,na.action = na.pass)
predtr = predict(tr,tsd)
E = ifelse(predtr[,1]>0.5,1,0)
cbind (E,tsd$League)
table (E,tsd$League)
#----second set where values putting as A and N
w=ifelse(predtr[,1]>0.5,"A","N")
cbind (w,tsd$League)
table (w,tsd$League)
#ifelse()

#-EXP2--------FOREST---------

rf = randomForest(League ~ .,data = trd,na.action = na.pass,ntree = 5)
pred = predict(rf,tsd)
cbind(pred,tsd$League)
cm = table(pred,Actual = tsd$League)
misclassification_rf = (1-(sum(diag(cm))/nrow(tsd))) * 100
misclassification_rf

#-EXP2---TREE-----
tr = tree::tree(League ~ .,data = trd,na.action = na.pass)
predtr = predict(tr,tsd$League)
W = ifelse(predtr[,1]>0.5,"A","N")
cbind(W,tsd$League)
cm1 = table(W,tsd$League)
misclassification_tr = (1-(sum(diag(cm1))/nrow(tsd))) * 100
#ifelse()

#------

A = data.frame(iris)
A[4,1]=NA
A[7,1]=NA
A[5,2]=NA
A[6,2]=NA
A[7,3]=NA
A[8,3]=NA
A[1:10,]
psych::pairs.panels(A)

# Treament for P.Length
trd = A[is.na(A$Petal.Length)==FALSE,]
tsd = A[is.na(A$Petal.Length)==TRUE,]
m1 = lm(Petal.Length~Petal.Width,trd)
mv = predict(m1,tsd)
A[is.na(A$Petal.Length)==TRUE,3] = mv #assigning predicted values to the places where NA were present
install.packages("tree")
library(tree)
library(ISLR)

A = data.frame(Hitters)
A = na.omit(A)
A$Salary1 = log(A$Salary)
fivenum(A$Salary1)
HighSal = ifelse(A$Salary1 < 6.05,"No","Yes")

A = data.frame(A,HighSal)

# Classification Tree
table(A$HighSal)

model1=tree(HighSal~Years+Hits,A)
model2 = tree(Species ~ .,data = iris)
model2 = tree(race ~ .,data = Wage)

Cars93$Make


plot(model1)

text(model1)

summary(model1)
model1
partition.tree(model1)

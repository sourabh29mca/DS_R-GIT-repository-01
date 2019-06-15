A = read.csv(file.choose())
str(A)
cor(A[,c(1,2,3,5)])
set.seed(123)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))
trd = A[sf==1,]
tsd = A[sf==2,]
nrow(A)
nrow(trd)
nrow(tsd)
model1 = lm(PROFIT ~ RND,data = trd)
model2 = lm(PROFIT ~ RND+MKT,data = trd)
model3 = lm(PROFIT ~ RND+MKT+ADMIN,data = trd)
model4 = lm(PROFIT ~ RND+ADMIN,data = trd)

summary (model1)
summary (model2)
summary (model3)
summary (model4)

model3 = lm(PROFIT ~ RND+MKT+STATE,data = trd)
summary (model3)

pred1 = predict(model1,tsd)
pred2 = predict(model2,tsd)
pred3 = predict(model3,tsd)
pred4 = predict(model4,tsd)

cbind(tsd,pred1,pred2,pred3,pred4)
#========================

library(ISLR)
A = data.frame(ISLR::Credit)
A = MASS::Cars93 
A = data.frame(iris)
A = data.frame(Hitters)
A = data.frame(ISLR::Wage)

191792.1 - (49120 + 0.7765*162597.7 + 0.0279*443898.5)

model3
RND = c(160000,170000)
MKT = c(450000,500000)
STATE = c("Florida","New York")
W = data.frame(RND,MKT,STATE)
pred_W = predict(model3,W)
pred_W
cbind(W,pred_W)

____________12 May________

install.packages("ggplot2")
library(ggplot2)

a = c(21,19,39,50,60,55,78,69,90,110,111,120,130,141)

b = c(32,46,38,47,40,48,67,50,40,52,65,74,85,79)

f = data.frame(a,b)

ggplot(f, aes(y=b, x=a)) + geom_point(alpha = .9) +
  stat_smooth(method = "lm", formula = y ~ I(x^2))

#Test#
ggplot(f, aes(y=b, x=a)) + geom_point(alpha = .9) +
stat_smooth(method = "lm", formula = y ~ I(x^4))

ggplot(f, aes(y=b, x=a)) + 
  geom_point(alpha = .9) + 
  stat_smooth(method = "lm", formula = y ~ I(x^2))
mapping
Default list of aesthetic mappings to use for plot. If not specified, must be supplied in each layer added to the plot
____

compare ridge ~ lasso ~ MLR
#=======================RIDGE REGRESSION========================

install.packages("caret")
install.packages("glmnet")

library(caret)
library(glmnet)
library(ISLR)

set.seed(123) 

tc = trainControl(method = "cv", number = 10,verboseIter = T)

A = read.csv(file.choose(),header = TRUE)

# A = data.frame(Hitters)

A1 = na.omit(A)

model1 = train(PROFIT ~ RND + MKT ,method = "glmnet",data = A, trControl = tc,tuneGrid = expand.grid(alpha = 0,lambda = seq(3000,5000,length=200)))

model1 = train(Salary ~ AtBat + Hits + League ,method = "glmnet",data = A1, trControl = tc,tuneGrid = expand.grid(alpha = 0,lambda = seq(40,50,length=20)))

model1$results
varImp(model1)

model1$bestTune

#model1(LOOK AT FINAL MODEL)

#plot(model1)   (PLOT IT)

#attributes(model1)  (SEE ITS ATTRIBUTES)

#model1$finalModel  (LOOK AT THE SELECTED MODEL COEFFICIENTS)

#plot(model1$finalModel,xvar="lambda")  (PLOT FINAL MODEL AGAINST PENALTY)

#plot(model1$finalModel,xvar="dev")  (PLOT FINAL MODEL AGAINST PENALTY)

#plot(varImp(model1,scale = FALSE))  (SEE VARIABLE SIGNIFICANCE...FEATURE SELECTION)

=======================LASSO REGRESSION========================
  
  library(caret)
library(glmnet)

set.seed(123) 

tc = trainControl(method = "cv", number = 10,verboseIter = T)

A = read.csv(file.choose())

model2 = train(PROFIT ~ . ,method = "glmnet",data = A, trControl = tc,tuneGrid = expand.grid(alpha = 1,lambda = seq(0.1,1,length=7)))

model2(LOOK AT FINAL MODEL)

plot(model1)   (PLOT IT)

attributes(model1)  (SEE ITS ATTRIBUTES)

model2$finalModel  (LOOK AT THE SELECTED MODEL COEFFICIENTS)

plot(model2$finalModel,xvar="lambda")  (PLOT FINAL MODEL AGAINST PENALTY)

#plot(model2$finalModel,xvar="dev")  (PLOT FINAL MODEL AGAINST PENALTY)

#plot(varImp(model2,scale = FALSE))  (SEE VARIABLE SIGNIFICANCE...FEATURE SELECTION)

#=================ELASTIC NET REGRESSION=================


library(caret)
library(glmnet)

set.seed(123) 

tc = trainControl(method = "cv", number = 10,verboseIter = T)

A = read.csv(file.choose())
A = data.frame(iris)

model3 = train(Sepal.Length ~ . ,method = "glmnet",data = A, trControl = tc,tuneGrid = expand.grid(alpha = seq(0,1,length=10),lambda = seq(0.1,1,length=7)))

model3 #(LOOK AT FINAL MODEL)
#pred = predict(model3,trd)
cbind(pred,tsd$Sepal.Length)

plot(varImp(model3))  # (PLOT IT)

attributes(model3) # (SEE ITS ATTRIBUTES)

model3$finalModel  (LOOK AT THE SELECTED MODEL COEFFICIENTS)


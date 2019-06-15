A = read.csv(file.choose())
str(A)
fivenum (A$PROFIT)
A$PROFIT_TYPE=ifelse(A$PROFIT<=107978,0,1)
A$PROFIT_TYPE = as.factor(A$PROFIT_TYPE)
sf= sample (2, nrow(A), replace = TRUE, prob = c(0.8,0.2))
trd = A[sf==1,]
tsd = A[sf==2,]
model1 = glm(PROFIT_TYPE ~ RND+MKT+ADMIN,data = trd,family = "binomial")
pred = predict(model1,tsd)
w = ifelse(pred <= 0.5,0,1)
cm = table(predicted = w,actual = tsd$PROFIT_TYPE)
cmcbind(tsd$PR0FIT_TYPE,w)

--
pred = predict(model1,tsd,type= "response")

------19 May-----

install.packages("naivebayes")  
library (naivebayes)
A= data.frame(iris)
head (A)
View(A)
A=na.omit(A)
sf= sample(2,nrow(A),replace =TRUE , prob = c(0.7,0.3))
trd= A[sf==1,]
tsd= A[sf==2,]
model1= naive_bayes(Species~.,data= trd)
pred=predict(model1,trd)
cbind(pred,tsd$Species)
w= table (pred, tsd$Species)
#misclassification
misclassified = (1-(sum(diag(w))/nrow(tsd)))*100
model1$data

                 
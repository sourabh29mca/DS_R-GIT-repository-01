A = read.csv(file.choose())
sf= sample (2,nrow(A),replace=TRUE, Prob=c(0.8,0.2))
trd=A[sf==1,]
tsd=A[sf=2,]
plot(A$)
model1 = lm (profilt ~ RND, data = trd)
pred= predict (model1,tsd)
cbind (pred,tsd$PROFIL)

library(psych)
install.packages("psych")
pairs.panels(A)

#-----28April-----
y (cap)=b0+b1x + E

library(ISLR)
library(psych)
A = data.frame(Credit)
str(A)
head(A)
pairs.panels(A)
numcols = unlist(lapply(A,is.numeric))
B = A[,numcols]
pairs.panels(B)
cor(B)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]

model1_Inc = lm(Income ~ Limit,data=trd)
#model2_Inc = lm(Income ~ Rating,data=trd)
#model3_Inc = lm(Income ~ Balance,data=trd)

#model1_Limit = lm(Limit ~ Balance,data=trd)
model2_Limit = lm(Limit ~ Rating,data=trd)
#model3_Limit = lm(Limit ~ Income,data=trd)

model1_Rating = lm(Rating ~ Balance,data=trd)

#model1_Bal = lm(Balance ~ Income,data=trd)
#model2_Bal = lm(Balance ~ Limit,data=trd)
model3_Bal = lm(Balance ~ Rating,data=trd)

pred_Inc = predict(model1_Inc,tsd)
cbind(tsd$Limit,pred_Inc,tsd$Income)
pred_Rating = predict(model1_Rating,tsd)

pred_Bal = predict(model3_Bal,tsd)

pred_Lim = predict(model2_Limit,tsd)

pairs.panels(A)

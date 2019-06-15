Age = c(20,22,24,26,28,30,32,34,36,38)
Height = c(150,152,154,156,157,158,159,162,163,164)
A= data.frame(Age,Height)
str(A)
plot(Age,Height,cex = 0.8,pch=7,col=4, xlim = c(0,80), ylim = c(0,180))
points(10,50)
points (Age*1.2, Height* 1.4, pch=2, col=2, cex=0.8)
--plot(A$Age.Lenght,A$Height.width,xlab = "SL", ylab = "SW", main = "SL vs SW")
cex= 2, pch=2, 0)

points()
hist(A$Age,col = c(2,1,3,4,5))
hist(A$Height,col = c(2,6,1,3,2,5,4))

--Vaibhav_bajaj-----------------------
Age = c(20,22,24,26,28,30,32,34,36)
Height = c(150,152,156,157,158,159,162,163,164)
A=data.frame(Age,Height)
plot(A$Age,A$Height,xlab="AGE",ylab = "HEIGHT", col=c(1,2,3,4,5,6,7,8,9),xlim = c(20,50),ylim = c(150,180))
points(45,170)

P = c(10,20,30,40,50)
Q = c(2,4,5,6,7)
plot(P<Q)
plot(P,Q)
plot(P,Q,cex = 0.8,pch = 3)
plot(P,Q,cex = 0.8,pch = 3,col = 3)
plot(P,Q,cex = 0.8,pch = 3,col = 4)
points(P*1.2,Q*1.4)
points(P*1.2,Q*1.4,pch = 4,col = 5,cex= 0.8 )
points(P*1.2,Q*1.4,pch = 2,col = 5,cex= 0.8 )
plot(P,Q,cex = 0.8,pch = 3,col = 4,xlim = c(0,80))
plot(P,Q,cex = 0.8,pch = 3,col = 4,xlim = c(0,80),ylim = (0,8))
plot(P,Q,cex = 0.8,pch = 3,col = 4,xlim = c(0,80),ylim = c(0,8))
p1 = P * 1.2
p1
q1 = q * 1.4
p1
q1
q1 = Q * 1.4
q1
p1
P
Q
points(p1,q1,col=2,pch=8)
plot(P,Q,cex = 0.8,pch = 3,col = 4,xlim = c(0,80),ylim = c(0,12))
points(p1,q1,col=2,pch=8)
points(p1+1,q1+1,col=4,pch=9)
Age = c(20,22,24,26,28,30,32,34,36)
Height = c(150,152,156,157,158,159,162,163,164)
A=data.frame(Age,Height)
plot(A,xlab="AGE", col=c(1,2,3,4,5,6,7,8,9))
plot(A$Age,A$Height,xlab="AGE",ylab = "HEIGHT" col=c(1,2,3,4,5,6,7,8,9))
plot(A$Age,A$Height,xlab="AGE",ylab = "HEIGHT", col=c(1,2,3,4,5,6,7,8,9),xlim = c(20,50))
plot(A$Age,A$Height,xlab="AGE",ylab = "HEIGHT", col=c(1,2,3,4,5,6,7,8,9),xlim = c(20,50),ylim = c(150,180))
points(52,170)
points(42,170)
library(ISLR)
A = data.frame(Credit)
A
head(A)
fivenum(A$Limit)6
colfinder = function(x){if(x<3087) 1 else if(x<4622) 2 else if(col<5876) 3 else 4}
newcol = colfinder(A$Limit)
newcol = if(A$Limit<3087) 1 else if(A$Limit<4622) 2 else if(A$Limit<5876) 3 else 4
newcol = ifelse(A$Limit<4622,1,2)
A = data.frame(A,newcol)
plot(A$Income,A$Limit,col=newcol)
newcol = ifelse(A$Limit<4622,2,4)
A = data.frame(A,newcol)
plot(A$Income,A$Limit,col=newcol)
newcol = ifelse(A$Limit<4622,2,ifelse(A$Limit<5087,3,5))
A = data.frame(A,newcol)
plot(A$Income,A$Limit,col=newcol)
fivenum(A$Limit)
Q = fivenum(A$Limit)
str(Q)
Q[1]
Q[2]
Q[3]
Q[4]
Q[5]
Q
plot(A$Income,A$Limit,xlim = c(0,300),ylim = c(0,20000))
plot(A$Income,A$Limit,xlim = c(0,300),ylim = c(0,20000),col = A$newcol
)
plot(A$Income,A$Limit,xlim = c(0,300),ylim = c(0,20000),col = A$newcol)
Q
newcol = ifelse(A$Limit<3087,2,ifelse(A$Limit<4622.5),3,ifelse(A$Limit<5876.5,4,5))

---------------

library(ISLR)  
A= data.frame(Credit)
head(A)
Q=fivenum(A$Limit)
str(Q)
newcol= ifelse(A$Limit<4622,24)
newcol = ifelse(A$Limit<3087,2,ifelse(A$Limit<4622.5,3,ifelse(A$Limit<5876.5,4,5)))

help(plot)

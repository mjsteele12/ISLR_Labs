#Lab for An Introduction to Machine Learning
#Chapter 6 

#Lab1: Subset Selection Methods

#load required package and view hitters dataset
library(ISLR)
fix(Hitters)

#view variables
names(Hitters)

#dataset dimensions
dim(Hitters)

#get a count of missing values for the salary variable
sum(is.na(Hitters$Salary))

#Drop rows that have na values
Hitters = na.omit(Hitters)

#New dimensions after dropping NA rows
dim(Hitters)

#Validate that na's dropped
sum(is.na(Hitters))

#we can use regsubsets() which is in the leaps library to find the best model (via residual sum of squares)
library(leaps)

#lets learn about regsubsets before runnin git

?regsubsets

#according to the docs, this function tries exhaustive, forward, backward, stepwise, or sequential replacement
#and returns a matrix showing what variables are included in each model. By default only returns 8 variables
#but nvmax option can return as many as desired

best_sub = regsubsets(Salary ~.,Hitters)
summary(best_sub)


#change nvmax to view more vars
best_sub2 = regsubsets(Salary ~., Hitters, nvmax = 19)
sub_summary = summary(best_sub2)

#summary also returns r squared, residual sum of squares, adjusted r squared, Cp and BIC
names(sub_summary)

#view r squared change for each variable
sub_summary$rsq

#plotting the fit statistics for each model at once 
par(mfrow=c(2,2))
plot(sub_summary$rss,xlab = "# of Variables", ylab = "RSS", type = "l")

#plot r squared
plot(sub_summary$adjr2, xlab = "# of Variables", ylab = "adjr2", type = "l")

#points() adds points to plot that has already been created, which.max() identifies the location of the max point in a vector
mp = which.max(sub_summary$adjr2)
points(mp,sub_summary$adjr2[mp], col = "red2",cex=2,pch=20)

#find smallest which.min for Cp and BIC
plot(sub_summary$cp,xlab = "# of variables", ylab = "Cp", type = "l")
mincp = which.min(sub_summary$cp)
points(mincp,sub_summary$cp[mincp], col = "blue",cex=2,pch=20)

minbic = which.min(sub_summary$bic)
plot(sub_summary$bic, xlab = "# of variables", ylab = "BIC", type = "l")
points(minbic,sub_summary$bic[minbic], col = "blue", cex=2, pch = 20)


#regsubsets has built in plots command  
plot(best_sub2,scale="r2")
plot(best_sub2,scale="bic")
plot(best_sub2,scale="adjr2")
plot(best_sub2,scale="Cp")


#use coef() to see coefficients for a particular model
coef(best_sub2,6)

#6.5.2 Forward and backward stepwise selection
#specify method = forward/backward using regsubsets
subset.forward = regsubsets(Salary~.,data=Hitters, nvmax = 19, method = "forward")
summary(subset.forward)

subset.backward = regsubsets(Salary~.,data = Hitters, nvmax = 19, method = "backward")
summary(subset.backward)

#6.5.3 Choosing among models using validation set/cross validation

#set random seed
set.seed(1)
#create random vector of rue/false, set it to size of hitters dataset
train = sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)
test=(!train)

bestfit = regsubsets(Salary~.,Hitters[train,],nvmax=19)

#model matrix is used to build an x matrix from data, loop through it and extract coefficients
test = model.matrix(Salary~.,data = Hitters[train,])

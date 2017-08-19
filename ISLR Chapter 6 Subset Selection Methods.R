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
testmodel = model.matrix(Salary~.,data = Hitters[train,])

val.errors = rep(NA,19)
for(i in 1:19) {
  coefs=coef(bestfit,id=i)
  pred=testmodel[,names(coefs)]%*%coefs
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}

#view errors, find best model which is lowest, and view it's coefficients
val.errors
which.min(val.errors)
coef(bestfit,10)

#Since we can't use a predict() function with regsubsets we can define our own
#This assumes our object will have a variable named call with at least 2 elements (yes 2, not 3 because R is weird)

predict.regsubsets=function(object,newdata,id,...) {
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata) 
  coefs=coef(object,id=id)
  xvars=names(coefs)
  mat[,xvars]%*%coefs
}

#perform best subset selection on entire dataset
regfit.best=regsubsets(Salary~.,data = Hitters,nvmax=19)
coef(regfit.best,10)

#now try to choose among models using 10-fold cross validation
set.seed(1)
#set k
k=10

#create 10 folds
folds = sample(1:k,nrow(Hitters),replace = TRUE)

#matrix of error terms
cv.errors = matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))

for(j in 1:k) {
  best.fit=regsubsets(Salary~., data = Hitters[folds!=j,],nvmax = 19)
  for(i in 1:19) {
    pred=predict.regsubsets(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
    
  }
  
}

#This returns 10x19 matrix, we can use apply() to average over the columns and get cv error 
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors

#from the plot we can see that the 11 variable model is best based on cv error
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')

#now perform best subset selection on full data to obtain 11 variable model
reg.best=regsubsets(Salary~.,data = Hitters, nvmax=19)
coef(reg.best,11)


#Lab2: Ridge Regression and Lasso

#will use glmnet package, start by removing NA as done before
#Hitters = na.omit(Hitters)
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

#6.6.1 Ridge Regression

#glmnet has an alpha parameter, if 0 then it will be ridge
library(glmnet)

#create a series of lambda values to try, this is the ridge hyperparameter that determines how much the coefficients will shrink
grid=10^seq(10,-2,length=100)

#fit
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

#we can see that it created 100 models
dim(coef(ridge.mod))


#split into train and test sets
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]


#train new model
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)


#best lambda value is 112
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam


#6.6.2 Lasso
#fit model
lasso.mod=glmnet(x[train,],y[train], alpha = 1, lambda=grid)
plot(lasso.mod)

#fit cv model
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)



#6.7 PCR and PLS regression

#6.7.1 Principla components regression
library(pls)
set.seed(2)

#default validation is 10 fold
pcr.fit=pcr(Salary~., data = Hitters, scale=TRUE, validation="CV")

summary(pcr.fit)

#Plot cross validation scores with mean square error
validationplot(pcr.fit,val.type="MSEP")

set.seed(2)
pcr.fit=pcr(Salary~., data= Hitters, subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type = "MSEP")

#Find lowest CV score with 7 principal components
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y[test])^2)

#fit on entire dataset
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)


#6.7.2 Partial Least Squares
set.seed(1)
pls.fit=plsr(Salary~., data = Hitters, subset=train,scale=TRUE, validation= "CV")
summary(pls.fit)

#predict
pls.pred = predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y[test])^2)


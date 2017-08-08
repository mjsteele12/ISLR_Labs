 #Lab 5.3: Cross VAlidation and the Bootstrap

#Set options required
library(ISLR)
set.seed(1)

#use sample function to split observations into two groups
train = sample(392, 196)

#fit linear model using training set
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)

#Attach the auto dataset, get the mean squared error by using predict function on
attach(Auto)
#this is the way in the book, doesn't seem right? las length 392 and it should only be 196 since it is on the training set
mean((mpg-predict(lm.fit,Auto))[-train]^2)

model_fit <- summary(lm.fit)

#different MSE
mse <- mean(model_fit$residuals^2)

#fit second degree polynomial regression, this has lower MSE
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)

model2_fit <- summary(lm.fit2)

mse2 <- mean(model2_fit$residuals^2)


#choose different training set via different random seed, different results
set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)

model_fit <-summary(lm.fit)

mse <- mean(model_fit$residuals^2)

lm.fit2 <-lm(mpg~poly(horsepower,2),data=Auto,subset=train)
model2_fit <-summary(lm.fit2)

mse2 <- mean(model2_fit$residuals^2)

#5.3.2 Leave-one-out Cross Validation

#LOOCV can be computed for any glm using cv.glm

glm.fit =glm(mpg~horsepower,data=Auto)

coef(glm.fit)

lm.fit = lm(mpg~horsepower,data = Auto)
coef(lm.fit)

#the above are identical regression models. let's use cv.glm to do LOOCV
library(boot)
glm.fit=glm(mpg~horsepower,data = Auto)

#fit cv.glm
cv.err = cv.glm(Auto,glm.fit)
#the two numbers in delta contain cross validation results correspond to what was covered in equation 5.1
cv.err$delta

#lets use a for loop to try different polynomials and evaluate using cross validation

#create blank list that will house the cross validated error scores
cv.error=rep(0,5)

#for loop to try 5 different polynomials
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i] = cv.glm(Auto,glm.fit)$delta[1]
}

#View errors, large drop between linear and quadratif cits. No improvement between 2-5 order polynomials
cv.error

#5.3.3 k-Fold Cross-Validation
set.seed(17)

cv.error.10=rep(10,0)

#for K fold, specify k in cv.glm
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data = Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

#look at 10 cross validation errors. There is still no evidence that higher order polynomials lead to increased performance
cv.error.10

#5.3.4 Bootstrap

#start by creating a function that outputs the estimate for alpha based on the selected observations
alpha.fn = function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

#apply function to portfolio dataset, all 100 observations
alpha.fn(Portfolio,1:100)

#use sample to randomly select observations with replacement, equivalent to creating a new bootstrap dataset
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))

#do this multiple times with boot function
boot(Portfolio,alpha.fn,R=1000)

#Estimating accuracy of linear model
boot.fn = function(data,index)
  return(coef(lm(mpg~horsepower,data = data,subset = index)))

boot.fn(Auto,1:392)

#boot.fn() can also be used to create estimates for intercept and slope terms
boot.fn(Auto, sample(392,392,replace=T))

#use boot() to compute standard errors of 1,000 bootstrap estimates for slope/intercept
boot(Auto,boot.fn,1000)

#The above shows the bootstrap estimate for intercept B0 = 0.86, and for intercept term B1 is 0.0074


#Can create bootstrapped CI for standard errors by taking them from summary
summary(lm(mpg~horsepower,data = Auto))$coef


boot.fn = function(data,index)
  coefficients(lm(mpg~horsepower + I(horsepower^2), data = data, subset = index))

boot(Auto,boot.fn,1000)







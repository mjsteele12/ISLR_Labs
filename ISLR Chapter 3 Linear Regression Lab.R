################################
### Simple Linear Regression ###
################################

install.packages("ISLR")
#loads the packages needed
library(MASS)
library(ISLR)

#fix displays the dataset in a very nice way, names tells you what the variables are
fix(Boston)
names(Boston)

#tells you more info about Boston dataset
?Boston

#the basic structure for simple linear regression is: lm(y~x,data)
lm.fit=lm(medv~lstat, Boston)

#if we don't attach the dataset, you constantly have to use $ in order to specify 
#where you are getting the variable, e.g. lm.fit=lm(Boston$medv~Boston$lstat)
attach(Boston)

#provides a summary of the model
summary(lm.fit)

#finds out what other information is in the model
names(lm.fit)

#displays coefficients from the model
coef(lm.fit)

#displays confidence intervals for coefficients
confint(lm.fit)

# produces confidence intervals for the prediction of medv given a value of lstat.
# Confidence intervals tell you about how well you have determined the mean. Assume that the data really are
# randomly sampled from a Gaussian distribution. If you do this many times, and calculate a confidence interval
# of the mean from each sample, you'd expect about 95 % of those intervals to include  the true value of the
# population mean. The key point is that the confidence interval tells you about the likely location of the
# true population parameter.
predict(lm.fit,data.frame(lstat=(c(5,10,15))),
        interval = "confidence")

#produces prediction intervals for the prediction of medv given a value of lstat.
#Prediction intervals tell you where you can expect to see the next data point sampled. Assume that the data
#really are randomly sampled from a Gaussian distribution. Collect a sample of data and calculate a prediction
#interval. Then sample one more value from the population. If you do this many times, you'd expect that next 
#value to lie within that prediction interval in 95% of the samples.The key point is that the prediction 
#interval tells you about the distribution of values, not the uncertainty in determining the population mean. 
predict(lm.fit,data.frame(lstat=(c(5,10,15))),
        interval = "confidence")

#plots medv and lstat along with the least squares regression line
plot(lstat,medv)
abline(lm.fit)

##abline can be used to draw any line, not just the least squares regression line. lwd=3 causes the width of 
#the line to be increased b a factor of 3, this workes for plot() and lines() too. We can also use
#pch option to create different plotting symbols.
abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3, col= "red")
plot(lstat, medv, col= "red")
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)

#par() tells R to split the display screen into separate panels so that multiple plots can be viewed. 
par(mfrow=c(2,2))
plot(lm.fit)

#alternatively, we can compute the residuals from a linear regression fit using the residuals() function which
#returns studentized residuals, and we can use this to plot against the fitted values.
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

#leverage statistics can be computed for any number of predictors using the hatvalues() function. leverage
#statistics tell us if a certain observation has an (overly)strong influence on the regression line.
plot(hatvalues(lm.fit))

#which.max() identifies the index of the largest element of a vector, in this case, telling us which 
#observation has the largest leverage statistic
which.max(hatvalues(lm.fit))

##################################
### Multiple Linear Regression ###
##################################

#syntax for multiple linear regresstion: lm(y1~x1+x2+x3)
#computing and summarizing below
mlm.fit=lm(medv~lstat+age)
summary(mlm.fit)

#to shorthand adding all the variables, you can use "."
mlm.fit=lm(medv~., data = Boston)
summary(mlm.fit)

#we can access individual components of summary object by name (type "?summary.lm") to see what is available. 
summary(mlm.fit)$r.sq
summary(mlm.fit)$sigma

#variance inflation factor (VIF) can be downloaded from car package
install.packages("car")
library(car)
vif(mlm.fit)

#if you want to perform regression with all variables except one (or more)
mlm.fit1 =lm(medv~.-age, data=Boston)
summary(mlm.fit1)

#alternatively, you can update the function using update()
mlm.fit1 = update(mlm.fit, ~.-age)
summary(mlm.fit1)

#########################
### Interaction Terms ###
########################

#It is easy to include interaction terms using lm function, the syntax lstat:black tells R to include interaction between 
#lstat and black. Lstat*age simultaneously includeslstat,age, and interaction term lstat*age, it is shorthand for lstat+age+lstat:age
summary(lm(medv~lstat*age, data = Boston))

################################################
### Non-linear Transformations of Predictors ###
################################################

#lm() can accomodate non-linear transformations of predictors. For example, given x we can maxe X^2 using I(X^2). I is needed because ^ has a special meaning
#in a formula; wrapping as we do allows the standard usage in r which is to raise to the power.
mlm.fit2=lm(medv~lstat+I(lstat^2))
summary(mlm.fit2)

#the near zero p-value in the model created in the previous 2 lines suggests that it leads to an improper model. We can use the anova() function to further
#quantify the extent to which the quadratic fit is superior to the linear fit.
lm.fit=lm(medv~lstat)
anova(lm.fit,mlm.fit2)

#in the output from the anova we see that Model 1 represents the linear model with one predictor lstat, vs model 2 with lstat and lstat^2. The anova() 
#function performs a hypothesis test comparing the two models. Null hypothesis is that models are equal, alternative is that the full model is
#superior. Here, F statistic is massive and p value is low, so we can reject null. Not surprising because we saw evidence for non-linearity in output.
#look at graphs:
par(mfrow=c(2,2))
plot(mlm.fit2)

#for cubic fit we can do I(lstat^3), but it will get cumbersome for higher order polynomials. Better approach is poly() function.E.g. 5th order:
mlm.fit5=lm(medv~poly(lstat,5))
summary(mlm.fit5)

#can do other transformations, lke log.
summary(lm(medv~log(rm), data=Boston))

##############################
### Qualitative Predictors ###
##############################

#now lets use carseats data in ISLR, attempting to predict sales in 400 locations based on predictors
remove(Boston)
attach(Carseats)
?Carseats
fix(Carseats)
summary(Carseats)

#R generates dummy variables automatically with multiple levels good, bad, medium for shelvloc. Here is a model with interaction terms:
lm.fit=lm(Sales~.+Income:Advertising+Price:Age, data = Carseats)
summary(lm.fit)

#The contrasts() function returns the coding that R uses for the dummy variables. 
contrasts(ShelveLoc)

?contrasts

#shelvelocgood has a higher coefficient and thus stronger predictor of sales. 




#########################
### Writing Functions ###
#########################

#can create user defined functions the following ways:
say_hi= function() print("hi")
say_hi

multi_statement_function = function() {
  print ("hi again")
  print ("this is a multi-line function")
}
multi_statement_function

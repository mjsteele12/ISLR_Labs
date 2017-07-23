##########################
### ISLR Chapter 4 Lab ###
##########################

#################################
###4.6.1 The Stock Market Data###
#################################
library(ISLR)
names(Smarket)

#view dataset Smarket in ISLR library
fix(Smarket)

#show varaible names, gets the matrix dimensions with dim, and shows summary statistics
names(Smarket)
dim(Smarket)
summary(Smarket)

#this plots every pair of variables from the given matrix
pairs(Smarket)

#shows the correlational matrix of the Smarket data, only using variables 1-8 which are numeric, and not 9 which is qualitative
cor(Smarket [0:8])

#attach dataset, plot the volume variable
attach(Smarket)
plot(Volume)

#################################
###4.6.1 Logistic Regression####
#################################

#fitting a logistic regression model in order to predict Direction using lag1-lag5 and volume. glm() function fitz generalized linear models, including logistic regression.
smarket.glm=glm(Direction~ Lag1 + Lag2 + Lag3 +Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(smarket.glm)

#use coef() to access just the coefficients for the fitted model. Can also use summary() to access particular aspects of the fitted model, such as p-values for the coefs.
coef(smarket.glm)
summary(smarket.glm)$coef

#the predict() function can be used to predict the probability that the direction will go up or down given values of the predictors. The "type = response" option tells
#R to output the probabilities of the form P(Y = 1 |X), as opposed to other information such as the logit. If no data set is supplied to the predict() function,
#R computes probability from the training data used to fit the model. Use indexing (I know this from python!) to ask for the first 10 predictions.
smarket.probs = predict(smarket.glm,type="response")
smarket.probs[1:10]

#to check the values of the dummy variable Direction (the outcome) we can use contrasts()
contrasts(Direction)

#in order to make a prediction as to whether the market will go up or down, we must convert predicted probabilities into class labels. Here we create a vector of class
#predictions based on whether the probability is greater or less than .5. rep() replicates values in a vector, in this case assigning 1250 rows to "Down". Then we
#tell R to switch all cases with probability over .5 to up.
smarket.pred=rep("Down", 1250)
smarket.pred[smarket.probs>.5]="Up"

#now we can make a confusion matrix using table()
table(smarket.pred, Direction)

#use mean() to get the fraction of correct predictions.
mean(smarket.pred==Direction)

#first model wasn't that helpful, no test data! To do this we must first create a vector of observations from 2001-2004, then use this vector to create a held-out
#set of observations from 2005.Training_set is a vector of 1250 elements, corresponding to observations in the data set. The elements of the vector that correspond
#to observations that occured before 2005 are set to true, the ones in 2005 are set to false. The object training_set is a boolean vector, since its elements are true
#and false. Boolean vectors can be used to obtain a subset of the rows or columns of a matrix. For instance, the command Smarket[training_set,] would pick out a submatrix
#of the stock market dataset corresponding only to the dates before 2005, since those are the ones which are true. The ! symbol reverses this in the next command to
#create the 2005 dataset. 
training_set=(Year<2005)
Smarket.2005=Smarket[!training_set,]
dim(Smarket.2005)
Direction.2005=Direction[!training_set]

#Now lets fit a logit model using only the training set, and apply it to the 2005 dataset!
train.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial, subset = training_set)
train.probs=predict(train.fit,Smarket.2005, type="response")

#Finally, we compare predictions for 2005 and actual movements of the markets over that period.
train.pred=rep("Down",252)
train.pred[train.probs >.5]="Up"
table(train.pred,Direction.2005)

#use mean() again to get fraction of correct predictions, or incorrect using !
mean(train.pred==Direction.2005)
mean(train.pred!=Direction.2005)

#if we want to predict returns associated with particular values of lag1 and lag2 (e.g. when lag1 and lag2 = 1.1 and 1.2 respectively) we do this using predict()
glm.fit=glm(Direction~Lag1+Lag2,data = Smarket,family=binomial,subset=training_set)
predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")

########################################
###4.6.3 Linear Discriminant Analysis###
########################################


#use lda() which is same syntaxically as glm() and lm()
library(MASS)
lda.fit = lda(Direction~Lag1 + Lag2, data = Smarket, subset=training_set)
lda.fit

#The output indicates that 


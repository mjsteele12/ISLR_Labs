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
#R computes probability from the training data used to fit the model. Use indexing to ask for the first 10 predictions.
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
logit.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial, subset = training_set)
logit.probs=predict(train.fit,Smarket.2005, type="response")

#Finally, we compare predictions for 2005 and actual movements of the markets over that period.
logit.pred=rep("Down",252)
logit.pred[logit.probs >.5]="Up"
table(logit.pred,Direction.2005)

#use mean() again to get fraction of correct predictions, or incorrect using !
mean(logit.pred==Direction.2005)
mean(logit.pred!=Direction.2005)

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

#The output indicates that 49% of the training observations correspond to days during which the market went down.

#The predict function returns a list with three elements. First, class, contains LDA's predictions about movement of the market. Second
#Posteriori, contains a matrix whose kth column belongs to the posterior proability that the corresponding observation belongs to the kth
#class. Last, x, contains the linear discriminants, described earlier.
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)

#Predictions are basically the same as logistic regression
lda.class=lda.pred$class
table(lda.class,Direction.2005)

#Use 50% threshold to recreate predictions
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<=.5)


###########################################
###4.6.4 Quadratic Discriminant Analysis###
###########################################

#Fit QDA to Smarket data using qda(), identical syntax to lda()
qda.fit = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = training_set)
qda.fit

#QDA correctly predicts 60% of the time, suggesting the nonlinear model captures the true relationship much better
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)

################################
###4.6.5 K-Nearest Neighbors ###
################################

library(class)

#KNN() function requires 4 inputs: 1. training matrix. 2. test matrix. 3. training labels. 4. value for K, the number of neighbors
#we can use cbind() to bind lag1 and lag2 variables together
#create train
train.X=cbind(Lag1, Lag2)[training_set,]

#create test
test.X=cbind(Lag1, Lag2)[!training_set,]

#create training labels
train.Direction=Direction[training_set]

#set seed for reproducibility- if there is a tie based on number of neighbors.
set.seed(1)

#knn where k=1
knn.pred = knn(train.X,test.X,train.Direction, k=1)
table(knn.pred,Direction.2005)


#try k = 3
knn.pred =knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)

mean(knn.pred==Direction.2005)




#################################################
###4.6.6 Application to Caravan Insurance Data###
#################################################

#Dataset dimensions
dim(Caravan)
attach(Caravan)
summary(Purchase)

#standardize data- all variables have mean zero and standard deviation of one. This will allow all variables to be compared easier
#because they are on the same scale. Compare var before and after.
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(standardized.X[,1])


#split into test set, with first 1,000 obs.
test = 1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)

#build knn
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")


table(knn.pred,test.Y)

#Using k=3 significatnly improves performance
knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)

#k=5
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)

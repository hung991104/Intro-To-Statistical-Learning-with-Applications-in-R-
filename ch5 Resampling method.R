# ch5 Resampling method


# Validation Set Approach -------------------------------------------------

library(ISLR)
attach(Auto)

set.seed(1)
train = sample(392, 196)

lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train] ^ 2)

lm.fit2=lm(mpg ~ poly(horsepower,2), data=Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train] ^ 2)    # 18.71646


# Leave-One-Out Cross-Validation ------------------------------------------

glm.fit = glm(mpg ~ horsepower, data = Auto, subset = train)    # still linear regression
coef(glm.fit)

library(boot)
glm.fit = glm(mpg ~ horsepower, data = Auto)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta

# iteratively fits polynomial regressions
cv.error = rep(0, 5)
for(i in 1:5){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.error


# k-Fold Cross-Validation -------------------------------------------------
set.seed(17)
cv.error = rep(0, 10)
for(i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error

# little evidence that using cubic or higher-order polynomial terms leads to lower test error than simply using a quadratic fit


# The Bootstrap -----------------------------------------------------------
# Estimating the Accuracy of a Statistic of Interest

alpha.fn = function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y) - cov(X, Y))/(var(X) + var(Y) - 2 * cov(X, Y)))
}

set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = TRUE))

boot(Portfolio, alpha.fn, R = 1000)

# Estimating the Accuracy of a Linear Regression Model

boot.fn = function(data, index){
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}

set.seed(1)
boot(Auto, boot.fn, R = 1000)

# standard formulas assume xi are fixed and all variability comes from the variation in error
# the bootstrap approach doesn't rely on any of these assumptions 

boot.fn = function(data, index){
  return(coef(lm(mpg ~ horsepower + I(horsepower ^ 2), data = data, subset = index)))
}
set.seed(1)
boot(Auto, boot.fn, R = 1000)

summary (lm(mpg~horsepower +I( horsepower ^2) ,data =Auto))$coef


# Exercise - Applied ------------------------------------------------------

# Q5 estimate test error of logistic regression model using validation set approach 
attach(Default)
str(Default)
contrasts(Default$student)
set.seed(1)

# Part a: Multiple logistic regression
glm.fit = glm(default ~ income + balance, data = Default, family = binomial)
summary(glm.fit)

# Part b
test.error = rep(0, 10)
for(i in seq_along(test.error)){
  train = sample(nrow(Default), floor(nrow(Default)/2))
  glm.fit = glm(default ~ income + balance, data = Default, subset = train, family = binomial)
  summary(glm.fit)
  glm.pred = ifelse(predict(glm.fit, Default[-train,]) > 0.5, "Yes", "No")
  
  table(glm.pred, Default[-train,]$default)
  print(mean(glm.pred != Default[-train,]$default)) 
  
  test.error[i] = mean(glm.pred != Default[-train,]$default)
}

test.error

# Part d: Predict using income balance and student
test.error2 = rep(0, 10)
for(i in seq_along(test.error2)){
  train = sample(nrow(Default), floor(nrow(Default)/2))
  glm.fit = glm(default ~ income + balance + student, data = Default, subset = train, family = binomial)
  summary(glm.fit)
  glm.pred = ifelse(predict(glm.fit, Default[-train,]) > 0.5, "Yes", "No")
  
  table(glm.pred, Default[-train,]$default)
  print(mean(glm.pred != Default[-train,]$default)) 
  
  test.error2[i] = mean(glm.pred != Default[-train,]$default)
}

data.frame(test.error, test.error2)
mean(test.error)
mean(test.error2)

# Including a dummy variable for student doesn't lead to a significant reduction in test error

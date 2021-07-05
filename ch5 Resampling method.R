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

# Q6: Multiple logistic regression on Default data set
set.seed(1)
str(Default)

# Part a: standard error of coefficients
glm.fit = glm(default ~ income + balance, data = Default, family = binomial)
glm.summary = summary(glm.fit)
names(glm.summary)
glm.summary$coefficients[2:3, 2]   # 4.985167e-06 2.273731e-04 

# Part b, c: find standard error with boot
glm.fn = function(data, index){
  glm.fit = glm(default ~ income + balance, data = Default, family = binomial, subset = index)
  return(coef(glm.fit))
}

boot(Default, glm.fn, R = 1000)   # 4.866284e-06 2.298949e-04

# Part d: The standard errors obtained by glm and boot are close

# Q7 LOOCV test error estimate
# Part a
glm.fit = glm(default ~ balance, data = Default, family = binomial)

# Part b
train = seq(2,nrow(Default))
glm.fit2 = glm(default ~ balance, data = Default, family = binomial, subset = train)
glm.probs = predict(glm.fit2, Default[1,], type = "response")
glm.pred = ifelse(glm.probs > 0.5, "Yes", "No")
glm.pred

# Part c
nrow(Default)
# error = rep(0, nrow(Default))
error = rep(0, 1000)
glm.probs = rep(0, 1000)
for(i in 1:1000){
  train.data = Default[-i,]
  test.data = Default[i,]
  glm.fit = glm(default ~ balance, data = train.data, family = binomial)
  glm.prob = predict(glm.fit, test.data, type = "response")
  glm.probs[i] = glm.prob
  glm.pred = ifelse(glm.prob > 0.5, "Yes", "No")
  if(glm.pred != Default$default[i]){
    error[i] = 1
  }
}
mean(error)
table(Default$default[1:1000])


library(boot)
glm.fit = glm(mpg ~ horsepower, data = Auto)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta

# iteratively fits polynomial regressions
nrow(Default)
cv.error = rep(0, 1000)
for(i in seq_along(cv.error)){
  glm.fit = glm(default ~ balance, data = Default, family = binomial)
  cv.error[i] = cv.glm(Default, glm.fit)$delta[1]
}
cv.error


# Q8 cross validation
# Part a
set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)

# Part b
plot(x,y)     # y = quadratic equations of x

# Part c LOOCV
library(boot)
set.seed(2)

data = data.frame(y = y, x = x)

# iteratively fits polynomial regressions
cv.error = rep(0, 4)
for(i in seq_along(cv.error)){
  glm.fit = glm(y ~ poly(x, i), data = data)
  cv.error[i] = cv.glm(data, glm.fit)$delta[1]
}
cv.error

# Part d LOOCV with another seed
set.seed(3)
cv.error2 = rep(0, 4)
for(i in seq_along(cv.error2)){
  glm.fit = glm(y ~ poly(x, i), data = data)
  cv.error2[i] = cv.glm(data, glm.fit)$delta[1]
}
cv.error2

# Same results as all samples are used once as the validation set. There is no random picking
# LOOCV predicts every observation using the all of the rest (no randomness involved)

# Part f
fits = lapply(1:4, function(i) lm(y ~ poly(x, i), data = data))
summaries = lapply(fits, summary)
for(i in seq_along(summaries)){
  print(summaries[i])
}

# the first and second degree terms are statistically significant


# Q9 Boston housing data set 
library(MASS)
attach(Boston)
str(Boston)

# Part a
medv_mean = mean(medv)

# Part b standard error of mean
medv_mean_se = sd(medv)/(nrow(Boston)) ^ 0.5

# Part c bootstrap
boot.fn = function(data, index){
  return(mean(data[index]))
}
set.seed(1)
boot_result = boot(Boston$medv, boot.fn, R = 1000)   # sd = 0.4106622

# Part d
names(t.test(Boston$medv))
t.test(Boston$medv)$conf.int
boot_result$t0 - 2 * sd(boot_result$t)
boot_result$t0 + 2 * sd(boot_result$t)

# Part e: medv median
(medv_median = median(Boston$medv))

# Part f: estimate the median using bootstrap
median.fn = function(data, index){
  return(median(data[index]))
}
set.seed(2)
(boot_median = boot(Boston$medv, median.fn, R = 1000))

# Part g
(quantile(Boston$medv, 0.1))

# Part h
tenth.fn = function(data, index){
  return(quantile(data[index], 0.1))
}
set.seed(3)
boot(Boston$medv, tenth.fn, R = 1000)

# sd of 10th quantile = 0.4873263

# P119 ex 3_6 Linear Regression

library(MASS)
library(ISLR)
library(skimr)
library(tidyverse)

# Boston dataset: medv => median house value 
# rm: average number of room per house
# age: average age of house
# lstat: % of house with low socioeconomic status

fix(Boston)
names(Boston)
str(Boston)
skim(Boston)
pairs(Boston)
attach(Boston)

# Fit simple linear regression --------------------------------------------
lm.fit = lm(medv ~ lstat, data = Boston)
names(lm.fit)
summary(lm.fit)

# get confidence interval for the coeff estimates
confint(lm.fit)

# predict() to get the confidence and prediction interval for a given value of lstat
# prediction interval is larger as it includes the variance of error 
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "confidence")
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "prediction")

# Plot points and lm line
# Some evidence of non-linear relationship
plot(lstat,medv, pch = "+")   # pch changes point symbol
abline(lm.fit, lwd = 3, col = "red")   # line width increased

# Plot residuals => non-linearity 
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))    # studentized residuals

# Compute leveraged statistics using hatvalues()
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))



# P123 Multiple linear regression --------------------------------------------------------------------
lm.fit = lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

# fit with all predictors
lm.fit = lm(medv ~ ., data = Boston)
summary(lm.fit)
names(summary(lm.fit))
summary(lm.fit)$r.squared
summary(lm.fit)$sigma      # RSE

# Use vif() to determine collinearity
library(car)
vif(lm.fit)
which.max(vif(lm.fit))

# age has high p-value => predict with all predictors except age
lm.fit1 = lm(medv ~ .-age, data = Boston)
summary(lm.fit1)

# update() => same effect
lm.fit1 = update(lm.fit, ~.-age)


# P125 Interaction term 
# shorthand for lstat+age+lstat:age.
summary(lm(medc ~ lstat * age, data = Boston))

# Non-linear Transformations of the Predictors
lm.fit2 = lm(medv ~ lstat + I(lstat ^ 2), data = Boston)
summary(lm.fit2)

# anova() to quantify whether quadratic fit is superior to linear fit
# low p-value => model 2 is superior
lm.fit = lm(medv ~ lstat, data = Boston)
anova(lm.fit, lm.fit2)

# poly() for higher order
lm.fit5 = lm(medv ~ poly(lstat, 5), data = Boston)
summary(lm.fit5)


# P127 Qualitative Predictors ---------------------------------------------
# Predict Sales
# Carseats include qualitative predictor: ShelveLoc = quality of shelving location
names(Carseats)
summary(Carseats)

lm.fit =lm( Sales ~ .+ Income : Advertising + Price :Age ,data= Carseats )
summary(lm.fit)

# contrasts() checks coding for dummary variable
contrasts(Carseats$ShelveLoc)


# P130 Exercise
# Q8
lm.fit1 = lm(mpg ~ horsepower, data = Auto)
summary(lm.fit1)
predict(lm.fit1, data.frame(horsepower = c(98)), interval = "confidence")
predict(lm.fit1, data.frame(horsepower = c(98)), interval = "prediction")

# Plot horsepower, mpg
attach(Auto)
plot(horsepower, mpg)
abline(lm.fit1, lwd = 3, col = "red")

# Q9
# scatterplot matrix
pairs(Auto)

# matrix of correlation
skim(Auto)
cor(Auto %>% select(-name))

# Q9c: fit with all variables except name
lm.fit2 = lm(mpg ~ .-name, data = Auto)
summary(lm.fit2)

# `weight`, `year`, `origin` and `displacement` have statistically significant relationships

# not constant error term
plot(predict(lm.fit2), residuals(lm.fit2))
plot(predict(lm.fit2), rstudent(lm.fit2))

# Compute leveraged statistics using hatvalues()
plot(hatvalues(lm.fit2))
which.max(hatvalues(lm.fit2))    # observation 14 has high leverage

par(mfrow = c(2,2))
plot(lm.fit2)


# Q9e: fit linear regression with interaction effect
lm.fit3 = lm(mpg~weight + year + origin + displacement, data = Auto)
summary(lm.fit3)

lm.fit4 = lm(mpg ~ year*origin + displacement + weight, data = Auto)
summary(lm.fit4)

lm.fit5 = lm(mpg ~ year*weight + origin + displacement, data = Auto)
summary(lm.fit5)

lm.fit6 = lm(mpg ~ year + origin + weight*displacement, data = Auto)
summary(lm.fit6)

# Q9f fit with other transformation
lm.fit8 = lm(mpg ~ log(displacement), data = Auto)
summary(lm.fit8)
plot(predict(lm.fit8), residuals(lm.fit8))   # seem more constant error term

lm.fit9 = lm(mpg ~ poly(horsepower, 2), data = Auto)
summary(lm.fit9)
plot(predict(lm.fit9), residuals(lm.fit9))

lm.fit10 = lm(mpg ~ I(horsepower^0.5), data = Auto)
summary(lm.fit10)
plot(predict(lm.fit10), residuals(lm.fit10))    # not good result

lm.fit11 = lm(mpg ~ I(log(displacement)) + year + origin + displacement, data = Auto)
summary(lm.fit11)
plot(lm.fit11)

lm.fit12 = lm(mpg ~ I(displacement ^ 2) + year + origin + displacement, data = Auto)
summary(lm.fit12)
plot(lm.fit12)


# Q10 ---------------------------------------------------------------------
# Carseats data set 
attach(Carseats)
str(Carseats)
skim(Carseats)
contrasts(Carseats$US)    # check factor levels

# Q10a multiple regression model
lm.fit1 = lm(Sales ~ Population + Urban + US, data = Carseats)
summary(lm.fit1)    

# Q10b: Only US is significant

# Q10d: reject null hypothesis for variables Population and Urban

# Q10e: fit with a smaller model
lm.fit2 = lm(Sales ~ US, data = Carseats)
summary(lm.fit2)

# Q10f: compare two models
anova(lm.fit2, lm.fit1)     # lm.fit2 better than lm.fit1

# Q10g 95% confidence intervals for the coefficient(s)
confint(lm.fit2)

# Q10h outliers/high leverage?
plot(US, Sales)

# plot residuals
plot(predict(lm.fit2), rstudent(lm.fit2))    # studentized residuals

par(mfrow = c(2,2))
plot(lm.fit2)    # residuals fitted don't show strong outliers

library(car)
leveragePlots(lm.fit2)
plot(hatvalues(lm.fit2))    # average leverage = (p + 1)/n


# Q11 Investigate t-statistic ---------------------------------------------
set.seed(1)
x = rnorm(100)
y = 2*x + rnorm(100)

# Part a: simple linear regression without an intercept
lm.fit1 = lm(y ~ x + 0)
summary(lm.fit1)

# Part b: simple linear regression of x on y
lm.fit2 = lm(x ~ y + 0)
summary(lm.fit2)

# Part f with intercept
lm.fit3 = lm(y ~ x)
lm.fit4 = lm(x ~ y)
summary(lm.fit3)
summary(lm.fit4)

# => same t-value
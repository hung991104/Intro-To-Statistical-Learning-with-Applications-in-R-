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

# Q12 SLR without intercept
# Part b: different coefficients
x = rnorm(100)
y = 2 * x + rnorm(100)
lm.fit1 = lm(y ~ x + 0)
lm.fit2 = lm(x ~ y + 0)
summary(lm.fit1)
summary(lm.fit2)

# Part c: same coefficient
set.seed(1)
x = rnorm(100, mean = 1000, sd = 2)
y = rnorm(100, mean = 1000, sd = 2)
lm.fit1 = lm(y ~ x + 0)
lm.fit2 = lm(x ~ y + 0)
summary(lm.fit1)
summary(lm.fit2)

# Q13 Create Simulated Data => SLR
# Part a
set.seed(100)
x = rnorm(100)

# Part b: create the noise vector
eps = rnorm(100, sd = 0.25)

# Part c
y = -1 + 0.5 * x + eps
length(y)

# Part d: scatterplot
plot(df1$x, df1$y)

# Part e
df1 = data.frame(x, y)
lm.fit1 = lm(y ~ x, data = df1)
summary(lm.fit1)$coefficients[[1]]
summary(lm.fit1)$coefficients[[2]]

# Part f
abline(lm.fit1, col = "red", lwd = 2, lty = 1)    # fitted regression line
abline(-1, 0.5, col = "blue", lwd = 2, lty = 2)   # true regression line 
legend("topleft", legend = c("fitted", "true"), col = c("red", "blue"), lty = 1:2, cex = 0.8)

# Part g: Polynomial regression
lm.fit2 = lm(y ~ x + I(x^2), data = df1)
summary(lm.fit2)
anova(lm.fit1, lm.fit2)     # linear model is not rejected


# Part j: confidence intervals for coefficients
confint(lm.fit1)   # for original dataset


# Q14 Collinearity
# Part a
set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100)/100
y =2+2* x1 +0.3* x2+ rnorm (100)

# Part b
cor(x1, x2)
plot(x1, x2)    # linear relationship between x1 and x2

# Part c
lm.fit1 = lm(y ~ x1 + x2)
summary(lm.fit1)
# cannot reject H0: B1 = 0

# Part d: predict y only with x1
lm.fit2 = lm(y ~ x1)
summary(lm.fit2)

# Part e: predict y only with x2
lm.fit3 = lm(y ~ x2)
summary(lm.fit3)

# Part f: Contradict with each other

# Part g: Additional information
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)

lm.fit1 = lm(y ~ x1 + x2)
lm.fit2 = lm(y ~ x1)
lm.fit3 = lm(y ~ x2)

plot(hatvalues(lm.fit1))
which.max(hatvalues(lm.fit1))

plot(hatvalues(lm.fit2))
which.max(hatvalues(lm.fit2))

plot(hatvalues(lm.fit3))
which.max(hatvalues(lm.fit3))

# High leverage point for the models involving x2; new observation = outlier

# Q15 Boston: predict crime rate 
# Part a: linear regression for each predictor
str(Boston)
which(names(Boston) == "crim")
skim(Boston)
attach(Boston)
data(Boston)

# make chas factor
Boston$chas = factor(Boston$chas, labels = c("N", "Y"))
table(Boston$chas)

# extract r-squared from models
lm.fits = lapply(2:ncol(Boston), function(i) lm(Boston[, 1] ~ Boston[, i]))
summaries = lapply(lm.fits, summary)
sapply(summaries, function(x) c(r_sq = x$r.squared, 
                                adj_r_sq = x$adj.r.squared))
data.frame(variables = names(Boston)[2:ncol(Boston)], 
        t_value = unname(sapply(summaries, function(x) c(t_value = x$coefficients[2, 3])))) %>%
  arrange(desc(t_value))

# extract p-values from models
lm_pvalue = function(model){
  if(class(model) != "lm")
    stop("The input should be an object of class 'lm'")
  f_stat = summary(model)$fstatistic
  
  # obtain p-value
  pvalue = pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
  attributes(pvalue) = NULL
  return(pvalue)
}

lm.fits = lapply(2:ncol(Boston), function(i) lm(Boston[, 1] ~ Boston[, i]))
pvalue_results = sapply(lm.fits, lm_pvalue)
pvalue_df = data.frame(variable = paste(names(Boston)[1], names(Boston)[-1], sep = "_"), p_value = round(pvalue_results,6)) %>%
  arrange(desc(p_value))
pvalue_df     # chas is the only non-significant predictors


# plot crim by chas
Boston %>%
  ggplot(aes(x = crim)) +
  geom_density(aes(fill = chas), alpha =0.4) +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#868686FF", "#EFC000FF"))+
  theme_bw()

# Part b: multiple regression
lm.fit.all = lm(crim~., data = Boston)
summary(lm.fit.all)

# can reject null hypothesis for zn, dis, rad, black, medv

# Part c
# Fewer predictors are statistically significant when more predictors are present
names(summary(lm.fit.all))
multi_coeffs = summary(lm.fit.all)$coefficients[,1][-1]

lm.fits = lapply(2:ncol(Boston), function(i) lm(Boston[, 1] ~ Boston[, i]))
summaries = lapply(lm.fits, summary)
single_coeffs = sapply(summaries, function(x) c(coeffs = x$coefficients[2,1]))

plot(single_coeffs, multi_coeffs)

data.frame(var = names(Boston)[-1], single = unname(single_coeffs), multi = unname(multi_coeffs)) %>%
  ggplot() +
  geom_point(aes(single, multi))   # values for nox are not matched

# exclude nox
data.frame(var = names(Boston)[-1], single = unname(single_coeffs), multi = unname(multi_coeffs)) %>%
  filter(var != "nox") %>%
  ggplot() +
  geom_point(aes(single, multi)) +
  xlim(-2, 2) +
  ylim(-2, 2) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_bw()

# univariate regression coefficients different from multiple regression coefficients 

# Part d: non-linear association
non.lm.fits = lapply(2:ncol(Boston), function(i) lm(Boston[, 1] ~ Boston[, i] + I(Boston[, i]^2) + I(Boston[, i]^3)))
summaries = lapply(non.lm.fits, summary)
names(summaries) = names(Boston)[2:ncol(Boston)]
summaries

test = lm(Boston[,1] ~ Boston[,11] + I(Boston[,11]^2) + I(Boston[,11] ^ 3))
summary(test)

# some predictors have non-linear relationship with the response 
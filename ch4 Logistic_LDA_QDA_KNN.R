# P166 Chapter 4 

library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
attach(Smarket)

# cor(): pairwise correlations among the predictors
cor(Smarket[,-9])

# Increasing Volume
plot(Smarket$Volume)


# Logistic regression: Predict Direction with Lags and Volume -------------

# use glm()
glm.fit = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fit)

# use coef() to access just the coefficients
coef(glm.fit)

# use summary() to access particular aspects like p-value
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]

# predict(): type="response" => output probablity of P(Y - 1|X)
glm.probs = predict(glm.fit, type = "response")
glm.probs[1:10]

# contrasts(): check dummy variable
contrasts(Smarket$Direction)    # 1 for Up

# convert probabilities to prediction of labels Up and Down
glm.pred = rep("Down", 1250)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Smarket$Direction)
mean(glm.pred == Smarket$Direction)    # testing error rate = 0.5216 which is far from desirable

# Create training and testing data set
train = (Year < 2005)
Smarket.2005 = Smarket[!train,]
Direction.2005 = Direction[!train]
dim(Smarket.2005)    # 252 9

# fit a logistic regression model using training data
glm.fit = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5 + Volume, data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fit, Smarket.2005,type = "response")
head(glm.probs, 10)

glm.pred = rep("Down", length(Direction.2005))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred != Direction.2005)   # test error rate = 0.52, which is worse than random guessing


# Remove variables which are not helpful in predicting Direction
glm.fit = glm(Direction ~ Lag1+Lag2, data = Smarket, family = binomial, subset = train)
summary(glm.fit)
glm.probs = predict(glm.fit, Smarket.2005,type = "response")
glm.pred = rep("Down", length(Direction.2005))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred != Direction.2005)  # test error rate = 0.44

# predict Direction given new data
newLags = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8))
predict(glm.fit, newdata = newLags, type = "response")



# LDA ---------------------------------------------------------------------
library(MASS)
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit

lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)   # class = predicted direction, posterior = post. prob 
table(lda.pred$class, Direction.2005)
mean(lda.pred$class == Direction.2005)

# equivalent to applying a 50% threshold to the posterior probabilities
head(lda.pred$posterior)
sum(lda.pred$posterior[,1] < 0.5)    # 182
sum(lda.pred$posterior[,1] >= 0.5)   # 70

# Note: posterior posterior output by the model correspond to "Down"
lda.pred$posterior[1:20, 1]
lda.pred$class[1:20]


# QDA ---------------------------------------------------------------------

qda.fit = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit    # Note: no coefficients as QDA classifier involves a quadratic function of predictors
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)   # 0.5992


# KNN ---------------------------------------------------------------------

library(class)
train.X = cbind(Lag1, Lag2)[train,]
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]

# test k = 1
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)   # 0.5

# test k = 3 
knn.pred = knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)   # 0.5357 => slight improvement but far from satisfactory


# An Application to Caravan Insurance Data --------------------------------
library(ISLR)
library(tidyverse)

# demographic characteristics for 5,822 individuals
# Purchase: whether or not a given individual purchases a caravan insurance policy
dim(Caravan)
attach(Caravan)
summary(Purchase)

# Scale matters for KNN => Standardize the data so that all variables are on a comparable scale 
standardized.X = scale(Caravan[,-86])
var(standardized.X[,1])

# Assign the first 1000 observations to test set
set.seed(1)
test = 1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]

knn.pred = knn(train.X, test.X, train.Y, k = 1)
table(knn.pred, test.Y)
mean(knn.pred != test.Y)    # 0.118
mean(test.Y != "No")        # 0.059
# Among 77 individuals who are willing to buy insurance, 9 actually purchase insurance
# Better than random guessing

knn.pred = knn(train.X, test.X, train.Y, k = 3)   # 5/26
knn.pred = knn(train.X, test.X, train.Y, k = 5)   # 4/15
# KNN is finding some real patterns in a difficult data set!

# Fit a logistic regression model to the data 
glm.fit = glm(Purchase ~ ., data = Caravan, family = binomial, subset = -test)
summary(glm.fit)
glm.probs = predict(glm.fit, Caravan[test, ] ,type = "response")
glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.5] = "Yes"
table(glm.pred, Purchase[test])     # the probability cut-off is too high that only 7 are predicted to purchase insurance and all are wrong

# lower the prob cut-off to 0.25  => better prediction
glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.25] = "Yes"
table(glm.pred, Purchase[test])     # the probability cut-off is too high that only 7 are predicted to purchase insurance and all are wrong



# Exercise - Applied ------------------------------------------------------
# Q10 Explore Weekly

# Part a
library(ISLR)
summary(Weekly)
dim(Weekly)
plot(Weekly$Year, Weekly$Today)

# Part b: Logistic regression with Direction as the response + Use full data set
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(glm.fit)
# Lag2 is statiscally significant

# Part c
glm.probs = predict(glm.fit, type = "response")

# convert probabilities to prediction of labels Up and Down
glm.pred = rep("Down", dim(Weekly)[1])
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Weekly$Direction)
mean(glm.pred == Weekly$Direction)   


# Pard d: Use Lag2 as only predictor
train = (Weekly$Year <= 2008 & Weekly$Year >= 1990)
glm.fit = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
summary(glm.fit)
glm.probs = predict(glm.fit, Weekly[!train,], type = "response")
glm.pred = ifelse(glm.probs > 0.5, "Up", "Down")
table(glm.pred, Weekly$Direction[!train])
mean(glm.pred == Weekly$Direction[!train])   # 0.5625

# Part e: LDA
attach(Weekly)
train.X = Weekly[train,]
test.X = Weekly[!train,]

library(MASS)
lda.fit = lda(Direction ~ Lag2, data = Weekly[train,])
lda.fit

lda.pred = predict(lda.fit, test.X)
names(lda.pred)    
table(lda.pred$class, Weekly$Direction[!train])
mean(lda.pred$class == Weekly$Direction[!train])  # 0.625


# Part f: QDA
qda.fit = qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.class = predict(qda.fit, test.X)$class
table(qda.class, Weekly$Direction[!train])
mean(qda.class == Weekly$Direction[!train])   # 0.5865385

# Part g: KNN with K = 1
library(class)
train.X = as.matrix(Weekly[train,]$Lag2)
test.X = as.matrix(Weekly[!train,]$Lag2)
train.Y = Weekly$Direction[train]
test.Y = Weekly$Direction[!train]

knn.pred = knn(train.X, test.X, train.Y, k = 1)
table(knn.pred, test.Y)
mean(knn.pred == test.Y)     # 0.5

# Part h: LDA provides better results

# Part i
# Non-linear Transformations of the Predictors + lda
glm.fit = glm(Direction ~ Lag2 + I(Lag2 ^ 2), data = Weekly, family = binomial, subset = train)
summary(glm.fit)
glm.probs = predict(glm.fit, Weekly[!train,], type = "response")
glm.pred = ifelse(glm.probs > 0.5, "Up", "Down")
table(glm.pred, Weekly$Direction[!train])
mean(glm.pred == Weekly$Direction[!train])   # 0.625

set.seed(1)
knn_num = c()
knn_accuracy = c()
for(i in seq(5, 25, by = 5)){
  knn.pred = knn(train.X, test.X, train.Y, k = i)
  knn_accuracy = c(knn_accuracy, mean(knn.pred == test.Y))
  knn_num = c(knn_num, i)
}
data.frame(knn_num, knn_accuracy)

# knn = 15 produces better result (0.5865)


# Q11 predict whether a given car gets high or low gas mileage based on the Auto data set

# Part a: Create a binary variable
library(ISLR)
summary(Auto)
attach(Auto)

mpg01 = ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
table(mpg01)

# Part b
pairs(Auto)
# displacement, horsepower and weight seem to be useful in predicting mpg

library(tidyverse)
Auto_select = Auto %>%
  select(mpg, displacement, horsepower, weight, acceleration) %>%
  mutate(mpg01 = ifelse(Auto$mpg > median(Auto$mpg), 1, 0),
         mpg01 = as.factor(mpg01))

Auto_select %>%
  ggplot() +
  geom_boxplot(aes(mpg01, displacement))

Auto_select %>%
  ggplot() +
  geom_boxplot(aes(mpg01, horsepower))

Auto_select %>%
  ggplot() +
  geom_boxplot(aes(mpg01, weight))

Auto_select %>%
  ggplot() +
  geom_boxplot(aes(mpg01, acceleration))

# Part c: split data
sample_size = floor(0.8 * nrow(Auto_select))
set.seed(100)
train_ind = sample(seq_len(nrow(Auto_select)), sample_size)
train_Auto = Auto_select[train_ind,]
test_Auto = Auto_select[-train_ind,]

# Part d: LDA
lda.fit = lda(mpg01 ~ displacement + horsepower + weight, data = train_Auto)
lda.pred = predict(lda.fit, test_Auto)$class
table(lda.pred, test_Auto$mpg01)
mean(lda.pred == test_Auto$mpg01)   # 0.924

# Part e: QDA
qda.fit = qda(mpg01 ~ displacement + horsepower + weight, data = train_Auto)
qda.pred = predict(qda.fit, test_Auto)$class
table(qda.pred, test_Auto$mpg01)
mean(qda.pred == test_Auto$mpg01)   # 0.936

# Part f: logistic regression
glm.fit = glm(mpg01 ~ displacement + horsepower + weight, data = train_Auto, family = binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, test_Auto, type = "response")
glm.pred = ifelse(glm.probs > 0.5, 1, 0)
table(glm.pred, test_Auto$mpg01)
mean(glm.pred == test_Auto$mpg01)  # 0.924

# Part g: KNN
train.X = train_Auto %>% select(-mpg, -mpg01) %>% as.matrix()
test.X = test_Auto %>% select(-mpg, -mpg01) %>% as.matrix()
train.Y = train_Auto %>% select(mpg01) %>% as.matrix()
test.Y = test_Auto %>% select(mpg01) %>% as.matrix()

set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
mean(knn.pred == test.Y)   # 0.92

knn.pred = knn(train.X, test.X, train.Y, k = 10)
table(knn.pred, test.Y)
mean(knn.pred == test.Y)   # 0.911

knn.pred = knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
mean(knn.pred == test.Y)   # 0.9493671


# Q12: writing functions
# Part b
Power2 = function(x,y){
  print(x ^ y)
}
Power2(2, 3)

# Part c
Power2(10, 3)
Power2(8, 17)
Power2(131, 3)

# Part d
Power3 = function(x, a){
  result = x ^ a
  return(result)
}

# Part e
x = c(1:10)
y = mapply(Power3, x, 2)
plot(x, y)
plot(x, y, log = "y")
plot(x, y, log = "xy")

# Part f
PlotPower = function(x, a){
  a = rep(a, length(x))
  y = mapply(Power3, x, a)
  plot(x, y)
}

PlotPower(1:10, 3)


# Q13: Explore Boston
library(MASS)
library(tidyverse)
summary(Boston)
nrow(Boston)
str(Boston)

Boston1 = Boston %>%
  mutate(crim01 = crim > median(crim))
pairs(Boston1)
sort(cor(Boston1)[1,])
sort(cor(Boston1)["crim01",])

train_ind = sample(seq_len(nrow(Boston1)), floor(0.8 * nrow(Auto_select)))
train_X = Boston1[train,]
test_X = Boston1[-train,]

# Logistic Regression
glm.fit = glm(crim01~ age+ dis+ lstat, data = Boston1[train,], family = binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, test_X, type = "response")
glm.pred = ifelse(glm.probs > 0.5, 1, 0)
table(glm.pred, test_X$crim01)
mean(glm.pred == test_X$crim01)  # 0.8039604

glm.fit2 = glm(crim01 ~ rad + age, data = train_X, family = binomial)
summary(glm.fit2)
glm.probs = predict(glm.fit2, test_X, type = "response")
glm.pred = ifelse(glm.probs > 0.5, 1, 0)
table(glm.pred, test_X$crim01)
mean(glm.pred == test_X$crim01)  # 0.8455446

# LDA
lda.fit = lda(crim01 ~ age + dis + lstat, data = train_X)
lda.pred = predict(lda.fit, test_X)$class
table(lda.pred, test_X$crim01)
mean(lda.pred == test_X$crim01)   # 0.792

lda.fit2 = lda(crim01 ~ rad + age, data = train_X)
lda.pred = predict(lda.fit2, test_X)$class
table(lda.pred, test_X$crim01)
mean(lda.pred == test_X$crim01)   # 0.8475

# QDA
qda.fit = qda(crim01 ~ age + dis + lstat, data = train_X)
qda.pred = predict(qda.fit, test_X)$class
table(qda.pred, test_X$crim01)
mean(qda.pred == test_X$crim01)   # 0.794

qda.fit2 = qda(crim01 ~ rad + age, data = train_X)
qda.pred = predict(qda.fit2, test_X)$class
table(qda.pred, test_X$crim01)
mean(qda.pred == test_X$crim01)   # 0.762376

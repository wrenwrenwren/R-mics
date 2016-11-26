library(ISLR)
names(Smarket)

smarket_data <- head(Smarket[,-9])
attach(Smarket)
head(Smarket)

#logistic regression
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family=binomial)
summary(glm.fit)
glm.probs <- predict(glm.fit, type="response")

glm.probs[1:10]
contrasts(Direction)
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > .5] = "Up"

table(glm.pred, Direction)
#You can see that only half is correct. 

train <- (Year<2005)
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)

Direction.2005 <- Direction[!train]
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset=train)
glm.probs <- predict(glm.fit, Smarket.2005, type="response")

glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)

# Summarize of the logistic regression procedure:
#   glm function with training data then predict function with experiment data.

#LDA linear discriminant analysis

library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.pred <- predict(lda.fit, Smarket.2005)

lda.class <- lda.pred$class
table(lda.class , Direction.2005)

#QDA quadratic discriminant analysis 
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

#K-Nearest Neighbors
library(class)
train.X <- cbind(Lag1, Lag2)[train,]
test.X <- cbind(Lag1, Lag2)[!train,]
train.Direction <- Direction[train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
knn.pred <- knn(train.X, test.X, train.Direction, k=10)
table(knn.pred, Direction.2005)


# Summary: glm, lda, qda and knn 

# Lab with Caravan Insurance Data comparing knn, glm, lda and qda

dim(Caravan)
attach(Caravan)
summary(Purchase)
standardized.X <- scale(Caravan[,-86])

var(Caravan[,1])
var(Caravan[,2])

test = 1:1000

train.X <- standardized.X[-test,]
test.X <- standardized.X[test,]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]

#method 1: KNN k nearest neighour 
set.seed(1)

train.pred <- knn(train.X, test.X, train.Y, k=1)
table(train.pred, test.Y)

train.pred <- knn(train.X, test.X, train.Y, k=3)
table(train.pred, test.Y)

train.pred <- knn(train.X, test.X, train.Y, k=5)
table(train.pred, test.Y)

#method 2: logistic regression

glm.fit <- glm(Purchase ~ ., data = Caravan, family=binomial, subset = -test)
test.probs <- predict(glm.fit, Caravan[test,], type="response")
test.pred <- rep("No", length(test.probs))
test.pred[test.probs > .5] <- "Yes"
table(test.pred, test.Y)

#Method 3: lda and qda

lda.fit <- lda(Purchase ~ ., data = Caravan, subset = -test)
test.pred <- predict(lda.fit, Caravan[test,])
table(test.pred$class, test.Y)

qda.fit <- qda(Purchase ~ ., data = Caravan, subset = -test)
test.pred <- predict(qda.fit, Caravan[test,])
table(test.pred$class, test.Y)





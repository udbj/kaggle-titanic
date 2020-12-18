library(data.table)

# train set preprocess
train <- fread('train.csv')

str(train)

train[,sum(Survived == 1)/.N, Pclass]
train[,sum(Survived == 1)/.N, Sex]
train[,sum(Survived == 1)/.N, Embarked]

train <- train[-c(which(train$Embarked == ""))]

train[,sum(Survived == 1)/.N, Embarked]

summary(train$Fare)
plot(Survived ~ Fare, train)

train[,sum(Survived == 1)/.N, Parch]

train[,sum(Survived == 1)/.N, SibSp]

table(train$Cabin)
table(train$Ticket)

summary(train$Age)

train[c(which(is.na(Age))),]$Age <- mean(train[-c(which(is.na(Age))), Age])

library(e1071)

titanic_modl <- svm(Survived ~ Pclass + Sex + Embarked + Fare + Parch + SibSp + Age,
                    data = train)

# load test set

test <- fread('test.csv')

table(test$Pclass)
table(test$Sex)
table(test$Embarked)
summary(test$Fare)

test[which(is.na(test$Fare)),]$Fare <- 0

table(test$Parch)
table(test$SibSp)
summary(test$Age)

test[c(which(is.na(Age))),]$Age <- mean(test[-c(which(is.na(Age))), Age])

predictions <- predict(titanic_modl,
                       test[,c('Pclass','Sex','Embarked','Fare','Parch','SibSp','Age')])



Survived <- unname(ifelse(predictions >= 0.5, 1, 0))

Submission <- data.table('PassengerID' = test$PassengerId, 'Survived' = Survived)

fwrite(Submission, 'titanic_svm.csv')

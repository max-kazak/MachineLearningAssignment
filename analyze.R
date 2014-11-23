library(caret);

data <- read.csv('pml-training.csv')
task <- read.csv('pml-testing.csv')

names(data)

data.rel <- data[,-seq(1,5)]
data.rel <- data.rel[sapply(data.rel, function(x) sum(is.na(x))<dim(data)[1]*0.9)]
head(data.rel)
data.rel <- data.rel[sapply(data.rel, function(x) sum(x=='')<dim(data)[1]*0.9)]
summary(data.rel)

predictors <- names(data.rel)
predictors <- predictors[-length(predictors)]

inTrain <- createDataPartition(data.rel$classe, p=0.7, list=F)
training <- data.rel[inTrain, ]
testing <- data.rel[-inTrain, ]
dim(training); dim(testing);


modelTree <- train(classe ~ ., data=training, method='rpart')

table(predict(modelTree, newdata=training), training$classe)
confusionMatrix(predict(modelTree, newdata=training), training$classe)
table(predict(modelTree, newdata=testing), testing$classe)
confusionMatrix(predict(modelTree, newdata=testing), testing$classe)

print(modelTree$finalModel)


modelLDA <- train(classe ~ ., data=training, method='lda')

table(predict(modelLDA, newdata=training), training$classe)
confusionMatrix(predict(modelLDA, newdata=training), training$classe)
table(predict(modelLDA, newdata=testing), testing$classe)
confusionMatrix(predict(modelLDA, newdata=testing), testing$classe)

modelRF <- train(classe ~ ., data=training, method='rf', preProcess=c('center', 'scale'))

table(predict(modelRF, newdata=training), training$classe)
confusionMatrix(predict(modelRF, newdata=training), training$classe)
table(predict(modelRF, newdata=testing), testing$classe)
confusionMatrix(predict(modelRF, newdata=testing), testing$classe)

answers <- predict(modelRF, newdata=task)



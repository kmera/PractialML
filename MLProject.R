library(data.table)
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(GGally)
library(parallel)
library(doParallel)

url_train <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url_test <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

train_file <- fread(url_train)
test_file <- fread(url_test)
train_file <- as.data.frame(train_file)
test_file <- as.data.frame(test_file)

set.seed(998)

# Training ----
inTrain <- createDataPartition(train_file$classe, p = .75, list = F)
training <- train_file[inTrain, ]
testing <- train_file[-inTrain, ]

NA.data <- function(x) {
        y <- sum(is.na(x))/nrow(training)
}

train.rate_na <- sapply(training, NA.data)
train_file2 <- training[!(train.rate_na > .95)]

new_training <- train_file2[c(8:60)]
new_training$classe <- as.factor(new_training$classe)

# Testing ----
NA.data <- function(x) {
        y <- sum(is.na(x))/nrow(testing)
}

test.rate_na <- sapply(testing, NA.data)
train_file2 <- testing[!(test.rate_na > .95)]

new_testing <- train_file2[c(8:60)]
new_testing$classe <- as.factor(new_testing$classe)

# Modeling ----
#Random Forest
#Paralell Processing
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControlRF <- trainControl(method = "repeatedcv",
                             number = 5,
                             repeats = 5,
                           allowParallel = TRUE)

modFitRF <- train(classe ~ ., data = new_training, method = "rf", 
                   trControl = fitControlRF, 
                   tuneGrid=data.frame(mtry=7))

stopCluster(cluster)
registerDoSEQ()

predRF <- predict(modFitRF, new_testing)
confmatRF <- confusionMatrix(predRF, new_testing$classe)
confmatRF[["overall"]][["Accuracy"]]

#Boosting
fitControlGBM <- trainControl(
        method = "repeatedcv",
        number = 5,
        repeats = 5)

modFitGBM <- train(classe ~ ., data = new_training, 
                 method = "gbm", 
                 trControl = fitControlGBM,
                 verbose = FALSE)
predGBM <- predict(gbmFit1, new_testing)
confmatGBM <- confusionMatrix(predGBM, new_testing$classe)
confmatGBM[["overall"]][["Accuracy"]]

# Combining predictors ----
predDF <- data.frame(predRF, predGBM, classe = new_testing$classe)
combModFit <- train(classe ~ ., method = "rf", data = predDF, 
                    trControl = fitControlRF, 
                    tuneGrid=data.frame(mtry=7))
combPred <- predict(combModFit, predDF)
confmatcomb <- confusionMatrix(combPred, new_testing$classe)
confmatcomb[["overall"]][["Accuracy"]]


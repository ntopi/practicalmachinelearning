# Data Preparation
library(caret)
pml_train <- read.csv("pml-training.csv")
pml_test <- read.csv("pml-testing.csv")
## Splitting the training data so as to be able to measure out of sample error.

set.seed(123)
inTrain <- createDataPartition(y = pml_train$classe, p = 0.7, list = F)
train1 <- pml_train[inTrain, ]
train2 <- pml_train[-inTrain, ]

## Reduce number of features by removing variables with nearly zero variance and NA

## remove variables with nearly zero variance
nzv <- nearZeroVar(train1)
train1 <- train1[, -nzv]
train2 <- train2[, -nzv]

## remove variables that are near NA
mostlyNA <- sapply(train1, function(x) mean(is.na(x))) > 0.97
train1 <- train1[, mostlyNA == F]
train2 <- train2[, mostlyNA == F]

## removing variables that cannot effect the model much
train1 <- train1[, -(1:5)]
train2 <- train2[, -(1:5)]

# Model Building
## Random Forest to fit the model on using 10-fold cross- validation to select optimal tuning parameters.

library(randomForest)
numFolds <- trainControl(method = "cv", number = 10, verboseIter = F)
fit <- train(classe ~., data = train1, method = "rf", trControl = numFolds)
fit$finalModel

# Model Evaluation and Selection
## predict the label in train2 split

predTrain <- predict(fit, newdata = train2)
confusionMatrix(train2$classe, predTrain)

# Model Retraining and Selection

## removing nearly zero variables
nzv <- nearZeroVar(pml_train)
pml_train <- pml_train[, -nzv]
pml_test <- pml_test[, -nzv]

## remove NAs
mostlyNA <- sapply(pml_train, function(x) mean(is.na(x))) > 0.97
pml_train <- pml_train[, mostlyNA == F]
pml_test <- pml_test[, mostlyNA == F]

## remove non importance variables
pml_train <- pml_train[, -(1:5)]
pml_test <- pml_test[, -(1:5)]

## Refit the Model using 3-Fold cross validation on the full training set
numFolds <- trainControl(method = "cv", number = 3, verboseIter = F)
modelFit <- train(classe ~., data = pml_train, method = "rf", trControl = numFolds)
modelFit$finalModel

## Test the Model
predTest <- predict(modelFit, newdata = pml_test)

## convert predictions to character vector
predTest <- as.character(predTest)

## create function to write predictions to files
pml_write_files <- function(x){
	n <- length(x)
	for (i in 1:n){
		filename <- paste0("problem_id_", i, ".txt")
		write.table(x[i], file = filename, quote = F, row.names = F, col.names = F)
	}
}

# create prediction files to submit
pml_write_files(predTest)
# practicalmachinelearning
Repository for JHU Practical Machine Learning Course

#Introduction
For this project we were data based on devices such as Jawbone Up, Nike FuelBand, and Fitbit usages. it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.

Below is the code I used when creating the model, estimating out of sample errors and making predictions

# Data Preparation
I loaded the caret package and read in the training and testing data

library(caret)
pml_train <- read.csv("pml-training.csv")
pml_test <- read.csv("pml-testing.csv")

## Splitting the training data so as to be able to measure out of sample error in a randomised fashon.

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
I see that the random forest used 500 trees and try 27 variables at each spliot.

# Model Evaluation and Selection
## predict the label in train2 split and then show the confusion matrix

predTrain <- predict(fit, newdata = train2)
confusionMatrix(train2$classe, predTrain)
The accuracy of the model was 99.8%, thusbmy predicted aacuracy for the out-of-sample error is 0.2%

# Model Retraining and Selection before predicting on the test set.

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

# Making Test Set Predictions

## predict on the test set
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

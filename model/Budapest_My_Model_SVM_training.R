library(caTools)
library(e1071)
library(caret)
library(pROC)

data <- read.csv2("data/final_data/encoded_hexapeptide_data.csv")

data_frame <- data.frame(data)

short_data <- data_frame[, 2:ncol(data_frame)]

set.seed(103)

short_data$Classification <- ifelse(short_data$Classification == "amyloid", "amyloid", 'non_amyloid')
short_data$Classification <- factor(short_data$Classification, levels = c("non_amyloid", "amyloid"))
split <- sample.split(short_data$Classification, SplitRatio = 0.75)

training_set <- subset(short_data, split == TRUE)
test_set <- subset(short_data, split == FALSE)

print(nrow(short_data))
print(nrow(training_set))
print(nrow(test_set))

train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE)

svm_model <- train(Classification ~ .,
                   data = training_set,
                   method = "svmRadial",
                   trControl = train_control,
                   tuneLength = 10)

prediction <- predict(svm_model, newdata = test_set, type = "prob")
predicted_classes <- ifelse(prediction[, 2] > 0.5, "amyloid", "non_amyloid")

print(head(prediction))

confusion_matrix <- table(test_set$Classification, predicted_classes)
print(confusion_matrix)

accuracy <- sum(predicted_classes == test_set$Classification) / nrow(test_set)

roc_object <- roc(test_set$Classification, as.numeric(prediction[,2]))
plot(roc_object, col="red", main="ROC curve SVM")
auc(roc_object)

print(accuracy)

library(caTools)
library(e1071)
library(caret)
library(pROC)

data_frame <- read.csv2("data/final_data/final_data_encoded.csv", sep = ";", dec = "." , header = TRUE)
# str(data_frame, list.len = 6000)
set.seed(103)

data_frame$Classification <- ifelse(data_frame$Classification == "amyloid", "amyloid", 'non_amyloid')
data_frame$Classification <- factor(data_frame$Classification, levels = c("non_amyloid", "amyloid"))
split <- sample.split(data_frame$Classification, SplitRatio = 0.75)

training_set <- subset(data_frame, split == TRUE)
test_set <- subset(data_frame, split == FALSE)

print(nrow(data_frame))
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

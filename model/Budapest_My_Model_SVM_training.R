library(caTools)
library(e1071)
library(caret)
library(pROC)

save_model <- FALSE
# Wybór zbioru uczącego
aaindex1 <- FALSE
aaindex3 <- FALSE
aaindex4 <- FALSE
normalized <- FALSE
standarizeed <- FALSE
balance_data <- FALSE

data_path <- "data/final_data/final_data_encoded.csv"
data_standarized_path <- "data/final_data/final_data_encoded_standarized.csv"
data_normalized_path <- "data/final_data/final_data_encoded_normalized.csv"
data_aaindex23_normalized_path <- "data/final_data/aaindex23_data_encoded_normalized.csv"
data_aaindex1_normalized_path <- "data/final_data/aaindex1_data_encoded_normalized.csv"
data_one_hot_encoded <- "data/final_data/one_hot_encoded.csv"
data_aaindex12_normalized_path <- "data/final_data/aaindex12_encoded_normalized.csv"
data_aaindex13_normalized_path <- "data/final_data/aaindex13_encoded_normalized.csv"
data_frame <- read.csv2(data_one_hot_encoded, sep = ";", dec = "." , header = TRUE)

set.seed(103)

data_frame$Classification <- ifelse(data_frame$Classification == "amyloid", "amyloid", 'non_amyloid')
data_frame$Classification <- factor(data_frame$Classification, levels = c("non_amyloid", "amyloid"))

if (balance_data) {
    data_frame <- downSample(x = data_frame[,-1], y = data_frame$Classification)
    data_frame <- cbind(data_frame$Class, data_frame[, -ncol(data_frame)])
    colnames(data_frame)[1] <- "Classification"
}

split <- sample.split(data_frame$Classification, SplitRatio = 0.75)
training_set <- subset(data_frame, split == TRUE)
test_set <- subset(data_frame, split == FALSE)

train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE)

svm_model <- train(Classification ~ .,
                   data = training_set,
                   method = "svmLinear",
                   trControl = train_control,
                   tuneLength = 10,
                   tuneGrid = data.frame(C = 10))

prediction <- predict(svm_model, newdata = test_set, type = "prob")
predicted_classes <- ifelse(prediction[, 2] > 0.5, "amyloid", "non_amyloid")

confusion_matrix <- table(test_set$Classification, predicted_classes)
print(confusion_matrix)

accuracy <- sum(predicted_classes == test_set$Classification) / nrow(test_set)

roc_object <- roc(test_set$Classification, as.numeric(prediction[,2]))
plot(roc_object, col="red", main="ROC curve SVM")
auc(roc_object)

print(accuracy)

if (save_model) {
    model_name <- "linear_normalized_aaindex12_c10"
    model_path <- paste("saved_models/", model_name, ".RData", sep = "")
    save(svm_model, file = model_path)
}
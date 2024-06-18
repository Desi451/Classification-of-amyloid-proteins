library(caTools)
library(e1071)
library(caret)
library(pROC)
library(MLmetrics)
library(irr)

save_model <- TRUE
model_name <- "aaindex1_best_model"

# Wybór zbioru uczącego
onehotencoded <- FALSE
aaindex1 <- TRUE
aaindex2 <- FALSE
aaindex3 <- FALSE
normalized <- FALSE
standarizeed <- FALSE
balance_data <- FALSE

data_path <- "data/final_data/"
if (aaindex1 || aaindex2 || aaindex3) {
    data_path <- paste(data_path, "aaindex", sep = "")
}
if (aaindex1) {
    data_path <- paste(data_path, "1", sep = "")
}
if (aaindex2) {
    data_path <- paste(data_path, "2", sep = "")
}
if (aaindex3) {
    data_path <- paste(data_path, "3", sep = "")
}
if (onehotencoded) {
    data_path <- paste(data_path, "one_hot", sep = "")
}

data_path <- paste(data_path, "_encoded", sep = "")

if (normalized) {
    data_path <- paste(data_path, "_normalized", sep = "")
}
if (standarizeed) {
    data_path <- paste(data_path, "_standarized", sep = "")
}

data_path <- paste(data_path, ".csv", sep = "")

data_frame <- read.csv2(data_path, sep = ";", dec = ".", header = TRUE)

set.seed(103)

data_frame$Classification <- ifelse(data_frame$Classification == "amyloid", "amyloid", "non_amyloid")
data_frame$Classification <- factor(data_frame$Classification, levels = c("non_amyloid", "amyloid"))
data_frame_unbalanced <- data_frame
if (balance_data) {
    data_frame <- downSample(x = data_frame[, -1], y = data_frame$Classification)
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
    tuneGrid = data.frame(C = 10)
)

prediction <- predict(svm_model, newdata = test_set, type = "prob")
predicted_classes <- ifelse(prediction[, 2] > 0.5, "amyloid", "non_amyloid")
predicted_factor <- factor(predicted_classes, levels = c("non_amyloid", "amyloid"))

confusion_matrix <- table(test_set$Classification, predicted_classes)
print(confusion_matrix)

sens <- sensitivity(predicted_factor, test_set$Classification)
print(sens)

spec <- specificity(predicted_factor, test_set$Classification)
print(spec)

prec <- precision(predicted_factor, test_set$Classification)
print(prec)

accuracy <- sum(predicted_classes == test_set$Classification) / nrow(test_set)
print(accuracy)

roc_object <- roc(test_set$Classification, as.numeric(prediction[, 2]))
plot(roc_object, col = "red", main = "ROC curve SVM")
auc(roc_object)

f1 <- F1_Score(predicted_classes, test_set$Classification)
print(f1)

golden_kappa <- kappa2(data.frame(test_set$Classification, predicted_classes))
print(golden_kappa$value)

if (save_model) {
    model_path <- paste("saved_models/", model_name, ".RData", sep = "")
    save(svm_model, file = model_path)
}

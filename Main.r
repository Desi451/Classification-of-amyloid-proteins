library(caTools)
library(e1071)
library(readxl)

data <- read.csv2("D:\\nauka\\studia\\magisterka semestr 1\\WKIRO\\waltzdb_export_encoded.csv")

data_frame <- data.frame(data)

short_data <- data_frame[, 2:ncol(data_frame)]

set.seed(103)

short_data$Classification <- factor(short_data$Classification,
    levels = c("non-amyloid", "amyloid")
)

split <- sample.split(short_data$Classification, SplitRatio = 0.75)

training_set <- subset(short_data, split == TRUE)
test_set <- subset(short_data, split == FALSE)

print(nrow(short_data))
print(nrow(training_set))
print(nrow(test_set))

classifier <- svm(
    formula = Classification ~ . - Classification,
    data = training_set,
    type = "C-classification",
    kernel = "linear"
)

y_pred <- predict(classifier, test_set)

print(y_pred)

print("Confusion matrix")

confusion_matrix <- table(test_set$Classification, y_pred)
print(confusion_matrix)

accuracy <- sum(y_pred == test_set$Classification) / nrow(test_set)

print(accuracy)

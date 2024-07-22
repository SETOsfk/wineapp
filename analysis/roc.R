# Load necessary libraries

library(caret)
library(randomForest)
library(dplyr)
library(pROC)
library(ROSE)
library(e1071)
library(ada)
library(readr)

BordeauxWines <- read_csv("BordeauxWines.csv")
wine <- BordeauxWines
str(wine) # First, examine the dataset to see the character types
# All values except the first four columns should be factors but are marked as double

# To transform this, the first four columns are excluded
wine <- wine[,-1:-4]

# Then, double variables are converted to factors
wine <- wine %>% mutate_if(is.double, as.factor)
str(wine)
# Some factors are marked as only 0, which will have zero variance and don't contribute to model training
# so they'll be removed from the dataset
cols_to_remove <- sapply(wine, function(x) is.factor(x) && length(levels(x)) == 1)
wine_main <- wine[, !cols_to_remove]
# All are factors and have two levels, 0 and 1
str(wine_main)
# Excluded columns are added back to the dataset, and finally, class transformation of the YEAR variable is done
birlestir <- BordeauxWines[, 1:4]
wine_main <- data.frame(birlestir, wine_main)
wine_main$Year <- as.Date(wine_main$Year)

# Score variable is divided into two categories: those with more than 90 and those with less than 89
Diagnose <- c(1:14349)
wine_main <- data.frame(wine_main, Diagnose)
wine_main <- wine_main %>%
  mutate(Diagnose = case_when(
    Score <= 89 ~ "X0",
    Score >= 90 ~ "X1",)) %>%
  select(Diagnose, Score, Name, Year, Price, everything())
wine_main$Diagnose <- as.factor(wine_main$Diagnose)
wine_main <- wine_main %>% select(-Year, -Name, -Price, -Score) # To obtain a simplified subset, variables that do not affect the score are removed from the dataset
str(wine_main$Diagnose)

# Develop a random forest model for dimensionality reduction using RFI
rf_model <- randomForest(Diagnose ~ ., data = wine_main, importance = TRUE, ntree = 100)

print(rf_model)

# Record and feature the important variables
importance_scores <- importance(rf_model, type = 1)

head(importance_scores)

str(importance_scores)

# If importance_scores is a matrix, extract the correct column
# Typically the first column contains MeanDecreaseAccuracy or MeanDecreaseGini
if (is.matrix(importance_scores)) {
  importance_values <- importance_scores[, 1] # Change 1 if another column is needed
} else {
  importance_values <- importance_scores
}

# Sort the importance levels in descending order and then record the names of the top 50
sorted_importance <- sort(importance_values, decreasing = TRUE)
important_features <- names(sorted_importance)[1:45]

print(important_features)

# Take the names and the DIAGNOSE column into a different dataset
X_reduced <- wine_main[, c(important_features)]
Diagnose <- wine_main$Diagnose
X_reduced <- data.frame(Diagnose, X_reduced)

# 2. Train-Test Split
trainIndex <- createDataPartition(X_reduced$Diagnose, p = .7, 
                                  list = FALSE, 
                                  times = 1)
train_data <- X_reduced[trainIndex,]
test_data  <- X_reduced[-trainIndex,]

barplot(prop.table(table(X_reduced$Diagnose)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")

#As you can see in the plot that there is a class imbalance.
#So that means: even if is accuracy calculated high there will lower predict rate
#for the minority class which will misslead us.
#You can observe class imblance by checking F score, Balanced accuracy, Precision and recall.

bothq <- ovun.sample(Diagnose ~ ., data = train_data, method = "both")
train_data <- bothq$data
# Solve the imbalance problem by both under and oversampling

barplot(prop.table(table(train_data$Diagnose)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")

train_control <- trainControl(
  method = "cv",       # Cross-validation
  number = 5,          # 5-fold cross-validation
  classProbs = TRUE,   # Compute class probabilities
  summaryFunction = twoClassSummary # Summary function for classification
)



# Logistic Regression
logistic_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "glm", 
  family = binomial, 
  trControl = train_control, 
  metric = "ROC"
)

# Random Forest
rf_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "rf", 
  trControl = train_control, 
  metric = "ROC"
)

# GBM
gbm_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "gbm", 
  trControl = train_control, 
  verbose = FALSE,
  metric = "ROC"
)

# SVM
svm_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "svmRadial", 
  trControl = train_control, 
  metric = "ROC"
)

# AdaBoost
ada_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "ada", 
  trControl = train_control, 
  metric = "ROC"
)

# CART
cart_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "rpart", 
  trControl = train_control, 
  metric = "ROC"
)

# LDA
lda_model <- train(
  Diagnose ~ ., data = train_data, 
  method = "lda", 
  trControl = train_control, 
  metric = "ROC"
)


# 5. Resample Comparison
results <- resamples(list(
  LOG = logistic_model, 
  RF = rf_model, 
  GBM = gbm_model, 
  SVM = svm_model,
  ADAB = ada_model, 
  CART = cart_model,
  LDA = lda_model
))

# 6. Summary of the results
summary(results)
dotplot(results)

# 7. Evaluate Models
# Logistic Regression
logistic_preds <- predict(logistic_model, newdata = test_data)
logistic_probs <- predict(logistic_model, newdata = test_data, type = "prob")

# Random Forest
rf_preds <- predict(rf_model, newdata = test_data)
rf_probs <- predict(rf_model, newdata = test_data, type = "prob")

# SVM
svm_preds <- predict(svm_model, newdata = test_data)
svm_probs <- predict(svm_model, newdata = test_data, type = "prob")

# GBM
gbm_preds <- predict(gbm_model, newdata = test_data)
gbm_probs <- predict(gbm_model, newdata = test_data, type = "prob")

# AdaBoost
ada_preds <- predict(ada_model, newdata = test_data)
ada_probs <- predict(ada_model, newdata = test_data, type = "prob")

# Confusion Matrices
print(confusionMatrix(logistic_preds, test_data$Diagnose))
print(confusionMatrix(rf_preds, test_data$Diagnose))
print(confusionMatrix(svm_preds, test_data$Diagnose))
print(confusionMatrix(gbm_preds, test_data$Diagnose))
print(confusionMatrix(ada_preds, test_data$Diagnose))

varimp_rf <- varImp(rf_model)
plot(varimp_rf, main = "Variable Importance for Random Forest")

varimp_svm <- varImp(svm_model)
plot(varimp_svm, main = "Variable Importance for SVM")

varimp_ada <- varImp(ada_model)
plot(varimp_ada, main = "Variable Importance for AdaBoost")

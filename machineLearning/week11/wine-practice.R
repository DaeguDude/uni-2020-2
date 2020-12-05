# caret 패키지 설치
install.packages("caret", dependencies = TRUE)
library(caret)

# 데이터 불러오기
wine_svm <- read.csv("wine.csv", header = TRUE)
# Change class variable to factor
wine_svm$Class <- as.factor(wine_svm$Class)
str(wine_svm)

set.seed(2020)
svm_total <- sample(nrow(wine_svm), nrow(wine_svm)*0.7) # 70%
wine_svm_train <- wine_svm[svm_total,] # 70%
wine_svm_test <- wine_svm[-svm_total, ] # 30%

train_x <- wine_svm_train[, 1:13] # Xs
train_y <- wine_svm_train[, 14] # y
test_x <- wine_svm_test[, 1:13] # Xs
test_y <- wine_svm_test[, 14] # y

# 선형, 비선형 모델 만듬
#선형
ctrl <- trainControl(method = "repeatedcv", repeats = 5)
svm_linear_fit <- train(Class ~ .,
                        data = wine_svm_train,
                        method = "svmLinear",
                        trControl = ctrl,
                        preProcess = c("center", "scale"),
                        metric = "Accuracy")

# We use `predict` to predict the model
pred_svm_linear_fit <- predict(svm_linear_fit, wine_svm_test)
confusionMatrix(pred_svm_linear_fit, wine_svm_test$Class)
tb <- table(pred_svm_linear_fit, test_y)
tb
#오분류율
error_tb <- 1-(sum(diag(tb)/sum(tb)))
error_tb
# 비선형
ctrl <- trainControl(method = "repeatedcv", repeats = 5)
svm_poly_fit <- train(Class ~ .,
                        data = wine_svm_train,
                        method = "svmPoly",
                        trControl = ctrl,
                        preProcess = c("center", "scale"),
                        metric = "Accuracy")

plot(svm_poly_fit)
pred_svm_poly_fit <- predict(svm_poly_fit, wine_svm_test)
confusionMatrix(pred_svm_poly_fit, wine_svm_test$Class)
importance_poly <- varImp(svm_poly_fit, scale=FALSE)
plot(importance_poly)





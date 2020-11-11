
# Install Package
install.packages("C50")
install.packages("gmodels")
library(C50)
library(gmodels)

# ------------------------------------------
# 1.Explore Data
# ------------------------------------------

# Iris Data
iris

# Show structure of Iris
str(iris)

# ------------------------------------------
# 2. Prepare Data
# ------------------------------------------
set.seed(1234)

# 훈련 100 테스트 50
train_sample <- sample(150, 100)

iris_train <- iris[train_sample, ]
iris_test <- iris[-train_sample, ]

prop.table(table(iris_train$Species))
prop.table(table(iris_test$Species))

# ------------------------------------------
# 3. Train Model
# ------------------------------------------

# Check and Factorise
str(iris_train$Species)
iris_train$Species <- as.factor(iris_train$Species)

# C5.0(Training Dataset, categorised)
iris_model <- C5.0(iris_train, iris_train$Species)

# ------------------------------------------
# 4. Evaluate The Model
# ------------------------------------------
iris_pred <- predict(iris_model, iris_test)
CrossTable(iris_test$Species, iris_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn=c('Actual Species', 'Predicted Species'))

# ------------------------------------------
# 5. Improvise The Model(No need for Improvisation)
# ------------------------------------------
iris_boost10 <- C5.0(iris_train, iris_train$Species, trials = 10)
iris_boost_pred10 <- predict(iris_boost10, iris_test)
CrossTable(iris_test$Species, iris_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn=c('Actual Species', 'Predicted Species'))



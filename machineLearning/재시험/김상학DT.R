# Decision Tree

# Install Package
# install.packages("C50")
# install.packages("gmodels")
library(C50)
library(gmodels)

# ------------------------------------------
# 1.Explore Data
# ------------------------------------------
# Read data
MTWines <- read.csv("MTwines.csv")
str(MTWines)

# Factorise
MTWines$quality <- as.factor(MTWines$quality)

# ------------------------------------------
# 2. Prepare Data
# ------------------------------------------
set.seed(2020)

# train 70% test 30%
train_sample <- sample(4898, 3429)

MTWines_train <- MTWines[train_sample, ]
MTWines_test <- MTWines[-train_sample, ]

# ------------------------------------------
# 3. Train Model
# ------------------------------------------

# C5.0(Training Dataset, category to be in)
MTWines_model <- C5.0(MTWines_train, MTWines_train$quality)

# ------------------------------------------
# 4. Evaluate The Model
# ------------------------------------------
MTWines_pred <- predict(MTWines_model, MTWines_test)
CrossTable(MTWines_test$quality, MTWines_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn=c('Actual Quality', 'Predicted Quality'))

# ------------------------------------------
# 5. Improvise The Model(No need for Improvisation)
# ------------------------------------------
MTWines_boost10 <- C5.0(MTWines_train, MTWines_train$quality, trials = 10)
MTWines_boost10_pred10 <- predict(MTWines_boost10, MTWines_test)
CrossTable(MTWines_test$quality, MTWines_boost10_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn=c('Actual Species', 'Predicted Species'))


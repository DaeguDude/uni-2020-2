heart_disease <- read.csv("data.csv")

# --------------------------------------------------------------------
# DATA PREPROCESSING
# --------------------------------------------------------------------
# remove unused variable 'oldpeak', 'slope', 'ca', 'thal'
heart_disease <- heart_disease[,-10:-13]
# change '?' to NA
heart_disease[heart_disease=='?'] <- NA
# remove rows that has 'NA'
heart_disease <- na.omit(heart_disease)

# --------------------------------------------------------------------
# Convert Variables
# --------------------------------------------------------------------
# Factorise variables
heart_disease$sex <- as.factor(heart_disease$sex)
heart_disease$cp <- as.factor(heart_disease$cp)
heart_disease$fbs <- as.factor(heart_disease$fbs)
heart_disease$restecg <- as.factor(heart_disease$restecg)
heart_disease$exang <- as.factor(heart_disease$exang)
heart_disease$num <- as.factor(heart_disease$num)

# numericalise variables
heart_disease$age <- as.numeric(heart_disease$age)
heart_disease$trestbps <- as.numeric(heart_disease$trestbps)
heart_disease$chol <- as.numeric(heart_disease$chol)
heart_disease$thalach <- as.numeric(heart_disease$thalach)

# --------------------------------------------------------------------
# Create Table One
# --------------------------------------------------------------------
library(tableone)

# variables to summarise, excluding result 'num'
list_vars <- names(heart_disease)[-10]

# categorical variables that need transformation
cat_vars <- c("sex", "cp", "fbs", "restecg", "exang", "num")


heart_disease_tableOne <- CreateTableOne(vars = list_vars, data=heart_disease,
                                         factorVars = cat_vars, strata="num")

# normality test for all continuous variables
shapiro.test(heart_disease$age)
shapiro.test(heart_disease$trestbps)
shapiro.test(heart_disease$chol)
shapiro.test(heart_disease$thalach)

cat_vars_without_y_var <- cat_vars[-6]
print(heart_disease_tableOne, showAllLevels = TRUE, 
      nonnormal=c("age", "trestbps", "chol", "thalach"), exact=cat_vars_without_y_var)

# remove variables not needed
rm(cat_vars)
rm(cat_vars_without_y_var)
rm(list_vars)

# --------------------------------------------------------------------
# Make Predict Model with Logistic Regression
# --------------------------------------------------------------------
library(tidyverse)
library(caret)
theme_set(theme_bw())

set.seed(123)
training_samples <- heart_disease$num %>% 
  createDataPartition(p = 0.7, list = FALSE)

# 70% for building a predictive model, 30% for evaluating the model
train_data <- heart_disease[training_samples, ]
test_data <- heart_disease[-training_samples, ]

# Fit the model
logistic_model <- glm(num ~., data = train_data, family = binomial)

# summarize the model
summary(logistic_model)


# Using our logistic_model we will get probability of outcome having '1' based on Xs.
probabilities <- logistic_model %>% predict(test_data, type = "response")

# Now we convert probabilities into heart disease outcome. Threshold 0.5
predicted_outcome <- ifelse(probabilities > 0.5, 1, 0)

# Model accuracy
mean(predicted_outcome == test_data$num)


# remove unused data
rm(probabilities)
rm(predicted_outcome)
rm(training_samples)

# --------------------------------------------------------------------
# Make Graph of Our Predict Model
# --------------------------------------------------------------------
# to draw the graph, we start by creating a new data.frame that
# contains the probabilities of having heart disease along with the actual heart disease status

predicted_data <- data.frame(
  probability_of_hd=logistic_model$fitted.values,
  hd = train_data$num)

# then we sort the data.frame from low probabilities to high probabilities
predicted_data <- predicted_data[
  order(predicted_data$probability_of_hd, decreasing = FALSE),]

# then we add a new column to the dataframe that has the rank of each same, from low to high probability
predicted_data$rank <- 1:nrow(predicted_data)

library(ggplot2)
library(cowplot)

ggplot(data=predicted_data, aes(x=rank, y=probability_of_hd)) +
  geom_point(aes(color=hd), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting heart disease")
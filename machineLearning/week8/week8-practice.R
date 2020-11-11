#install.packages("RWeka")
#install.packages("OneR")
# setwd("~/Desktop/University/machineLearning/week8")
library(RWeka)
library(OneR)

# 'stringAsFactors' makes everything factor
mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE)
str(mushrooms)

#변수제거
mushrooms$veil_type <- NULL
table(mushrooms$type)

#oneR modeling
#oneR(class ~ predictor(. - everything))
# predictor '.' means everything
mushroom_1R <- OneR(type ~ ., data=mushrooms)

# evaluate model

mushroom_1R_pred <- predict(mushroom_1R, mushrooms)
table(actual=mushrooms$type, predicted=mushroom_1R_pred)

# improvise the model(use RIPPER algorithm)

mushroom_JRip <- JRip(type ~ ., data=mushrooms)
mushroom_JRip_pred <- predict(mushroom_JRip, mushrooms)

table(actual=mushrooms$type, predicted=mushroom_JRip_pred)


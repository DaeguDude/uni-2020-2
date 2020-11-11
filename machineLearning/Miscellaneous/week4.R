wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

# structure 
str(wbcd) 

# First row is id
wbcd <- wbcd[-1]

str(wbcd)

table(wbcd$diagnosis)

# Make it as factors
wbcd$diagnosis <- factor(wbcd$diagnosis,
                         levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

# 상대도수 계산
prop.table(table(wbcd$diagnosis))*100
round(prop.table(table(wbcd$diagnosis))*100, digits = 1)

summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# 정규화과정
normalize <- function(x) {
  return( ( x-min(x) ) / ( max(x) - min(x) ) )
}

# 제대로 작동하는지 확인
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

lapply(wbcd[2:31], normalize)

# Make it as a data frame
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

# See the summary of radius_mean, area_mean, and smoothness_mean
summary(wbcd_n[c("radius_mean", "area_mean", "smoothness_mean")])

# ------------------------------------------------------------------------
# 훈련 및 테스트 데이터셋 생성

# 훈련 데이터와 테스트 데이터 생성
# [row, col]
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

# knn 모델훈련 -> class label 벡터생성
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

# class 패키지의 knn() 함수 이용
install.packages("class")
library(class)

# 469개에 루트를 씌운 값 = 21
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl=wbcd_train_labels, k=21)

#-------------------------------------------
# 모델 성능평가
# install.packages("gmodels")
library(gmodels)
# This is called Confusion Matrix(혼동 행렬)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, chisq = TRUE)

#-------------------------------------------
# 모델 성능개선 #1. 데이터표준화 z-score

wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)

# Training Dataset
wbcd_z_train <- wbcd_z[1:469, ]

wbcd_z_test <- wbcd_z[470:569, ]

wbcd_z_train_labels <- wbcd[1:469, 1]

wbcd_z_test_labels <- wbcd[470:569, 1]
wbcd_z_test_pred <- knn(train = wbcd_z_train, test = wbcd_z_test,
                        cl = wbcd_z_train_labels, k=21)

CrossTable(x=wbcd_z_test_labels, y=wbcd_z_test_pred, chisq=FALSE)

#-------------------------------------------
# 모델 성능개선 #2. K개수 조정

# k = 1
wbcd_test_pred3 <- knn(train = wbcd_train, test = wbcd_test,
                       cl = wbcd_train_labels, k=1)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred3, chisq = TRUE)

# k = 5
wbcd_test_pred3 <- knn(train = wbcd_train, test = wbcd_test,
                       cl = wbcd_train_labels, k=5)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred3, chisq = TRUE)

# k = 11
wbcd_test_pred3 <- knn(train = wbcd_train, test = wbcd_test,
                       cl = wbcd_train_labels, k=11)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred3, chisq = TRUE)

# k = 15
wbcd_test_pred3 <- knn(train = wbcd_train, test = wbcd_test,
                       cl = wbcd_train_labels, k=15)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred3, chisq = TRUE)

# k = 21
wbcd_test_pred3 <- knn(train = wbcd_train, test = wbcd_test,
                       cl = wbcd_train_labels, k=21)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred3, chisq = TRUE)

# k = 27
wbcd_test_pred3 <- knn(train = wbcd_train, test = wbcd_test,
                       cl = wbcd_train_labels, k=27)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred3, chisq = TRUE)







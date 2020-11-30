install.packages('survival')
library(survival)
data(colon)


# ----------------------------------------
# 1번 문제
# dim(colon)


# ----------------------------------------
# 2번 문제
aggregate(nodes ~ sex, data=colon, FUN=mean)
aggregate(nodes ~ sex, data=colon, FUN=sd)
# quantile은 어떻게하지?
aggregate(nodes ~ sex, data=colon, FUN=median)


# ----------------------------------------
# 3번 문제
# 확률표
prop.table(table(colon$sex, colon$differ), 2) * 100
# 그냥 표
table(colon$sex, colon$differ)


# ----------------------------------------
# 4번 문제
# https://www.dummies.com/programming/r/how-to-remove-rows-with-missing-data-in-r/

data2 <- colon[complete.cases(colon), ]

# ----------------------------------------
# 5번 문제 
# Lev를 포함하고 있다면 1 아니면 0
rx <- ifelse(data2$rx%in%"Lev", 1, 0)
# 팩터화
rx <- as.factor(rx)
# 카운트
table(rx)
# 퍼센트
prop.table(table(rx)) * 100

# ----------------------------------------
# 6번 문제 

age <- ifelse(data2$age < 50, 1, ifelse(data2$age >= 70, 3, 2))
age <- as.factor(age)
# 카운트
table(age)
# 퍼센트
prop.table(table(age)) * 100


# ----------------------------------------
# 테이블 1 만들기
data3 <- data.frame(data2$sex, age, data2$obstruct, data2$nodes, data2$time, rx)
names(data3) <- c('sex', 'age', 'obstruct', 'nodes', 'time', 'rx')

# install.packages("tableone")
library(tableone)

variable_names = names(data3)[-6]

CreateTableOne(data=data3)


listVars <- names(data)[-7] # remove group variable 'Gender'
(table2 <- CreateTableOne(listVars, data, catVars, strata=c("Education")))
print(table2, showAllLevels = TRUE)
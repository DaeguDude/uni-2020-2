# install.packages('survival')
library(survival)
data(colon)


# ----------------------------------------
# Problem 1
dim(colon)

# ----------------------------------------
# Problem 2
aggregate(nodes ~ sex, data=colon, FUN=mean)
aggregate(nodes ~ sex, data=colon, FUN=sd)
aggregate(nodes ~ sex, data=colon, FUN=median)
aggregate(nodes ~ sex, data=colon, FUN=quantile)


# ----------------------------------------
# Problem 3
# Percentage Table
prop.table(table(colon$sex, colon$differ), 2) * 100
# Count table
table(colon$sex, colon$differ)


# ----------------------------------------
# Problem 4
# https://www.dummies.com/programming/r/how-to-remove-rows-with-missing-data-in-r/

data2 <- colon[complete.cases(colon), ]
dim(data2)

# ----------------------------------------
# Problem 5
# Obs - 0, Lev & Lev+5Fu - 1
rx <- ifelse(data2$rx%in%"Lev", 1, 0)
# factorise
rx <- as.factor(rx)
# count table of 'rx'
table(rx)
# percentage table of 'rx'
prop.table(table(rx)) * 100

# ----------------------------------------
# Problem 6

# categorise
age <- ifelse(data2$age < 50, 1, ifelse(data2$age >= 70, 3, 2))
# factorise
age <- as.factor(age)
# count table of 'age'
table(age)
# percentage table of 'age'
prop.table(table(age)) * 100


# ----------------------------------------
# Create Table1
data3 <- data.frame(data2$sex, age, data2$obstruct, data2$nodes, data2$time, rx)
names(data3) <- c('sex', 'age', 'obstruct', 'nodes', 'time', 'rx')

library(tableone)







##### One-sample mean
# one sample t-test
mu0 <- 66.5
n <- 50; m <- 67.8; sdev <- 5.6
# Prob(X>67.8|H0)
sm <- (m-mu0)/(sdev/sqrt(n))
pv <- pt(sm, n-1, lower.tail=F)
cat("Stat=", sm, "\tP-value=", pv, "\n")

# one sample t-test with data
d1 <- c(66.1, 65.2, 60.5, 70.5, 65.8, 68.2, 63.3, 69.1, 66.2, 64.7)
t0 <- t.test(d1, mu=66.5); t0
t0$conf.int


###### Two-sample means
### Known population variance
sdev <- 1.6; alp <- 0.05
n1 <- 203; n2 <- 3257
m1 <- 14.6; m2 <- 14.2
se <- sdev*sqrt(1/n1+1/n2)
pv <- 2*pnorm(m2-m1, 0, se)
tstat <- abs(m1-m2)/se
cat("Stat=", tstat, "\tP-value=", pv, "\n")


### Unknown population variance - same sample variance
# With statistics
alp <- 0.05; n1 <- 203; n2 <- 3257
m1 <- 14.6; m2 <- 14.2
sd1 <- 1.7; sd2 <- 1.6
sp <- ((n1-1)*sd1^2+(n2-1)*sd2^2)/(n1+n2-2)
se <- sqrt(sp*(1/n1+1/n2))
cat("Sp^2=", sp, "\ts.e=", se, "\n")

tstat <- abs(m1-m2)/se
pv <- 2*pt(-tstat, n1+n2-2)
cat("Stat=", tstat, "\tP-value=", pv, "\n")

# With data
d1 <- c(66.1, 65.2, 60.5, 70.5, 65.8, 68.2, 63.3, 69.1, 66.2, 64.7)
d2 <- c(61.5, 63.9, 64.5, 65.2, 62.1, 59.1, 58.9, 60.9, 62.1, 59.2, 60.0)

(t1 <- t.test(d1, d2, var.equal=TRUE))
t1$stat; t1$p.val; t1$conf

df1 <- data.frame(Group=c(rep("A", length(d1)), rep("B", length(d2))), Value=c(d1, d2))
(t2 <- t.test(Value~Group, data=df1, var.equal=TRUE))



######## Baseline characteristics
## www.r-bloggers.com
setwd("folder_path")
data <- read.csv("Chap5_Table1_data.csv", header=TRUE)
head(data)

### Using R-package
# install.packages("tableone")
library(tableone)

catVars <- c("Gender", "Smoking", "Education")

# By the 'Gender' variable
listVars <- names(data)[-2] # remove group variable 'Gender'
(table2 <- CreateTableOne(listVars, data, catVars, strata=c("Gender")))
print(table2, showAllLevels = TRUE)
summary(table2)
print(table2, nonnormal="BMI")
print(table2, nonnormal="BMI", smd=TRUE)


### Using function
t.test(Age~Gender, data=data, var.equal=TRUE)
t.test(Age~Gender, data=data, var.equal=FALSE)


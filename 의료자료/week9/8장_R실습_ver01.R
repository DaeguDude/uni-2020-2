###### ANOVA test : one-way
y <- c(79,83,88,78,75, 81,89,91,84,86,82, 86,91,93,90,89, 76,81,82,79)
f <- c(rep(10,5), rep(200,6), rep(250,5), rep(300,4))
af <- as.factor(f)
ym <- tapply(y, af, mean); ym

# graph
win.graph(7,5)
boxplot(y ~ af, col=7, xlab="온도 수준", ylab="수율")
points(af, y, pch=19, col=2, cex=1.2)
lines(1:4, ym, type="b", lty=2, pch=17, col=4, cex=1.2)

# ANOVA
an1 <- aov(y ~ af)
ans1 <- summary(an1); ans1

# graph - check assumptions
win.graph(7,4); par(mfrow=c(1,2))
plot(an1, which=1:2)



###### ANOVA test : two-way
y <- c(76,79,81,79,83,85, 79,81,84,86,89,88, 87,91,91,94,88,86, 79,82,85,84,77,76)
f1 <- c(rep(100,6), rep(150,6), rep(200,6), rep(250,6))
f2 <- rep(c(1,1,2,2,3,3), 4)

temp <- as.factor(f1); pres <- as.factor(f2)

# graph
win.graph(7,5)
interaction.plot(temp, pres, y, col=c(1,2,4), lwd=2, ylab="Average of Yield")
grid(col=3)

# Using the function
an2 <- aov(y~temp * pres)
summary(an2)

# graph - check assumptions
win.graph(7,3.5); par(mfrow=c(1,2))
plot(an2, which=1:2)



###### Post-hoc analysis for one-way ANOVA
y1 <- c(79,83,88,78,75, 81,89,91,84,86,82, 86,91,93,90,89, 76,81,82,79)
f <- c(rep(10,5), rep(200,6), rep(250,5), rep(300,4))
af <- as.factor(f)

pairwise.t.test(y1, af, p.adj="bonf")
TukeyHSD(an2, "temp")
TukeyHSD(an2, "pres")


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
listVars <- names(data)[-7] # remove group variable 'Gender'
(table2 <- CreateTableOne(listVars, data, catVars, strata=c("Education")))
print(table2, showAllLevels = TRUE)
summary(table2)
print(table2, nonnormal="BMI")
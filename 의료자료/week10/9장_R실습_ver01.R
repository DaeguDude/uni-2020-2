###### Paired test
x <- c(68,61,60,68,67,64,66,67,66,67,72,74,61,71,58,77)
y <- c(56,55,67,62,59,67,50,60,59,53,60,65,62,61,64,57)

d <- x-y; mean(d); sd(d)

# paired t-test ~= one-sample t-test
t.test(x, y, paired=TRUE)$conf
t.test(d)$conf

# testing
x0 <- t.test(d)
cat(paste0("t(", x0$para, ")"), " : Stat =", round(x0$stat,4), "\tP-value=", round(x0$p.val,4), "\n")



###### Normality test
shapiro.test(x); shapiro.test(y) 

data1 <- data.frame(Group=c(rep(1, length(x)), rep(2, length(y))), Weight=c(x,y))
shapiro.test(data1[data1[,"Group"]%in%1,"Weight"])
shapiro.test(data1[data1[,"Group"]%in%2,"Weight"])



###### McNemar's test
M <- matrix(c(10,30,20,40), nrow = 2,
      dimnames = list("Pre DM" = c("Yes", "No"),
                      "Post DM" = c("Yes", "No")))
mcnemar.test(M, correct=T)
mcnemar.test(M, correct=F)
###### 1) Wilcoxon rank sum test
x<- c(7.3, 5.7, 6.0, 5.9, 7.6, 6.5)
y<- c(4.9, 7.4, 5.3, 4.6)

wilcox.test(x, y)
t.test(x, y)


###### 2) Wilcoxon signed rank test
x<- c(38,26,34,5,68,30,35,19,33,69)
y<- c(28,21,31,11,59,28,28,23,32,38)

wilcox.test(x, y, alternative = "greater", paired = TRUE)
t.test(x, y, alternative = "greater", paired = TRUE)



###### 3) Kruskal-Wallis test
x<- rep(1:4, each=5)
y<- c(4.6,10.6,8.0,25.0,7.1,2.9,10.1,3.2,3.8,6.6,6.8,9.4,26.5,12.8,8.3,3.4,3.9,6.0,8.6,5.0)

kruskal.test(y~x)
anova(lm(y ~ as.factor(x)))



##### One-sample proportion - 95% CIs
n<-200; x<-15; alp<-0.05
p <- x/n
err <- qnorm(1- alp/2)*sqrt(p*(1-p)/n)

binom.test(15,200)$conf


##### One-sample proportion - testing
n <- 200; p0 <- 0.1
x <- 15; ph <- x/n
se <- sqrt(n*p0*(1-p0))
tstat <- (x-n*p0)/se; pv <- pnorm(tstat)
cat("Norm: Stat=", tstat, "\t P-value=", pv, "\n")


##### Confidence intervals
ci <- matrix(0, nrow=100, ncol=3)
n <- 16; mu<-10; sig<-2; alp<-0.05; ir<-1:100
#
tv <- qt(1-alp/2, n-1)

set.seed(1234)
for(i in ir){
  x <- rnorm(n, mu, sig)
  xm <- mean(x)
  xs <- sd(x)
  lcl <- xm-tv*xs/sqrt(n)
  ucl <- xm+tv*xs/sqrt(n)
  ci[i,] <- c(lcl, xm, ucl)
}

# graph
win.graph(7,4)
plot(ir, ci[,2], type="p", pch=19, cex=0.6, col=1, ylim=c(min(ci), max(ci)), main="모평균에 대한 신뢰구간 모의실험", ylab="CIs", xlab="Iterations")
abline(h=mu, col=2)
arrows(ir, ci[,1], ir, ci[,3], length=0.03, code=3, angle=90, lwd=1.2, col=ifelse((ci[,1]>mu | ci[,3]<mu), 2, 4))
sum(ci[,1]>mu); sum(ci[,3]<mu)



##### Statistical power (two-sided test)
x <- seq(95, 105, by=0.02)
n <- c(10, 30, 50, 100)
mu0 <- 100; alp <- 0.05; sig <- 5
# function of power
pwr2 <- function(n, mu) pnorm(qnorm(alp/2)+n^0.5*(mu0-mu)/sig)+1-pnorm(qnorm(1-alp/2)+n^0.5*(mu0-mu)/sig)
# graph
win.graph(7,6)
plot(x, pwr2(10, x), type="n", ylim=c(0,1), ylab="power", xlab="mu")
for(i in 1:4) lines(x, pwr2(n[i], x), lty=i, lwd=2)
grid(col=3)
# depict p-values * sample size
abline(h=alp, lty=2, col=2)
text(104, alp, labels=paste0("alpha=", alp), col=2, pos=3)
text(101, pwr2(n, 101), labels=paste0("n=", n), col=2, cex=1.5)
text(98, pwr2(n, 98), labels=format(pwr2(n, 98), digits=3), col=4, cex=1.5)
text(102, pwr2(n, 102), labels=format(pwr2(n, 102), digits=3), col=4, cex=1.5)

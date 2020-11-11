##### Chapter 4 - normal distribution - example
x <- (-140:140)/20
mu <- 2; sig <- 2; a <-- 1; b <- 4;
fx <- matrix(c(dnorm(x,0,1), dnorm(x, mu, sig)), ncol=2, byrow=F)
px <- pnorm(4, mu, sig) - pnorm(-1, mu, sig); px
c <- (a-mu)/sig; d<-(b-mu)/sig
pz <- pnorm(d) - pnorm(c); pz

win.graph(7,6); par(mfrow=c(1,2), mar=c(3,4,3,1))
# Normal distribution
plot(x, fx[,2], type="n", main="N(2,4)", ylim=c(0,0.4), ylab="f(x)")
cord.x <- c(a, seq(a,b,0.01), b)
cord.y <- c(0, dnorm(seq(a,b,0.01), mu, sig), 0)
polygon(cord.x, cord.y, col="lightcyan")
text(1.5, 0.05, labels=paste0("P(",a,"<X<",b,")\n=", round(px,4)))
lines(x, fx[,2], lwd=2, col="Dark Green")

# Standardized ND
plot(x, fx[,1], type="n", main="N(0,1)", ylim=c(0,0.4), ylab="f(x)")
cord.x <- c(c, seq(c,d,0.01), d)
cord.y <- c(0, dnorm(seq(c,d,0.01)), 0)
polygon(cord.x, cord.y, col="lightcyan")
text(-0.25, 0.1, labels=paste0("P(",c,"<Z<",d,")\n=", round(pz,4)))
lines(x, fx[,1], lwd=2, col="Dark Green")


##### chi-square distribution
nu <- c(5,10,30,100)
x <- (0:100/100) %o% c(18,25,70,170)
par(mfrow=c(2,2))
for(i in 1:4){
  plot(x[,i], dchisq(x[,i], nu[i]), type="l", main=paste0("Chi-Sq(",nu[i],")"), lwd=2, col=2, ylab="f(x)")
  abline(v=nu[i], lty=2, col=4)
}

##### t-distribution
x <- (-100:100)/30
nu <- c(1,5,10,30)

win.graph(7,6)
plot(x, dnorm(x), type="l", main="N(0,1) vs. t-dist.", lwd=2, ylab="f(x)", xlab="x")
abline(v=0, lty=2, col=3)
for(i in 1:4){lines(x, dt(x, nu[i]), lty=i+1, col=2*i)}
legend(2.0, 0.35, c("N(0,1)","t(1)","t(5)","t(10)","t(30)"), col=c(1,2,4,6,8), lty=c(1,2,3,4,5), lwd=2)

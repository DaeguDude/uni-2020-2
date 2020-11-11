######## 표본평균 vs. 표준화된 표본평균 
# 표본평균
n<-10; mu<-100; sig<-10; N<-10000
# 모의실험 10,000회 반복
set.seed(1234)
xb <- zb <- xx <- ss <- NULL
for(k in 1:N) xb <- c(xb, mean(rnorm(n, mu, sig)))
# 표준화
zb <- (xb-mu)/sig*sqrt(n)
# 모집단 및 표본평균 분포함수 정의
popd <- function(x) dnorm(x, mu, sig)
smd <- function(x) dnorm(x, mu, sig/sqrt(n))

# 그래프
win.graph(7,6)
par(mfrow=c(2,1))
hist(xb, breaks=seq(from=70, to=130, length.out=50), prob=T, col=7, main="N(100,100)에서 샘플링한 10개 표본평균의 분포", ylab="f(x)")
curve(popd, 70, 130, col=4, add=T)
curve(smd, 70, 130, col=2, add=T)

# 표준화 통계량 분포
hist(zb, breaks=seq(from=-5, to=5, length.out=50), prob=T, col="cyan", main="표준화한 표본평균의 분포", ylab="f(x)")
curve(dnorm, -4, 4, col=2, add=T)

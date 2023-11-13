
#1.1
poisson <- function(l){
  exp(-l)*l**5 / factorial(5)
}

#1.2
curve(3*x^2 + x, from=0, to=10)
curve(sin(x) + cos(x), from=0, to=2*pi)
curve(0.5*sin(x) + cos(x), from=0, to=2*pi, add=T, col='red')

curve(poisson(x), from=0.1, to=20)
#about 5

#1.3
log_poisson <- function(l){
  log( exp(-l)*l**5 / factorial(5) )
}

curve(log_poisson(x), from=0.1, to=20)
#yes same value of 5 to maximise

#2.1
traffic <- rep(0:12, c(14,30,36,68,43,43,30,14,10,6,4,1,1))

hist(traffic)
#yes posson model seems good


#2.2
logLike <- function(l){
  -length(traffic)*l + sum(traffic)*log(l)
}
curve(logLike(x), from=0.1, to=20)

#2.3
optim(1, logLike, lower=0.1, upper=20, method='Brent',control=list(fnscale=-1))
#l=3.893
mean(traffic)
#xbar=3.893

#2.4
mle<-optim(1, logLike, lower=0.1, upper=20, method='Brent',control=list(fnscale=-1))$par
curve(logLike(x), from=0.1, to=20)
abline(v=mle, col='red')
abline(v=mean(traffic), col='blue')

#2.5
poisson_mle <- function(l){
  exp(-l)*l**mle / factorial(mle)
}
hist(traffic)
curve(300*poisson_mle(x), from=0.1, to=12, add=T, col='red')

#2.6
optim(1, logLike, lower=0.1, upper=20, method='Brent',control=list(fnscale=-1), hessian=T)

#2.7
obs_info<--1*optim(1, logLike, lower=0.1, upper=20, method='Brent',control=list(fnscale=-1), hessian=T)$hessian
varMLE <- 1/obs_info[1,1]

#2.8
zcrit_95 <- 1.96
ci<- c(mle-zcrit_95*sqrt(varMLE),mle+zcrit_95*sqrt(varMLE))

hist(traffic)
curve(300*poisson_mle(x), from=0.1, to=12, add=T, col='red')
abline(v=mle, col='red')
abline(v=ci[1], col='blue')
abline(v=ci[2], col='blue')

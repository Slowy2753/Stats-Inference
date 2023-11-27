library(ggplot2)

#1.1
volcano <- data.frame(Year = 2000:2018, 
                      Small = c(30, 25, 30, 32, 29, 29, 34, 29, 25, 28, 39, 30, 
                                40, 40, 41, 26, 37, 30, 34), 
                      Medium = c(6, 6, 5, 5, 8, 4, 3, 3, 7, 2, 6, 6, 4, 7, 7, 
                                 5, 4, 6, 4),
                      Large = c(0, 0, 0, 2, 1, 1, 1, 3, 2, 1, 3, 0, 1, 0, 1, 
                                0, 2, 0, 1))

#1.2
plot(volcano$Year, volcano$Small, ylim=c(0,50))

ggplot(data=volcano, aes(y=Small, x=Year))+
  geom_smooth(aes(y=Small, x=Year), col='blue')+
  geom_smooth(aes(y=Medium, x=Year), col='yellow')+
  geom_smooth(aes(y=Large, x=Year), col='red')

#1.3
hist(table(volcano$Small))
hist(table(volcano$Medium))

#2.1
lambdavals=seq(from=0, to=10, length.out=500)

plot(lambdavals, dgamma(lambdavals, shape = 1, rate = 1), type='l')

#2.2

#for gamma 1,1
#e(lambda)=1
#sd(lambda)=1

#expert
#E(lambda)=3.5
#sd(lambda)=1.25

#prior
#a=7.84
#b=2.24


cnjPrior <- dgamma(lambdavals, shape = 7.84, rate = 2.24)
cnjPrior <- cnjPrior/sum(cnjPrior)

plot(lambdavals, cnjPrior, ylim=c(0,0.02), type='l', col='red')
abline(v=3.5, col='red', lty=2)

#2.3
prior_CI <- c(qgamma(0.025, shape = 7.84, rate = 2.24), qgamma(0.975, shape = 7.84, rate = 2.24))
print(prior_CI)

#2.4
poisLike <- function(l){
  vals <- dpois(lambda=l, volcano$Medium)
  return(prod(vals))
}



#2.5
like<-sapply(lambdavals,poisLike)
like<- like/sum(like)

plot(lambdavals, cnjPrior, ylim=c(0,0.02), type='l', col='red')
abline(v=3.5, col='red', lty=2)
points(lambdavals, like, type='l', col='blue')
abline(v=, col='blue', lty=2)

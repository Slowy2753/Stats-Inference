
#2.1
x <- 0:120
pxho <- dbinom(x, size=120, p=1/6)
plot(x,pxho, type='h')

#2.2
xget20 <- x>=20
pxhoget20 <- pxho[xget20==T]
sum(pxhoget20)

#2.3
for(i in 0:121){
  if(i==0){alpha<-c()}
  xget <- x>=i
  pxhoget <- pxho[xget==T]
  alpha[i] <- sum(pxhoget)
}

#2.4
alpha<=0.05
#x=28 is the first value satisfyihng this
test<- data.frame(x, alpha)

#3.1
for(i in 0:121){
  if(i==0){power<-c()
  pxh1 <- dbinom(x, size=120, p=(1/6+0.02))}
  x1get <- x>=i
  pxh1get <- pxh1[x1get==T]
  power[i] <- sum(pxh1get)
}
test<- data.frame(x, alpha, power)

#3.2
plot(alpha, power, type='l')

#3.3
powerFun <- function(a){
    for(i in 0:121){
      if(i==0){power<-c()
      pxh1 <- dbinom(x, size=120, p=(1/6+a))}
      x1get <- x>=i
      pxh1get <- pxh1[x1get==T]
      power[i] <- sum(pxh1get)}
  return(power[28])}

#3.4
p1vals<- seq(from=0, to=5/6, length.out=100)
tPowers<-sapply(p1vals, powerFun)
plot(tPowers, p1vals, type='l')

#3.5
power1 <- function(n){
  cv<-qbinom(0.95, n, 1/6)
  for(i in 0:n){
    if(i==0){power<-c()
    pxh1 <- dbinom(x, size=n, p=(1/6+0.02))}
    x1get <- x>=i
    pxh1get <- pxh1[x1get==T]
    power[i] <- sum(pxh1get)}
  return(power[cv])
}


#3.6/3.7
ns <-1:2500
plot(ns, sapply(ns, power1), type='l')

#3.8

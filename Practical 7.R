x = c(0.8, 0.8, 1.3, 1.5, 1.8, 1.9, 1.9, 2.1, 2.6, 2.7, 2.9, 3.1, 3.2, 3.3, 3.5, 3.6, 4.0, 4.1, 4.2, 4.2, 4.3, 4.3, 4.4, 4.4, 4.6, 4.7, 4.7, 4.8, 4.9, 4.9)
summary(x)
hist(x, col = 2, main = 'Waiting times (minutes) in banks')


#3.1/3.2

l0=0.5
l1=0.6
n=length(x)

p0=dexp(x, rate=l0)
p1=dexp(x, rate=l1)

B01=prod(p0)/prod(p1)
2*log(1/B01)

#3.3
f<-function(data, l0, l1){
  p0=dexp(x, rate=l0)
  p1=dexp(x, rate=l1)
  B01=prod(p0)/prod(p1)
  return(2*log(1/B01))
}

#3.4
grid1<-seq(from=0, to=1, length.out=100)
for (i in 1:100){
  if (i==1){
    logB10<-c()
  }
  logB10<-append(logB10, f(x,0.5,grid1[i]))
}
rejectlambdas<-grid1[logB10>1]
minl1<-min(rejectlambdas)
maxl1<-max(rejectlambdas)

#3.5
plot(x=grid1,y=logB10,type='l')
abline(h=1)
abline(v=minl1)
abline(v=maxl1)

#5.1
f2<-function(data,l0,alpha,beta){
  n=length(data)
  
  numerator=l0**n *gamma(alpha) * (sum(data)+beta)**(n+alpha)
  denom=exp(l0*sum(x))* gamma(n+alpha) *beta**alpha
  
  B01<-numerator/denom
  
  return(1/(1+1/B01))
}

f2(x,0.3,2,4)

#5.2
f3<-function(data,l0,pi0,alpha,beta){
  n=length(data)
  
  numerator=l0**n *gamma(alpha) * (sum(data)+beta)**(n+alpha)
  denom=exp(l0*sum(x))* gamma(n+alpha) *beta**alpha
  
  B01<-numerator/denom
  
  return(1/(1+ (1-pi0)/pi0 * 1/B01))
}
f3(x,0.3, pi0=0.7 ,2,4)

#5.3
f2(x, 0.3, 0.25,0.5)




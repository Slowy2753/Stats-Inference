#2.1

x <- sample(c(0,1), 100, replace=TRUE, prob=c(0.74,0.26))

#2.2

sum(x)

#2.3
jurypanel <- function(){
  x <- sample(c(0,1), 100, replace=TRUE, prob=c(0.74,0.26))
  return(sum(x))
}

#2.4
Distn<-c()
for (i in 1:5000){
  Distn <- append( Distn, jurypanel() )  
}
hist(Distn)
abline(v=8)

#2.5
BDistn <- rbinom(5000, 100,0.26)
hist(BDistn)
abline(v=8)

#2.6
mean(BDistn)
var(BDistn)

#2.7
#Normal by CLT
NDistn <- rnorm(5000, 26, 19.24/10)
hist(NDistn, xlim=c(0,50))

#2.8
abline(v=8)

#2.10
BDistn <- rbinom(5000, 100,0.26)
sum(BDistn<=8)/length(BDistn)

#3.1
for (i in 1:5000){
  
  if(i==1){
    Blacks_on_Jury <- c()
  }
  
  panel <- rbinom(1,100,0.26)
  p=panel/100
  
  actual <- sample(c(0,1), 10, replace=T, prob=c(1-p,p))
  
  Blacks_on_Jury <- append(Blacks_on_Jury, sum(actual))
  }

hist(Blacks_on_Jury)

hist(rbinom(5000, 10, 0.26))

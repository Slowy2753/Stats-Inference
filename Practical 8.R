
#1.1
sample(1:6, 10, replace=T)

#1.2
rolls<-sample(1:6, 1200, replace=T)

#2.1
roll_data<-as.data.frame(table(rolls))
roll_data$Exp<-rep(1200/6,6)
roll_data$TS<- (roll_data$Freq-roll_data$Exp)**2/roll_data$Exp
Test_S<-sum(roll_data$TS)

#3.1
rollFUN<-function(){
  FUN_rolls<-sample(1:6, 1200, replace=T)
  FUN_roll_data<-as.data.frame(table(FUN_rolls))
  FUN_roll_data$Exp<-rep(1200/6,6)
  FUN_roll_data$TS<- ( FUN_roll_data$Freq- FUN_roll_data$Exp)**2/FUN_roll_data$Exp
  return(sum( FUN_roll_data$TS))
}

#3.2
rollFUN()

#3.3
roll1200<-replicate(1200, rollFUN())
hist(roll1200,freq=F, breaks=50)

#3.4
x<-seq(from=0, to=25, len=1000)
lines(x, dchisq(x,df=5))

#3.5
abline(v=Test_S,col='red')

#3.6
length(which(roll1200<Test_S))/1200
pchisq(Test_S, df=5,lower.tail = T)


#4.1
rollFUN_2<-function(probs){
  FUN_rolls<-sample(1:6, 1200, prob=probs, replace=T)
  FUN_roll_data<-as.data.frame(table(FUN_rolls))
  FUN_roll_data$Exp<-rep(1200/6,6)
  FUN_roll_data$TS<- ( FUN_roll_data$Freq- FUN_roll_data$Exp)**2/FUN_roll_data$Exp
  return(sum( FUN_roll_data$TS))
}
rollFUN_2(rep(1/6,6))


#4.2
bp <- c(1/6 + 0.02, 1/6, 1/6, 1/6, 1/6, 1/6 - 0.02)
abline(v=rollFUN_2(bp),col='blue')

#4.3
replicate(10, abline(v=rollFUN_2(bp),col='blue'))

#The blue lines tend to be more extreme--
#may suspect the die to be bias

#4.5
bp <- c(1/6 + 0.04, 1/6, 1/6, 1/6, 1/6, 1/6 - 0.04)
replicate(10, abline(v=rollFUN_2(bp),col='pink'))
qchisq(0.95, df=5)

library(stats)


#1.1
A <- c(1042, 1617,	1180,	973,	1552,	1251,	1151,	1511,	728, 1079,	951	, 1391)
B <- c(874,	389,	612,	798,	1152,	893,	541,	741,	1064,	862,	213)

summary(A)
summary(B)

#1.2
boxplot(A,B,horizontal = T)

par(mfrow=c(1,2))
hist(A)
hist(B)

#1.3
qqnorm(A)
qqline(A)
qqnorm(B)
qqline(B)

#3.1
abar<-mean(A)
sa2<-var(A)
n<-length(A)

bbar<-mean(B)
sb2<-var(B)
m<-length(B)

#3.2
sp2<- ((n-1)*sa2 + (m-1)*sb2) / (n+m-2)

#3.3
t<-(abar-bbar) / (sqrt(sp2)*sqrt(1/n + 1/m))
df<-n+m-2

#3.4
qt(0.025,df)
qt(0.975,df)

#3.5
SE<-(sqrt(sp2)*sqrt(1/n + 1/m))

#As t>2.079614 reject H0
Ci<-c((abar-bbar)-SE*qt(0.975,df),(abar-bbar)+SE*qt(0.975,df))

2*pt((abar-bbar), df,lower.tail = F)

#Copnclude the population means differ
#Equal variances- sa2 and sb2 within 10% so borderline

#4.1
t_2<- (abar-bbar) / sqrt(sa2/n + sb2/m)
df_2=min(n,m)-1

abs(t_2)>qt(0.975,df_2)
#so we still reject the null under unequal sa2 and sb2


#5.1
t.test(A,B,var.equal=TRUE)

t.test(A,B,var.equal=F)



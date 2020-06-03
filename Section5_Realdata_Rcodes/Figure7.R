####################Figure 7################
rm(list=ls())
load("D:/zhou/pm25R/T4Data.RData")
time<-result$V1
time<-as.character(time)
time<-as.Date(time)
resultTable<-c
nday<-2191
q1<-Q[1]
sig<-285
al<-2.329384
for(i in 1:(nday-1)){
  sig<-c(sig,exp((resultTable[2])+resultTable[3]*log(sig[i])+resultTable[4]*exp(-(resultTable[5])*Q[i])))
  q1<-c(q1,(resultTable[1])+sig[(i+1)]*rweibull(1,1,1)^(1/resultTable[6]))
}
qqplot(q1,Q,xlab="",ylab="")#Figure 7(a)
lines(c(0,2000),c(0,2000))
plot(time,q1,xlab="",ylab="",type="l",col="blue")#Figure 7(b)
lines(time,Q,col="red")
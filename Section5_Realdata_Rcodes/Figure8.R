
#################Figure 8#################
rm(list=ls())
library(evir)
result<-read.table("D:/zhou/pm25R/maxvalue2.txt",sep="\t",header=F) #read data
Q<-result$V2
Q[366]<-583
Q[1704]<-100
Q[1765]<-151
minq<-min(Q)
n<-2191
nday<-2191
f<-function(param){# Log-likelihood function for ACF
  Qmq<-Q[1:nday]-((minq+500)/(1+exp(param[1]))-500)
  f_21<-105
  f_31<-10/(1+exp(param[6]))
  for(i in 1:(nday-1)){
    f_21<-c(f_21,exp((20/(1+exp(param[2]))-10)+(2/(1+exp(param[3]))-1)*log(f_21[i])+(10/(1+exp(param[4]))-5)*exp(-(1/(1+exp(param[5])))*Q[i]))) 
 }
  ll<-(-1)*(nday*log(f_31)-f_31*sum(log(Qmq/f_21))-sum(log(Qmq))-sum((Qmq/f_21)^(-f_31)))
  ll}  

l1<-matrix(nrow=3000,ncol=6) #3000start points to estimate parameters in ACF model
for(i in 1:3000){
  l1[i,]<-c(runif(1,min=-3,max=3),runif(1,min=-3,max=3),runif(1,min=-3,max=3),runif(1,min=-3,max=3),runif(1,min=-3,max=3),runif(1,min=-3,max=3))#,runif(1,min=-3,max=3),runif(1,min=-3,max=3),runif(1,min=-3,max=3))
}
nl<-rep(NULL,3000)
l2<-matrix(nrow=3000,ncol=6)
for(v in 1:3000)
{
  print(v)
  est1<-try(nlm(f,l1[v,],iterlim=200),silent=TRUE)
  if ('try-error' %in% class(est1)) {
    nl[v]<-100000 
    next}
  else{l2[v,]<-est1$estimate
  nl[v]<-est1$minimum
  }
}
min(nl)
wm<-which.min(nl)
wm1<-which(nl<min(nl)+0.5)
resultF9<-c(((minq+500)/(1+exp(l2[wm,1]))-500),20/(1+exp(l2[wm,2]))-10,2/(1+exp(l2[wm,3]))-1,10/(1+exp(l2[wm,4]))-5,1/(1+exp(l2[wm,5])),10/(1+exp(l2[wm,6])))#,2/(1+exp(l2[wm,7]))-1,700/(1+exp(l2[wm,8]))-350,1/(1+exp(l2[wm,9])))

#
#

#install.packages("VGAM")
library(VGAM)
#One simulation sequence of ACF
q1<-Q[1]
sig<-105
al<-resultF9
for(i in 1:(nday-1)){
  sig<-c(sig,exp(( resultF9[2])+resultF9[3] *log(sig[i])+resultF9[4] *exp(-(resultF9[5]*Q[i]))))
  q1<-c(q1,(resultF9[1])+sig[(i+1)]*rfrechet(1,0,1,1)^(1/resultF9[6]))
}

#One simulation sequence of DCW
resultTable<-c( 32.183382390,  4.885210577,  0.246124918, -2.194786534,  0.004613078,  2.320488595)
q2<-Q[1]
sig<-285
al<-resultTable[6]
for(i in 1:(nday-1)){
  sig<-c(sig,exp((resultTable[2])+resultTable[3]*log(sig[i])+resultTable[4]*exp(-(resultTable[5])*Q[i])))
  q2<-c(q2,(resultTable[1])+sig[(i+1)]*rweibull(1,1,1)^(1/resultTable[6]))
} 
hist(Q,breaks=seq(0,1600,10),main="",xlab="",ylab="",freq=F) #Figure 8(a)
lines(density(q2),col="red")
lines(density(q1),col="blue")

qqplot(q1,Q,xlab="",ylab="") #Figure 8(b)
lines(c(0,2000),c(0,2000)) 

save.image("D:/zhou/pm25R/finalCode/F9Data.RData")
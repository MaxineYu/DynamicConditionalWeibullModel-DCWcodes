##################Figure 6################# 
#test the MLE(without considering weather factors)
rm(list=ls())
result<-read.table("D:/zhou/pm25R/maxvalue2.txt",sep="\t",header=FALSE) #read weather data
time<-result$V1
time<-as.character(time)
time<-as.Date(time)
Q<-result$V2
Q[366]<-583
Q[1704]<-100
Q[1765]<-151
minq<-min(Q)
n<-2191
nday<-2191
f<-function(param){#conditional log-likelihood function
  Qmq<-Q[1:n]-minq/(1+exp(param[1]))
  f_21<-285
  f_31<-2.5
  for(i in 1:n-1){
    f_21<-c(f_21,exp(10/(1+exp(param[2]))+(2/(1+exp(param[3]))-1)*log(f_21[i])+(10/(1+exp(param[4]))-5)*exp(-(1/(1+exp(param[5])))*Q[i]))) 
    f_31<-c(f_31,exp((5/(1+exp(param[6]))-1)+(1/(1+exp(param[7]))-0.5)*log(f_31[i])+(700/(1+exp(param[8]))-350)*exp(-(1/(1+exp(param[9]))-0.5)*Q[i])))
  }
  ll<-(-1)*(sum(log(f_31))-sum(log(f_21))+sum(log(Qmq/f_21)*(f_31-1))-sum((Qmq/f_21)^(f_31)))
  ll}  

l1<-matrix(nrow=3000,ncol=9)#3000 start points 
for(i in 1:3000){
  l1[i,]<-c(runif(1,min=-3,max=3),runif(1,min=-3,max=3),runif(1,min=-3,max=3),runif(1,min=-3,max=3),runif(1,min=0,max=5),runif(1,min=-3,max=3),runif(1,min=-3,max=3),runif(1,min=-3,max=3),runif(1,min=-3,max=3))
}
nl<-rep(NULL,3000)
l2<-matrix(nrow=3000,ncol=9)

Nsample=3000
Nparam=9


library(doParallel)
library(foreach)
cl <- makeCluster(15) 
registerDoParallel(cl) 

estData <- foreach(v=1:Nsample) %dopar% {#estimate parameters
  est<-try(nlm(f,l1[v,],iterlim=200),silent=TRUE)
  if ('try-error' %in% class(est)) {
    return(rep(1e10, Nparam*2+1))}
  else{
    return(c(est$minimum,est$estimate,est$gradient))
  }
  
}
stopCluster(cl)
estData=t(matrix(unlist(estData),Nparam*2+1,Nsample))
nl<-estData[,1]
l2<-estData[,2:(Nparam+1)]
S3<-estData[,(Nparam+2):(Nparam*2+1)]




min(nl) #the best one is select
wm<-which.min(nl)
wm1<-which(nl<min(nl)+0.5)
resultF7<-c(minq/(1+exp(l2[wm,1])),10/(1+exp(l2[wm,2])),2/(1+exp(l2[wm,3]))-1,10/(1+exp(l2[wm,4]))-5,1/(1+exp(l2[wm,5])),5/(1+exp(l2[wm,6]))-1,1/(1+exp(l2[wm,7]))-0.5,700/(1+exp(l2[wm,8]))-350,1/(1+exp(l2[wm,9]))-0.5)
rexultF7_2<-c(minq/(1+exp(l2[wm1,1])),10/(1+exp(l2[wm1,2])),2/(1+exp(l2[wm1,3]))-1,10/(1+exp(l2[wm1,4]))-5,1/(1+exp(l2[wm1,5])),5/(1+exp(l2[wm1,6]))-1,1/(1+exp(l2[wm1,7]))-0.5,700/(1+exp(l2[wm1,8]))-350,1/(1+exp(l2[wm1,9]))-0.5)
resultF7
rexultF7_2

q1<-Q[1]
sig<-285
al<-0.5
for(i in 1:n-1){
  sig<-c(sig,exp((resultF7[2])+resultF7[3]*log(sig[i])+resultF7[4]*exp(-(resultF7[5])*Q[i])))
  al<-c(al,exp((resultF7[6])+resultF7[7]*log(al[i])+resultF7[8]*exp(-(resultF7[9])*Q[i])))
  q1<-c(q1,( resultF7[1])+sig[(i+1)]*rweibull(1,1,1)^(1/al[i+1]))
}
plot(time,sig,xlab="",ylab="",type="l") #Figure 7(a)
plot(time,al,xlab="",ylab="",type="l") #Figure 7(b)

lines(c(time[1],time[length(time)]),c(2.398198221,2.398198221),col="red")


save.image("D:/zhou/pm25R/F7Data.RData")
#test the MLE
library(evir)
result<-read.table("D:/zhou/pm25R/maxvalue2.txt",sep="\t",header=FALSE) #read smog data
Q<-result$V2
Q[366]<-583 #average of several days around day 336
Q[1704]<-100
Q[1765]<-151
minq<-min(Q)
nday<-2191
f<-function(param){#likelihood function
  Qmq<-Q[1:nday]-minq/(1+exp(param[1]))
  f_21<-285
  f_31<-5/(1+exp(param[6]))
  for(i in 1:(nday-1)){
    f_21<-c(f_21,exp(10/(1+exp(param[2]))+(2/(1+exp(param[3]))-1)*log(f_21[i])+(10/(1+exp(param[4]))-5)*exp(-(1/(1+exp(param[5])))*Q[i])))
  }
  ll<-(-1)*(nday*log(f_31)-sum(log(f_21))+sum(log(Qmq/f_21))*(f_31-1)-sum((Qmq/f_21)^(f_31)))
  ll}  

l1<-matrix(nrow=1000,ncol=6) #parameter estimation
for(i in 1:1000){
  l1[i,]<-c(runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=0,max=2),runif(1,min=-2,max=2))
}
nl<-rep(NULL,1000)
l2<-matrix(nrow=1000,ncol=6)
for(v in 1:1000)
{
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
c<-c(minq/(1+exp(l2[wm,1])),10/(1+exp(l2[wm,2])),2/(1+exp(l2[wm,3]))-1,10/(1+exp(l2[wm,4]))-5,1/(1+exp(l2[wm,5])),5/(1+exp(l2[wm,6])))

t1<-c#estimation result
f1<-function(param){
  Qmq<-Q[1:nday]-param[1]
  f_21<-285
  f_31<-param[6]
  for(i in 1:(nday-1)){
    f_21<-c(f_21,exp(param[2]+param[3]*log(f_21[i])+param[4]*exp(-(param[5])*Q[i])))
  }
  ll<-(-1)*(nday*log(f_31)-sum(log(f_21))+sum(log(Qmq/f_21))*(f_31-1)-sum((Qmq/f_21)^(f_31)))
  ll}  
f1(t1)
I11<-matrix(ncol=6,nrow=6)
for(i in 1:6){
  I11[i,]<-rep(0,6)
}
diag(I11)<-rep(c(1e-5,1e-6,1e-7,1e-6,1e-9,1e-6)) 

Ix<-matrix(ncol=6,nrow=6) #compute standard error
for(j in 1:6){
  for(i in 1:6){
    Ix[j,i]<-(f1(I11[i,]+t1+I11[j,])-f1(t1-I11[i,]+I11[j,])-f1(t1+I11[i,]-I11[j,])+f1(t1-I11[i,]-I11[j,]))*(1/(I11[i,i]*I11[j,j]))
  }
}
Ix6<-solve(Ix)
sqrt(diag(Ix6))


save.image("D:/zhou/pm25R/T4Data.RData")
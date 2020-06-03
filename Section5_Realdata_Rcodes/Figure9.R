

###############Figure 9#################
rm(list=ls())
result<-read.table("D:/zhou/pm25R/maxvalue2.txt",sep="\t",header=FALSE) #read smog data
Q<-result$V2
Q[366]<-583
Q[1704]<-100
Q[1765]<-151
minq<-min(Q)
nday<-2191
n<-2191
time<-result$V1
time<-as.character(time)
time<-as.Date(time)
w456<-read.table("D:/zhou/pm25R/weather.txt",sep="\t",header=FALSE) #read weather data year 2014-2016
w789<-read.table("D:/zhou/pm25R/weather789.txt",sep="\t",header=FALSE) #weather data year 2017-2019
w456789=rbind(w456,w789)
w456789<-w456789[,-1]
names(w456789)<-c("time","temp","humi","level","dirc","area")

sp<-split(w456789,w456789[,c("time")],drop=TRUE)
temp<-sapply(sp,FUN=function(x) max(x$temp))# extract the maximum of tempture data
names(temp)<-NULL
humi<-sapply(sp,FUN=function(x) min(x$humi))#extract the minimum of humidity data
names(humi)<-NULL

getmode <- function(v) {#This is a function to extract the mode
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

windlevel<-sapply(sp,FUN=function(x) {uniqv <-unique(x$level)#extract the mode of windlevel
uniqv[which.max(tabulate(match(x$level, uniqv)))]})
names(windlevel)<-NULL


windir<-sapply(sp,FUN=function(x) {uniqv <- unique(x$dirc)# extract the mode of winddirection
uniqv[which.max(tabulate(match(x$dirc, uniqv)))]})
names(windir)<-NULL
windir[411]<-3#This value was NA originall, I assigned it as 3 because of periodicity
windir[1772]<-windir[1772-365]
windir[1778]<-windir[1778-365]
windir[1779]<-windir[1779-365]
windir[1784]<-windir[1784-365]
windir[1787]<-windir[1787-365]
windir[1790]<-windir[1790-365]
windir[1791]<-windir[1791-365]
windir[1796]<-windir[1796-365]
windir[1800]<-windir[1800-365]
windir[1804]<-windir[1804-365]
windir[1813]<-windir[1813-365]
windir[1815]<-windir[1815-365]
windir[1825]<-windir[1825-365]
windir[1826]<-windir[1826-365]
windir[1828]<-windir[1828-365]
windir[1832]<-windir[1832-365]
windir[1839]<-windir[1836-365]
windir[1843]<-windir[1843-365]
windir[1849]<-windir[1849-365]
windir[1865]<-windir[1865-365]
windir[1871]<-windir[1871-365]
windir[1879]<-windir[1879-365]
windir[1881]<-windir[1881-365]
windir[1895]<-windir[1895-365]
windir[1905]<-windir[1905-365]
windir[2158]<-windir[2158-365]
windir[2117]<-windir[2116]

g<-matrix(nrow=nday,ncol=7)#construct the dummy variables
for(i in 1:nday){
  if(windir[i]==1){
    g[i,]<-c(1,0,0,0,0,0,0)
  }
  if(windir[i]==2){
    g[i,]<-c(0,1,0,0,0,0,0)
  }
  if(windir[i]==3){
    g[i,]<-c(0,0,1,0,0,0,0)
  }
  if(windir[i]==4){
    g[i,]<-c(0,0,0,1,0,0,0)
  }
  if(windir[i]==5){
    g[i,]<-c(0,0,0,0,1,0,0)
  }
  if(windir[i]==6){
    g[i,]<-c(0,0,0,0,0,1,0)
  }
  if(windir[i]==7){
    g[i,]<-c(0,0,0,0,0,0,1)
  }
  if(windir[i]==8){
    g[i,]<-c(0,0,0,0,0,0,0)
  }
  
}
x1<-g[,1]
x2<-g[,2]
x3<-g[,3]
x4<-g[,4]
x5<-g[,5]
x6<-g[,6]
x7<-g[,7]

Qs<-Q

f<-function(param){
  Qmq<-Qs[1:n]-minq/(1+exp(param[1]))
  f_21<-316.98363
  f_31<-2.5
  for(i in 1:(n-1)){
    f_21<-c(f_21,exp(10/(1+exp(param[2]))+(2/(1+exp(param[3]))-1)*log(f_21[i])+(10/(1+exp(param[4]))-5)*exp(-(1/(1+exp(param[5])))*Qs[i]+(1.1/(1+exp(param[6]))-1)*temp[i]+(1.1/(1+exp(param[7]))-1)*humi[i]+(1.5/(1+exp(param[8]))-1)*windlevel[i]+(2/(1+exp(param[9]))-1)*x1[i]+(2/(1+exp(param[10]))-1)*x2[i]+(2/(1+exp(param[11]))-1)*x3[i]+(2/(1+exp(param[12]))-1)*x4[i]+(2/(1+exp(param[13]))-1)*x5[i]+(2/(1+exp(param[14]))-1)*x6[i]+(2/(1+exp(param[15]))-1)*x7[i])))
    f_31<-c(f_31,exp(5/(1+exp(param[16]))+(1.98/(1+exp(param[17]))-0.99)*log(f_31[i])+(700/(1+exp(param[18]))-350)*exp(-(1/(1+exp(param[19])))*Qs[i])))
  }
  ll<-(-1)*(sum(log(f_31))-sum(log(f_21))+sum(log(Qmq/f_21)*(f_31-1))-sum((Qmq/f_21)^(f_31)))
  ll
}

Nsample<-2000
Nparam<-19
library(doParallel)
library(foreach)
cl <- makeCluster(15) 
registerDoParallel(cl) 

l1<-matrix(nrow=Nsample,ncol=19)
set.seed(1230)
for(i in 1:Nsample){
  l1[i,]<-c(runif(1,min=-3,max=3),runif(1,min=-2,max=2),runif(1,min=-3,max=3),runif(1,min=-3,max=3),runif(1,min=-3,max=3),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2))
}
nl<-rep(NULL,Nsample)
l2<-matrix(nrow=Nsample,ncol=19)

estData <- foreach(v=1:Nsample) %dopar% {
  est<-try(nlm(f,l1[v,],iterlim=400),silent=TRUE)
  if ('try-error' %in% class(est)) {
    return(rep(1e6, Nparam*2+1))}
  else{
    return(c(est$minimum,est$estimate,est$gradient))
  }
  
}
stopCluster(cl)
estData=t(matrix(unlist(estData),Nparam*2+1,Nsample))
nl<-estData[,1]
l2<-estData[,2:(Nparam+1)]

# 
# for(v in 1:3000)
# {
#   print(v)
#   est1<-try(nlm(f,l1[v,],iterlim=400),silent=TRUE)
#   if ('try-error' %in% class(est1)) {
#     nl[v]<-100000 
#     next}
#   else{l2[v,]<-est1$estimate
#   nl[v]<-est1$minimum
#   }
# }
min(nl)
wm<-which.min(nl)
wm1<-which(nl<min(nl)+0.5)
resultsF10_1<-c(minq/(1+exp(l2[wm,1])),10/(1+exp(l2[wm,2])),2/(1+exp(l2[wm,3]))-1,10/(1+exp(l2[wm,4]))-5,1/(1+exp(l2[wm,5])),(1.1/(1+exp(l2[wm,6]))-1),(1.1/(1+exp(l2[wm,7]))-1),(1.5/(1+exp(l2[wm,8]))-1),(2/(1+exp(l2[wm,9]))-1),(2/(1+exp(l2[wm,10]))-1),(2/(1+exp(l2[wm,11]))-1),(2/(1+exp(l2[wm,12]))-1),(2/(1+exp(l2[wm,13]))-1),(2/(1+exp(l2[wm,14]))-1),(2/(1+exp(l2[wm,15]))-1),5/(1+exp(l2[wm,16])),1.98/(1+exp(l2[wm,17]))-0.99,700/(1+exp(l2[wm,18]))-350,1/(1+exp(l2[wm,19])))
resultsF10_2<-c(minq/(1+exp(l2[wm1,1])),10/(1+exp(l2[wm1,2])),2/(1+exp(l2[wm1,3]))-1,10/(1+exp(l2[wm1,4]))-5,1/(1+exp(l2[wm1,5])),(1.1/(1+exp(l2[wm1,6]))-1),(1.1/(1+exp(l2[wm1,7]))-1),(1.5/(1+exp(l2[wm1,8]))-1),(2/(1+exp(l2[wm1,9]))-1),(2/(1+exp(l2[wm1,10]))-1),(2/(1+exp(l2[wm1,11]))-1),(2/(1+exp(l2[wm1,12]))-1),(2/(1+exp(l2[wm1,13]))-1),(2/(1+exp(l2[wm1,14]))-1),(2/(1+exp(l2[wm1,15]))-1),5/(1+exp(l2[wm1,16])),1.98/(1+exp(l2[wm1,17]))-0.99,700/(1+exp(l2[wm1,18]))-350,1/(1+exp(l2[wm1,19])))
resultsF10<-resultsF10_1

# for(k in 1:19)  resultsF10[k]<-resultsF10_2[2*k-1]

#One Simulation sequence after considering weather:
sig<-316.98363
al<-2.5
q1<-Q[1]
for(j in 1:(nday-1)){
  sig<-c(sig,exp((resultsF10[2])+resultsF10[3]*log(sig[j])+resultsF10[4]*exp(-(resultsF10[5])*Q[j]+resultsF10[6]*temp[j]+resultsF10[7]*humi[j]+resultsF10[8]*windlevel[j]+resultsF10[9]*x1[j]+resultsF10[10]*x2[j]+resultsF10[11]*x3[j]+resultsF10[12]*x4[j]+resultsF10[13]*x5[j]+resultsF10[14]*x6[j]+resultsF10[15]*x7[j])))
  al<-c(al,exp(resultsF10[16]+resultsF10[17]*log(al[j])+resultsF10[18]*exp(-resultsF10[19]*Q[j])))
  q1<-c(q1,(resultsF10[1])+sig[(j+1)]*rweibull(1,1,1)^(1/al[(j+1)]))
}
plot(time,sig,xlab="",ylab="",type="l") #Figure 10(a)
plot(time,al,xlab="", ylab="",type="l") #Figure 10(b)
save.image("D:/zhou/pm25R/F10data.RData")
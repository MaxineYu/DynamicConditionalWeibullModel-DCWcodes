############################################table 5##########################################################
rm(list=ls())

# install.packages('doParallel')
# instal0.4l.packages('foreach')
library(doParallel)
library(foreach)
cl <- makeCluster(8) 
registerDoParallel(cl) 

nday<-2191
n<-2191
w456<-read.table("D:/zhou/pm25R/weather.txt",sep="\t",header=FALSE)
w789<-read.table("D:/zhou/pm25R/weather789.txt",sep="\t",header=FALSE)
w456789=rbind(w456,w789)
w456789<-w456789[,c(-1,-7)] #delete the first column
names(w456789)<-c("time","temp","humi","level","dirc")


sp<-split(w456789,w456789[,c("time")],drop=TRUE)
temp<-sapply(sp,FUN=function(x) max(x$temp))# extract the maximum of tempture data
names(temp)<-NULL
#temp1<-sapply(sp,FUN=function(x) min(x$temp))
humi<-sapply(sp,FUN=function(x) min(x$humi))#extract the minimum of humidity data
names(humi)<-NULL
#humi1<-sapply(sp,FUN=function(x) max(x$humi))

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


result<-read.table("D:/zhou/pm25R/maxvalue2.txt",sep="\t",header=FALSE)
Q<-result$V2
Q[366]<-583
Q[1704]<-100
Q[1765]<-151
Qs<-Q
minq<-min(Qs)-0.01

f<-function(param){ #likelihood function
  Qmq<-Qs[1:n]-minq/(1+exp(param[1]))
  f_21<-316.98363
  f_31<-5/(1+exp(param[16]))
  for(i in 1:(n-1)){
    f_21<-c(f_21,exp(10/(1+exp(param[2]))+(2/(1+exp(param[3]))-1)*log(f_21[i])+(10/(1+exp(param[4]))-5)*exp(-(1/(1+exp(param[5])))*Qs[i]+(1.1/(1+exp(param[6]))-1)*temp[i]+(1.1/(1+exp(param[7]))-1)*humi[i]+(1.5/(1+exp(param[8]))-1)*windlevel[i]+(2/(1+exp(param[9]))-1)*x1[i]+(2/(1+exp(param[10]))-1)*x2[i]+(2/(1+exp(param[11]))-1)*x3[i]+(2/(1+exp(param[12]))-1)*x4[i]+(2/(1+exp(param[13]))-1)*x5[i]+(2/(1+exp(param[14]))-1)*x6[i]+(2/(1+exp(param[15]))-1)*x7[i])))
  }
  ll<-(-1)*(n*log(f_31)-sum(log(f_21))+sum(log(Qmq/f_21))*(f_31-1)-sum((Qmq/f_21)^(f_31)))
  ll
}

Nsample<-4000
Nparam<-16
l1<-matrix(nrow=Nsample,ncol=Nparam)  #1000 start points 
for(i in 1:Nsample){
  l1[i,]<-c(runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2))
}
rt<-rep(1000000,Nsample)
nl<-rt
l2<-matrix(nrow=Nsample,ncol=Nparam)
######@@@@@@@@@
estData <- foreach(v=1:Nsample) %dopar% { #estimation
  est<-try(nlm(f,l1[v,],iterlim=400),silent=TRUE)
  if ('try-error' %in% class(est)) {
    return(rep(1e10, Nparam*2+1))}
  else
    return(c(est$minimum,est$estimate,est$gradient))
}
estData=t(matrix(unlist(estData),Nparam*2+1,Nsample))
nl<-estData[,1]
l2<-estData[,2:(Nparam+1)]
S3<-estData[,(Nparam+2):(Nparam*2+1)]


min(nl)
wm<-which.min(nl)
t2<-c(minq/(1+exp(l2[wm,1])),10/(1+exp(l2[wm,2])),2/(1+exp(l2[wm,3]))-1,10/(1+exp(l2[wm,4]))-5,1/(1+exp(l2[wm,5])),(1.1/(1+exp(l2[wm,6]))-1),(1.1/(1+exp(l2[wm,7]))-1),(1.5/(1+exp(l2[wm,8]))-1),(2/(1+exp(l2[wm,9]))-1),(2/(1+exp(l2[wm,10]))-1),(2/(1+exp(l2[wm,11]))-1),(2/(1+exp(l2[wm,12]))-1),(2/(1+exp(l2[wm,13]))-1),(2/(1+exp(l2[wm,14]))-1),(2/(1+exp(l2[wm,15]))-1),5/(1+exp(l2[wm,16])))
#t2 is the estimated parameters

f<-function(param){
  Qmq<-Qs[1:n]-minq/(1+exp(param[1]))
  f_21<-316.98363
  f_31<-5/(1+exp(param[16]))
  for(i in 1:(n-1)){
    f_21<-c(f_21,exp(10/(1+exp(param[2]))+(2/(1+exp(param[3]))-1)*log(f_21[i])+(10/(1+exp(param[4]))-5)*exp(-(1/(1+exp(param[5])))*Qs[i]+(1.1/(1+exp(param[6]))-1)*temp[i]+(1.1/(1+exp(param[7]))-1)*humi[i]+(1.5/(1+exp(param[8]))-1)*windlevel[i]+(2/(1+exp(param[9]))-1)*x1[i]+(2/(1+exp(param[10]))-1)*x2[i]+(2/(1+exp(param[11]))-1)*x3[i]+(2/(1+exp(param[12]))-1)*x4[i]+(2/(1+exp(param[13]))-1)*x5[i]+(2/(1+exp(param[14]))-1)*x6[i]+(2/(1+exp(param[15]))-1)*x7[i])))
  }
  ll<-(-1)*(n*log(f_31)-sum(log(f_21))+sum(log(Qmq/f_21))*(f_31-1)-sum((Qmq/f_21)^(f_31)))
  ll
}
f<-function(param){
  Qmq<-Qs[1:n]-param[1]
  f_21<-316.98363
  f_31<-param[16]
  for(i in 1:(n-1)){
    f_21<-c(f_21,exp(param[2]+param[3]*log(f_21[i])+param[4]*exp(-param[5]*Qs[i]+param[6]*temp[i]+param[7]*humi[i]+param[8]*windlevel[i]+param[9]*x1[i]+param[10]*x2[i]+param[11]*x3[i]+param[12]*x4[i]+param[13]*x5[i]+param[14]*x6[i]+param[15]*x7[i])))
  }
  ll<-(-1)*(n*log(f_31)-sum(log(f_21))+sum(log(Qmq/f_21))*(f_31-1)-sum((Qmq/f_21)^(f_31)))
  ll
}
f(t2)
I11<-matrix(ncol=16,nrow=16)
for(i in 1:16){
  I11[i,]<-rep(0,16)
}
diag(I11)<-rep(c(1e-5,1e-6,1e-7,1e-6,1e-9,1e-8,1e-9,1e-07,1e-07,1e-07,1e-07,1e-07,1e-08,1e-07,1e-07,1e-06)) 

Ix<-matrix(ncol=16,nrow=16) #standard error
for(j in 1:16){
  for(i in 1:16){
    Ix[j,i]<-(f(I11[i,]+t2+I11[j,])-f(t2-I11[i,]+I11[j,])-f(t2+I11[i,]-I11[j,])+f(t2-I11[i,]-I11[j,]))*(1/(I11[i,i]*I11[j,j]))
  }
}
Ix6<-solve(Ix)
sqrt(diag(Ix6))
save.image("D:/zhou/pm25R/T5Data.RData")

stopCluster(cl)
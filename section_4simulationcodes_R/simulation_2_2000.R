w456<-read.table("weather.txt",sep="\t",header=FALSE)         #read the weather data in Disk C
w456<-w456[,c(-1,-7)] #delete the first column
names(w456)<-c("time","temp","humi","level","dirc")


sp<-split(w456,w456[,c("time")],drop=TRUE)
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

g<-matrix(nrow=1096,ncol=7)#construct the dummy variables
for(i in 1:1096){
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


for(i in 1:15){
  temp1<-round(temp[(length(temp)-364):length(temp)]+rnorm(365,mean=0,sd=1))
  temp<-c(temp,temp1)}

for(i in 1:15){
  humi1<-round(humi[(length(humi)-364):length(humi)]+rnorm(365,mean=0,sd=1))
  humi<-c(humi,humi1)
}

windlevel<-rep(windlevel,5)
x1<-rep(x1,5)
x2<-rep(x2,5)
x3<-rep(x3,5)
x4<-rep(x4,5)
x5<-rep(x5,5)
x6<-rep(x6,5)
x7<-rep(x7,5)



f<-function(param){ #log-likelihood function
  Qmq<-Qs[1:n]-minq/(1+exp(param[1]))
  f_21<-316.98363
  f_31<-5/(1+exp(param[16]))
  for(i in 1:(n-1)){
    f_21<-c(f_21,exp(10/(1+exp(param[2]))+(2/(1+exp(param[3]))-1)*log(f_21[i])+(10/(1+exp(param[4]))-5)*exp(-(1/(1+exp(param[5])))*Qs[i]+(1.1/(1+exp(param[6]))-1)*temp[i]+(1.1/(1+exp(param[7]))-1)*humi[i]+(1.5/(1+exp(param[8]))-1)*windlevel[i]+(2/(1+exp(param[9]))-1)*x1[i]+(2/(1+exp(param[10]))-1)*x2[i]+(2/(1+exp(param[11]))-1)*x3[i]+(2/(1+exp(param[12]))-1)*x4[i]+(2/(1+exp(param[13]))-1)*x5[i]+(2/(1+exp(param[14]))-1)*x6[i]+(2/(1+exp(param[15]))-1)*x7[i])))
  }
  ll<-(-1)*(n*log(f_31)-sum(log(f_21))+sum(log(Qmq/f_21))*(f_31-1)-sum((Qmq/f_21)^(f_31)))
  ll
}

simu<-matrix(nrow=500,ncol=2000)
for(i in 1:500){
  sig<-316.98363
  al<-2.570708e+00
  q1<-46.236507891+316.98363*rweibull(1,1,1)^(1/2.571733591)
  for(j in 1:1999){
    sig<-c(sig,exp((6.010081264)+0.125717023*log(sig[j])-1.436611647*exp(-(0.002237116)*q1[j]+0.009282694*temp[j]+0.002622494*humi[j]+0.102128852*windlevel[j]+0.007165176*x1[j]-0.148789068*x2[j]-0.104239352*x3[j]-0.088352830*x4[j]-0.041896947*x5[j]+0.019924528*x6[j]+0.045767338*x7[j])))
    q1<-c(q1,(46.236507891)+sig[(j+1)]*rweibull(1,1,1)^(1/2.571733591))
  }
  simu[i,]<-q1
}


rt<-rep(1000000,50)
nl1<-numeric()
mat1<-matrix(nrow=500,ncol=16)
for(h in 1:500){
  Qs<-simu[h,]
  minq<-min(Qs)-0.01
  n<-2000
  l1<-matrix(nrow=50,ncol=16) #50 start points
  for(i in 1:50){
    l1[i,]<-c(runif(1,min=-3,max=3),runif(1,min=-1,max=1),runif(1,min=-1,max=1),runif(1,min=-1,max=1),runif(1,min=4,max=8),runif(1,min=-3,max=1),runif(1,min=-3,max=1),runif(1,min=-1,max=1),runif(1,min=-1,max=1),runif(1,min=-1,max=1),runif(1,min=-1,max=1),runif(1,min=-1,max=1),runif(1,min=-1,max=1),runif(1,min=-1,max=1),runif(1,min=-1,max=1),runif(1,min=-1,max=1))#,runif(1,min=-2,max=2),runif(1,min=-2,max=2),runif(1,min=-2,max=2))
  }
  nl<-rt
  l2<-matrix(nrow=50,ncol=16)
  for(v in 1:50)
  {
    est<-try(nlm(f,l1[v,],iterlim=400),silent=TRUE)
    if ('try-error' %in% class(est)) {
      next}
    else{
      l2[v,]<-est$estimate
      nl[v]<-est$minimum}
  }
  print(min(nl))
  print(sum(nl<min(nl)+0.001))
  nl1<-c(nl1,min(nl))
  print(l2[which.min(nl),])
  l3<-c((minq/(1+exp(l2[which.min(nl),1]))),10/(1+exp(l2[which.min(nl),2])),2/(1+exp(l2[which.min(nl),3]))-1,10/(1+exp(l2[which.min(nl),4]))-5,1/(1+exp(l2[which.min(nl),5])),1.1/(1+exp(l2[which.min(nl),6]))-1,1.1/(1+exp(l2[which.min(nl),7]))-1,1.5/(1+exp(l2[which.min(nl),8]))-1,2/(1+exp(l2[which.min(nl),9]))-1,2/(1+exp(l2[which.min(nl),10]))-1,2/(1+exp(l2[which.min(nl),11]))-1,2/(1+exp(l2[which.min(nl),12]))-1,2/(1+exp(l2[which.min(nl),13]))-1,2/(1+exp(l2[which.min(nl),14]))-1,2/(1+exp(l2[which.min(nl),15]))-1,5/(1+exp(l2[which.min(nl),16])))
  print(l3)
  mat1[h,]<-l3
}
asd1<-apply(mat1,2,mean)
asd2<-apply(mat1,2,sd)
mat<-rbind(mat1,asd1,asd2)
write.table(mat1,"simulation2_2000.xls")

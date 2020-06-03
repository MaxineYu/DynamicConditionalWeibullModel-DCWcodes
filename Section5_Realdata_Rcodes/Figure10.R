rm(list=ls())
load("D:/zhou/pm25R/finalCode/T5Data_p3_1_N2000.RData")
#result<-read.table("D:/zhou/pm25R/maxvalue2.txt",sep="\t",header=FALSE)
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
sig<-316.98363    
al<-t2[16]
q1<-Q[1]
for(j in 1:(nday-1)){
  sig<-c(sig,exp((t2[2])+t2[3]*log(sig[j])+t2[4]*exp(-(t2[5])*Q[j]+t2[6]*temp[j]+t2[7]*humi[j]+t2[8]*windlevel[j]
                                                     +t2[9]*x1[j]+t2[10]*x2[j]+t2[11]*x3[j]+t2[12]*x4[j]+t2[13]*x5[j]
                                                     +t2[14]*x6[j]+t2[15]*x7[j])))
  q1<-c(q1,(t2[1])+sig[(j+1)]*rweibull(1,1,1)^(1/t2[16]))
}
qqplot(q1,Q,xlab="",ylab="") #Figure 11(a)
lines(c(0,2000),c(0,2000))


plot(time,q1,xlab="",ylab="", col="blue",type="l")##Figure 11(b)
lines(time,Q,col="red")
# save.image("D:/zhou/pm25R/F11Data.RData")
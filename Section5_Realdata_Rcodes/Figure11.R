rm(list=ls())
nday<-2191
getmode <- function(v) {#This is a function to extract the mode
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
weather2001<-read.table("D:/zhou/pm25R/weatherData/202001.txt",sep="\t",header=TRUE)
y1<-weather2001$temp
y2<-weather2001$humi
y3<-weather2001$windlevel
y4<-rep(NULL,length(weather2001$winddirection))
y4[weather2001$winddirection=="东风"]<-1
y4[weather2001$winddirection=="东南风"]<-2
y4[weather2001$winddirection=="南风"]<-3
y4[weather2001$winddirection=="西南风"]<-4
y4[weather2001$winddirection=="西风"]<-5
y4[weather2001$winddirection=="西北风"]<-6
y4[weather2001$winddirection=="北风"]<-7
y4[weather2001$winddirection=="东北风"]<-8
y4[weather2001$winddirection=="暂无风向"]<-9
y5<-weather2001$cityname
t1<-as.Date(weather2001$time)
wea2001<-data.frame(t1,y1,y2,y3,y4,y5)
wea2001<-wea2001[y5=="北京"|y5=="天津"|y5=="石家庄"|y5=="唐山"|y5=="秦皇岛"|y5=="邯郸"|y5=="保定"|y5=="张家口"|y5=="承德"|y5=="廊坊"|y5=="沧州"|y5=="衡水"|y5=="邢台",]
names(wea2001)<-c("time","temp","humi","windlevel","winddirection","area")
wea2001$area<-factor(wea2001$area,levels=c("北京","天津", "石家庄", "唐山", "秦皇岛", "邯郸","保定","张家口","承德","廊坊","沧州","衡水","邢台"),order=T)
wea2001<-wea2001[order(wea2001$time,wea2001$area),]

weather2002<-read.table("D:/zhou/pm25R/weatherData/202002.txt",sep="\t",header=TRUE)
y1<-weather2002$temp
y2<-weather2002$humi
y3<-weather2002$windlevel
y4<-rep(NULL,length(weather2002$winddirection))
y4[weather2002$winddirection=="东风"]<-1
y4[weather2002$winddirection=="东南风"]<-2
y4[weather2002$winddirection=="南风"]<-3
y4[weather2002$winddirection=="西南风"]<-4
y4[weather2002$winddirection=="西风"]<-5
y4[weather2002$winddirection=="西北风"]<-6
y4[weather2002$winddirection=="北风"]<-7
y4[weather2002$winddirection=="东北风"]<-8
y4[weather2002$winddirection=="暂无风向"]<-9
y5<-weather2002$cityname
t1<-as.Date(weather2002$time)
wea2002<-data.frame(t1,y1,y2,y3,y4,y5)
wea2002<-wea2002[y5=="北京"|y5=="天津"|y5=="石家庄"|y5=="唐山"|y5=="秦皇岛"|y5=="邯郸"|y5=="保定"|y5=="张家口"|y5=="承德"|y5=="廊坊"|y5=="沧州"|y5=="衡水"|y5=="邢台",]
names(wea2002)<-c("time","temp","humi","windlevel","winddirection","area")
wea2002$area<-factor(wea2002$area,levels=c("北京","天津", "石家庄", "唐山", "秦皇岛", "邯郸","保定","张家口","承德","廊坊","沧州","衡水","邢台"),order=T)
wea2002<-wea2002[order(wea2002$time,wea2002$area),]

weather2003<-read.table("D:/zhou/pm25R/weatherData/202003.txt",sep="\t",header=TRUE)
y1<-weather2003$temp
y2<-weather2003$humi
y3<-weather2003$windlevel
y4<-rep(NULL,length(weather2003$winddirection))
y4[weather2003$winddirection=="东风"]<-1
y4[weather2003$winddirection=="东南风"]<-2
y4[weather2003$winddirection=="南风"]<-3
y4[weather2003$winddirection=="西南风"]<-4
y4[weather2003$winddirection=="西风"]<-5
y4[weather2003$winddirection=="西北风"]<-6
y4[weather2003$winddirection=="北风"]<-7
y4[weather2003$winddirection=="东北风"]<-8
y4[weather2003$winddirection=="暂无风向"]<-9
y5<-weather2003$cityname
t1<-as.Date(weather2003$time)
wea2003<-data.frame(t1,y1,y2,y3,y4,y5)
wea2003<-wea2003[y5=="北京"|y5=="天津"|y5=="石家庄"|y5=="唐山"|y5=="秦皇岛"|y5=="邯郸"|y5=="保定"|y5=="张家口"|y5=="承德"|y5=="廊坊"|y5=="沧州"|y5=="衡水"|y5=="邢台",]
names(wea2003)<-c("time","temp","humi","windlevel","winddirection","area")
wea2003$area<-factor(wea2003$area,levels=c("北京","天津", "石家庄", "唐山", "秦皇岛", "邯郸","保定","张家口","承德","廊坊","沧州","衡水","邢台"),order=T)
wea2003<-wea2003[order(wea2003$time,wea2003$area),]

w20_123=rbind(wea2001,wea2002,wea2003)
# write.table(w20_123,file="D:/zhou/pm25R/weather20_123.txt",sep="\t",col.names = FALSE)
# w20_123<-read.table("D:/zhou/pm25R/weather20_123.txt",sep="\t",header=FALSE)

names(w20_123)<-c("time","temp","humi","level","dirc")
sp<-split(w20_123,w20_123[,c("time")],drop=TRUE)

temp<-sapply(sp,FUN=function(x) max(x$temp))# extract the maximum of tempture data
names(temp)<-NULL
humi<-sapply(sp,FUN=function(x) min(x$humi))#extract the minimum of humidity data
names(humi)<-NULL
windlevel<-sapply(sp,FUN=function(x) {uniqv <-unique(x$level)#extract the mode of windlevel
uniqv[which.max(tabulate(match(x$level, uniqv)))]})
names(windlevel)<-NULL
windir<-sapply(sp,FUN=function(x) {uniqv <- unique(x$dirc)# extract the mode of winddirection
uniqv[which.max(tabulate(match(x$dirc, uniqv)))]})
names(windir)<-NULL
windir[22]=2
windir[27]=8
N_pred=91
g<-matrix(nrow=(N_pred),ncol=7)#construct the dummy variables
for(i in 1:N_pred){
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

result3<-read.table("D:/zhou/pm25R/maxvalue2.txt",sep="\t",header=FALSE)  #read the weather data in Disk C
result1<-result3$V2
Qresult<-result1[nday]

result4<-read.table("D:/zhou/pm25R/mavvalue20_123.txt",sep="\t",header=FALSE)  #read the weather data in Disk C
result20_123<-result4$V2
result20_123=c(result1[nday],result20_123)
windlevel=c(1,windlevel)
windir=c(4,windir)
temp=c(0,temp)
humi=c(14,humi)

t2=c(32.989999976,5.419855149,0.173078859, -1.785828493,0.003724074,0.007685324,0.002761116,0.015306995,0.013917796,-0.123021101, -0.165974508, -0.123851076, -0.085925037, -0.018640542,0.017690399,2.408072644)
Qw<-matrix(nrow=5000,ncol=N_pred+1)
for(i in 1:5000){
  sig<-218.2465
  #sig<-285
  al<-2.408072644
  Qresult<-result1[nday]
  q1<-Qresult
  for(j in 1:N_pred){
    sig<-c(sig,exp((t2[2])+t2[3]*log(sig[j])+t2[4]*exp(-(t2[5])*result20_123[j]+t2[6]*temp[j]+t2[7]*humi[j]+t2[8]*windlevel[j]+t2[9]*x1[j]+t2[10]*x2[j]+t2[11]*x3[j]+t2[12]*x4[j]+t2[13]*x5[j]+t2[14]*x6[j]+t2[15]*x7[j])))
    Qresult<-(t2[1])+sig[(j+1)]*rweibull(1,1,1)^(1/t2[16])
    q1<-c(q1,Qresult)
   # Qresult<-q1
  }

  Qw[i,]<-q1
}
fun1<-function(x){quantile(x,probs=0.025)}
fun2<-function(x){quantile(x,probs=0.975)}
Qw1<-apply(Qw,2,FUN=fun1)
Qw2<-apply(Qw,2,FUN=fun2)
Qw3<-apply(Qw,2,FUN=mean)
plot(Qw3[2:(N_pred+1)],xlab="",ylab="",ylim=c(0,1500),col="red",type="l") #red lines
lines(Qw2[2:(N_pred+1)],col="blue")
lines(Qw1[2:(N_pred+1)],col="blue")
lines(result20_123[2:(N_pred+1)],col="pink")

plot(sig,xlab="",ylab="",col="black",type="l")

#root mean squared error ss1
#mean absolute error ss2
#mean absolute percentage error ss3
ss1<-0
ss2<-0
ss3<-0
for(i in 2:(N_pred+1)){
  ss1<-ss1+(result20_123[i]-Qw3[i])^2
  ss2<-ss2+abs(result20_123[i]-Qw3[i])
  ss3<-ss3+abs(result20_123[i]-Qw3[i])/result20_123[i]
}
ss1<-(ss1/N_pred)^0.5
ss2<-ss2/N_pred
ss3<-ss3*100/N_pred
# plot(date2017,Qw3[1097:1461],xlab="",ylab="",ylim=c(0,1500),col="red",type="l") #red lines
# lines(date2017,result1[1097:1461],col="pink")
# lines(date2017,Qw1[1097:1461],col="blue",lty=3)
# 
# lines(date2017,Qw2[1097:1461],col="blue",lty=3)
# #polygon(c(1:365,1:365),c(Qw1[1097:1461],Qw2[1097:1461]),col = 'yellow')
# legend("topright", inset=.1, title="prediction", c("Estimated","True","95%CI"),
#        lty=c(1,1,3), col=c("red", "pink","blue"))
# 
# sig<-218.2465
# al<-2.408072644
# Qresult<-result1[nday]
# q1<-Qresult
# Yt<-((q1-t2[1])/sig)^(t2[16])
# for(j in 1:N_pred){
#   sig<-c(sig,exp((t2[2])+t2[3]*log(sig[j])+t2[4]*exp(-(t2[5])*result20_123[j]+t2[6]*temp[j]+t2[7]*humi[j]+t2[8]*windlevel[j]+t2[9]*x1[j]+t2[10]*x2[j]+t2[11]*x3[j]+t2[12]*x4[j]+t2[13]*x5[j]+t2[14]*x6[j]+t2[15]*x7[j])))
#   Qresult<-(t2[1])+sig[(j+1)]*Yt^(1/t2[16])
#   q1<-c(q1,Qresult)
#   Yt<-((Qresult-t2[1])/sig[j+1])^(t2[16])
#   # Qresult<-q1
# }
# plot(q1,xlab="",ylab="",ylim=c(0,1500),col="red",type="l") #red lines
# lines(result4,col="pink")

simu<-matrix(nrow=500,ncol=5000)# 500 sequences of simulated data with length 5000
for(j in 1:500){
  q1<-46.771139731+285*rweibull(1,1,1)^(1/2.398198221)
  sig<-285
  al<-2.398198221
  for(i in 1:4999){
    sig<-c(sig,exp((5.387443227)+0.191177266*log(sig[i])-2.218516794*exp(-(0.003439144)*q1[i])))
    q1<-c(q1,(46.771139731)+sig[(i+1)]*rweibull(1,1,1)^(1/2.398198221))
  }
  simu[j,]<-q1
}

#(True parameters):46.771139731  5.387443227  0.191177266 -2.218516794  0.003439144  2.398198221

f1<-function(param){ #likelihood function
  p6<-5/(1+exp(param[6]))
  Qmq<-Q1[1:n]-minq/(1+exp(param[1]))
  f_21<-285
  for(i in 1:4999){
    f_21<-c(f_21,exp(10/(1+exp(param[2]))+(2/(1+exp(param[3]))-1)*log(f_21[i])+(10/(1+exp(param[4]))-5)*exp(-(1/(1+exp(param[5])))*Q1[i])))
  }
  ll<-(-1)*(n*log(p6)-sum(log(f_21))+sum(log(Qmq/f_21))*(p6-1)-sum((Qmq/f_21)^(p6)))
  ll}  

nl1<-numeric()
mat1<-matrix(nrow=500,ncol=6) #procedure for estimating parameters of those 500 simulated sequences
for(h in 1:500){
  Q1<-simu[h,]
  minq<-min(Q1)-0.01
  n<-5000
  l1<-matrix(nrow=100,ncol=6)
  for(i in 1:100){
    l1[i,]<-c(runif(1,min=-3,max=3),runif(1,min=-3,max=3),runif(1,min=-10,max=10),runif(1,min=-3,max=3),runif(1,min=0,max=5),runif(1,min=-3,max=3))
  }
  nl<-rep(NULL,100)
  l2<-matrix(nrow=100,ncol=6)
  for(v in 1:100)  #100 start points
  {
    est1<-try(nlm(f1,l1[v,],iterlim=200),silent=TRUE)
    if ('try-error' %in% class(est1)) {
      nl[v]<-100000 
      next}
    else{l2[v,]<-est1$estimate
    nl[v]<-est1$minimum
    }
  }
  print(min(nl))
  nl1<-c(nl1,min(nl))
  print(l2[which.min(nl),])
  l3<-c((min(Q1)-0.01)/(1+exp(l2[which.min(nl),1])),10/(1+exp(l2[which.min(nl),2])),2/(1+exp(l2[which.min(nl),3]))-1,10/(1+exp(l2[which.min(nl),4]))-5,1/(1+exp(l2[which.min(nl),5])),5/(1+exp(l2[which.min(nl),6])))
  print(l3)
  mat1[h,]<-l3
}
asd1<-apply(mat1,2,mean)
asd2<-apply(mat1,2,sd)
mat<-rbind(mat1,asd1,asd2)

write.table(mat,"simulation1_5000.xls")



#1 Simulation을 이용한 Portfolio 최적화
z <-matrix(rnorm(1000*20),ncol=20)
a = 0.5
n = 5
i = 0.02
V0 = 1
mu =0.05
sig = 0.3
r = 0.04
v <- matrix(ncol=20,nrow=1000)

v[,1] = V0*(1+(1-a)*i+a*(exp(mu+sig*z[,1])-1))
for(t in 2:20){
  v[,t] = v[,t-1]*(1+(1-a)*i+a*(exp(mu+sig*z[,t])-1))
}

count = c()
for(i in 1:20){
  count[i] = sum(v[,i]>V0*(1+r)^i)/1000
}


aa = seq(0,1,by=0.1)


pf <- function(r,a){
  zz = matrix(rnorm(1000*20),ncol=20)
  vv = matrix(ncol=20,nrow=1000)
  vv[,1] = V0*(1+(1-a)*i+a*(exp(mu+sig*zz[,1])-1))
  for(t in 2:20){
    vv[,t] = vv[,t-1]*(1+(1-a)*i+a*(exp(mu+sig*zz[,t])-1))
  }
  
  cc = c()
  for(i in 1:20){
    cc[i] = sum(vv[,i]>V0*(1+r)^i)/1000
  }
  cc
}


vq = cbind(rep(1,5),v[1:5,1:10])
seq = seq(1,11)
vq1 = vq[1,]
vq2 = vq[2,] 
vq3 = vq[3,]
vq4 = vq[4,] 
vq5 = vq[5,]
s = data.frame(seq,vq1,vq2,vq3,vq4,vq5)

attach(s)
ggplot(s, aes(x=seq,y=vq1))+geom_line()+
  geom_line(aes(x=seq,y=vq2),colour="yellow") +
  geom_line(aes(x=seq,y=vq3),colour="red") +
  geom_line(aes(x=seq,y=vq4),colour="green") +
  geom_line(aes(x=seq,y=vq5),colour="blue")






#2 Stock Price Model
kospi <- read.table("C:/Users/LEEEUNJIN/Desktop/2018-1/이론통계학1/2주차/kospi.txt")
kospi$date = seq(as.Date("2004/01/01"), as.Date("2018/01/01"), "1 month")
colnames(kospi) = c("pt","date")


library(ggplot2)
ggplot(kospi,aes(x=date, y=pt)) + geom_line(col=colors()[490], size=1)

lnpt <- log(kospi$pt)
ggplot(kospi,aes(x=date, y=lnpt)) + geom_line(col=colors()[490], size=1)

dlnpt = c()
dlnpt[1]=0
for(i in 2:length(lnpt)){dlnpt[i]=lnpt[i]-lnpt[i-1]}
ggplot(kospi,aes(x=date, y=dlnpt)) + geom_line(col=colors()[490], size=1)
hist(dlnpt)

qqnorm(dlnpt)





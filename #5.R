library(optimx) 
library(numDeriv)
library(ggplot2)
library(dplyr)
library(gridExtra)


## Part1 ##

#(a)
part1 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/대학원/이론통계학1/5주차/part1.csv")
part1 <- part1  %>% mutate(q = deaths/pop, lq = log(-log(1-q)))
part1.1 <- part1 %>% 
  filter(type == 1) %>%
  select(-type)

part1.2 <- part1 %>% 
  filter(type == 2) %>%
  select(-type)

part1.3 <- part1 %>% 
  filter(type == 3) %>%
  select(-type)

part1.4 <- part1 %>% 
  filter(type == 4) %>%
  select(-type)


part14 <- part1 %>% filter(type ==1 | type ==4) 
ggplot(part14,aes(q,age))+geom_point(col=part14$type)
ggplot(part14,aes(lq,age))+geom_point(col=part14$type)+geom_smooth(data=part1.1,method="lm",col=1)+geom_smooth(data=part1.4,method="lm",col=4)

# 변수 lq가 직선에 가까우므로 Gompertz법칙이 성립한다.

#(b)

# 방법1
lm(data=part1.1,lq~age)
lm(data=part1.4,lq~age)

# 방법2
f1<-function(data,x) {  
  xx = data[1]+data[2]*x$age
  f = 1-exp(-exp(xx))
  sum(x$deaths*log(f)+(x$pop-x$deaths)*log(1-f))
}


result1 <-optimx(x=part1.1,par=c(-6.38560,0.07087),f1,control=list(all.methods=TRUE,maximize=TRUE,save.failures=TRUE,trace=0))
result4 <-optimx(x=part1.4,par=c(-6.38560,0.07087),f1,control=list(all.methods=TRUE,maximize=TRUE,save.failures=TRUE,trace=0))

result1; result4

#(c)
a1 = result1[1,c(1:2)]
a2 = result4[1,c(1:2)]

l1 = c(); l2 = c()
a1; a2
x = 20; t = seq(5,70,by=5)
l1[1] = 10^5; l2[1] = 10^5
l1[2:(length(t)+1)] = 10^5*exp(-exp(a1[1,1]+a1[1,2]*x)*((exp(a1[1,2]*t)-1)/a1[1,2]))
l2[2:(length(t)+1)] = 10^5*exp(-exp(a2[1,1]+a2[1,2]*x)*((exp(a2[1,2]*t)-1)/a2[1,2]))


e1 = c(); e2 = c()
for(i in 1:15){
  e1[i] = sum(l1[(i+1):length(l1)])/l1[i] + 0.5
  e2[i] = sum(l2[(i+1):length(l2)])/l2[i] + 0.5
  e1[15]=0.5; e2[15]=0.5
  }

delta_e = e1 - e2
today = 24*delta_e/e1

x0 = c(20,x+t)
data1 = data.frame(cbind(x0,l1,e1,l2,e2,delta_e,today))
data1

#(d)

# lx
ggplot(data1,aes(x=x0, y=l1)) + geom_line(col=colors()[490], size=1) + geom_line(y=data1$l2,col="red", size=1)

# ex
ggplot(data1,aes(x=x0, y=e1)) + geom_line(col=colors()[490], size=1) + geom_line(y=data1$e2,col="red", size=1)



## Part2 ##

part2 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/대학원/이론통계학1/5주차/part2.csv")
colnames(part2) <- c("y","d")


#(a)
part2 <- part2 %>% mutate(lny = log(y))

complete <-part2 %>% filter(d==1)
ggplot(complete, aes(y)) + geom_histogram(bins=8)
ggplot(complete, aes(lny)) + geom_histogram(bins=8)


#(b)

part2 <- part2 %>% mutate(rank = rank(y)/(length(y)+1),gompit=log(-log(1-rank)), probit = qnorm(rank), gumpit = -log(-log(rank)))


#2-parameter Weibull Q-Q plot
ggplot(part2,aes(gompit,lny))+geom_point()+ggtitle("2-parameter Weibull")+geom_smooth(method="lm")

#3-parameter Weibull Q-Q plot
lny2 = matrix(nrow=length(part2$y),ncol=length(a)); a = seq(-10,10,by=2)
for(i in 1:length(a)){
  lny2[,i] = log(part2$y-a[i])
}
p1 = ggplot(part2,aes(gompit,lny2[,1]))+geom_point()+ggtitle("3-parameter Weibull")+geom_smooth(method="lm")
p2 = ggplot(part2,aes(gompit,lny2[,2]))+geom_point()+ggtitle("3-parameter Weibull")+geom_smooth(method="lm")
p3 = ggplot(part2,aes(gompit,lny2[,3]))+geom_point()+ggtitle("3-parameter Weibull")+geom_smooth(method="lm")
p4 = ggplot(part2,aes(gompit,lny2[,4]))+geom_point()+ggtitle("3-parameter Weibull")+geom_smooth(method="lm")
p5 = ggplot(part2,aes(gompit,lny2[,5]))+geom_point()+ggtitle("3-parameter Weibull")+geom_smooth(method="lm")
p6 = ggplot(part2,aes(gompit,lny2[,6]))+geom_point()+ggtitle("3-parameter Weibull")+geom_smooth(method="lm")
p7 = ggplot(part2,aes(gompit,lny2[,7]))+geom_point()+ggtitle("3-parameter Weibull")+geom_smooth(method="lm")
p8 = ggplot(part2,aes(gompit,lny2[,8]))+geom_point()+ggtitle("3-parameter Weibull")+geom_smooth(method="lm")
p9 = ggplot(part2,aes(gompit,lny2[,9]))+geom_point()+ggtitle("3-parameter Weibull")+geom_smooth(method="lm")
p10 = ggplot(part2,aes(gompit,lny2[,10]))+geom_point()+ggtitle("3-parameter Weibull")+geom_smooth(method="lm")
p11 = ggplot(part2,aes(gompit,lny2[,11]))+geom_point()+ggtitle("3-parameter Weibull")+geom_smooth(method="lm")

for(i in 1:length(a)){
  p = ggplot(part2,aes(gompit,lny2[,i]))+geom_point()+ggtitle("3-parameter Weibull")+geom_smooth(method="lm")
}

grid.arrange(p1,p2,p3,p4,p5,ncol=2)
grid.arrange(p6,p7,p8,p9,p10,p11,ncol=2)

#log-normal Q-Q plot
ggplot(part2,aes(probit,lny))+geom_point()+ggtitle("log-normal Q-Q plot")+geom_smooth(method="lm")

#Gompertz Q-Q plot
ggplot(part2,aes(gompit,y))+geom_point()+ggtitle("Gompertz Q-Q plot")+geom_smooth(method="lm")

#Gumbel Q-Q plot
ggplot(part2,aes(gumpit,y))+geom_point()+ggtitle("Gumbel Q-Q plot")+geom_smooth(method="lm")

#Normal Q-Q plot
ggplot(part2,aes(probit,y))+geom_point()+ggtitle("Normal Q-Q plot")+geom_smooth(method="lm")


#(c)

AIC = c()

#2-parameter Weibull MLE
weibull2_part2 <- function(data){
  u = (log(part2$y)-data[1])/data[2]
  du = 1/(data[2]*(part2$y))
  result = sum(part2$d*log(exp((u-exp(u)))*du)+(1-part2$d)*log(exp(-exp(u))))
  result
}
lm(data=part2, lny~gompit)
result <-optimx(par=c(11.8025,0.2988),weibull2_part2,control=list(all.methods=TRUE,maximize=TRUE,save.failures=TRUE,trace=0))

logL <- result$value[3]
b <- as.matrix(result[3,c(1:2)])
AIC[1] <- -2*weibull2_part2(b)+2*length(b) 


#3-parameter Weibull MLE
weibull3_part2 <- function(data){
  u = (log(part2$y - data[3])-data[1])/data[2]
  du = 1/(data[2]*(part2$y-data[3]))
  result = sum(part2$d*log(exp((u-exp(u)))*du)+(1-part2$d)*log(exp(-exp(u))))
  result
}
result <-optimx(par=c(11.8025,0.2988,0),weibull3_part2,control=list(all.methods=TRUE,maximize=TRUE,save.failures=TRUE,trace=0))

logL <- result$value[3]
b <- as.matrix(result[3,c(1:3)])
AIC[2] <- -2*weibull3_part2(b)+2*length(b) 


# log-normal MLE
log_normal_part2 <- function(data){
  u = (log(part2$y)-data[1])/data[2]
  du = 1/(data[2]*(part2$y))
  sum(part2$d*log(dnorm(u)*du)+(1-part2$d)*log(1-pnorm(u)))
}
lm(data=part2, lny~probit)

result <-optimx(par=c(11.653,0.405),log_normal_part2,control=list(all.methods=TRUE,maximize=TRUE,save.failures=TRUE,trace=0))
logL <- result$value[3]
b <- as.matrix(result[3,c(1:2)])
AIC[3] <- -2*log_normal_part2(b)+2*length(b) 



#Gompertz MLE
gompit_part2 <- function(data){
  u = (part2$y-data[1])/data[2]
  du = 1/data[2]
  sum(part2$d*log(exp((u-exp(u)))*du)+(1-part2$d)*log(exp(-exp(u))))
}
lm(data=part2, y~gompit)

result <-optimx(par=c(132453,25587),gompit_part2,control=list(all.methods=TRUE,maximize=TRUE,save.failures=TRUE,trace=0))


logL <- result$value[3]
b <- as.matrix(result[3,c(1:2)])
AIC[4] <- -2*gompit_part2(b)+2*length(b) 


#Gumbel MLE

gumpit_part2 <- function(data){
  u = (part2$y-data[1])/data[2]
  du = 1/data[2]
  sum(part2$d*log(exp(-exp(-u))*exp(-u)*du)+(1-part2$d)*log(1-exp(-exp(-u))))
}
lm(data=part2, y~gumpit)

result <-optimx(par=c(103856,33958),gumpit_part2,control=list(all.methods=TRUE,maximize=TRUE,save.failures=TRUE,trace=0))

logL <- result$value[3]
b <- as.matrix(result[3,c(1:2)])
AIC[5] <- -2*gumpit_part2(b)+2*length(b) 


#Normal MLE

normal_part2 <- function(data){
  u = (part2$y-data[1])/data[2]
  du = 1/data[2]
  sum(part2$d*log(dnorm(u)*du)+(1-part2$d)*log(1-pnorm(u)))
  
}
lm(data=part2, y~probit)

result <-optimx(par=c(119750,35573),normal_part2,control=list(all.methods=TRUE,maximize=TRUE,save.failures=TRUE,trace=0))

logL <- result$value[3]
b <- as.matrix(result[3,c(1:2)])
AIC[6] <- -2*normal_part2(b)+2*length(b) 

AIC

#(d) 적절한 분포 2개 찾기
## *model : 2-Weibull
## **model : log-normal


## *model : mu=12.11948, sigma=0.4291728
## **model : mu=12.0249, sigma=0.7055311


t = c(150000,200000,300000)
pp = matrix(nrow=3,ncol=2); mu=c(); sigma=c()
mu[1]=12.11948; mu[2]=12.0249; sigma[1]=0.4291728; sigma[2]=0.7055311
for(i in 1:3){
  pp[i,1]=exp(-exp((log(t[i])-mu[1])/sigma[1]))
  pp[i,2]=exp(-exp((log(t[i])-mu[2])/sigma[2]))
}

#(e)


## Part3 ##

part3 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/대학원/이론통계학1/5주차/part3.csv")

female <- part3 %>% filter(Gender == 0)
male <- part3 %>% filter(Gender == 1)

#(a)
step(lm(Stature ~ Humerus+Radius+Ulna+Femur+Tibia+Fibula,data=female),direction="both")






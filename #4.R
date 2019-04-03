#4주차 과제

#Part 1

library(optimx) 
library(numDeriv)
library(ggplot2)


O_ring <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/이론통계학1/4주차/O-ring.csv")
colnames(O_ring) <- c("NO","n","y","x")
O_ring[O_ring=="*"] <- NA
O_ring$y <- as.numeric(O_ring$y)


##(a)

f1<-function(data) {  
  xx = data[1]+data[2]*O_ring$x
  sum(log((1/(1+exp(-xx)))^O_ring$y*(1-(1/(1+exp(-xx))))^(O_ring$n-O_ring$y)))
}

f2 <-function(data){
  xx = data[1]+data[2]*O_ring$x
  sum(log(pnorm(xx)^O_ring$y*(1-pnorm(xx))^(O_ring$n-O_ring$y)))
}


f3 <-function(data){
  xx = data[1]+data[2]*O_ring$x
  sum(log((1-exp(-exp(xx)))^O_ring$y*(1-(1-exp(-exp(xx))))^(O_ring$n-O_ring$y)))
}

result1 <-optimx(par=c(0,0),f1,control=list(all.methods=TRUE,maximize=TRUE,save.failures=TRUE,trace=0))
result2 <-optimx(par=c(0,0),f2,control=list(all.methods=TRUE,maximize=TRUE,save.failures=TRUE,trace=0))
result3 <-optimx(par=c(0,0),f3,control=list(all.methods=TRUE,maximize=TRUE,save.failures=TRUE,trace=0))

result1
result2
result3





logL1 <- -result1$value[3]           
b1 <- as.matrix( result1[3,c(1:2)] )  
AIC1 <- -2*f1(b1)+2*length(b1)         


logL2 <- -result2$value[3]           
b2 <- as.matrix( result2[3,c(1:2)] )  
AIC2 <- -2*f2(b2)+2*length(b2)         



logL3 <- -result3$value[3]           
b3 <- as.matrix( result3[3,c(1:2)] )  
AIC3 <- -2*f3(b3)+2*length(b3) 

cbind(AIC1,AIC2,AIC3)

#gompit 모형 선택      
       
  
##(b)

f0<-function(data) {  
  xx = data[1]
  sum(log((1-exp(-exp(xx)))^O_ring$y*(1-(1-exp(-exp(xx))))^(O_ring$n-O_ring$y)))
}  

result0 <-optimx(par=c(0),f0,control=list(all.methods=TRUE,maximize=TRUE,save.failures=TRUE,trace=0))




logL0 <- -result0$value[3]           
b0 <- as.matrix( result0[1,1] )  
AIC0 <- -2*f0(b0)+2*length(b0) 

cbind(AIC0,AIC1)

# H1모형 선택      

## (c)

b3

grad(f3, x=b3)     
J<-hessian(f3, x=b3)
V<-solve(-J)                    
se<-sqrt(diag(V))              


se

cbind(b3[1]-1.96*se[1],b3[1]+1.96*se[1])
cbind(b3[2]-1.96*se[2],b3[2]+1.96*se[2])

## (d)
1/(1+exp(-b3[1]-b3[2]*31))
6*1/(1+exp(-b3[1]-b3[2]*31))


#Part2
part2 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/이론통계학1/4주차/Leukemia.csv")
colnames(part2) <- c("y","x")

## (a)

plot(log(part2$y),log(part2$x))
lm = lm(log(part2$y)~log(part2$x))
alpha_e = lm$coefficients[1]; beta_e = lm$coefficients[2]
cbind(alpha_e,beta_e)

ftn_a <- function(data){
  -sum((log(part2$y)-(data[1]+data[2]*log(part2$x)))^2)
}

result_e <-optimx(par=c(5.425396,-0.8178007),ftn_a,control=list(all.methods=TRUE,maximize=TRUE,save.failures=TRUE,trace=0))


result_e



logLe <- result_e$value[3]
bei <- as.matrix(result_e[3,c(1:2)])
ei <- log(part2$y)-(bei[1]-bei[2]*log(part2$x))
AIC_e <- -2*ftn(bei)+2*length(bei) 


rank_ei <- rank(ei)/(length(ei)+1)
Gompit_ei = log(-log(1-rank_ei))
qnorm_ei = qnorm(rank_ei)
part2 <- data.frame(part2,rank_ei,qnorm_ei,Gompit_ei,ei)
ggplot(part2,aes(qnorm_ei,ei))+geom_point()+ggtitle("Normal Q-Q plot")+geom_smooth(method="lm")
ggplot(part2,aes(Gompit_ei,ei))+geom_point()+ggtitle("Gompertz Q-Q plot")+geom_smooth(method="lm")


## (b)

ftn_b <- function(data){
  xx = (log(part2$y)-(data[1]+data[2]*log(part2$x)))/data[3]
  sum(log(exp(-xx^2/2)))
}

result_b <-optimx(par=c(5.425396,-0.8178007,1),ftn_b,control=list(all.methods=TRUE,maximize=TRUE,save.failures=TRUE,trace=0))

result_b

logL_b <- result_b$value[2]
b_b <- as.matrix(result_b[2,c(1:3)])
AIC_b <- -2*ftn_b(b_b)+2*length(b_b) 
AIC_b



## (c)

ftn_c <-function(data){
  xx = (log(part2$y)-(data[1]+data[2]*log(part2$x)))/data[3]
  sum(log(exp(xx)*exp(-exp(xx))))
}

result_c <-optimx(par=c(5.425396,-0.8178007,1),ftn_c,control=list(all.methods=TRUE,maximize=TRUE,save.failures=TRUE,trace=0))



logL_c <- result_c$value[2]
b_c <- as.matrix(result_c[2,c(1:3)])
AIC_c <- -2*ftn_c(b_c)+2*length(b_c) 
AIC_c



## (d)
grad(ftn_c, x=b_c)     
J_c<-hessian(ftn_c, x=b_c) 
V_c<-solve(-J_c)                   
se_c<-sqrt(diag(V_c))              

cbind(b_c[1]-1.96*se_c[1],b_c[1]+1.96*se_c[1])
cbind(b_c[2]-1.96*se_c[2],b_c[2]+1.96*se_c[2])
cbind(b_c[3]-1.96*se_c[3],b_c[3]+1.96*se_c[3])


## (e)
cbind(AIC_b, AIC_c)

# AIC가 더 작은 log-normal모형이 채택

## (f)
t = c(52,104,260)
mhat_b = b_b[1] + b_b[2]*log(t)
1-log(1-exp(-exp((log(t)-mhat_b)/b_b[3])))

mhat_c = b_c[1] + b_c[2]*log(t)
1-log(1-exp(-exp((log(t)-mhat_c)/b_c[3])))



#Part 3
part3.1 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/이론통계학1/4주차/table1.csv")
part3.2 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/이론통계학1/4주차/table3.csv")


### table 1
## (b)

#방법1

rank_table1 <- rank(part3.1$x)/(length(part3.1$x)+1)
alpha = c(1.5,2.0,2.5)
log_alpha1 = (-log(1-rank_table1))^(1/alpha[1]) 
log_alpha2 = (-log(1-rank_table1))^(1/alpha[2]) 
log_alpha3 = (-log(1-rank_table1))^(1/alpha[3])

qq_table1 <- data.frame(part3.1$x,log_alpha1,log_alpha2,log_alpha3)
colnames(qq_table1) <- c("x","alpha1","alpha2","alpha3")
ggplot(qq_table1,aes(alpha1,x))+geom_point()+geom_smooth(method="lm")
ggplot(qq_table1,aes(alpha2,x))+geom_point()+geom_smooth(method="lm")
ggplot(qq_table1,aes(alpha3,x))+geom_point()+geom_smooth(method="lm")


# alpha = 2.5
lm(part3.1$x~log_alpha3)

# a = 27.910, b = 9.901


#방법2
rank_table1 <- rank(part3.1$x)/(length(part3.1$x)+1)
log_log <- function(data){
  log(-log(1-data))
}

qq2_table1 <- data.frame(log(part3.1$x-28),log(part3.1$x-29),log(part3.1$x-30),log_log(rank_table1))
colnames(qq2_table1) <- c("x1","x2","x3","y")
ggplot(qq2_table1,aes(y,x1))+geom_point()+geom_smooth(method="lm")
ggplot(qq2_table1,aes(y,x2))+geom_point()+geom_smooth(method="lm")
ggplot(qq2_table1,aes(y,x3))+geom_point()+geom_smooth(method="lm")


# a = 29
yy = log(part3.1$x-29); xx = log_log(rank_table1)
lm(yy~xx)

# mu = 2.1682, sigma = 0.4661

a = 29; b = exp(2.1682); alpha = 1/0.4661

cbind(a,b,alpha)


## (c)


ww <- function(data){
  p = c();  f = c()
  f = 1-exp(-((part3.1$x-data[1])/data[2])^data[3])
  p[1] = f[1]
  p = part3.1$observed*log(diff(c(0,f)))
  -sum(p)
}



result_table1 <-optimx(par=c(a,b,alpha),ww,control=list(all.methods=TRUE,maximize=TRUE,save.failures=TRUE,trace=0))

result_table1

## (d)
logL_table1 <- result_table1$value[6]
b_table1 <- as.matrix(result_table1[6,c(1:3)])

grad(ww, x=b_table1)     
J_table1<-hessian(ww, x=b_table1) 
V_table1<-solve(-J_table1)                   
se_table1<-sqrt(diag(V_table1))    

se_table1

#알파 추정값이 너무 작게 나와 역행렬을 구할 수 없었다.



### table 3

## (b)

#방법1

rank_table3 <- rank(part3.2$x)/(length(part3.2$x)+1)
alpha = c(1.5,2.0,2.5)
log_alpha1 = (-log(1-rank_table3))^(1/alpha[1]) 
log_alpha2 = (-log(1-rank_table3))^(1/alpha[2]) 
log_alpha3 = (-log(1-rank_table3))^(1/alpha[3])

qq_table3 <- data.frame(part3.2$x,log_alpha1,log_alpha2,log_alpha3)
colnames(qq_table1) <- c("x","alpha1","alpha2","alpha3")
ggplot(qq_table1,aes(alpha1,x))+geom_point()+geom_smooth(method="lm")
ggplot(qq_table1,aes(alpha2,x))+geom_point()+geom_smooth(method="lm")
ggplot(qq_table1,aes(alpha3,x))+geom_point()+geom_smooth(method="lm")


# alpha = 2.5
lm(part3.2$x~log_alpha3)

# a = -3.8, b = 13.98


#방법2
rank_table3 <- rank(part3.2$x)/(length(part3.2$x)+1)
log_log <- function(data){
  log(-log(1-data))
}

qq2_table3 <- data.frame(log(part3.2$x+4),log(part3.2$x+3),log(part3.2$x+2),log_log(rank_table3))
colnames(qq2_table3) <- c("x1","x2","x3","y")
ggplot(qq2_table1,aes(y,x1))+geom_point()+geom_smooth(method="lm")
ggplot(qq2_table1,aes(y,x2))+geom_point()+geom_smooth(method="lm")
ggplot(qq2_table1,aes(y,x3))+geom_point()+geom_smooth(method="lm")


# a = -4
yy = log(part3.2$x+4); xx = log_log(rank_table3)
lm(yy~xx)

# mu = 2.6527, sigma = 0.3985

a = -4; b = exp(2.6527); alpha = 1/0.3985

cbind(a,b,alpha)


## (c)


ww <- function(data){
  p = c();  f = c()
  f = 1-exp(-((part3.2$x-data[1])/data[2])^data[3])
  p[1] = f[1]
  p = part3.2$observed*log(diff(c(0,f)))
  return(-sum(p))
}



result_table3 <-optimx(par=c(a,b,alpha),ww,control=list(all.methods=TRUE,maximize=TRUE,save.failures=TRUE,trace=0))
result_table3

## (d)
logL_table3 <- result_table3$value[5]
b_table3 <- as.matrix(result_table3[5,c(1:3)])

grad(ww, x=b_table3)     
J_table3<-hessian(ww, x=b_table3) 
V_table3<-solve(-J_table3)                   
se_table3<-sqrt(diag(V_table3))    

se_table3


# 알파 추정값이 너무 작게 나와 역행렬을 구할 수 없었다.



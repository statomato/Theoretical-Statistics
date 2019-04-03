library(rjags)
library(runjags)
library(stats)
library(MASS)
library(optimx) 
library(numDeriv)
library(ggplot2)

## Part1 ##

x <- c(176,182,60,17)
n = 435
s = 1/sqrt(n)

##(a)
likelihood_function <- function(theta){
  theta0 = 1- theta[1] - theta[2]
  p0 = theta0^2
  p1 = theta[1]^2 + 2*theta[1]*theta0
  p2 = theta[2]^2 + 2*theta[2]*theta0
  p3 = 2*theta[1]*theta[2]
  
  L = (p0^x[1])*(p1^x[2])*(p2^x[3])*(p3^x[4])
  return(L)
}

logL_function <- function(theta){
  theta0 = 1- theta[1] - theta[2]
  p0 = theta0^2
  p1 = theta[1]^2 + 2*theta[1]*theta0
  p2 = theta[2]^2 + 2*theta[2]*theta0
  p3 = 2*theta[1]*theta[2]
  
  L = sum(log((p0^x[1])*(p1^x[2])*(p2^x[3])*(p3^x[4])))
  return(L)
}

h_function <- function(theta1,theta2){
  ifelse(theta1 >= 0 && theta2 >= 0 && theta1+theta2 <= 1,2,0)
}

set.seed(13)
t = matrix(nrow=1000,ncol=2)
t_star = matrix(nrow=1000,ncol=2)
z11 = rnorm(1000,0,1); z12 = rnorm(1000,0,1)
u = runif(1000,0,1)


for(i in 1:999){
  t[1,] = c(0.272009,0.0655)
  
  t_star[i,1] = t[i,1] + z11[i]*s
  t_star[i,2] = t[i,2] + z12[i]*s
  
  m = likelihood_function(c(t_star[i,1],t_star[i,2]))*h_function(t_star[i,1],t_star[i,2])/(likelihood_function(c(t[i,1],t[i,2]))*h_function(t[i,1],t[i,2]))
  a = min(1,m)
  t1 = ifelse(u[i] > a, t[i,1], t_star[i,1])
  t2 = ifelse(u[i]> a, t[i,2], t_star[i,2])
  t[i+1,1] = t1; t[i+1,2] = t2
}

quantile(t[,1],0.025); quantile(t[,2],0.025)
quantile(t[,1],0.25); quantile(t[,2],0.25)
quantile(t[,1],0.5); quantile(t[,2],0.5)
quantile(t[,1],0.75); quantile(t[,2],0.75)
quantile(t[,1],0.975); quantile(t[,2],0.975)

##(b)

result_table <-optimx(par=c(0.2,0.05),logL_function,control=list(all.methods=TRUE,maximize=TRUE,save.failures=TRUE,trace=0))
logL_table <- result_table$value[3]
b_table <- as.matrix(result_table[3,c(1:2)])

J_table <- hessian(logL_function, x=b_table) 
V_table <- solve(-J_table)                   
se_table <- sqrt(diag(V_table))    

cbind(b_table[1,1]-1.96*se_table[1],b_table[1,1]+1.96*se_table[1])
cbind(b_table[1,2]-1.96*se_table[2],b_table[1,2]+1.96*se_table[2])

##(c)
plot(t[,1])
plot(t[,2])

m1 = mean(t[,1])
m2 = mean(t[,2])
cov(t)

cbind(m1,m2); as.numeric(b_table)
#### 대표본에서의 사후분포에 대한 Laplace근사, MLE가 성립한다고 할 수 있다.


## Part2 ##

part2 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/대학원/이론통계학1/8주차/Mining-Accidents-CPA.csv")
colnames(part2) <- c("years","y","x1888")
y = part2$y
t = part2$years

##(a)

## Likelihood function : L(mu,sigma | Data)
part2_function <- "model{
for(i in 1:length(y)) {
  u[i] = exp(a-b*ifelse(t[i]>= c,1,0))
  y[i] ~ dpois(u[i])
}
  ## prior distribution for parameters (a,b,c)
  a ~ dunif(0,6)
  b ~ dunif(0,6)
  c ~ dunif(1851,1962)
  
}"

#### Running the model in JAGS

model <- jags.model(textConnection(part2_function), data = list(y = y,t=t),
                    n.chains = 3, n.adapt= 10000)


update(model, 10000); ## Burnin for 10000 samples

## use coda to summarize posterior samples

##  thin
mcmc_samples <- coda.samples(model, variable.names=c("a","b","c"), 
                             n.iter=20000, thin=10)
plot(mcmc_samples) 

##(b)
summary(mcmc_samples)
##autocorr.plot(mcmc_samples[,3])

##(c)

####Bootstrap

####parametric
logL_function2 <- function(yy,param){
  H = ifelse(part2$years>= param[3],1,0)
  u = exp(param[1]-param[2]*H)
  ff = factorial(part2$y)
  sum(-u+yy*log(u)-log(ff))
}
(initial_table <-optim(par=c(1,1,1890),logL_function2,yy=part2$y,control=list(fnscale=-1),hessian=T))
(initial <- initial_table$par)

B<-1000
boot1 <- matrix(0, ncol = 3, nrow = B)

y_star <- c()
for(i in 1:B){
  for(j in 1:length(t)){
    a = initial[1]; b = initial[2]; c = initial[3]
    y_star[j] = rpois(1,exp(a-b*ifelse(t[j]>=c,1,0)))
  }
  boot1[i,] <- optim(initial,logL_function2,y=y_star,control=list(fnscale=-1))$par
}

colnames(boot1) <-c("a","b","c")
head(boot1); tail(boot1)

LL_a <- quantile(boot1[,1],0.025)
UL_a <- quantile(boot1[,1],0.975)
mean(boot1[,1])
cbind(LL_a, UL_a)

LL_b <- quantile(boot1[,2],0.025)
UL_b <- quantile(boot1[,2],0.975)
mean(boot1[,2])
cbind(LL_b, UL_b)

LL_c <- quantile(boot1[,3],0.025)
UL_c <- quantile(boot1[,3],0.975)
mean(boot1[,3])
cbind(LL_c, UL_c)

####nonparametric
B<-1000
boot2 <- matrix(0, ncol = 3, nrow = B)

y_star1 <- c()
for(i in 1:B){
  for(j in 1:length(t)){
    a = initial[1]; b = initial[2]; c = initial[3]
    y_star1[j] = sample(y,replace=T)
  }
  boot2[i,] <- optim(initial,logL_function2,y=y_star1,control=list(fnscale=-1))$par
}

colnames(boot2) <-c("a","b","c")
head(boot2); tail(boot2)

LL_a <- quantile(boot2[,1],0.025)
UL_a <- quantile(boot2[,1],0.975)
mean(boot2[,1])
cbind(LL_a, UL_a)

LL_b <- quantile(boot2[,2],0.025)
UL_b <- quantile(boot2[,2],0.975)
mean(boot2[,2])
cbind(LL_b, UL_b)

LL_c <- quantile(boot2[,3],0.025)
UL_c <- quantile(boot2[,3],0.975)
mean(boot2[,3])
cbind(LL_c, UL_c)


## Part3 ##

part3 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/대학원/이론통계학1/8주차/Shark.csv")
x <- part3$L1; y <- part3$L2
t <- part3$Days/365.25

##(a)

## Likelihood function : L(mu,sigma | Data)
part3_function <- "model{
for(i in 1:length(y)) {
  y[i] ~ dnorm(mu[i],tau)
  mu[i] <- a-(a-x[i])*exp(-b*t[i])
  
}
## prior distribution for parameters (a,b,sigma)
  a ~ dunif(250,350)
  b ~ dunif(0,1)
  sigma ~ dunif(0,30)
  tau <- 1 / pow(sigma, 2)

}"

#### Running the model in JAGS

model3 <- jags.model(textConnection(part3_function), data = list(y = y,x = x,t = t),
                    n.chains = 3, n.adapt= 10000)

update(model3, 10000); ## Burnin for 10000 samples

## use coda to summarize posterior samples

##  thin
mcmc_samples3 <- coda.samples(model3, variable.names=c("a", "b", "sigma"), 
                             n.iter=20000, thin=10)
plot(mcmc_samples3) 

##(b)
summary(mcmc_samples3)
autocorr.plot(mcmc_samples3[,3])

##(c)
logL_function3 <- function(param){
  mm = param[1]-(param[1]-part3$L1)*exp(-param[2]*part3$Days/365.25)
  sig = param[3]
  sum(log(dnorm(part3$L2,mm,sig)))
}

(result3 <- optim(par=c(285,1,11),logL_function3,control = list(fnscale=-1),hessian = T))

##(d)
tseq = seq(-0.5,30.5,by=1)

MC_a = 285.2451
MC_b = 0.3605
MC_L0 = MC_a/MC_b
MC_t0 = log(1-51.5/MC_L0)/MC_b
MC_L = MC_a*(1-exp(-MC_b*(tseq-MC_t0)))

MLE_a = result3$par[1]
MLE_b = result3$par[2]
MLE_L0 = MLE_a/MLE_b
MLE_t0 = log(1-51.5/MLE_L0)/MLE_b
MLE_L = MLE_a*(1-exp(-MLE_b*(tseq-MLE_t0)))

ggplot()+geom_line(aes(tseq,MC_L),col="blue")+geom_line(aes(tseq,MLE_L),col="red")

library(rjags)
library(runjags)
library(ggplot2)
library(dplyr)

## Part1 ##
part1_1 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/대학원/이론통계학1/9주차/Data1.csv")
part1_2 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/대학원/이론통계학1/9주차/Alzheimer.csv")
head(part1_1)

##(a)
ggplot() + geom_point(data=part1_1,aes(x=(t+1),y=y,col=as.factor(lecithin))) + geom_smooth(data=part1_1,aes(x=(t+1),y=y,col=as.factor(lecithin)),se=F,method="loess")+theme_bw()

##(b)
par(mfrow=c(1,1))
n1 <- part1_2 %>% filter(lecithin == 0)
n2 <- part1_2 %>% filter(lecithin == 1)
boxplot(n1$y0,n1$y1,n1$y2,n1$y3,n1$y4,n2$y0,n2$y1,n2$y2,n2$y3,n2$y4,names=c(0,1,2,3,4,10,11,12,13,14))

##(c)
delta <- matrix(rep(part1_2$lecithin,5),ncol=5)
y <- part1_1$y
t <- part1_1$t
n <- nrow(part1_2)
## Likelihood function : 
part1_function_c <- "model{
  for(j in 1:5){
    for(i in 1:n) {
    y[i,j] ~ dbinom(p[i,j],30)
    x[i,j] <- a + c*delta[i,j] + (d*delta[i,j] + b)*(j-1)/12
    p[i,j] <- exp(x[i,j])/(1+exp(x[i,j]))
  }
}
## prior distribution for parameters (a,b,c,d)
  a ~ dunif(-10,10)
  b ~ dunif(-10,10)
  c ~ dunif(-10,10)
  d ~ dunif(-10,10)
}"

#### Running the model in JAGS

model1_c <- jags.model(textConnection(part1_function_c), data = list(delta=delta,y=part1_2[,2:6],n=nrow(part1_2)),
                     n.chains = 3, n.adapt= 10000)

update(model1_c, 10000); ## Burnin for 10000 samples

## use coda to summarize posterior samples

##  thin
mcmc_samples1_c <- coda.samples(model1_c, variable.names=c("a", "b", "c", "d"), 
                              n.iter=20000, thin=10)
plot(mcmc_samples1_c) 
summary(mcmc_samples1_c)

##(d)
## Likelihood function : 
part1_function_d <- "model{
  for(j in 1:5){
    for(i in 1:n) {
      y[i,j] ~ dbinom(p[i,j],30)
      x[i,j] <- alpha[i,j] + beta[i,j]*((j-1)/12)
      p[i,j] <- exp(x[i,j])/(1+exp(x[i,j]))
      alpha[i,j] <- a + sigma*u[i] + c*delta[i,j]
      beta[i,j] <- b + d*delta[i,j]
    }
  }
  for(i in 1:n){
    u[i] ~ dnorm(0,1)
  }
## prior distribution for parameters (a,b,c,d,u,sigma)
  a ~ dunif(-10,10)
  b ~ dunif(-10,10)
  c ~ dunif(-10,10)
  d ~ dunif(-10,10)
  sigma ~ dunif(0,1)
  
}"

#### Running the model in JAGS

model1_d <- jags.model(textConnection(part1_function_d), data = list(delta=delta,y=part1_2[,2:6],n=nrow(part1_2)),
                     n.chains = 3, n.adapt= 10000)

update(model1_d, 10000); ## Burnin for 10000 samples

## use coda to summarize posterior samples

##  thin
mcmc_samples1_d <- coda.samples(model1_d, variable.names=c("a", "b", "c", "d", "u", "sigma"), 
                              n.iter=20000, thin=10)
## plot(mcmc_samples1_d) 
summary(mcmc_samples1_d)


##(e)

##(f)
u0 <-summary(mcmc_samples1_d)$statistics[6:52,1]
a <- summary(mcmc_samples1_d)$statistics[1,1]
c <- summary(mcmc_samples1_d)$statistics[3,1]
sigma <- summary(mcmc_samples1_d)$statistics[5,1]

alpha_star <- log((part1_2$y0+0.5)/(20-part1_2$y0+0.5))
alpha <- a + sigma*u0 + c*part1_2$lecithin
plot(alpha_star,alpha)

## Part2 ##
part2 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/대학원/이론통계학1/9주차/Shark.csv")
part2 <- part2[1:36,]
x <- part2$L1; y <- part2$L2
t <- part2$Days/365.25

##(a)
s = ifelse(part2$Sex=="F",0,1)

## Likelihood function : L(mu,sigma | Data)
part2_function_a <- "model{
  for(i in 1:length(s)) {
    y[i] ~ dnorm(mu[i],tau)
    mu[i] <- a0[i]-(a0[i]-x[i])*exp(-b0[i]*t[i])
    a0[i] <- a + c1*s[i]
    b0[i] <- b + c2*s[i]

  }
## prior distribution for parameters (a,b,c1,c2,sigma)
  a ~ dunif(250,350)
  b ~ dunif(0,1)
  c1 ~ dunif(-10,10)
  c2 ~ dunif(-10,10)
  sigma ~ dunif(0,30)
  tau <- 1 / pow(sigma, 2)

}"

#### Running the model in JAGS

model2_a <- jags.model(textConnection(part2_function), data = list(y = y,x = x,t = t, s = s),
                     n.chains = 3, n.adapt= 10000)

update(model2_a, 10000); ## Burnin for 10000 samples

## use coda to summarize posterior samples

##  thin
mcmc_samples2_a <- coda.samples(model2_a, variable.names=c("a", "b", "c1","c2","sigma"), 
                              n.iter=20000, thin=10)
plot(mcmc_samples2_a) 
summary(mcmc_samples2_a)


##(b)


##(c)

## Likelihood function : L(mu,sigma | Data)
part2_function_c <- "model{
  for(i in 1:length(y)) {
    y[i] ~ dnorm(mu[i],tau)
    mu[i] <- a0[i]-(a0[i]-x[i])*exp(-b0[i]*t[i])
    a0[i] <- a + u[i]*sigma_a + c1*s[i]
    b0[i] <- b + v[i]*sigma_b + c2*s[i]
    u[i] ~ dnorm(0,1)
    v[i] ~ dnorm(0,1)
 
}
  ## prior distribution for parameters (a,b,c1,c2,sigma_a,sigma_b,sigma_e,u,v)
    a ~ dunif(250,350)
    b ~ dunif(0,1)
    c1 ~ dunif(-10,10)
    c2 ~ dunif(-10,10)
    sigma_a ~ dunif(0,30)
    sigma_b ~ dunif(0,30)
    sigma_e ~ dunif(0,30)
    tau <- 1 / pow(sigma_e, 2)

}"

#### Running the model in JAGS

model2_c <- jags.model(textConnection(part2_function_c), data = list(y = y,x = x,t = t, s = s),
                     n.chains = 3, n.adapt= 10000)

update(model2_c, 10000); ## Burnin for 10000 samples

## use coda to summarize posterior samples

##  thin
mcmc_samples2_c <- coda.samples(model2_c, variable.names=c("a", "b", "c1","c2","sigma_a","sigma_b","sigma_e","u","v"), 
                              n.iter=20000, thin=10)
plot(mcmc_samples2_c) 
summary(mcmc_samples2_c)

##(d)

##(e)

##(f)
ui = summary(mcmc_samples2_c)$statistics[8:43,1]
vi = summary(mcmc_samples2_c)$statistics[44:79,1]
a = summary(mcmc_samples2_c)$statistics[1,1]
b = summary(mcmc_samples2_c)$statistics[2,1]
c1 = summary(mcmc_samples2_c)$statistics[3,1]
c2 = summary(mcmc_samples2_c)$statistics[4,1]
sigma_a = summary(mcmc_samples2_c)$statistics[5,1]
sigma_b = summary(mcmc_samples2_c)$statistics[6,1]
sigma_e = summary(mcmc_samples2_c)$statistics[7,1]

ai = a + ui*sigma_a + c1*s
bi = b + vi*sigma_b + c2*s

pp = data.frame(ui,vi,ai,bi,s)

ggplot() + geom_point(data=pp ,aes(x=ui,y=vi,col=as.factor(s)))
ggplot() + geom_point(data=pp ,aes(x=ai,y=bi,col=as.factor(s)))

##(g)
which.max(ai); max(ai)

##(h)

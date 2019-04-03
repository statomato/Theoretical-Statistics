library(dplyr)
library(stats)
library(optimx)
library(survival)
library(VGAM)
library(ggplot2)

##Part1##
part1 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/대학원/이론통계학1/11주차/part1.csv")
Spring <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/대학원/이론통계학1/11주차/Spring.csv")

#(a)
par(mfrow=c(1,2))
plot(log(part1$y),part1$x); plot(log(part1$y),log(part1$x))

## x와 ln(y)가 음의 선형으로 보인다. 마찬가지로 ln(x)와 ln(y)도 음의 선형 관계

#(b)
par(mfrow=c(1,1))
boxplot(Spring$S800,Spring$S850,Spring$S900,Spring$S950,ylab="y",names=c(800,850,900,950))
boxplot(log(Spring$S800),log(Spring$S850),log(Spring$S900),log(Spring$S950),ylab="log(y)",names=c(800,850,900,950))

qqnorm(part1$y[1:41])
qqnorm(log(part1$y[1:41]))

y = part1$y
r = rank(part1$y)/(length(part1$y)+1)
plot(qexp(r),y)
par(mfrow=c(2,4))
plot(qweibull(r,0.2),y)
plot(qweibull(r,0.5),y)
plot(qweibull(r,0.8),y)
plot(qweibull(r,1),y)
plot(qweibull(r,1.3),y)
plot(qweibull(r,1.5),y)
plot(qweibull(r,1.8),y)
plot(qweibull(r,2),y)

par(mfrow=c(1,1))
plot(qlnorm(r),y)

#(c)
likelihood_ln_normal <- function(param){
  mu <- param[1] + param[2]*log(part1$x)
  s <- 1- pnorm((log(y)-mu)/param[3])
  f <- dnorm((log(y)-mu)/param[3])/(param[3]*y)
  sum(part1$d*log(f)+(1-part1$d)*log(s))
}

optim(par=c(5,0,1),likelihood_ln_normal,method = "BFGS", control = list(fnscale=-1), hessian=T)

likelihood_normal <- function(param){
  mu <- param[1] + param[2]*part1$x
  s <- 1- pnorm((log(y)-mu)/param[3])
  f <- dnorm((log(y)-mu)/param[3])/(param[3]*y)
  sum(part1$d*log(f)+(1-part1$d)*log(s))
}

optim(par=c(5,0,1),likelihood_normal,method = "BFGS", control = list(fnscale=-1), hessian=T)

likelihood_ln_logistic <- function(param){
  mu <- param[1] + param[2]*log(part1$x)
  s <- 1- pnorm((log(y)-mu)/param[3])
  f <- dnorm((log(y)-mu)/param[3])/(param[3]*y)
  sum(part1$d*log(f)+(1-part1$d)*log(s))
}
gaussian <- survreg(Surv(y, d) ~ x, dist='gaussian', part1)
lognormal <- survreg(Surv(y, d) ~ x, dist='lognormal', part1)
logistic <- survreg(Surv(y, d) ~ x, dist='logistic', part1)
loglogistic <- survreg(Surv(y, d) ~ x, dist='loglogistic', part1)
gompertz <- survreg(Surv(y, d) ~ log(x), dist='weibull', part1)
weibull <-survreg(Surv(y, d) ~ x, dist='weibull', part1)

L_gaussian <-gaussian$loglik[2]
L_lognormal <-lognormal$loglik[2]
L_logistic <- logistic$loglik[2]
L_loglogistic <- loglogistic$loglik[2]
L_gompertz <- gompertz$loglik[2]
L_weibull <-weibull$loglik[2]

cbind(L_gaussian,L_lognormal,L_logistic,L_loglogistic,L_gompertz,L_weibull)

####p가 같으므로 loglikelihood가 가장 작은 lognormal모형을 채택하였다.

#(d)
lognormal
x <- c(700,650)
mu_hat <- as.numeric(lognormal$coefficients[1]) + as.numeric(lognormal$coefficients[2])*x
sigma_hat <- as.numeric(lognormal$scale)
1-pnorm((log(10000)-mu_hat)/sigma_hat)

##Part2##
part2 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/대학원/이론통계학1/11주차/part2.csv")
colnames(part2) <- c("no","y","d","x1","x2","lnT")
part2[part2 == "*"] <- NA
part2$x2 <- as.numeric(part2$x2)

#(a)
par(mfrow=c(1,2))
plot(log(part2$y),part2$x1); plot(log(part2$y),part2$x2)
plot(log(part2$y),log(part2$x1)); plot(log(part2$y),log(part2$x2))

#(b)
AIC_f <- function(data){
  result <- (-2)*data$loglik[2] + 2*length(data$coefficients)
  result
}


gaussian2 <- survreg(Surv(y, d) ~ x1 + x2, dist='gaussian', part2)
gaussian2_1 <- survreg(Surv(y, d) ~ x1 + x2  + x1*x2, dist='gaussian', part2)
gaussian2_2 <- survreg(Surv(y, d) ~ log(x1) + log(x2) + log(x1)*log(x2), dist='gaussian', part2)
logistic2 <- survreg(Surv(y, d) ~ x1 + x2, dist='logistic', part2)
logistic2_1 <- survreg(Surv(y, d) ~ x1 + x2 + x1*x2, dist='logistic', part2)
logistic2_2 <- survreg(Surv(y, d) ~ log(x1) + log(x2) + log(x1)*log(x2), dist='logistic', part2)
gompertz2 <- survreg(Surv(y, d) ~ x1 + x2, dist='weibull', part2)
gompertz2_1 <- survreg(Surv(y, d) ~ x1 + x2 + x1*x2, dist='weibull', part2)
gompertz2_2 <- survreg(Surv(y, d) ~ log(x1) + log(x2) + log(x1)*log(x2), dist='weibull', part2)

AIC_f(gaussian2); AIC_f(gaussian2_1); AIC_f(gaussian2_2)
AIC_f(logistic2); AIC_f(logistic2_1); AIC_f(logistic2_2)
AIC_f(gompertz2); AIC_f(gompertz2_1); AIC_f(gompertz2_2)

#### AIC가 가장 작은 교호작용이 있는 곰페르쯔 모형을 채택하였다.

#(c)
x1 = 50; x2 = 1
beta0 = as.numeric(gompertz2_1$coefficients[1])
beta1 = as.numeric(gompertz2_1$coefficients[2])
beta2 = as.numeric(gompertz2_1$coefficients[3])
beta3 = as.numeric(gompertz2_1$coefficients[4])
sigma_2 = as.numeric(gompertz2_1$scale)
1-pgompertz((log(365)-beta0+beta1*x1+beta2*x2+beta3*x1*x2)/sigma_2,shape=1)

#(d)
modelc <- list()
modelc[[1]]<-coxph(Surv(y, d) ~ x1+x2, data = part2)
modelc[[2]]<-coxph(Surv(y, d) ~ x1+x2+x1*x2, data = part2)
modelc[[3]]<-coxph(Surv(y, d) ~ log(x1)+x2, data = part2)

modelc_aic <- data.frame()
for (i in 1:3){
  modelc_aic[i,1] <- extractAIC(modelc[[i]])[1]
  modelc_aic[i,2] <- extractAIC(modelc[[i]])[2]
}

colnames(modelc_aic)<-c("parameter","AIC")
modelc_aic

summary(coxph(Surv(y, d) ~ x1+x2+x1*x2, data = part2))


#### 교호작용이 포함된, 모형이 더 좋다.
#### 연려이 높아지고, mismatch 점수가 높을수록, 더 빨리 죽는다.

cox<-survfit(modelc[[2]])
cox_df<-data.frame(time=cox$time)
cox_df$s<-cox$surv; cox_df$h<--log(cox$surv)

ggplot(cox_df)+geom_point(aes(time,s))+ggtitle("S(t)")
ggplot(cox_df)+geom_point(aes(time,h))+ggtitle("H(t)")

ggplot(cox_df)+geom_point(aes(time,log(h)))+ggtitle("Gomepertz")
ggplot(cox_df)+geom_point(aes(log(time),log(h)))+ggtitle("Weibull")

cox_lm<-lm(log(h)~log(time), data=cox_df)
a<-cox_lm$coef[1]; b<-cox_lm$coef[2]

#### weibull 모형을 채택한다.

beta1c<-as.numeric(modelc[[2]]$coef[1]);beta2c<-as.numeric(modelc[[2]]$coef[2])
beta3c<-as.numeric(modelc[[2]]$coef[3])
mu<-exp(beta1c*(x1-mean(part2$x1))+beta2c*(x2-mean(part2$x2,na.rm=T))+beta3c*(x1*x2-mean(part2$x1*part2$x2,na.rm=T)))

St<-exp(-exp(a+b*log(365)))
as.numeric(St^mu)
1-pgompertz((log(365)-beta0+beta1*x1+beta2*x2+beta3*x1*x2)/sigma_2,shape=1)


#(e)
u<-(log(part2$y)-mu)/sigma_2
par(mfrow=c(1,1))
qqnorm(u)


####결과적으로 나오는 확률은 비슷하다.

####ALT모형은 모형을 가정해야하지만, PLM은 baseline hazard rate function을 몰라도 구할 수 있다. PLM모형은 ALT모형과 달리 잔차가 존재하지 않는다.
####ALT와 PLM모형의 확률은 비슷하게 구해진다.
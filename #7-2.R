library(optimx) 
library(numDeriv)
library(ggplot2)
library(dplyr)
library(glmulti)
library(leaps)
library(gdata)
library(MASS)

#Part1#

part1 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/대학원/이론통계학1/7주차/APT.csv")
colnames(part1) <- c("ydown","yup","y","x1","x2","x3","x4","x5","years","x21","x22","x11")

ylog <- log(part1$y)
part1 <- data.frame(ylog,part1)

##(a)

par(mfrow=c(2,2))
plot(part1$x2,part1$y); plot(part1$x3,part1$y); plot(part1$x4,part1$y); plot(part1$x5,part1$y)
plot(part1$x2,log(part1$y)); plot(part1$x3,log(part1$y)); plot(part1$x4,log(part1$y)); plot(part1$x5,log(part1$y))


plot(part1$x3,part1$y); plot(log(part1$x3),part1$y); plot(1/part1$x3,part1$y); plot(part1$x3^2,part1$y)
plot(part1$x4,part1$y); plot(log(part1$x4),part1$y); plot(1/part1$x4,part1$y); plot(part1$x4^2,part1$y)
plot(part1$x5,part1$y); plot(log(part1$x5),part1$y); plot(1/part1$x5,part1$y); plot(part1$x5^2,part1$y)

plot(part1$x3,log(part1$y)); plot(log(part1$x3),log(part1$y)); plot(1/part1$x3,log(part1$y)); plot(part1$x3^2,log(part1$y))
plot(part1$x4,log(part1$y)); plot(log(part1$x4),log(part1$y)); plot(1/part1$x4,log(part1$y)); plot(part1$x4^2,log(part1$y))
plot(part1$x5,log(part1$y)); plot(log(part1$x5),log(part1$y)); plot(1/part1$x5,log(part1$y)); plot(part1$x5^2,log(part1$y))



par(mfrow=c(1,1))
plot(1/part1$x3,part1$y)
plot(log(part1$x5),part1$y)


##(b)
part1 <- part1%>% mutate (x4.log = log(x4), x4i = 1/x4, x5.log = log(x5), x5i = 1/x5) 

regsubsets.out1 <-regsubsets(y~x11+x2+x3+x4+x5,data = part1,nbest = 1,nvmax = NULL,force.in = NULL, force.out = NULL, method = "exhaustive")
best.subset1 <- summary(regsubsets.out1)
coef(regsubsets.out,which.min(best.subset1$bic))

regsubsets.out2 <-regsubsets(y~x11+x2+x3+x4.log+x5,data = part1,nbest = 1,nvmax = NULL,force.in = NULL, force.out = NULL, method = "exhaustive")
best.subset2 <- summary(regsubsets.out2)
coef(regsubsets.out2,which.min(best.subset2$bic))

regsubsets.out3 <-regsubsets(y~x11+x2+x3+x4+x5.log,data = part1,nbest = 1,nvmax = NULL,force.in = NULL, force.out = NULL, method = "exhaustive")
best.subset3 <- summary(regsubsets.out3)
coef(regsubsets.out3,which.min(best.subset3$bic))

regsubsets.out4 <-regsubsets(y~x11+x2+x3+x4.log+x5.log,data = part1,nbest = 1,nvmax = NULL,force.in = NULL, force.out = NULL, method = "exhaustive")
best.subset4 <- summary(regsubsets.out4)
coef(regsubsets.out4,which.min(best.subset4$bic))

regsubsets.out5 <-regsubsets(y~x11+x2+x3+x4i+x5,data = part1,nbest = 1,nvmax = NULL,force.in = NULL, force.out = NULL, method = "exhaustive")
best.subset5 <- summary(regsubsets.out5)
coef(regsubsets.out5,which.min(best.subset5$bic))

regsubsets.out6 <-regsubsets(y~x11+x2+x3+x4i+x5.log,data = part1,nbest = 1,nvmax = NULL,force.in = NULL, force.out = NULL, method = "exhaustive")
best.subset6 <- summary(regsubsets.out6)
coef(regsubsets.out6,which.min(best.subset6$bic))

fit1 <- lm(y~x11+x2+x3+x4+x5,data=part1)
fit2 <- lm(y~x11+x2+x3+x4.log+x5,data=part1)
fit3 <- lm(y~x11+x2+x3+x4+x5.log,data=part1)
fit4 <- lm(y~x11+x2+x3+x4.log+x5.log,data=part1)
fit5 <- lm(y~x11+x2+x3+x4i+x5,data=part1)
fit6 <- lm(y~x11+x2+x3+x4i+x5.log,data=part1)
step1 <- stepAIC(fit1, direction="both")
step2 <- stepAIC(fit2, direction="both")
step3 <- stepAIC(fit3, direction="both")
step4 <- stepAIC(fit4, direction="both")
step5 <- stepAIC(fit5, direction="both")
step6 <- stepAIC(fit6, direction="both")
step1$anova; step2$anova; step3$anova; step4$anova; step5$anova; step6$anova;

qqnorm(part1$y); qqline(part1$y)

##(c)
regsubsets.out1.log <-regsubsets(ylog~x11+x2+x3+x4+x5,data = part1,nbest = 1,nvmax = NULL,force.in = NULL, force.out = NULL, method = "exhaustive")
best.subset1.log <- summary(regsubsets.out1.log)
coef(regsubsets.out1.log,which.min(best.subset1.log$bic))

regsubsets.out2.log <-regsubsets(ylog~x11+x2+x3+x4.log+x5,data = part1,nbest = 1,nvmax = NULL,force.in = NULL, force.out = NULL, method = "exhaustive")
best.subset2.log <- summary(regsubsets.out2.log)
coef(regsubsets.out2.log,which.min(best.subset2.log$bic))

regsubsets.out3.log <-regsubsets(ylog~x11+x2+x3+x4+x5.log,data = part1,nbest = 1,nvmax = NULL,force.in = NULL, force.out = NULL, method = "exhaustive")
best.subset3.log <- summary(regsubsets.out3.log)
coef(regsubsets.out3.log,which.min(best.subset3.log$bic))

regsubsets.out4.log <-regsubsets(ylog~x11+x2+x3+x4.log+x5.log,data = part1,nbest = 1,nvmax = NULL,force.in = NULL, force.out = NULL, method = "exhaustive")
best.subset4.log <- summary(regsubsets.out4.log)
coef(regsubsets.out4.log,which.min(best.subset4.log$bic))

regsubsets.out5.log <-regsubsets(ylog~x11+x2+x3+x4+x5i,data = part1,nbest = 1,nvmax = NULL,force.in = NULL, force.out = NULL, method = "exhaustive")
best.subset5.log <- summary(regsubsets.out5.log)
coef(regsubsets.out5.log,which.min(best.subset5.log$bic))

regsubsets.out6.log <-regsubsets(ylog~x11+x2+x3+x4.log+x5i,data = part1,nbest = 1,nvmax = NULL,force.in = NULL, force.out = NULL, method = "exhaustive")
best.subset6.log <- summary(regsubsets.out6.log)
coef(regsubsets.out6.log,which.min(best.subset6.log$bic))

fit1.log <- glm(y~x11+x2+x3+x4+x5,data=part1, family=gaussian(link=log))
fit2.log <- glm(y~x11+x2+x3+x4.log+x5,data=part1, family=gaussian(link=log))
fit3.log <- glm(y~x11+x2+x3+x4+x5.log,data=part1, family=gaussian(link=log))
fit4.log <- glm(y~x11+x2+x3+x4.log+x5.log,data=part1, family=gaussian(link=log))
fit5.log <- glm(y~x11+x2+x3+x4+x5i,data=part1, family=gaussian(link=log))
fit6.log <- glm(y~x11+x2+x3+x4.log+x5i,data=part1, family=gaussian(link=log))
step1.log <- stepAIC(fit1.log, direction="both")
step2.log <- stepAIC(fit2.log, direction="both")
step3.log <- stepAIC(fit3.log, direction="both")
step4.log <- stepAIC(fit4.log, direction="both")
step5.log <- stepAIC(fit5.log, direction="both")
step6.log <- stepAIC(fit6.log, direction="both")

step1.log$anova; step2.log$anova; step3.log$anova; step4.log$anova; step5.log$anova; step6.log$anova;



qqnorm(part1$ylog); qqline(part1$ylog)

##(d)
lm.y <- lm(y~x11+x3+x4+x5.log,data=part1)
summary(lm.y)


glm.ylog <- glm(y~x11+x2+x3+x4.log+x5.log,data=part1, family=gaussian(link=log))
summary(glm.ylog)

AIC(lm.y)
cbind(AIC(fit1),AIC(fit2),AIC(fit3),AIC(fit4),AIC(fit5),AIC(fit6))

AIC(glm.ylog)
cbind(AIC(fit1.log),AIC(fit2.log),AIC(fit3.log),AIC(fit4.log),AIC(fit5.log),AIC(fit6.log))

#Part2#

part2 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/대학원/이론통계학1/7주차/Antoine.csv")
colnames(part2) <- c("tC","T","P")

##(a)
plot(part2$P,part2$T)
plot(log(part2$P),part2$T)
plot(log(part2$P),1/part2$T)

##(b)

###(방법1)
f1 <-function(data){
  cor(log(part2$P),1/(part2$T-data[1]))
}

cc = c(-10,0,10,20,30,40,50)
result1 = list(NA)
for(i in 1:length(cc)){
  result1[[i]] <-optimx(par=cc[i],f1,control=list(all.methods=TRUE,maximize=TRUE,save.failures=TRUE,trace=0))
}

chat = c()
for(i in 1:length(cc)){chat[i] = result1[[i]]$p1[1]}
chat
e = c()
for(i in 1:length(cc)){}

f2 <- function(data){
  (log(part2$P)-data[1]-data[2]/(part2$T-c))^2
}

lm(log(part2$P)~1/(part2$T-c))


###(방법2)
####Step1
Plog = log(part2$P)
pt = Plog/part2$T
T1 = 1/part2$T
lm(Plog~T1+pt)

a0 = 23.16
b0 = -4891.55+23.16*39.22
c0 = 39.22

####Step2
theta = c(a0,b0,c0)
for(i in 1:100){
  X = matrix(nrow=13,ncol=3)
  X[,1]=rep(1,13); X[,2]=1/(part2$T-theta[3]); X[,3]=theta[2]/(part2$T-theta[3])^2
  
  z = matrix(nrow=13,ncol=1)
  z = Plog-(theta[1]+theta[2]/(part2$T-theta[3]))
  
  betahat = solve(t(X)%*%X)%*%t(X)%*%z
  theta = theta+betahat
}
betahat
theta

####Step3
sum((Plog-(theta[1]+theta[2]/(part2$T-theta[3])))^2)/(13-3)

##(c)
nls(Plog~a+b/(T-c),start=list(a=a0,b=b0,c=c0),data=part2)

##(d)
C = seq(from=0,to=370,by=10)
TT = 273.15 + C
ahat=23.17; bhat=-3992.47; chat=38.90
pT = exp(ahat+bhat/(TT-chat))
data.frame(TT,C,pT)

plot(pT,TT,type="l")
plot(log(pT),TT,type="l")

#Part3#

part3 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/대학원/이론통계학1/7주차/Shark.csv")
colnames(part3) <- c("Sex","Tag.Date","Recapture.Date","Days","PCL1","PCL2")
attach(part3)

##(a)
t = Days/365.25
dL = (PCL2-PCL1)/t
Lbar = (PCL1+PCL2)/2
lmresult = lm(dL~Lbar)
alpha = as.numeric(lmresult$coefficients[1])
beta = as.numeric(lmresult$coefficients[2])
k = -beta
L0 = alpha/k

nls(PCL2~ a-(a-PCL1)*exp(-b*t),start=list(a=L0,b=k),data=part3)

##(b)
t0 = log(1-51.5/L0)/k
tseq = seq(-0.5,30.5,by=1)
L = L0*(1-exp(-k*(tseq-t0)))
plot(tseq,L,type="l")

##(c)
part3m <- part3 %>% filter(Sex == "M")
part3f <- part3 %>% filter(Sex == "F")

m_t = part3m$Days/365.25
m_dL = (part3m$PCL2-part3m$PCL1)/m_t
m_Lbar = (part3m$PCL1+part3m$PCL2)/2
m_lmresult = lm(m_dL~m_Lbar)
m_alpha = as.numeric(m_lmresult$coefficients[1])
m_beta = as.numeric(m_lmresult$coefficients[2])
m_k = -m_beta
m_L0 = m_alpha/m_k
m_L = m_L0*(1-exp(-m_k*(tseq-t0)))

f_t = part3f$Days/365.25
f_dL = (part3f$PCL2-part3f$PCL1)/f_t
f_Lbar = (part3f$PCL1+part3f$PCL2)/2
f_lmresult = lm(f_dL~f_Lbar)
f_alpha = as.numeric(f_lmresult$coefficients[1])
f_beta = as.numeric(f_lmresult$coefficients[2])
f_k = -f_beta
f_L0 = f_alpha/f_k
f_L = f_L0*(1-exp(-f_k*(tseq-t0)))

ggplot()+geom_line(aes(tseq,m_L),col="blue")+geom_line(aes(tseq,f_L),col="red")
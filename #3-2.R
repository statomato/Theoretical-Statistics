
#part3

library(dplyr);library(ggplot2)
p3.1 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/이론통계학1/3주차/part3.1.csv")
p3.2 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/이론통계학1/3주차/part3.2.csv")
colnames(p3.1)
colnames(p3.2) = c("year","y")
p3 <-left_join(p3.1,p3.2,by=c("year"="year"))
data3 <-data.frame(p3$year,p3$rain,p3$y)
colnames(data3) = c("year","x","y")
data3 <- na.omit(data3)
ggplot(p3.1,aes(x=year, y=rain)) + geom_line(col=colors()[490], size=1)
hist(p3.1$rain, breaks=15)


lny = log(data3$y)
plot(data3$x, lny)
lm4 = lm(lny ~ data3$x)
summary(lm4)


ggplot(data3,aes(x=year, y=y)) + geom_line(col=colors()[490], size=1)
ggplot(data3,aes(x=year, y=lny)) + geom_line(col=colors()[490], size=1)


hist(data3$y)
hist(lny)


rank = rank(data3$y)
pr = rank/36
g3 = -log(-log(pr))
plot(g3,lny)
lm4.1 = lm(lny ~ g3)
summary(lm4.1)


#part4

p4 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/이론통계학1/3주차/part4.csv",stringsAsFactors=F)
p4[p4=="*"] <- NA
p4$max.rainfall.cm <- as.numeric(p4$max.rainfall.cm)
lnmax.rainfall = log(p4.1$max.rainfall.cm)
par(mfrow = c(2,2))
p4.1 = p4[p4$old.new==1,]
qqnorm(p4.1$max.rainfall.cm)
pr4 = rank(p4.1$max.rainfall.cm)/(length(p4.1$max.rainfall.cm)+1)
loglogistic = log(pr4/(1-pr4))
gumbel = -log(-log(pr4))
frechet = -log(-log(pr4))
plot(loglogistic,p4.1$max.rainfall.cm)
plot(gumbel,p4.1$max.rainfall.cm)
plot(frechet,lnmax.rainfall)

lm.part4 = lm(lnmax.rainfall~frechet)
summary(lm.part4)


mu.part4 = 2.238955; sigma.part4 = 0.340319
T = c(50,100,200)

ln.ut = mu.part4 + sigma.part4*(-log(-log(1-1/T)))
ut = exp(mu.part4 + sigma.part4*(-log(-log(1-1/T))))
ln.ut; ut

par(mfrow = c(1,1))
ggplot(p4,aes(x=years, y=max.rainfall.cm)) + geom_line(col=colors()[490], size=1)
length = length(p4$years)
t4 = seq(1:length)
lm.part4c = lm(p4$max.rainfall.cm~t4)
summary(lm.part4c)


alpha = 11.13762; beta = 0.01802
xhat = alpha + beta*t4
et = p4$max.rainfall.cm - xhat
pr_et = rank(et)/(length(et)+1)
g_pr_et = -log(-log(pr_et))
plot(g_pr_et,et)
lm.et = lm(et ~ g_pr_et)
summary(lm.et)


mu.d = -2.54213; sigma.d = 5.25198
mu.t = alpha + mu.d + beta*t4
mu.2016 = alpha + mu.d + beta*240
1-exp(-exp(-(30-mu.2016)/sigma.d))


#part1

p1 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/대학원/이론통계학1/3주차/part1.csv")
colnames(p1)
hist(as.numeric(p1$rt),breaks=80)
par(mfrow=c(2,3))
qqnorm(p1$rt); qqline(p1$rt)
pr1 = rank(p1$rt)/(length(p1$rt)+1)
logistic_1 = log(pr1/(1-pr1))
laplace_1 = qlaplace(pr1)
t_1.1 = qt(pr1,df=1)
t_1.2 = qt(pr1,df=2)
t_1.3 = qt(pr1,df=3)


plot(logistic_1,p1$rt)
plot(laplace_1,p1$rt)
plot(t_1.1,p1$rt)
plot(t_1.2,p1$rt)
plot(t_1.3,p1$rt)




par(mfrow=c(1,1))
alpha_1b = c(0.01,0.004,0.001, 0.0004)
Varalpha = -qt(alpha_1b, df=3)

exess = -p1$rt - 0.04
pr_exess = rank(exess)/(length(exess)+1)
plot(qpareto(pr_exess,1),exess)

     













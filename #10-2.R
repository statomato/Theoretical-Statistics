library("dplyr")
library(ggplot2)
library(MASS)
part2 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/대학원/이론통계학1/10주차/yieldcurve.csv")
colnames(part2) <- c("num","x","block","z","y")
attach(part2)
block <- as.factor(block)

#(a)
par(mfrow=c(1,3))
plot(x,y); plot(x,y/x); plot(x,x/y)

#(b)
glm_b <- glm(y ~ I(1/x), family=Gamma)
(beta0 <- as.numeric(glm_b$coefficients[1]))
(beta1 <- as.numeric(glm_b$coefficients[2]))
confint(glm_b)
alpha <- as.numeric(gamma.shape(glm_b)$alpha)
alpha_se <-as.numeric(gamma.shape(glm_b)$SE)
alpha
cbind(alpha-alpha_se*1.96,alpha+alpha_se*1.96)

#(c)
ggplot(part2, aes(x=x, y=y),)+geom_point()+geom_line(aes(x=x,y=glm_b$fitted.values),colour="red")

#(d)
step(glm(y ~ I(1/x) + block/x + block + x),direction = "backward")
(best_gamma <- glm(y ~ I(1/x) + x))
confint(best_gamma)

#(e)

#(f)
par(mfrow=c(1,1))
plot(x,z/x)

#(g)
logx <- log(x)
step(glm(cbind(z,x-z) ~ block + block*x + I(1/x) + logx + x, family = binomial),direction = "backward")
best_logistic <- glm(cbind(z, x - z) ~ block + x + logx + block*x, family = binomial)

step(glm(z ~ block + block*x + I(1/x) + logx + x, family = gaussian),direction = "backward")
best_normal <- glm(z ~ block + x + I(1/x) + logx, family = gaussian)

step(glm(cbind(z,x-z) ~ block + block*x + I(1/x) + logx + x, family = binomial(link=cloglog)),direction = "backward")
best_gompertz <- glm(cbind(z, x - z) ~ block + x + logx + block*x, family = binomial(link = cloglog))

best_logistic$aic; best_normal$aic; best_gompertz$aic     

#(h)

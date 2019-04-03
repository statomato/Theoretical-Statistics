library(optimx) 
library(numDeriv)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(glmulti)
library(leaps)
library(lmtest)

## Part1 ##

data <- select(data, R1:R24, Y); data1 <- data
AIC_Hp <- data.frame(p=24:0, AIC=0); var.list <- list()

for(i in 1:24){
  var.list[[i]] <- colnames(data1)
  H <- glm( Y ~ . , family = binomial, data = data1 )
  k <- which.min( abs( summary(H)$coef[-1,3] ) ); data1 <- data1[,-k]
  AIC_Hp$AIC[i] <- AIC(H)
}

AIC_Hp$AIC[25] <- AIC( glm( Y ~ 1, family = binomial, data = data ) )

ggplot( AIC_Hp, aes( p, AIC ) ) + geom_point() + geom_line() +
  scale_x_continuous(breaks=0:24) + theme( panel.grid.minor = element_blank() )

var.list[[ which.min( AIC_Hp$AIC ) ]]






#(a)
part1 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/대학원/이론통계학1/6주차/part1.csv")

sub.f <- regsubsets(Y ~ R1+R2+R3+R4+R5+R6+R7+R8+R9+R10+R11+R12+R13+R14+R15+R16+R17+R18+R19+R20+R21+R22+R23+R24, 
                    data = part1, nbest = 1, nvmax = NULL, method = "backward")
summary(sub.f)
names(summary(sub.f))
a <- summary(sub.f)$which

data1 <- part1 %>%
  select(-c(NO,Year))

glm.f <- list(NA)
AIC.f <- rep(0, 24)
for (i in 1:24) {
  glm.f[[i]] <- glm(Y ~ ., data = data1[, a[i,]],family=binomial(link=logit))
  AIC.f[i] <-AIC(glm.f[[i]]) 
}
AIC.f

plot(AIC.f, type = "b")
which.min(AIC.f)

#(b)
g1 <- glm(Y~.,data=data1,family=binomial(link=logit))
g2 <- glm(Y~R5+R6+R16+R18,data=data1,family=binomial(link=logit))

#(c)
lrtest(g2,g1)

#(d)
data1$Y <- as.factor(data1$Y)
plot(data.frame(data1$R5,data1$R6,data1$R16,data1$R18),col=data1$Y)

fitted = fitted(g1)
data1 <- data1 %>% mutate(fitted = fitted)

ggplot(data1, aes(x=Y, y=fitted)) + geom_boxplot()
ggplot(data1) + geom_density(aes(fitted,col=Y)) + xlim(-0.5,1.5)


#(e)
vcov <- vcov(g2)
l = c(); u = c()
for(i in 1:5){
  l[i]=g2$coefficients[[i]]-1.28*sqrt(vcov[i,i])
  u[i]=g2$coefficients[[i]]+1.28*sqrt(vcov[i,i])
}

#(f)
#최적 모형
predicted <- ifelse(fitted(g2) >0.5, 1, 0)
m = table(data1$Y, predicted)
mis = (m[1,2]+m[2,1])/100
m
mis

#full model
predicted1 <- ifelse(fitted(g1) >0.5, 1, 0)
m1 = table(data1$Y, predicted1)
mis1 = (m1[1,2]+m1[2,1])/100
m1
mis1




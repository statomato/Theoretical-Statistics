library(optimx) 
library(numDeriv)
library(ggplot2)
library(dplyr)
library(glmulti)
library(leaps)



#Part1#

part1 <- read.csv("C:/Users/LEEEUNJIN/Desktop/2018-1/대학원/이론통계학1/7주차/APT.csv")
colnames(part1) <- c("ydown","yup","y","x1","x2","x3","x4","x5","years","x21","x22","x11")

ylog <- log(part1$y)
part1 <- data.frame(ylog,part1)

##(a)

## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
} 

## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

## put linear regression line on the scatter plot
panel.lm <- function(x, y, col=par("col"), bg=NA, pch=par("pch"), 
                     cex=1, col.smooth="black", ...) {
  points(x, y, pch=pch, col=col, bg=bg, cex=cex) 
  abline(stats::lm(y~x), col=col.smooth, ...)
} 

pairs(part1[,4:9],  lower.panel = panel.lm, upper.panel = panel.cor, diag.panel = panel.hist, main = "scatter-plot mtrix, correlation coef., histogram"   )
pairs(part1[,c(1,5:9)],  lower.panel = panel.lm, upper.panel = panel.cor, diag.panel = panel.hist, main = "scatter-plot mtrix, correlation coef., histogram"   )


##산점도
par(mfrow=c(2,2))
plot(part1$x2,part1$y); plot(part1$x3,part1$y); plot(part1$x4,part1$y); plot(part1$x5,part1$y)
plot(part1$x2,log(part1$y)); plot(part1$x3,log(part1$y)); plot(part1$x4,log(part1$y)); plot(part1$x5,log(part1$y))

par(mfrow=c(1,3))
plot(log(part1$x3),part1$y); plot(1/part1$x3,part1$y); plot(part1$x3^2,part1$y)
plot(log(part1$x4),part1$y); plot(1/part1$x4,part1$y); plot(part1$x4^2,part1$y)
plot(log(part1$x5),part1$y); plot(1/part1$x5,part1$y); plot(part1$x5^2,part1$y)

par(mfrow=c(1,1))
plot(log(part1$x5),part1$y)













glm.f <- list(NA)
AIC.glm <- c()
glm.f[[i]] <- glm(y ~ x1+x2+x3+x4+x5, data = part1, family=gaussian(link=log))
AIC.glm[i] <-AIC(glm.f[[i]]) 







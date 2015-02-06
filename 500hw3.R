# STATS 500 HW3
library(MASS)
mu <- c(3,4)
sigma <- matrix(c(1.0,0.8,0.8,1.0),nrow=2)
datam <- data.frame(mvrnorm(100,mu,sigma))
colnames(datam) <- c("x","y")

plot(y~x, datam, xlim=c(0,7),ylim=c(0,7))
lm <- lm(y~x, datam)
abline(lm)
lm2 <- lm(x~y, datam)
lines(lm2$fitted.values, datam$y)

summary(lm)
summary(lm2)
# Same for both models. 

## e
left <- (lm$fitted.values - mean(datam$y))/(sd(datam$y))
left[1:5]
r = sqrt(1-(sum(lm$residuals^2))/(sum((datam$y - mean(datam$y))^2)))
right <- r*(datam$x - mean(datam$x))/(sd(datam$x))
right[1:5]
sum(left - right)

sum((datam$y - mean(datam$y))^2)
  
# 2 
setwd("~/Documents/GoBlue/F14/F14R")
college <- read.csv('college.csv')
head(college)
pairs(college[c(1,2,4,5,6)])
lm <- lm(gradrat~.-lenroll, data = college)
summary(lm)
## b
lm2 <- lm(gradrat ~ . - lenroll - private - stufac - rmbrd, data = college) 
anova(lm, lm2)
## c
summary(lm)$coefficients
coe <- summary(lm)$coefficients
2*pt((coe[2, 1] - 0.05)/coe[2,2], 120 - 6)

## d
lm3 <- lm(gradrat ~ . - lenroll - private - act, data = college)
anova(lm, lm3)

## e
lm4 <- lm(gradrat ~ I(rmbrd + act) + csat + private + stufac, college)
anova(lm, lm3)
# 3
x <- c(1, 0, -1)
y <- c(1, 0, 2)
lm <- lm(y ~ x + I(3*x^2 -2))
summary(lm)
plot(y, x  + (3*x^2 -2))


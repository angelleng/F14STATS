## 500 hw 5 ## 
library(faraway)
head(pipeline)
# 1 
lm <- lm(Lab~Field, data = pipeline)
lms <- summary(lm)
lms
lminf <- influence(lm)
stud <- residuals(lm)/(lms$sig*sqrt(1-lminf$hat))
# qqnorm (stud)
# abline (0, 1)
plot(fitted(lm), stud)

i <- order(pipeline$Field)
npipe <- pipeline[i,]
ff <- gl(12,9)[-108]
meanfield <- unlist(lapply(split(npipe$Field,ff),mean))
varlab <- unlist(lapply(split(npipe$Lab,ff),var))

x <- log(meanfield)
# lm2 <- lm(log(varlab) ~ log(meanfield))
lm2 <- lm(log(varlab) ~ x)
summary(lm2)

wt <- exp(predict(lm2, data.frame(x = log(pipeline$Field))))

lm3 <- lm(Lab~Field, pipeline, weights = wt)
summary(lm3)


plot(pipeline$Field, pipeline$Lab)
abline(lm)
abline(lm3, lty = 2)

### d 
sum(lm3$residuals)
sum(lm3$residuals*wt)

### e 
# wt2 <- var(pipeline$Lab)

lm4 <- lm(Lab ~ 1, data = pipeline, weights = wt)
summary(lm4)

anova(lm4, lm3)

sst <- sum((lm4$residuals)^2*wt)
sst
sse <- 349156
ssr <- sst - sse
ssr

## 2 
### a 
setwd("~/Documents/GoBlue/F14/F14R")
icu <- read.csv("icu.csv")

head(icu)
icu$race <- factor(icu$race)
glm <- glm(sta ~ ., family=binomial, data = icu)
summary(glm)

### b 
glm2 <- glm(sta~age + can + race, family = binomial, data = icu)
anova(glm2, glm)
1-pchisq(12.8,2)
qchisq(.95,2)

library(aod)
wald.test(b = coef(glm), Sigma = vcov(glm), Terms = c(4,5))

### c 
x <- model.matrix(glm)[,-1]
newdat <- data.frame(sta=glm$y,x)
glm3 <- glm(sta ~ age + can + cpr + inf + I(race2+race3), family = binomial, data = newdat)
anova(glm3, glm)
1-pchisq(0.962,1)
qchisq(.95,2)

lc <- cbind(0,0,0,0,0,1,-1)
wald.test(b = coef(glm), Sigma = vcov(glm), L=lc)

### d 
confint(glm3)

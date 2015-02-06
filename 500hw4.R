library(faraway)
### 1 
lm <- lm(lpsa~., prostate)
summary(lm)
##### a
confint(lm)
confint(lm, level = 0.9)
##### b
library(ellipse)
plot(ellipse(lm, c("age", "lbph")),
     type="l")
points(lm$coef["age"],lm$coef["lbph"],pch=18)
points(0,0)
##### c
lcavol lweight age lbph svi lcp gleason pgg45
1.44692 3.62301 65 0.3001 0 -0.79851 7 15

lcavol lweight age lbph svi lcp gleason pgg45
1.44692 3.62301 65 0.3001 0 -0.79851 7 15


newdata <- data.frame(lcavol = 1.44692, lweight = 3.62301, age = 65, lbph = 0.3001, 
                      svi = 0, lcp = -0.79851, gleason = 7, pgg45 = 15)

predict(lm, newdata, interval = "confidence")
predict(lm, newdata, interval = "prediction")
### 2 
lm2 <- lm(gamble ~ ., data = teengamb)
##### a
st_res <- rstandard(lm2)
plot(fitted.values(lm2),st_res,
     xlab="Fitted values",
     ylab="Standardized residuals")
abline(h = 0)
# constant variance doesn't hold. 
newgamble <- sqrt(teengamb$gamble)
teengamb$newgamble <- newgamble
lm3 <- lm(newgamble ~ . - gamble, data = teengamb)

st_res <- rstandard(lm3)
plot(fitted.values(lm3),st_res,
     xlab="Fitted values",
     ylab="Standardized residuals")
abline(h = 0)
# constant variance

##### b 
qqnorm(lm3$residual, ylab="Residuals")
qqline(lm3$residual)
##### c 
halfnorm(influence(lm3)$hat, nlab = 2, ylab="Leverages")
teengamb[c(42, 35), ]
##### d
ti <- rstudent(lm3)
max(abs(ti))
which(abs(ti) == max(abs(ti)))

2*(1-pt(max(abs(ti)), df=47-4-1))
0.05/47 
# no outliers 
##### e
cook <- cooks.distance(lm3)
halfnorm(cook, nlab = 3, ylab="Cooks distance")

lm4 <- lm(newgamble ~ . - gamble, data = teengamb[-24, ])
summary(lm4)
predict(lm4, teengamb[24,], interval ="prediction")
teengamb[24,]$newgamble

lm5 <- lm(newgamble ~ . - gamble, data = teengamb[-39, ])
predict(lm5, teengamb[39,], interval ="prediction")
teengamb[39,]$newgamble


lm6 <- lm(newgamble ~ . - gamble, data = teengamb[-5, ])
predict(lm6, teengamb[5,], interval ="prediction")
teengamb[5,]$newgamble

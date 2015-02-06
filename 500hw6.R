### 500 hw 6 ###
cancer <- c(13, 55, 489, 475, 293, 38)
control <- c(61, 129, 570, 431, 154, 12)
y <- cbind(cancer, control)
daily = c(1,2,3,4,5,6)
lung <- data.frame(daily = c(1,2,3,4,5,6), rate = cancer / (cancer + control) ) 

m1 <- glm(y ~ daily, family = binomial)
summary(m1)

plot(daily, log(cancer/(cancer + control)))


### b
phat <- fitted.values(m1)
ni = rowSums(y)
nphat = ni * phat
chis <- sum((y[,1] - nphat) ^ 2/ (nphat * (1-phat)))
chis
1-pchisq(chis,4)

### c 
m0 <- glm(y ~ 1, family = binomial)
anova(m0, m1)
1-pchisq(123.42,1)

library(aod)
wald.test(b = coef(m1), Sigma = vcov(m1), Terms = 1)

### d 
summary(m1)$coef
confint(m1)

### e
y2 <- matrix(c(68, 190, 964, 1001, 331, 166), ncol = 2)
m2 <- glm(y2 ~ c(1,2,3), family = binomial)
summary(m2)$coef
confint(m2)


## 2 
deaths <- c(2,32,12,104,28,206,28,186,31,102)
py <- c(18793,52407,10673,43248,5710,28612,2585,12663,1462,5317)
logpy <- log(py)
age <- gl(5,2)
smoke <- gl(2,1,length=10)
dataf <- data.frame(age=age,smoke=smoke,deaths=deaths, 
                    py=py,logpy=logpy)
m1 <- glm(deaths ~ age+smoke,offset=logpy,family=poisson,
            data=dataf)
summary(m1)

dataf$inter <- c(0,1,0,2,0,3,0,4,0,5)
m2 <- glm(deaths~ age+smoke+inter,offset=logpy,family=poisson, data=dataf)
summary(m2)

### b 
# first iteration of IRLS
obsrate <- deaths/py
lam <- obsrate
eta <- log(lam)
z <- eta
w <- deaths
# do 5 Fisher scoring iterations
for (i in 1:4) {
m3 <- lm(z ~ age + smoke, weights=w) # regress z on X weights w
eta <- fitted(m3)
lam <- exp(eta)
z <- eta + (obsrate - lam)/lam
w <- py * lam }
coef(m3) # beta hat


varbeta <- summary(m3)$cov.unscaled # (X'WX)^-1
sqrt(diag(varbeta)) # standard errors
varbeta
vcov(m1)

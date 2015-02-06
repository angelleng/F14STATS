# 500 hw2
library(MASS)
mu <- c(3,4)
sigma <- matrix(c(1.0,0.8,0.8,1.0),nrow=2)
datam <- data.frame(mvrnorm(100,mu,sigma))
colnames(datam) <- c("x","y")

# plot 
plot(datam$x, datam$y, xlim = c(0, 7), ylim = c(0, 7), type = 'p')

# fit model
lm <- lm(y ~ x, data = datam)
abline(lm)
summary(lm)

?pnorm

pt((1-0.79582)/0.06385, 98)

### 2
m0 <- lm(y ~ x -1 , data=datam)
summary(m0)
abline(m0, lty = 2)

sum(m0$residuals)


sum(m0$residuals * x)
sum(m0$residuals * m0$fitted.values)
m0$fitted.values

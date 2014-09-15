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

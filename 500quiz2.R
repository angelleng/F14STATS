##### 500 quiz 2 ##### 
library(faraway)
head(fat)

rmse <- function(x,y) sqrt(mean((x-y)^2)) 

disc <- names(fat) %in% c("brozek", "density", "free") 
c(rep(T, 9), F)
fat.tr <- fat[c(rep(T, 9), F), !disc]
fat.ts <- fat[c(rep(F, 9), T), !disc]
head(fat.ts)
dim(fat)
dim(fat.ts)

## linear regression 
m1 <- lm(siri ~ ., data = fat.tr)
summary(m1)  
m1.rmse.tr <- rmse(m1$fitted.values, fat.tr$siri); m1.rmse.tr
ypred <- predict(m1, fat.ts[-1])
m1.rmse.ts <- rmse(ypred, fat.ts$siri); m1.rmse.ts

## linear regression with variable selecting using AIC 
m2 <- step(m1)
summary(m2)
m2.rmse.tr <- rmse(m2$fitted.values, fat.tr$siri); m2.rmse.tr
ypred <- predict(m2, fat.ts[-1])
m2.rmse.ts <- rmse(ypred, fat.ts$siri); m2.rmse.ts

## Principal component regression 
library(pls)
m3 <- pcr(siri ~ ., data=fat.tr, ncomp=14) 

rmsemeat <- NULL 
for (k in 1:14) {
  pv <- predict(m3, newdata=fat.ts, ncomp=k)
  rmsemeat[k] <- rmse(pv, fat.ts$siri) 
}

min(rmsemeat)
which.min(rmsemeat)

plot(rmsemeat, xlab="PC number", ylab="Test RMSE")

m4 <- pcr(siri ~ ., data=fat.tr, ncomp=14,
             validation="CV", segments = 10)
rmseCV <- RMSEP(m4, estimate="CV", intercept=F)
which.min(rmseCV$val)
m4.rmse.tr <- min(rmseCV$val)
plot(rmseCV$val, xlab="PC number", ylab="CV RMSEP")

yfit <- predict(m4, newdata=fat.ts, ncomp=which.min(rmseCV$val))
m4.rmse.ts <- rmse(yfit, fat.ts$siri); m4.rmse.ts

##  Partial least squares regression
m5 <- plsr(siri ~., data = fat.tr, ncomp = 14, validation = "CV")
summary(m5)
pls_rmsCV <- RMSEP(m5, estimate="CV", intercept=F)

plot(pls_rmsCV$val, xlab="Number of components",
     ylab="CV RMS")
m5.rmse.tr <- min(pls_rmsCV$val); m5.rmse.tr
which.min(pls_rmsCV$val)
ytpred <- predict(m5, fat.ts, ncomp=which.min(pls_rmsCV$val))
m5.rmse.ts <- rmse(ytpred, fat.ts$siri); m5.rmse.ts

## Ridge regression 
library(MASS)
m6 = lm.ridge(siri ~ ., lambda=seq(0, 6, 0.1),
                  data = fat.tr)
select(m6)
matplot(m6$lambda, t(m6$coef), type="l", lty=1,
        xlab=expression(lambda), ylab=expression(hat(beta)))
abline(v=1.1)
which.min(m6$GCV)

yfit <- m6$ym + scale(fat.tr[,-1], center=m6$xm,
                           scale=m6$scales ) %*% m6$coef[, 12]
m6.rmse.tr <- rmse(yfit, fat.tr$siri); m6.rmse.tr

ypred <- m6$ym + scale(fat.ts[,-1], center=m6$xm, 
                           scale = m6$scales ) %*% m6$coef[,12] 
m6.rmse.ts <- rmse(ypred, fat.ts$siri); m6.rmse.ts





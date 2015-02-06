#### 406 hw 9 #### 
## 1 
### a 
airline <- read.table('airlines.dta', head = F)
head(airline)
airline$V3 <- airline$V3/1000
lamhat <- sum(airline[2])/sum(airline[3])
B = 200 
n = nrow(airline)
lamhati <- numeric(B)
for ( i in 1:B) {
  bs <- rpois(n, lamhat * airline[[3]])
  lamhati[i] <- sum(bs)/sum(airline[3])
}
# bias <- 1/B*sum(lamhati) - lamhat
bias <- mean(lamhati - lamhat)
mse <- 1/B* sum((lamhati - lamhat)^2)
mse <- mean((lamhati - lamhat)^2)
bias;mse
hist(lamhati)


### b 
B=2000 
lamhati=numeric(B)
for (i in 1:B){
  U=1+n*runif(n,0,1); S=airline[floor(U), ]
  lamhati[i] <- sum(S[2])/sum(S[3])
}
hist(lamhati - lamhat,30,main='hist.',xlab='',ylab='')
mean(lamhati - lamhat)  
mse <- mean((lamhati - lamhat)^2)
bias; mse


### c 

B = 2000 
lamhati = numeric(B)


trial <- function(a, b) {
  beta0 <- b
  alpha0 <- a
  betastar <- beta0 + sum(airline[3])
  alphastar <- alpha0 + sum(airline[2])
  
  mean <- alphastar/betastar
  var <- alphastar/(betastar^2)
  mean;var
  
  xgrid <- seq(mean - 4*var, mean + 4*var, length.out = 100) 
  y <- dgamma(xgrid, shape = alphastar, scale = 1/betastar)
  plot(xgrid, y, 'l', main = paste0("alpha =", a, ", beta = ", b))
  return (c(a, b, mean, var))
}

a <- c(1, 200, 400, 1000)
b <- c(1, 20, 50, 100)
abgrid <- expand.grid(a, b)
par(mfrow = c(2, 2))
miumiu <- sapply(1:16, function(i) {trial(abgrid[i,1], abgrid[i,2])})
table <- data.frame(alpha = miumiu[1,], beta = miumiu[2, ], mean = miumiu[3, ], var = miumiu[4,])

### d 
func <- function(a, b) {
  beta0 <- b
  alpha0 <- a
  betastar <- beta0 + sum(airline[3])
  alphastar <- alpha0 + sum(airline[2])
  lami <- rexp(n, 40)
  weight <- lami^(alphastar-1)*exp(-(betastar-1)*lami)
  sum(weight)
  mean <- mean(lami*weight/sum(weight))
  var <- var(lami*weight/sum(weight))  
  return (c(a, b, mean, var))
}


miumiu <- sapply(1:16, function(i) {func(abgrid[i,1], abgrid[i,2])})
table <- data.frame(alpha = miumiu[1,], beta = miumiu[2, ], mean = miumiu[3, ], var = miumiu[4,])


func(abgrid[3, 1], abgrid[3, 2])

## 2 
LAI = read.table("LAI.csv", sep = ",")
LAI = as.matrix(LAI)

T = dim(LAI)[1] 
Tlist = 1:T 
LAI = sapply(Tlist, function(i) matrix(LAI[i,], nrow=120, ncol=60, byrow=T), 
             simplify="array")
library(fields) 

L = (LAI[,,7] > 0) * 3
image.plot(L, horizontal = T)
pool <- which(L > 0)
T = 0
# begein of loop 
par(mfrow = c(2,3))
repeat {
  x <- sample(pool, 1) 
  
  repeat {
    if (L[x] == 3) L[x] = 1 
    pool <- pool[pool != x] 
    if (length(pool) == 0) break  
    if (x <= 120 | x == 1) {
      possible <- c(1, -1, 120) +x 
    }else possible <- c(-1, 1, 120, -120) +x 
    
    if (sum(L[possible] == 3) != 0)  {# find new point {
      newx <- ifelse(sum(L[possible] == 3) == 1, possible[L[possible] == 3], sample(possible[L[possible] == 3], 1)) 
    } else {
      break   
    }
    
    x = newx 
    T = T + 1
    if (sum(T == c(50, 100, 150, 300, 1000, 2000)) == 1) {
      
      image.plot(L, horizontal = T)
      
    }
 
  }
    if (T >= 3000)break
  if (length(pool) == 0) break 
}

image.plot(L, horizontal = T)




# 
# if (x%%60 == 1) {
#   if (x ==1) {
#     movement <- sample(c(1, 60), 1)
#   }
#   else if (x == 7141) {
#     movement <- sample(c(1, -60), 1)
#   }
#   else {
#   movement <- sample(c(1, 60, -60), 1)
#   }
# }
# else if (x %% 60 == 0) {
#   if (x == 60) {
#     movement <- sample(c(-1, 60), 1)
#   }
#   else if (x == 120*60) {
#     movement <- sample(c(-1, -60))
#   }
# }
# 406 hw 10 
library(scatterplot3d)
mix <- read.table("mixgauss.dat")

c1 <- c(-2, -2, -4) # observed centers
c2 <- c(-4, -2, 2)
c3 <- c(4, 6, 2)

centroids <- rbind(c1, c2, c3)


centroids <- rbind(rep(0,3), rep(1, 3), rep(2, 3))



classify <- function(x) {
  dis <- apply(centroids, 1, function(y) sqrt(sum((x - y)^2))); 
  which(dis == min(dis))
}


label <- apply(mix, 1, classify )

label_old <- rep(1, 300)
i = 0
obj = c()
while (1) {
  label <- apply(mix, 1, classify)  
  if (sum(label != label_old) == 0) break 
  centroids[1,] <- apply(mix[label==1,], 2, mean) 
  centroids[2,] <- apply(mix[label==2,], 2, mean)
  centroids[3,] <- apply(mix[label==3,], 2, mean)
  label_old <- label
  i = i+1  
  obj = c(obj, sum(sapply(1:3, function(i) sum(apply(mix[label == i, ], 1, function(y) sqrt(sum((centroids[i,] - y)^2)))))))
  
}

# obj = sum(sapply(1:3, function(i) sum(apply(mix[label == i, ], 1, function(y) sqrt(sum((centroids[i,] - y)^2))))))


scatterplot3d(mix[,1:3], color = label + 1, pch = ifelse(label < 3, label -1, label + 2))
# 
# plot3d(mix, type = 'p', col = label + 1, size = 10) 
# plot3d(centers, type = 'p', col = 'black', size = 20, add = T)

centroids <- rbind(rep(0,3), rep(1, 3), rep(2, 3)) 



### 2 
c1 <- c(-2, -2, -4) # observed centers
c2 <- c(-4, -2, 2)
c3 <- c(4, 6, 2)
centroids <- rbind(c1, c2, c3)
centroids <- rbind(rep(0,3), rep(1, 3), rep(2, 3)) 


pi <- rep(1/3, 3)
mu <- centroids
Sig <- array(rep(diag(3), 3), c(3, 3, 3) )

library(MASS)
library(mvtnorm)
tao = matrix(0, 300, 3)
for (l in 1: 30) {
  for (n in 1:300) {
    fenmu <- sum(sapply(1:3, function(k) pi[k]*dmvnorm(as.numeric(mix[n,]), as.numeric(mu[k,]), Sig[,,k])))
    for (k in 1:3)
      tao[n,k] = pi[k]*dmvnorm(mix[n,], mu[k,], Sig[,,k])/fenmu
  }
  
  for (k in 1:3) {
    fenmu = sum(tao[,k])
    mu[k,] =rowSums(sapply(1:300, function(i) tao[i, k] * as.numeric(mix[i,]))) / fenmu 
    Sig[,,k] = rowSums(sapply(1:300, function(i) tao[i,k] * as.numeric(mix[i,] - mu[k,])  %*% t(as.numeric(mix[i,] - mu[k,])))) / fenmu
    pi[k] = 1/300*fenmu 
  }
}

centroids <- mu  

label <- apply(mix, 1, classify )

label

scatterplot3d(mix[,1:3], color = label + 1, pch = ifelse(label < 3, label -1, label + 2))
# 
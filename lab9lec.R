library(gtools) 
mix = rdirichlet(1,c(1,1,1))
M = list(8, 12)
mu = lapply(M, function(m) matrix(runif(6, -m, m),ncol=3))
A = replicate(3, matrix(runif(4, -2, 2),nrow=2), simplify = F)

N = 300
S = 4 #number of samples to generate
d = 2 #2-dimensional Gaussian components
Z <- sample(seq(3), size = N, replace = T, prob = mix)

R <- lapply(1:length(M), function(m) replicate(S, sapply(Z, function(i) c(mu[[m]][,i] + t(A[[i]])%*%rnorm(d, 0, 1), i), simplify = T), simplify = "array"))
R <- lapply(1:length(M), function(m) replicate(S, sapply(Z, function(i) c(mu[[m]][,i] + t(A[[i]])%*%rnorm(d, 0, 1), i), simplify = T), simplify = "array"))



class(R)
class(R[[1]])
sapply(R, dim)
apply(R[[1]], 3, dim)
R[[1]][,,1][3,] #this shows the third row of the first matrix in the first array entry of list R.

for(m in 1:length(M)){
#   pdf(paste("plot", m, ".pdf", sep = ""))
  par(mfrow=c(2,2))
  for(i in 1:S){
    plot(R[[m]][,,i][1,(R[[m]][,,i][3,]==1)], R[[2]][,,i][2,(R[[m]][,,i][3,]==1)],
         xlim=c(-14,14), ylim=c(-14,14), col=1, main=paste("Gaussian Mixture with
M=", M[[m]]))
    points(R[[m]][,,i][1,(R[[m]][,,i][3,]==2)], R[[2]][,,i][2,(R[[m]][,,i][3,]==2)], col=2)
    points(R[[m]][,,i][1,(R[[m]][,,i][3,]==3)], R[[2]][,,i][2,(R[[m]][,,i][3,]==3)], col=3)
  }
  print("heelow")
}


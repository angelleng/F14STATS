
library(gtools)
mix = rdirichlet(1,c(1,1,1))
M = list(8, 12)
mu = lapply(M, function(m) matrix(runif(6, -m, m),ncol=3))
A = replicate(3, matrix(runif(9, -1, 1),nrow=3), simplify = F)
replicate(3, matrix(1:9, nrow = 3), simplify = F)

N = 300
S = 4
d = 2
Z = sample(seq(3), size = N, replace = T, prob = mix)
R = sapply(1:length(M), function(m) replicate(S, sapply(Z, function(i) c(mu[[m]][,i] + t(A[[i]])%*%rnorm(d, 0, 1), i), simplify = T), simplify = "array"), simplify = "array")

for(m in 1:length(M)){
	par(mfrow=c(2,2))
	for(i in 1:S){
    	plot(R[1,(R[3,,i,m]==1),i,m], R[2,(R[3,,i,m]==1),i,m], xlim=c(-18,18), ylim=c(-18,18), col=1)
		points(R[1,(R[3,,i,m]==2),i,m], R[2,(R[3,,i,m]==2),i,m], col=2)
		points(R[1,(R[3,,i,m]==3),i,m], R[2,(R[3,,i,m]==3),i,m], col=3)
		
	}
}
A[[3]] 
summary(R)
dim(R)
R[3,,1,1]

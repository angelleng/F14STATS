n = 100
lambda = 3
  
par(mfrow = c(1,1))
u <- runif(n)
x <- 2/sqrt(1-u) 
  
hist(x, prob = TRUE)
y = seq(0,10,length = 1000)
lines(y, 8*y^-3)

# n = 10
# k = 5
# lambda = 3
# # x = matrix(-(1/lambda)*log(runif(n*k)), ncol=k)
# x = matrix( 2/sqrt(1 - runif(n*k)), ncol = k) 
# g = 

n = c(10, 20, 50, 100, 200, 1000)
g = list()
k = 5
lambda = 3
g = lapply(n, function(i) matrix(-(1/lambda)*log(runif(i*k)), ncol=k))
class(g) #to see what is the data structure used for g
sapply(g, dim) #check the dimension of each matrix in g
## the following will get the gamma random numbers of different sizes n. ##
gamma = lapply(g, function(M) apply(M, 1, sum))
class(gamma) #to see what is the data structure
sapply(gamma, length) #check the dimension of each vector in g


#continue with gamma from the output of part (c)
grid = 1000
y = seq(0,10,length = grid)
plot(0,xlim=c(0,10),ylim=c(0,1),xlab="x",ylab="Density",main="Gamma density estimation", type='n')
par(mfrow=c(2,3))
for(i in 1:6){
  di = density(gamma[[i]],from=0,to=10, n = grid, bw=0.2)
  plot(di, xlim=c(0,10), ylim=c(0,1), axes=F, col=rainbow(6)[i], lwd = 2, main="Gamma density estimation")
   lines(y, 0, col = 'orange', lty = 3, lwd = 2)
}

names(di) #check to see what are in the density estimator output di, notice di$x and di$y
##multiple plot in R : http://cran.r-project.org/doc/contrib/Lemon-kickstart/kr_addat.html
MeanDensDiff <- function(gamma_i){
  grid = 1000
  z = seq(0,10,length = grid)
  d <- density(gamma_i,from=2,to=10, n=grid, bw=.2)
  return(mean(abs(d$y -dgamma(z, 5, 3))))
}


prt <- pareto[[1]]
den <- density(prt, from = 2, to = 10)
diff = mean(abs(den$y - 8*den$x^-3))


meandiff = lapply(gamma, MeanDensDiff)
plot(n, meandiff, xlab = 'n - sample size used to estimate density', ylab = 'mean difference', main =
       'mean difference of density estimator and dGamma(5,3)')

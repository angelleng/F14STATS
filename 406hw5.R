######## 406 hw5 ########
u = runif(1000)
x <- 2/sqrt(1-u) 
hist(x, prob = T)
y <- seq(2, max(x), 0.1)
lines(y, 8*y^-3)
# 
# n <- c( 10, 20, 50, 100, 200, 1000, 5000)
# par(mfrow = c(2,4))
# for (i in 1:length(n)) {
#   u <- runif(n[i])
#   x <- 2/sqrt(1-u) 
#   hist(x, prob = T)
#   y <- seq(2, max(x), 0.1)
#   lines(y, 8*y^-3)
# }
# 
# u <- runif(1000)
# x <- 2/sqrt(1-u) 


n = c(10, 20, 50, 100, 200, 1000)
pareto = lapply(n, function(i) 2/sqrt(1-runif(i)) )
sapply(pareto, length)

grid = 1000
y = seq(0,10,length = grid)
plot(0,xlim=c(0,10),ylim=c(0,1),xlab="x",ylab="Density",main="Gamma density estimation", type='n')
par(mfrow=c(2,3))
for(i in 1:6){
  di = density(pareto[[i]],from=0,to=max(pareto[[i]]), n = grid, bw=0.2)
  plot(di, xlim=c(0,10), ylim=c(0,1), col=rainbow(6)[i], lwd = 2, main="Pareto density estimation")
  lines(y, 8*y^-3, col = 'orange', lty = 3, lwd = 2)
}

MeanDensDiff <- function(pareto_i){
  grid = 1000
  z = seq(0,10,length = grid)
  d <- density(pareto_i,from=2 ,to=10, n=grid, bw=.2)
  return(mean(abs( d$y - 8*d$x^-3 )))
}
meandiff = lapply(pareto, MeanDensDiff)
plot(n, meandiff, xlab = 'n - sample size used to estimate density', ylab = 'mean difference', main =
       'mean difference of density estimator and ideal den') 

########## problem 2 ##########
k = 10 
n <- c(10, 50, 100, 500, 1000)
p <- c(rep(1/k^2, k), 1-1/k)

gen <- function(m) {
  random <- integer(m)
  for (j in 1:m) {
  u <- runif(1)
  
  i = 1
  s = p[i]
  while (s < u) {
    i = i+1
    s = s + p[i]
  }
  random[j] = i
  }
  return (random)
}

time <- sapply(n, function(i) system.time(gen(i))) 
sp <- lapply(n, gen)

sp <- lapply(sp, function(i) i = factor(i, levels(i) <- c(1:(k+1))))

frame()
a <- lapply(sp, function(i) {tb <- prop.table(table(i)); par(new = T); barplot(tb, ylim = c(0,1)) }) 
points( p)
par(new = T) 

true <- matrix(p, nrow = 1)
colnames(true) <- 1:(k+1)
barplot(true, ylim = c(0,1))

### c
k <- 1000
p <- c(rep(1/k^2, k), 1-1/k)
n <- c(100, 1000, 10000)
sapply(n, function(i) system.time(gen(i))) 

newgen <- function(m) {
  random <- integer(m)
  cp <- sapply(1:(k+1), function(a) sum(p[1:a]))
  
  
  for (j in 1:m) {
    u <- runif(1)
    ind = 0
    for (i in 1:(k+1)) {
      if (u >= cp[i]) {
        ind = i
        break 
      }
    }
    random[j] = ind 
  }
  return (random)
}

sapply(n, function(i) system.time(newgen(i))) 



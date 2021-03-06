#406 hw 4 
######## problem 1 ########
phim <- function(Xseq, m) {
  f <- function(x) {
    return (1/m * sum((Xseq <= x)[1:m])); 
  }
  return (f)
}
# plot 

Xseq <- rnorm(200)
x <- seq(-3.5,3.5,0.1)
cdf <- pnorm(x)
mseq <- c(5, 10, 20, 50, 100)

par(mfrow = c(2, 3))
for (j in 1:length(mseq)) {
  y <- c() 
  for (i in 1:length(x)) {
    y <- c(y, phim(Xseq, mseq[j])(x[i])) 
  }
  plot(x, y) 
  lines(x, cdf) 
  
  print(max(abs(y - cdf))) 
}
par(mfrow = c(1, 1))

######## problem 2 ########
LAI <- read.csv('LAI.csv', head = F) 
LAI = as.matrix(LAI) #convert to matrix for later use of apply() function to the data set.

library(fields)
T = dim(LAI)[1] # 72 time points
Tlist = 1:T
LAI = sapply(Tlist, function(i) matrix(LAI[i,],nrow=120,ncol=60,byrow=T),simplify="array")
dim(LAI) # check the dimension of the converted data.

nseq = c(5, 10, 20, 50) 

L = (LAI[,,7] > 0) * 3

par(mfrow = c(2, 2))
for (j in 1:length(nseq)) { 
  
  LAI1 = L
  
  i = 0 
  while (i <= nseq[j]){
    
    rindex = sample(1:120,1,replace=FALSE)
    cindex = sample(1:60,1,replace=FALSE)
    if (LAI1[rindex, cindex] == 3) {
      i = i+1
      LAI1[rindex, cindex] = 10
    }
  }
  image.plot(LAI1)
}
par(mfrow = c(1,1 ))

nonocean.id = which(L!=0, arr.ind = TRUE)
AccLAI = array(0, c(dim(nonocean.id)[1],6))
for (i in 1:dim(nonocean.id)[1]){
  for (j in 0:5){
    AccLAI[i,j+1] = sum(LAI[nonocean.id[i, 1], nonocean.id[i, 2], (j*12 + 1):(j*12+12)])
  }
}
# plot the histogram
hist(as.numeric(AccLAI),breaks=100,freq=FALSE,col=4,main="histogram of accumualted LAI",xlab="accumulated LAI")

### c
nseq = c(5, 10, 20, 50, 100, 200, 500, 1000) 
par(mfrow = c(3,3))
for (i in 1:length(nseq)) {
    sampled.nonocean = sample(1:dim(nonocean.id)[1],size=nseq[i],replace=FALSE)
    print(abs(mean(AccLAI[sampled.nonocean,]) - mean(AccLAI)))
}

###d 

nseq = c(5, 10, 20, 50, 100, 200, 500, 1000) 
par(mfrow = c(3,3))
for (i in 1:length(nseq)) {
  average.sample <- c() 
  for (j in 1:1000) {
    sampled.nonocean = sample(1:dim(nonocean.id)[1],size=nseq[i],replace=FALSE)
    # print(abs(mean(AccLAI[sampled.nonocean,]) - mean(AccLAI)))
    average.sample <- c(average.sample, mean(AccLAI[sampled.nonocean,]))
  }
  hist(average.sample, freq = F, col = 4, xlim = c(0, 40), main = paste("n = ", nseq[i]) )
}



## STATS 406 hw
setwd("~/Documents/GoBlue/F14/F14R")
mix <- read.table("mixgauss.dat")

# 1 
LAI = read.table("LAI.csv", sep = ",")
LAI = as.matrix(LAI)

T = dim(LAI)[1] 
Tlist = 1:T 
LAI = sapply(Tlist, function(i) matrix(LAI[i,], nrow=120, ncol=60, byrow=T), 
simplify="array")

library(fields) 
LAIsp <- LAI[,,7]
### a 
bound <- array(0, c(120, 60))


for (i in 1:120) {
  for (j in 1:60) {
    if (LAIsp[i, j] == 0 && sum(LAIsp[(i-1):min((i+1), 120), (j-1):min((j+1), 60)]) > 0 ) {
      bound[i, j] <- 1 
    }
  }
}


image.plot(bound, horizontal = T)
image.plot(LAI[,,7], horizontal = T)

LAIsp[, 60]

### b
map.div <- array(0, c(120, 60))
# use breadth first search to markup the ocean, the rest of waters is lake. 

L = (LAI[,,7] > 0) * 3
image.plot(L, horizontal = T)

n = dim(L)[1]
m = dim(L)[2]

unexplored = array(0, c(120, 60))
crt_r = c(1) #row index
crt_c = c(1) #column index
unexplored[1,1] = 1

step_r = c(-1, 1, 0, 0) #searching direction in row
step_c = c(0, 0, -1, 1) #searching direction in column

par(new = T)
while (length(crt_r) != 0){
  nbh_r = nbh_c = c()
  for (i in 1:length(crt_r)){
    L[crt_r[i],crt_c[i]] = 1.3 #grean ocean:)
    for (j in 1:4){ #search for 4 directions
      next_r = min(max(crt_r[i]+step_r[j], 1), n) #row index of the new point
      next_c = min(max(crt_c[i]+step_c[j], 1), m) #column index of the new point
      if(L[next_r, next_c] == 0 & unexplored[next_r, next_c] == 0){
        nbh_r = c(nbh_r, next_r)
        nbh_c = c(nbh_c, next_c)
        unexplored[next_r, next_c] = 1
      }
    }
  }
  crt_r = nbh_r  
  crt_c = nbh_c
  image.plot(L) #plot the root points in this search phase
  Sys.sleep(0.1)
}

# 2 
mysort <- function (x, y, z) {
  if (x > y) {
    t = x 
    x = y
    y = t
  }
  if (y > z) {
    t = y
    y = z
    z = t
    if (x > y) {
      t = x 
      x = y
      y = t
    }
  }
  return (c(x, y, z))
}

mysort(1,5,2)

mysort2 <- function (x) {
  
  for (j in 1:(length(x) -1) ) {
    
    for (i in 1:(length(x) - j)) {
      if (x[i] > x[i+1]) {
        t = x[i]
        x[i] = x[i+1] 
        x[i+1] = t
      }
    }
  }
  return (x)
} 

mysort2(c(10,6,3,12,5,3,5,6,9))

mysort3 <- function (s, d) {
  s <- mysort2(s)
  if (d == 1) {
    return (s)
  }
  if (d == 0) {
    for (i in 1:(length(s)/2)) {
      t = s[i]
      s[i] = s[length(s) - i +1]
      s[length(s) - i + 1] = t
    }
    return (s)
  }
  
  
}
mysort3(c(14, 12, 19, 3, 9, 21, 19), 0)
mysort3(c(14, 12, 19, 3, 9, 21, 19), 1)

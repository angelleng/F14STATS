# 406 hw1 

setwd("~/Documents/GoBlue/F14/F14R")
# problem 2
# a). 
vote <- read.table("Pairwise.dat")
rowSums(vote)
# b). 
simmat <- matrix(numeric(0), 4, 4)
for (i in 1:4 ) {
  for (j in 1:4) {
    simmat[i, j] = (sum(vote[i,] * vote[j, ]) +sum( (1 - vote[i,]) * (1 - vote[j,]))) / (30)    
  }
}
# c).
place <- c(NA, 0, NA, 1) 
place[1] = simmat[1, 2]/(simmat[1, 4] + simmat[1,2])
place[3] = simmat[3, 2]/(simmat[3, 2] + simmat[3, 4]) 

# problem 3
# a). 
rm(list=ls())
load('FrontRange.RData')
stainfo = FR$info

which(stainfo$elev == max(stainfo$elev))
which(stainfo$elev == min(stainfo$elev))

prec36 <- FR$precip[[36]]
prec27 <- FR$precip[[27]]
time36 <- FR$time[[36]]
plot(FR$time[[36]], FR$precip[[36]], main = "rain fall records in station of highest elevation")
plot(FR$time[[27]], FR$precip[[27]], main = "rain fall records in station of lowest elevation")
# b).
sum(FR$precip[[36]] >= 10) / length(FR$precip[[3]])
sum(FR$precip[[27]] >= 10) / length(FR$precip[[3]])

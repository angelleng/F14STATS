# CAUTION: SOME CODES ARE COPIED FROM .PDF FILE, SYMBOLS/CHARACTERS MAY NOT CONFORM TO R.
# CHECKED ALREADY, ALL CODES CAN RUN, EXCEPT FOR THE ABOVE MENTIONED POTENTIAL PROBLEM.

#### Q 1 ####
## a ##
#install.packages('rgl')
library(rgl)

## b ##
ball = read.table('ball.data')
dim(ball)
cloud = read.table('cloud.data')
dim(cloud)
mixed = read.table('mixed.data')
dim(mixed)

## c ##
bx = ball[,1]
by = ball[,2]
bz = ball[,3]
cx = cloud[,1]
cy = cloud[,2]
cz = cloud[,3]
mx = mixed[,1]
my = mixed[,2]
mz = mixed[,3]

## d ##
plot3d(bx, by, bz, col = 'blue')
plot3d(cx, cy, cz)
plot3d(mx, my, mz)

## e ##
plot(mx,my)
plot(my,mz)
plot(mx,mz)

c1 = c(0, 0, 0)
c2 = c(-1, 1, 1)

## f ##
n1 = 0
n2 = 0
mixedlength = dim(mixed)[1]
vecn = function(x) sqrt(sum(x^2))
for (i in seq(mixedlength)){
	d1 = vecn(mixed[i,] - c1)
	d2 = vecn(mixed[i,] - c2)
	if(d1 <= d2){
		n1 = n1 + 1
	}
	else{
		n2 = n2 + 1
	}
	d1 = 0
	d2 = 0
}
n1
n2

#### Q 2 ####
## a ##
LAI = read.table("LAI.csv", sep = ",")
#'.csv' files entries are separated by commas(,). So we need to tell this to R by adding the argument sep=','.
class(LAI)
LAI = as.matrix(LAI) #convert to matrix for later use of apply() function to the data set.
dim(LAI)
LAI[1,]

## b ##
T = dim(LAI)[1] # 72 time points
Tlist = 1:T
LAI = sapply(Tlist, function(i) matrix(LAI[i,],nrow=120,ncol=60,byrow=T),simplify="array")
dim(LAI) # check the dimension of the converted data.

## c ##
monthagg <- apply(LAI, 3, sum) # montly aggregate, along which axis?(3-time) by which function?(sum)
maxmonths <- sapply(0:5, function(i) which.max(monthagg[(12*i+1):(12*i+12)])) #use sapply to get the months with largest LAI.
maxmonths # 7 7 7 7 7 7

## d ##
yearcol = matrix(monthagg,nrow=12)
> #now with the above, using apply(), how do you find the yearly aggregated LAI over the continent?
yragg = apply(yearcol, 2, sum) #along which axis?(2-column), by which function?(sum)

## f ##
install.packages('fields') #first install the 'fields' package in which certain functions can help you produce a heat map.
library('fields') #quote relevant library
image.plot(apply(LAI, c(1, 2), median), horizontal=T, main="median LAI")




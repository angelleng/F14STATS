## define the h function for the Gaussian case
hgauss = function(x){
	h_x = sqrt(pi/2)*(sin(x))^2*(x^2)
	return(h_x)
}

## define the h function for the Weibull case
hweibull = function(x){
	h_x = 0.5*(sin(x))^2*x*exp(x^2/2)
}

## sample path
sample_seq = seq(20,10000,by=100)

## generate the data first
X_gauss = rnorm(max(sample_seq))
X_weibull = rweibull(max(sample_seq),shape=2,scale=1) #k=2,lambda=1#
h_gauss = hgauss(X_gauss)
h_weibull = hweibull(X_weibull)

# initialize a list of two components to save the result
Res = list()
Res$gauss = array(0,c(length(sample_seq),3))
Res$weibull = array(0,c(length(sample_seq),3))

z = qnorm(0.975)  # upper 0.025 quantile

## calculate and plot
for (i in 1:length(sample_seq)){
	subspl_gauss = h_gauss[1:sample_seq[i]]
	subspl_weibull = h_weibull[1:sample_seq[i]]
	
	m_gauss = mean(subspl_gauss)
	m_weibull = mean(subspl_weibull)
	
	se_gauss = sqrt(var(h_gauss)/sample_seq[i])
	se_weibull = sqrt(var(h_weibull)/sample_seq[i])
	
	# save in a row, mean, lower bd, upper bd
	Res$gauss[i,] = c(m_gauss,m_gauss-z*se_gauss,m_gauss+z*se_gauss) 
	Res$weibull[i,] = c(m_weibull,m_weibull-z*se_weibull,m_weibull+z*se_weibull) 
}

C_true = 0.5*(1-3/exp(1)^2)*sqrt(pi/2)
## now plot
par(mfrow=c(1,2))
plot(Res$gauss[,1],type="l",lwd=2,ylab="C",xlab="",ylim=c(-0.9,1.5),col=4)
lines(Res$gauss[,2],lwd=2,lty=2,col=4)
lines(Res$gauss[,3],lwd=2,lty=2,col=4)
abline(h=C_true)
title("Sampling from Gaussian ")
plot(Res$weibull[,1],type='l',lwd=2,col=2,ylab="C",xlab="",ylim=c(-0.9,1.5))
lines(Res$weibull[,2],col=2,lwd=2,lty=2)
lines(Res$weibull[,3],col=2,lwd=2,lty=2)
abline(h=C_true)
title("Sampling from Weibull")

## part 3
# this is omega(x) function
omegafunc = function(x){
	return(2/0.372*(sin(x))^2*exp(-x^2/2+x))
}
Y = rgamma(max(sample_seq),shape=3,rate=1)
Yminus1sq = (Y-1)^2
Y_ind = (1<Y)*(Y<5)
omega_Y = omegafunc(Y)

ResI = array(0,c(length(sample_seq),4))

for (i in 1:length(sample_seq)){
	subspl = Y_ind[1:sample_seq[i]]*omega_Y[1:sample_seq[i]]*Yminus1sq[1:sample_seq[i]]
	m = mean(subspl)
	MC_error = sqrt(var(subspl)/sample_seq[i])
	lb = m-MC_error*z
	ub = m+MC_error*z
	ResI[i,] = c(m,lb,ub,MC_error)
}
plot(ResI[,1],type="l",lwd=2,ylim=c(min(ResI[,-4]),max(ResI[,-4])),xlab="",ylab="I")
lines(ResI[,2],col=2,lwd=2,lty=2)
lines(ResI[,3],col=2,lwd=2,lty=2)
title("Importance Sampling from Gamma(3,1)")















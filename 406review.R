### review 406 ### 
hfun=function(x){
  return((cos(50*x)+sin(20*x))^2)
}
nSple=10000
uniforms=runif(nSple)
hvalues=hfun(uniforms)
Ihat=mean(hvalues) #Monte Carlo estimate
var(hvalues)
se=sqrt(var(hvalues)/nSple) # Monte Carlo error
z=qnorm(0.975)
CI=c(Ihat-z*se,Ihat+z*se) # 95% confid. interv.

#Build a sample path of the estimates
step=100;nseq=seq(from=1,to=nSple,by=100)
K=length(nseq)
alpha=0.975;z=qnorm(alpha)
ResIhat=matrix(NA,ncol=3,nrow=K)
for (i in 1:K){
  subSple=hvalues[1:nseq[i]]
  m=mean(subSple)
  se=sqrt(var(subSple)/length(subSple))
  ResIhat[i,1]=m;
  ResIhat[i,2]=m-se*z;ResIhat[i,3]=m+se*z
}

#Plotting
plot(ResIhat[,1],ylim=c(0.6,1.2),type='l',col='blue'
     ,xlab='',ylab='')
par(new=T)
plot(ResIhat[,2],ylim=c(0.6,1.2),type='l',col='blue',
     xlab='',ylab='')
par(new=T)
plot(ResIhat[,3],ylim=c(0.6,1.2),type='l',col='blue', 
     xlab='',ylab='')
abline(h=0.965,col='red') 



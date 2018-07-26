require(ggplot2)
require(fGarch)
require(flexmix)


#http://stats.stackexchange.com/questions/7440/kl-divergence-between-two-univariate-gaussians
## KL example

KLnormalnormal <- function(m1,m2,s1,s2){
  if(length(m2)!=1 | length(s2)!=1){
    stop("Length m2 or s2 are not 1")
  }
  return(.5 * (log(s2^2/s1^2) - 1 + ((s1^2 + (m1-m2)^2)/s2^2 ) ) ) ### KL divergence manual
}

## build KLnormalnormal fucntion based on mathematical expression for KL divergence between two normal distributions

## 2 distributions that are used, mathematical KL divergence calculated to see if matches with function KLdiv
KLnormalnormal(m1=0,s1=1,m2=0.5,s2=1) #example one KL=0.125
KLnormalnormal(m1=0,s1=1,m2=0,s2=30) #example two KL=2.901753



## function that provides denstity for distribution 1, distribution 2 and the KL div between 1 and 2
KLskewednormal <- function(mu1,mu2,sigma1,sigma2,gamma1,gamma2,x){
  y1 <- dsnorm(x, mean=mu1, sd=sigma1,xi=gamma1, log=F) #density pi(1)
  y2 <- dsnorm(x, mean=mu2, sd=sigma2,xi=gamma2, log=F) # density pi(2)
  y3 <- (dsnorm(x, mean=mu1, sd=sigma1,xi=gamma1, log=TRUE) -
           dsnorm(x, mean=mu2, sd=sigma2,xi=gamma2, log=TRUE)) *
    dsnorm(x, mean=mu1, sd=sigma1,xi=gamma1) # kl divergence KL(pi(1)||pi(2))
  out <- cbind(y1,y2,y3)
  colnames(out) <- c("density pi-1","density pi-2","density kl(pi-1||pi2)")
  return(out)
}

png(file="figure2.png",width=1400,height=700) #to store in png
par(mfrow=c(1,2))
par(mar = c(5, 4, 3, 0.2)) ## setting for legend function


## figure 1
x.axis <- seq(-10,10,by=.001) #get range for x-axis
out <- KLskewednormal(mu1=0,mu2=0.5,sigma1=1,sigma2=1,gamma1 = 1,gamma2 = 1,x=x.axis) 
KLdiv(out[,1:2],eps=1e-8)[1,2] # #.125 exact equal to what should be: 0.125 

plot(x.axis,out[,1],col="red",type="l",xlab="",ylab="density",xlim=c(-10,10),ylim=c(-.1,1.3),lwd=3,cex=2,cex.lab=1.6, cex.axis=1.5)
#polygon(x.axis,out[,1],col="red",density=10)
lines(x.axis,out[,2],col="blue",type="l",lwd=3)
#polygon(x.axis,out[,2],col="blue",density=10)
lines(x.axis,out[,3],col="darkgreen",lwd=3)
polygon(x.axis,out[,3],col="darkgreen",density=10)

## for figure 2
x.axis <- seq(-100,100,by=.001) #get range for x-axis
out <- KLskewednormal(mu1=0,mu2=0,sigma1=1,sigma2=30,gamma1 = 1,gamma2 = 1,x=x.axis) 
KLdiv(out[,1:2],eps=1e-8)[1,2] # #2.900864 should be: 2.901753

plot(x.axis,out[,1],col="red",type="l",xlab="",ylab="density",xlim=c(-10,10),ylim=c(-.1,1.3),lwd=3,cex=2,cex.lab=1.6, cex.axis=1.5)
#polygon(x.axis,out[,1],col="red",density=10)
lines(x.axis,out[,2],col="darkgoldenrod",type="l",lwd=3)
#polygon(x.axis,out[,2],col="darkgoldenrod",density=10)
lines(x.axis,out[,3],col="darkgreen",lwd=3)
polygon(x.axis,out[,3],col="darkgreen",density=10)


## legend function from https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}
add_legend("top", legend=c("Data", "Expert","Benchmark","KL-divergence"), lty=1, 
           col=c("red", "blue","darkgoldenrod","darkgreen"),
           horiz=TRUE, bty='n', cex=2,lwd=2)

dev.off() # get png filled



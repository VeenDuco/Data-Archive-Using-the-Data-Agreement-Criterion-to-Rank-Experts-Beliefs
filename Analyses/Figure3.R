require(fGarch)
setwd("C:/Users/5507553/surfdrive/PhD werk/Projecten/P1 (DAC)/Working directory/DAC part/figures")
##basis
x <- seq(-10,10,by=.001)
y1 <- dsnorm(x,3,1,1)#normal distribution with mean of 3 and sd of 1  (skewness of 1 == normal)

png(file="figure3.png",width=1000,height=1000) #to store in png
par(mfrow=c(2,1))

#plot2 more experts
y5 <- dsnorm(x,.8,2,1)
y6 <- dsnorm(x,.5,.6,1)
y7 <- dsnorm(x,3,4.5,1)
y8 <- dsnorm(x,4.5,.75,1)
plot(x,y1,type="l",col="red",lwd=2,xlab="",ylab="",ylim=c(0,.7),xaxt="n",yaxt="n")
axis(side=1,at=0,labels=0)
lines(x,y5,type="l",col="blue",lwd=2,lty=1)
lines(x,y6,type="l",col="blue",lwd=2,lty=2)
lines(x,y7,type="l",col="blue",lwd=2,lty=3)
lines(x,y8,type="l",col="blue",lwd=2,lty=4)
arrows(x0=x[which.max(y5)],x1=3,y0=y5[which.max(y5)],y1=y5[which.max(y5)],length=.1,code=2) #from x0,y0 to x1,y1 arrowlength in inches, arrowhead both sides
arrows(x0=x[which.max(y6)],x1=3,y0=y6[which.max(y6)],y1=y6[which.max(y6)],length=.1,code=2) 
#arrows(x0=x[which.max(y7)],x1=3,y0=y7[which.max(y7)],y1=y7[which.max(y7)],length=.1,code=2) 
arrows(x0=x[which.max(y8)],x1=3,y0=y8[which.max(y8)],y1=y8[which.max(y8)],length=.1,code=2) 

legend("topright",bty="n",lty=c(1,1,2,3,4),lwd=c(2,2,2,2),col=c("red","blue","blue","blue","blue"),
       legend=c("Data","Expert 1", "Expert 2","Expert 3","Expert 4"),cex=2)
#add legend




# plot 3, experts agree but disagree with data
# might the data be polouted?
y9 <- dsnorm(x,-6,1,1)
y10 <- dsnorm(x,-6.6,1.3,1)
y11 <- dsnorm(x,-6.3,.9,1)
y12 <- dsnorm(x,-7.5,1,1)
y13 <- dsnorm(x,-5.4,.85,1)
y14 <- dsnorm(x,-7.2,.8,1)
plot(x,y1,type="l",col="red",lwd=2,xlab="",ylab="",ylim=c(0,.7),xaxt="n",yaxt="n")
#plot for "data" with beta on x-axis
axis(side=1,at=0,labels=0)
# add zero value lable at xaxis
lines(x,y9,type="l",col="blue",lwd=2)
lines(x,y10,type="l",col="blue",lwd=2)
lines(x,y11,type="l",col="blue",lwd=2)
lines(x,y12,type="l",col="blue",lwd=2)
lines(x,y13,type="l",col="blue",lwd=2)
lines(x,y14,type="l",col="blue",lwd=2)
arrows(x0=-8,x1=-5,y0=y14[which.max(y14)]+.03,y1=y14[which.max(y14)]+.03,length=.1,code=3) 
arrows(x0=-8,x1=3,y0=y14[which.max(y14)]+.08,y1=y14[which.max(y14)]+.08,length=.1,code=2) 
legend("topright",bty="n",lty=c(1,1),lwd=c(2,2),col=c("red","blue"),legend=c("Data","Expert 1-6"),cex=2)
#add legend

dev.off() # get png filled

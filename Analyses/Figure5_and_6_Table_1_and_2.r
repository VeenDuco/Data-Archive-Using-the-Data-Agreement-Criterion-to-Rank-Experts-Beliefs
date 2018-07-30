# note that coda.samples function does not work with set.seed before it, resutls should be stable but will fluctuate a little bit. 
require(fGarch)
require(flexmix)
require(rjags)
## load required packages

#specifying data and expert priors here directy so script can run without source functions
data <- c(3.2144360,4.2319224,0.7292747,1.1946721,2.3536029,2.2197631,0.8950535,4.4319216,1.6593090,1.5847846,0.9710988,1.2760406,3.4996059,
          4.1239381,2.5939061,2.1703336,0.8980953,3.8805930,2.8197607,2.7018905,2.9604446,0.8631145,4.4311612,1.0106424,0.3323181,1.6737576,
          1.4030363,2.4144391,2.4235646,1.2631129,1.2631129,0.8486658,1.6463813,2.9984672,0.9049394,1.8448596,1.1460031,1.5285111,1.0334560,
          1.5330738,1.9589276,1.4129222,2.7893426,0.7566510,3.0007486,2.6623469,1.3163446,3.1110143,2.0866838,3.8798326,1.6159632,3.7840155,
          1.5634919,1.3726181,2.9345892,2.6866814,2.2075958,3.4638646,2.3345915,2.8037912,4.7178521,2.0897256,2.6319288,2.7657685,2.3300288,
          2.1117787,2.9368705,2.1756568,2.7406736,2.5414348,1.9536044,4.6547345,2.8965665,1.8631105,2.4380132,2.5307885,2.3551238,2.9148174,
          0.6281344,2.4402945,2.1239460,2.7094950,1.7650120,1.6304118,3.1209002,4.1566376,2.0775583,2.4676709,2.1954286,2.8250839,2.4296482,
          1.9954294,2.4927658,1.6212864,1.6402977,3.3513176,4.4342030,1.5688151,2.0646306,3.5064500,2.2098772,0.2357405,2.5368721,2.1186228)
#Data
expert_priors <- data.frame(matrix(c(2.15222623,0.09222707,0.77699404,2.15968728,0.06550168,0.82304321,
                          1.9704492,0.1061564,0.8242798,2.3513215,0.1129617,0.9365372),ncol=4,nrow=3))
rownames(expert_priors) <- c("mean","sd","skewness")
colnames(expert_priors) <- c("Expert.1","Expert.2","Expert.3","Expert.4")
#experts' priors

## function to calculate KL divergence between two normal distributions
#http://stats.stackexchange.com/questions/7440/kl-divergence-between-two-univariate-gaussians
KLnormalnormal <- function(m1,m2,s1,s2){
  if(length(m2)!=1 | length(s2)!=1){
    stop("Length m2 or s2 are not 1")
  }
  return(.5 * (log(s2^2/s1^2) - 1 + ((s1^2 + (m1-m2)^2)/s2^2 ) ) ) ### KL divergence manual
}

##function to calculate density of KL-divergence between two skewed normal distributions
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
#to check if this provides same solution using KLdiv function of flexmix package as it would by numerically calculating
# via the mathematical solution
expert0 <- KLskewednormal(mu1=2.285,sigma1=sqrt(.009),gamma1 = 1,mu2=2,sigma2 =.1 ,gamma2 =1 ,x=seq(0,5,by=0.001))
KLdiv(expert0[,1:2],eps=1e-50)[1,2] #4.063
KLnormalnormal(m1=2.285,m2=2,s1=sqrt(.009),s2=.1) #4.063, nice
## approximation using denstiy functions in KLskewednormal function and using the KLdiv function
## results in the same estimate for calculating KL-divergence as numerical calculation, nice results, works fine!

## now a function to calculate density of kl divergence between a (skewed) normal and a uniform, note that the
# uniform is the approximation of the normal, not other way around. 
KLnormaluniform <- function(mu1,sigma1,gamma1,unifmin,unifmax,x){
  y1 <- dsnorm(x, mean=mu1, sd=sigma1,xi=gamma1, log=F) #density pi(1)
  y2 <- dunif(x, unifmin,unifmax, log=F) # density pi(2)
  y3 <- (dsnorm(x, mean=mu1, sd=sigma1,xi=gamma1, log=TRUE) -
           dunif(x, unifmin,unifmax,log=TRUE)) *
    dsnorm(x, mean=mu1, sd=sigma1,xi=gamma1) # kl divergence KL(pi(1)||pi(2))
  out <- cbind(y1,y2,y3)
  colnames(out) <- c("density pi-1","density pi-2","density kl(pi-1||pi2)")
  return(out)
}


## calculating posterior distribution data (normal) and uniform benchmark using rjags (JAGS)

## specifying model within r code using textConnection fucntion.
#https://darrenjw.wordpress.com/2012/10/02/inlining-jags-models-in-r-scripts-for-rjags/
modelstring="
  model {
	for (i in 1:N) {
		x[i] ~ dnorm(mu, tau)
}
mu ~ dunif(0,5)
tau <- pow(sigma, -2)
sigma ~ dunif(0, 100)
}
"
## data is uniformly distributed with mean mu and precision tau
## prior for mu is uniform 0,5
## prior for precision is prior for sd^-2
## prior for sd is uniform 0,100

N <- length(data)
# specify length of data
jags <- jags.model(textConnection(modelstring),
                   data = list('x' = data,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 1000)
# jags.model is used to create an object representing a Bayesian graphical model, 
# specified with a BUGS-language description of the prior distribution, and a set of data.
# burn-in of 1000 samples, 4 chains.
out <- coda.samples(jags,
                    c('mu', 'tau'),
                    25000)
# Extract random samples from the posterior distribution of the parameters of a jags model.
# get sample of 25,000 with 4 chains and burn-in set at 1000, thus 100,000 samples
# monitor the parameters for mu and tau.

# checking output: 
#gelman-rubin statistic, auto correlation, convergence. delete # before following lines to see plots
#gelman.plot(out)
#gelman.diag(out)
#autocorr.plot(out)
#plot(out)

## all seems fine

#summary(out) # get output
post_mean_mu <- summary(out)$statistics[1,1] #get posterior mu for mean parameter
post_mean_sd <- summary(out)$statistics[1,2] #get posterior sd for mean parameter


# specify KL divergence between 2 (skewed) normal distributions, 
# the preferred distribution is the posterior form the data and the uniform benchmark
expert1 <- KLskewednormal(mu1=post_mean_mu,sigma1=post_mean_sd,gamma1 = 1,
                          mu2=expert_priors[1,1],sigma2 =expert_priors[2,1] ,gamma2 =expert_priors[3,1] ,x=seq(0,5,by=0.001))
# approximate is expert 1 prior
KL_expert1 <- KLdiv(expert1[,1:2],eps=1e-50)[1,2] 
# get KL div.


expert2 <- KLskewednormal(mu1=post_mean_mu,sigma1=post_mean_sd,gamma1 = 1,
                          mu2=expert_priors[1,2],sigma2 =expert_priors[2,2] ,gamma2 =expert_priors[3,2] ,x=seq(0,5,by=0.001))
# approximate is expert 2 prior
KL_expert2 <- KLdiv(expert2[,1:2],eps=1e-50)[1,2] 
#get KL div.

expert3 <- KLskewednormal(mu1=post_mean_mu,sigma1=post_mean_sd,gamma1 = 1,
                          mu2=expert_priors[1,3],sigma2 =expert_priors[2,3] ,gamma2 =expert_priors[3,3] ,x=seq(0,5,by=0.001))
# approximate is expert 3 prior
KL_expert3 <- KLdiv(expert3[,1:2],eps=1e-50)[1,2] 
# get KL div.

expert4 <- KLskewednormal(mu1=post_mean_mu,sigma1=post_mean_sd,gamma1 = 1,
                          mu2=expert_priors[1,4],sigma2 =expert_priors[2,4] ,gamma2 =expert_priors[3,4] ,x=seq(0,5,by=0.001))
# approximate is expert 4 prior
KL_expert4 <- KLdiv(expert4[,1:2],eps=1e-50)[1,2] 
# get KL div.

# specify KL divergence between (skewed) normal distributions and uniform distribution 
benchmark <- KLnormaluniform(2.285,sqrt(.009),1,0,5,x=seq(0,5,by=.001))
# approximate is benchmark
KL_benchmark <- KLdiv(benchmark[,1:2],eps=1e-50)[1,2]
# Get KL div.


# get DACd scores
DAC_expert1 <- KL_expert1 / KL_benchmark
DAC_expert2 <- KL_expert2 / KL_benchmark
DAC_expert3 <- KL_expert3 / KL_benchmark
DAC_expert4 <- KL_expert4 / KL_benchmark

DACd <- c(DAC_expert1,DAC_expert2,DAC_expert3,DAC_expert4)






#### KL divergence plots

## legend function from https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=T)
  on.exit(par(opar))
  
  legend(...)
}
setwd(paste0(getwd(),"/Analyses"))
png(file="Figure6.png",width=3200,height=3200,res=300) #to store in png
par(mfrow=c(3,2))
par(mar = c(5, 4, 3, 0.2)) ## setting for legend function

## figure expert 1
x.axis <- seq(0,5,by=.001) #get range for x-axis
plot(x.axis,expert1[,1],col="red",type="l",xlab="",ylab="density",xlim=c(1.5,3),ylim=c(floor(min(expert2)),ceiling(max(expert3))),
     lwd=3,cex.lab=1.5,cex.axis=1.5)
lines(x.axis,expert1[,2],col="blue",type="l",lwd=3)
lines(x.axis,expert1[,3],col="darkgreen",lwd=3)
polygon(x.axis[1000:4000],expert1[1000:4000,3],col="darkgreen",density=10)
legend("topleft","A",bty="n", cex=2)
legend("topright","d = 1",bty="n", cex=2)

#dev.off() # get png filled


#png(file="figure6_2.png",width=800,height=400) #to store in png
## figure expert 2
plot(x.axis,expert2[,1],col="red",type="l",xlab="",ylab="density",xlim=c(1.5,3),ylim=c(floor(min(expert2)),ceiling(max(expert3))),
     lwd=3,cex.lab=1.5,cex.axis=1.5)
lines(x.axis,expert2[,2],col="blue",type="l",lwd=3)
lines(x.axis,expert2[,3],col="darkgreen",lwd=3)
polygon(x.axis[1000:4000],expert2[1000:4000,3],col="darkgreen",density=10)
legend("topleft","B",bty="n", cex=2)
legend("topright","d = 2",bty="n", cex=2)


#dev.off() # get png filled


#png(file="figure6_3.png",width=800,height=400) #to store in png
## figure expert 3
plot(x.axis,expert3[,1],col="red",type="l",xlab="",ylab="density",xlim=c(1.5,3),ylim=c(floor(min(expert2)),ceiling(max(expert3))),
     lwd=3,cex.lab=1.5,cex.axis=1.5)
lines(x.axis,expert3[,2],col="blue",type="l",lwd=3)
lines(x.axis,expert3[,3],col="darkgreen",lwd=3)
polygon(x.axis[1000:4000],expert3[1000:4000,3],col="darkgreen",density=10)
legend("topleft","C",bty="n", cex=2)
legend("topright","d = 3",bty="n", cex=2)

#dev.off() # get png filled


#png(file="figure6_4.png",width=800,height=400) #to store in png
## figure expert 4
plot(x.axis,expert4[,1],col="red",type="l",xlab="",ylab="density",xlim=c(1.5,3),ylim=c(floor(min(expert2)),ceiling(max(expert3))),
     lwd=3,cex.lab=1.5,cex.axis=1.5)
lines(x.axis,expert4[,2],col="blue",type="l",lwd=3)
lines(x.axis,expert4[,3],col="darkgreen",lwd=3)
polygon(x.axis[1000:4000],expert4[1000:4000,3],col="darkgreen",density=10)
legend("topleft","D",bty="n", cex=2)
legend("topright","d = 4",bty="n", cex=2)


#dev.off() # get png filled

#png(file="figure6_bench.png",width=800,height=400) #to store in png
## figure expert 4
plot(x.axis,benchmark[,1],col="red",type="l",xlab="",ylab="density",xlim=c(1.5,3),ylim=c(floor(min(expert2)),ceiling(max(expert3))),
     lwd=3,cex.lab=1.5,cex.axis=1.5)
lines(x.axis,benchmark[,2],col="darkgoldenrod",type="l",lwd=3)
lines(x.axis,benchmark[,3],col="darkgreen",lwd=3)
polygon(x.axis,benchmark[,3],col="darkgreen",density=10)
legend("topleft","E",bty="n", cex=2)



plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n',ylab="",xlab="")
legend("center", legend=c(expression(paste(pi^J,"(",theta,"|y)")),expression(paste(pi[d],"(",theta,")")),
                          expression(paste(pi^J,"(",theta,")")),
                          expression(paste("KL[",pi^J,"(.|y)","||",pi[d],"]"," (panel A-D)")),
                          expression(paste("KL[",pi^J,"(.|y)","||",pi^J,"]"," (panel E)"))), lty=1, 
       col=c("red", "blue","darkgoldenrod","darkgreen", "darkgreen" ),
       horiz=F, bty='n', cex=2.5,lwd=3)

dev.off() # get png filled



png(file="figure5.png",width=800,height=400) #to store in png
## figure expert 4
par(mar = c(3,3,3,3)) ## setting for legend function
plot(x.axis,benchmark[,1],col="red",type="l",xlab="",ylab="density",xlim=c(1.5,3),ylim=c(0,ceiling(max(expert2[,2]))),lwd=3)
lines(x.axis,benchmark[,2],col="darkgoldenrod",type="l",lwd=3)
lines(x.axis,expert1[,2],col="blue",lty=2,lwd=3)
lines(x.axis,expert2[,2],col="blue",lty=3,lwd=3)
lines(x.axis,expert3[,2],col="blue",lty=4,lwd=3)
lines(x.axis,expert4[,2],col="blue",lty=5,lwd=3)

legend("topright",bty="n",lty=c(1,1,2,3,4,5),lwd=c(2,2,2,2,2,2),col=c("red","darkgoldenrod","blue","blue","blue","blue"),
       legend=c(expression(paste(pi^J,"(",theta,"|y)")),expression(paste(pi^J,"(",theta,")")),
                expression(paste(pi[1],"(",theta,")")),expression(paste(pi[2],"(",theta,")")),expression(paste(pi[3],"(",theta,")")),
                expression(paste(pi[4],"(",theta,")"))),cex=1.5,inset=c(0.05,0,0,0))

dev.off() # get png filled



### get tables exported
#table 1:
write.csv(signif(t(expert_priors),3),"table1.csv")

#table 2:
KL <- c(KL_expert1,KL_expert2,KL_expert3,KL_expert4,KL_benchmark)
table2 <- cbind(KL,c(DACd,NA),c(rank(DACd),NA))
colnames(table2) <- c("KL divergence","DACd","Ranking")
rownames(table2) <- c("Expert 1","Expert 2","Expert 3","Expert 4","Benchmark")
write.csv(table2,"table2.csv")

#save global environment to store post mean en posterior sd for mu so results are replicable given that distribution
#this is because JAGS does not work with set.seed.
save.image(file="Figure5_and_6_Table_1_and_2.RData")
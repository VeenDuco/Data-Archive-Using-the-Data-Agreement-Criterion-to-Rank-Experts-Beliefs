require(rjags)
require(plotly)
require(flexmix)
require(fGarch)
#We show what happens to the DAC_d values if experts would represent their prior beliefs by a normal distribution. 
#All panels use the same data, 100 samples drawn from a standard normal distribution. 
#The x-axis represents the distance from the sample mean for the mean the expert's normal distribution, 
#the y-axis represents the (un)certainty of the experts, expressed as the variance of the normal distribution 
#and the z-axis shows the resulting DAC_d values.


## function to calculate KL div for 2 normals
KLnormalnormal <- function(m1,m2,s1,s2){
  if(length(m2)!=1 | length(s2)!=1){
    stop("Length m2 or s2 are not 1")
  }
  return(.5 * (log(s2^2/s1^2) - 1 + ((s1^2 + (m1-m2)^2)/s2^2 ) ) ) ### KL divergence manual
}

## function to get kl density for normal and uniform
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




## DATA
set.seed(13213455)
sim.data <- rnorm(100,0,1)
#All panels use the same data, 100 samples drawn from a standard normal distribution.



## Fictive experts for simulations
sd <- seq(0.1,3,by=.1)
distance.to.mean <- seq(-5,5,by=.1)
#experts would represent their prior beliefs by a normal distribution. 





#The benchmark prior in panel A is a normal distribution with a mean of 0 and a variance of 10000, 

## getting the posterior from data and benchmark using rjags (JAGS)
## calculating posterior distribution data (normal) and uniform benchmark 

## specifying model within r code using textConnection fucntion.
#https://darrenjw.wordpress.com/2012/10/02/inlining-jags-models-in-r-scripts-for-rjags/
modelstring.panel.a="
model {
for (i in 1:N) {
x[i] ~ dnorm(mu, tau)
}
mu ~ dnorm(0,.0001)
tau <- pow(sigma, -2)
sigma ~ dunif(0, 100)
}
"
## data is uniformly distributed with mean mu and precision tau
## prior for mu is normal with mean 0 and precicions .0001 == var of 10000
## prior for precision is prior for sd^-2
## prior for sd is uniform 0,100

N <- length(sim.data)
# specify length of data
jags.panel.a <- jags.model(textConnection(modelstring.panel.a),
                   data = list('x' = sim.data,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 1000)
# jags.model is used to create an object representing a Bayesian graphical model, 
# specified with a BUGS-language description of the prior distribution, and a set of data.
# burn-in of 1000 samples, 4 chains.
out.panel.a <- coda.samples(jags.panel.a,
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
post_mean_mu.panel.a <- summary(out.panel.a)$statistics[1,1] #get posterior mu for mean parameter
post_mean_sd.panel.a <- summary(out.panel.a)$statistics[1,2] #get posterior sd for mean parameter

## get KL divergence between posterior and benchmark

benchmark.kl.panel.a <- KLnormalnormal(m1=post_mean_mu.panel.a,s1=post_mean_sd.panel.a,
                                       m2=0,s2=sqrt(10000))

expert.kl.panel.a <- matrix(NA,nrow=30,ncol=101)
#nrow for sd seq(1,10,by=.1), ncol for distance to mean seq(-5,5,by=.1)
for(ii in 1:101){
for(i in 1:30){
expert.kl.panel.a[i,ii] <- KLnormalnormal(m1=post_mean_mu.panel.a,s1=post_mean_sd.panel.a,
                                          m2=(mean(sim.data)+seq(-5,5,by=.1)[ii]),
                                          s2=seq(0.1,3,by=.1)[i])
}
}
DAC.panel.a <- expert.kl.panel.a / benchmark.kl.panel.a


#plot panel a 
{
trace1 <- list(
  x = distance.to.mean, 
  y = sd, 
  z = DAC.panel.a, 
  autocolorscale = FALSE, 
  cauto = FALSE, 
  cmax = 2, 
  cmin = 0, 
  colorbar = list(
    x = 0.710443070582, 
    y = 0.601226993865, 
    outlinewidth = 0, 
    showticklabels = TRUE, 
    thickness = 30, 
    thicknessmode = "pixels", 
    ticklen = 42, 
    ticks = "", 
    tickwidth = 1, 
    title = "", 
    ypad = 225
  ), 
  colorscale = list(c(0, "rgb(0,0,155)"),list(0.35, "rgb(0,108,255)"),list(0.5, "rgb(98,255,146)"),list(0.6, "rgb(255,147,0)"),list(0.7, "rgb(255,47,0)"),list(1, "rgb(216,0,0)")), 
  name = "A", 
  opacity = 1, 
  reversescale = FALSE, 
  showscale = TRUE, 
  type = "surface", 
  uid = "8317b3", 
  xsrc = "ducoveen:3:3558a7", 
  ysrc = "ducoveen:1:d606dd", 
  zsrc = "ducoveen:0:484c39,552091,aac067,da133b,8bf81e,ba90ad,3c934b,3e50ce,d723cf,fa4077,1d6c52,bc2a5d,1e69e8,52f1b5,d58dd4"
)
data <- list(trace1)
layout <- list(
  autosize = TRUE, 
  hovermode = "closest", 
  scene = list(
    aspectratio = list(
      x = 1, 
      y = 1, 
      z = 1
    ), 
    camera = list(
      center = list(
        x = 0, 
        y = 0, 
        z = 0
      ), 
      eye = list(
        x = -0.558638018887, 
        y = 1.96129585775, 
        z = 0.0279768702355
      ), 
      up = list(
        x = 0, 
        y = 0, 
        z = 1
      )
    ), 
    xaxis = list(
      showspikes = TRUE, 
      spikesides = TRUE, 
      spikethickness = 2, 
      title = "Distance to mean of data", 
      type = "linear"
    ), 
    yaxis = list(
      title = "sd", 
      type = "linear"
    ), 
    zaxis = list(
      autorange = FALSE, 
      range = c(-1, 4), 
      title = "DAC", 
      titlefont = list(size = 13)
    )
  ), 
  title = "Click to enter Plot title", 
  xaxis = list(title = "D"), 
  yaxis = list(title = "E")
)
p <- plot_ly()
p <- add_trace(p, x=trace1$x, y=trace1$y, z=trace1$z, autocolorscale=trace1$autocolorscale, cauto=trace1$cauto, cmax=trace1$cmax, cmin=trace1$cmin, colorbar=trace1$colorbar, colorscale=trace1$colorscale, name=trace1$name, opacity=trace1$opacity, reversescale=trace1$reversescale, showscale=trace1$showscale, type=trace1$type, uid=trace1$uid, xsrc=trace1$xsrc, ysrc=trace1$ysrc, zsrc=trace1$zsrc)
p <- layout(p, autosize=layout$autosize, hovermode=layout$hovermode, scene=layout$scene, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis)

}

htmlwidgets::saveWidget(p,'Figure4_A.html')

#in panel B a normal distribution with a mean of 0 and a variance of 1 is used, 

modelstring.panel.b="
model {
for (i in 1:N) {
x[i] ~ dnorm(mu, tau)
}
mu ~ dnorm(0,1)
tau <- pow(sigma, -2)
sigma ~ dunif(0, 100)
}
"
## data is uniformly distributed with mean mu and precision tau
## prior for mu is normal with mean 0 and precicions 1 == var of 1
## prior for precision is prior for sd^-2
## prior for sd is uniform 0,100

N <- length(sim.data)
# specify length of data
jags.panel.b <- jags.model(textConnection(modelstring.panel.b),
                           data = list('x' = sim.data,
                                       'N' = N),
                           n.chains = 4,
                           n.adapt = 1000)
# jags.model is used to create an object representing a Bayesian graphical model, 
# specified with a BUGS-language description of the prior distribution, and a set of data.
# burn-in of 1000 samples, 4 chains.
out.panel.b <- coda.samples(jags.panel.b,
                            c('mu', 'tau'),
                            25000)
# Extract random samples from the posterior distribution of the parameters of a jags model.
# get sample of 25,000 with 4 chains and burn-in set at 1000, thus 100,000 samples
# monitor the parameters for mu and tau.

# checking output: 
#gelman-rubin statistic, auto correlation, convergence. delete # before following lines to see plots
#gelman.plot(out.panel.b)
#gelman.diag(out.panel.b)
#autocorr.plot(out.panel.b)
#plot(out.panel.b)
## all seems fine

#summary(out) # get output
post_mean_mu.panel.b <- summary(out.panel.b)$statistics[1,1] #get posterior mu for mean parameter
post_mean_sd.panel.b <- summary(out.panel.b)$statistics[1,2] #get posterior sd for mean parameter

## get KL divergence between posterior and benchmark

benchmark.kl.panel.b <- KLnormalnormal(m1=post_mean_mu.panel.b,s1=post_mean_sd.panel.b,
                                       m2=0,s2=sqrt(1))

expert.kl.panel.b <- matrix(NA,nrow=30,ncol=101)
#nrow for sd seq(1,10,by=.1), ncol for distance to mean seq(-5,5,by=.1)
for(ii in 1:101){
  for(i in 1:30){
    expert.kl.panel.b[i,ii] <- KLnormalnormal(m1=post_mean_mu.panel.b,s1=post_mean_sd.panel.b,
                                              m2=(mean(sim.data)+seq(-5,5,by=.1)[ii]),
                                              s2=seq(0.1,3,by=.1)[i])
  }
}
DAC.panel.b <- expert.kl.panel.b / benchmark.kl.panel.b


#plot panel b 
{
  trace2 <- list(
    x = distance.to.mean, 
    y = sd, 
    z = DAC.panel.b, 
    autocolorscale = FALSE, 
    cauto = FALSE, 
    cmax = 2, 
    cmin = 0, 
    colorbar = list(
      x = 0.710443070582, 
      y = 0.601226993865, 
      outlinewidth = 0, 
      showticklabels = TRUE, 
      thickness = 30, 
      thicknessmode = "pixels", 
      ticklen = 42, 
      ticks = "", 
      tickwidth = 1, 
      title = "", 
      ypad = 225
    ), 
    colorscale = list(c(0, "rgb(0,0,155)"),list(0.35, "rgb(0,108,255)"),list(0.5, "rgb(98,255,146)"),list(0.6, "rgb(255,147,0)"),list(0.7, "rgb(255,47,0)"),list(1, "rgb(216,0,0)")), 
    name = "A", 
    opacity = 1, 
    reversescale = FALSE, 
    showscale = TRUE, 
    type = "surface", 
    uid = "8317b3", 
    xsrc = "ducoveen:3:3558a7", 
    ysrc = "ducoveen:1:d606dd", 
    zsrc = "ducoveen:0:484c39,552091,aac067,da133b,8bf81e,ba90ad,3c934b,3e50ce,d723cf,fa4077,1d6c52,bc2a5d,1e69e8,52f1b5,d58dd4"
  )
  data2 <- list(trace2)
  layout <- list(
    autosize = TRUE, 
    hovermode = "closest", 
    scene = list(
      aspectratio = list(
        x = 1, 
        y = 1, 
        z = 1
      ), 
      camera = list(
        center = list(
          x = 0, 
          y = 0, 
          z = 0
        ), 
        eye = list(
          x = -0.558638018887, 
          y = 1.96129585775, 
          z = 0.0279768702355
        ), 
        up = list(
          x = 0, 
          y = 0, 
          z = 1
        )
      ), 
      xaxis = list(
        showspikes = TRUE, 
        spikesides = TRUE, 
        spikethickness = 2, 
        title = "Distance to mean of data", 
        type = "linear"
      ), 
      yaxis = list(
        title = "sd", 
        type = "linear"
      ), 
      zaxis = list(
        autorange = FALSE, 
        range = c(-1, 4), 
        title = "DAC", 
        titlefont = list(size = 13)
      )
    ), 
    title = "Click to enter Plot title", 
    xaxis = list(title = "D"), 
    yaxis = list(title = "E")
  )
  p2 <- plot_ly()
  p2 <- add_trace(p2, x=trace2$x, y=trace2$y, z=trace2$z, autocolorscale=trace2$autocolorscale, cauto=trace2$cauto, cmax=trace2$cmax, cmin=trace2$cmin, colorbar=trace2$colorbar, colorscale=trace2$colorscale, name=trace2$name, opacity=trace2$opacity, reversescale=trace2$reversescale, showscale=trace2$showscale, type=trace2$type, uid=trace2$uid, xsrc=trace2$xsrc, ysrc=trace2$ysrc, zsrc=trace2$zsrc)
  p2 <- layout(p2, autosize=layout$autosize, hovermode=layout$hovermode, scene=layout$scene, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis)
  
}

htmlwidgets::saveWidget(p2,'Figure4_B.html')







#in panel C a uniform prior from -50 to 50 is used 
modelstring.panel.c="
model {
for (i in 1:N) {
x[i] ~ dnorm(mu, tau)
}
mu ~ dunif(-50,50)
tau <- pow(sigma, -2)
sigma ~ dunif(0, 100)
}
"
## data is uniformly distributed with mean mu and precision tau
## prior for mu is uniform between -50 and 50
## prior for precision is prior for sd^-2
## prior for sd is uniform 0,100

N <- length(sim.data)
# specify length of data
jags.panel.c <- jags.model(textConnection(modelstring.panel.c),
                           data = list('x' = sim.data,
                                       'N' = N),
                           n.chains = 4,
                           n.adapt = 1000)
# jags.model is used to create an object representing a Bayesian graphical model, 
# specified with a BUGS-language description of the prior distribution, and a set of data.
# burn-in of 1000 samples, 4 chains.
out.panel.c <- coda.samples(jags.panel.c,
                            c('mu', 'tau'),
                            25000)
# Extract random samples from the posterior distribution of the parameters of a jags model.
# get sample of 25,000 with 4 chains and burn-in set at 1000, thus 100,000 samples
# monitor the parameters for mu and tau.

# checking output: 
#gelman-rubin statistic, auto correlation, convergence. delete # before following lines to see plots
#gelman.plot(out.panel.c)
#gelman.diag(out.panel.c)
#autocorr.plot(out.panel.c)
#plot(out.panel.c)
## all seems fine

#summary(out) # get output
post_mean_mu.panel.c <- summary(out.panel.c)$statistics[1,1] #get posterior mu for mean parameter
post_mean_sd.panel.c <- summary(out.panel.c)$statistics[1,2] #get posterior sd for mean parameter

## get KL divergence between posterior and benchmark

normalunif.panel.c <- KLnormaluniform(mu1=post_mean_mu.panel.c,sigma1=post_mean_sd.panel.c,gamma1=1,
                unifmin = -50,unifmax = 50,x=seq(-50,50,by=.001))

benchmark.kl.panel.c <- KLdiv(normalunif.panel.c[,1:2],eps=1e-50)[1,2]

expert.kl.panel.c <- matrix(NA,nrow=30,ncol=101)
#nrow for sd seq(1,10,by=.1), ncol for distance to mean seq(-5,5,by=.1)
for(ii in 1:101){
  for(i in 1:30){
    expert.kl.panel.c[i,ii] <- KLnormalnormal(m1=post_mean_mu.panel.c,s1=post_mean_sd.panel.c,
                                              m2=(mean(sim.data)+seq(-5,5,by=.1)[ii]),
                                              s2=seq(0.1,3,by=.1)[i])
  }
}
DAC.panel.c <- expert.kl.panel.c / benchmark.kl.panel.c


#plot panel c 
{
  trace3 <- list(
    x = distance.to.mean, 
    y = sd, 
    z = DAC.panel.c, 
    autocolorscale = FALSE, 
    cauto = FALSE, 
    cmax = 2, 
    cmin = 0, 
    colorbar = list(
      x = 0.710443070582, 
      y = 0.601226993865, 
      outlinewidth = 0, 
      showticklabels = TRUE, 
      thickness = 30, 
      thicknessmode = "pixels", 
      ticklen = 42, 
      ticks = "", 
      tickwidth = 1, 
      title = "", 
      ypad = 225
    ), 
    colorscale = list(c(0, "rgb(0,0,155)"),list(0.35, "rgb(0,108,255)"),list(0.5, "rgb(98,255,146)"),list(0.6, "rgb(255,147,0)"),list(0.7, "rgb(255,47,0)"),list(1, "rgb(216,0,0)")), 
    name = "A", 
    opacity = 1, 
    reversescale = FALSE, 
    showscale = TRUE, 
    type = "surface", 
    uid = "8317b3", 
    xsrc = "ducoveen:3:3558a7", 
    ysrc = "ducoveen:1:d606dd", 
    zsrc = "ducoveen:0:484c39,552091,aac067,da133b,8bf81e,ba90ad,3c934b,3e50ce,d723cf,fa4077,1d6c52,bc2a5d,1e69e8,52f1b5,d58dd4"
  )
  data2 <- list(trace3)
  layout <- list(
    autosize = TRUE, 
    hovermode = "closest", 
    scene = list(
      aspectratio = list(
        x = 1, 
        y = 1, 
        z = 1
      ), 
      camera = list(
        center = list(
          x = 0, 
          y = 0, 
          z = 0
        ), 
        eye = list(
          x = -0.558638018887, 
          y = 1.96129585775, 
          z = 0.0279768702355
        ), 
        up = list(
          x = 0, 
          y = 0, 
          z = 1
        )
      ), 
      xaxis = list(
        showspikes = TRUE, 
        spikesides = TRUE, 
        spikethickness = 2, 
        title = "Distance to mean of data", 
        type = "linear"
      ), 
      yaxis = list(
        title = "sd", 
        type = "linear"
      ), 
      zaxis = list(
        autorange = FALSE, 
        range = c(-1, 4), 
        title = "DAC", 
        titlefont = list(size = 13)
      )
    ), 
    title = "Click to enter Plot title", 
    xaxis = list(title = "D"), 
    yaxis = list(title = "E")
  )
  p3 <- plot_ly()
  p3 <- add_trace(p3, x=trace3$x, y=trace3$y, z=trace3$z, autocolorscale=trace3$autocolorscale, cauto=trace3$cauto, cmax=trace3$cmax, cmin=trace3$cmin, colorbar=trace3$colorbar, colorscale=trace3$colorscale, name=trace3$name, opacity=trace3$opacity, reversescale=trace3$reversescale, showscale=trace3$showscale, type=trace3$type, uid=trace3$uid, xsrc=trace3$xsrc, ysrc=trace3$ysrc, zsrc=trace3$zsrc)
  p3 <- layout(p3, autosize=layout$autosize, hovermode=layout$hovermode, scene=layout$scene, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis)
  
}

htmlwidgets::saveWidget(p3,'Figure4_C.html')









#and in panel D a normal distribution with a mean of 5 and a variance of .5 is used. 

## getting the posterior from data and benchmark using rjags (JAGS)
## calculating posterior distribution data (normal) and uniform benchmark 

## specifying model within r code using textConnection fucntion.
#https://darrenjw.wordpress.com/2012/10/02/inlining-jags-models-in-r-scripts-for-rjags/
modelstring.panel.d="
model {
for (i in 1:N) {
x[i] ~ dnorm(mu, tau)
}
mu ~ dnorm(5,2)
tau <- pow(sigma, -2)
sigma ~ dunif(0, 100)
}
"
## data is uniformly distributed with mean mu and precision tau
## prior for mu is normal with mean 5 and precicions 2 == var of .5
## prior for precision is prior for sd^-2
## prior for sd is uniform 0,100

N <- length(sim.data)
# specify length of data
jags.panel.d <- jags.model(textConnection(modelstring.panel.d),
                           data = list('x' = sim.data,
                                       'N' = N),
                           n.chains = 4,
                           n.adapt = 1000)
# jags.model is used to create an object representing a Bayesian graphical model, 
# specified with a BUGS-language description of the prior distribution, and a set of data.
# burn-in of 1000 samples, 4 chains.
out.panel.d <- coda.samples(jags.panel.d,
                            c('mu', 'tau'),
                            25000)
# Extract random samples from the posterior distribution of the parameters of a jags model.
# get sample of 25,000 with 4 chains and burn-in set at 1000, thus 100,000 samples
# monitor the parameters for mu and tau.

# checking output: 
#gelman-rubin statistic, auto correlation, convergence. delete # before following lines to see plots
#gelman.plot(out.panel.d)
#gelman.diag(out.panel.d)
#autocorr.plot(out.panel.d)
#plot(out.panel.d)
## all seems fine

#summary(out) # get output
post_mean_mu.panel.d <- summary(out.panel.d)$statistics[1,1] #get posterior mu for mean parameter
post_mean_sd.panel.d <- summary(out.panel.d)$statistics[1,2] #get posterior sd for mean parameter

## get KL divergence between posterior and benchmark

benchmark.kl.panel.d <- KLnormalnormal(m1=post_mean_mu.panel.d,s1=post_mean_sd.panel.d,
                                       m2=5,s2=sqrt(.5))

expert.kl.panel.d <- matrix(NA,nrow=30,ncol=101)
#nrow for sd seq(1,10,by=.1), ncol for distance to mean seq(-5,5,by=.1)
for(ii in 1:101){
  for(i in 1:30){
    expert.kl.panel.d[i,ii] <- KLnormalnormal(m1=post_mean_mu.panel.d,s1=post_mean_sd.panel.d,
                                              m2=(mean(sim.data)+seq(-5,5,by=.1)[ii]),
                                              s2=seq(0.1,3,by=.1)[i])
  }
}
DAC.panel.d <- expert.kl.panel.d / benchmark.kl.panel.d


#plot panel d 
{
  trace4 <- list(
    x = distance.to.mean, 
    y = sd, 
    z = DAC.panel.d, 
    autocolorscale = FALSE, 
    cauto = FALSE, 
    cmax = 2, 
    cmin = 0, 
    colorbar = list(
      x = 0.710443070582, 
      y = 0.601226993865, 
      outlinewidth = 0, 
      showticklabels = TRUE, 
      thickness = 30, 
      thicknessmode = "pixels", 
      ticklen = 42, 
      ticks = "", 
      tickwidth = 1, 
      title = "", 
      ypad = 225
    ), 
    colorscale = list(c(0, "rgb(0,0,155)"),list(0.35, "rgb(0,108,255)"),list(0.5, "rgb(98,255,146)"),list(0.6, "rgb(255,147,0)"),list(0.7, "rgb(255,47,0)"),list(1, "rgb(216,0,0)")), 
    name = "A", 
    opacity = 1, 
    reversescale = FALSE, 
    showscale = TRUE, 
    type = "surface", 
    uid = "8317b3", 
    xsrc = "ducoveen:3:3558a7", 
    ysrc = "ducoveen:1:d606dd", 
    zsrc = "ducoveen:0:484c39,552091,aac067,da133b,8bf81e,ba90ad,3c934b,3e50ce,d723cf,fa4077,1d6c52,bc2a5d,1e69e8,52f1b5,d58dd4"
  )
  data4 <- list(trace4)
  layout <- list(
    autosize = TRUE, 
    hovermode = "closest", 
    scene = list(
      aspectratio = list(
        x = 1, 
        y = 1, 
        z = 1
      ), 
      camera = list(
        center = list(
          x = 0, 
          y = 0, 
          z = 0
        ), 
        eye = list(
          x = -0.558638018887, 
          y = 1.96129585775, 
          z = 0.0279768702355
        ), 
        up = list(
          x = 0, 
          y = 0, 
          z = 1
        )
      ), 
      xaxis = list(
        showspikes = TRUE, 
        spikesides = TRUE, 
        spikethickness = 2, 
        title = "Distance to mean of data", 
        type = "linear"
      ), 
      yaxis = list(
        title = "sd", 
        type = "linear"
      ), 
      zaxis = list(
        autorange = FALSE, 
        range = c(-1, 4), 
        title = "DAC", 
        titlefont = list(size = 13)
      )
    ), 
    title = "Click to enter Plot title", 
    xaxis = list(title = "D"), 
    yaxis = list(title = "E")
  )
  p4 <- plot_ly()
  p4 <- add_trace(p4, x=trace4$x, y=trace4$y, z=trace4$z, autocolorscale=trace4$autocolorscale, cauto=trace4$cauto, cmax=trace4$cmax, cmin=trace4$cmin, colorbar=trace4$colorbar, colorscale=trace4$colorscale, name=trace4$name, opacity=trace4$opacity, reversescale=trace4$reversescale, showscale=trace4$showscale, type=trace4$type, uid=trace4$uid, xsrc=trace4$xsrc, ysrc=trace4$ysrc, zsrc=trace4$zsrc)
  p4 <- layout(p4, autosize=layout$autosize, hovermode=layout$hovermode, scene=layout$scene, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis)
  
}
htmlwidgets::saveWidget(p4,'Figure4_D.html')

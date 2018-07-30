# loading required packages via pacman package manager function p_load
## getting started, pacman is a package that will automatically check if your packages are installed before loading
if (!require("pacman")) install.packages("pacman", repos = "https://cloud.r-project.org/")
pacman::p_load(rstan, dplyr, magrittr, bridgesampling, brms, fGarch)
# get data loaded
dat <- c(3.2144360,4.2319224,0.7292747,1.1946721,2.3536029,2.2197631,0.8950535,4.4319216,1.6593090,1.5847846,0.9710988,1.2760406,3.4996059,
            4.1239381,2.5939061,2.1703336,0.8980953,3.8805930,2.8197607,2.7018905,2.9604446,0.8631145,4.4311612,1.0106424,0.3323181,1.6737576,
            1.4030363,2.4144391,2.4235646,1.2631129,1.2631129,0.8486658,1.6463813,2.9984672,0.9049394,1.8448596,1.1460031,1.5285111,1.0334560,
            1.5330738,1.9589276,1.4129222,2.7893426,0.7566510,3.0007486,2.6623469,1.3163446,3.1110143,2.0866838,3.8798326,1.6159632,3.7840155,
            1.5634919,1.3726181,2.9345892,2.6866814,2.2075958,3.4638646,2.3345915,2.8037912,4.7178521,2.0897256,2.6319288,2.7657685,2.3300288,
            2.1117787,2.9368705,2.1756568,2.7406736,2.5414348,1.9536044,4.6547345,2.8965665,1.8631105,2.4380132,2.5307885,2.3551238,2.9148174,
            0.6281344,2.4402945,2.1239460,2.7094950,1.7650120,1.6304118,3.1209002,4.1566376,2.0775583,2.4676709,2.1954286,2.8250839,2.4296482,
            1.9954294,2.4927658,1.6212864,1.6402977,3.3513176,4.4342030,1.5688151,2.0646306,3.5064500,2.2098772,0.2357405,2.5368721,2.1186228)
experts  <- data.frame(matrix(c(2.15222623,0.09222707,0.77699404,2.15968728,0.06550168,0.82304321,
                                     1.9704492,0.1061564,0.8242798,2.3513215,0.1129617,0.9365372),ncol=4,nrow=3))
rownames(experts) <- c("mean","sd","skewness")
colnames(experts) <- c("Expert.1","Expert.2","Expert.3","Expert.4")
experts <- t(experts)
#experts' priors


# To show translation of skewness parameter from Fernandez & Steel one to
# the skew_normal used in stan:
xx <- seq(0,5,by=.01)
for(i in 1:4){
  plot(xx,dskew_normal(xx, mu = experts[i,1], sigma = experts[i,2], alpha = -(1/experts[2,3]), xi = NULL, omega = NULL,
                       log = FALSE),type="l",col="blue")
  
  lines(xx,dsnorm(xx,experts[i,1],experts[i,2],experts[i,3]),col="red")
  par(new=T)
}
par(new=F)


# list to store fitted posterior distributions
fit.experts <- list()

# fit for all experts a posterior distribution
for(i in 1:nrow(experts)){
fit.experts[[i]] <- paste0("
data{
int<lower=0> n;
vector[n] y;
}

parameters{
real alpha;
real sigma;
}

model{
alpha ~ skew_normal(", experts[i,1], ",", experts[i,2],",",(-1/experts[i,3]), ");
sigma ~ uniform(0,100);
y ~ normal(alpha, sigma);
}

")  %>%
  stan_model(model_code = .) %>%
  sampling(data = list(n = length(dat),y= dat), seed = 11235) 
}
#launch_shinystan(fit.intercept.only)

# get some benchmarks 
# list to store benchmark results
fit.benchmarks <- list()

# define benchmarks
benchmarks <- c("uniform(0, 5)", "uniform(-10,10)",
                paste0("normal(0,sqrt(10))"),
                paste0("normal(0,sqrt(100))"),
                paste0("normal(0,sqrt(1000))"))


for(i in 1:length(benchmarks)){
fit.benchmarks[[i]] <- paste0("
data{
int<lower=0> n;
vector[n] y;
}

parameters{
real alpha;
real sigma;
}

model{
alpha ~ ",benchmarks[i], ";
sigma ~ uniform(0,100);
y ~ normal(alpha, sigma);
}

") %>%
  stan_model(model_code = .) %>%
  sampling(data = list(n = length(dat),y= dat), seed = 11235) 
}



bridge.experts <- lapply(fit.experts, bridge_sampler)
bridge.benchmarks <- lapply(fit.benchmarks, bridge_sampler)

post_prob(bridge.experts[[1]], bridge.experts[[2]], 
          bridge.experts[[3]], bridge.experts[[4]],
          bridge.benchmarks[[1]])

bf.exp.bench <- c()
for(i in 1:4){
bf.exp.bench[i] <-  bridgesampling::bf(bridge.benchmarks[[1]],
                                       bridge.experts[[i]])$bf
}

for(i in 1:4){
for(j in 1:4){
 print(bridgesampling::bf(bridge.experts[[i]],bridge.experts[[j]])$bf)
}
}


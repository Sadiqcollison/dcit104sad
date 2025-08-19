######QUestion1###################################################

library(insuranceData)
data(AutoClaims)
head(AutoClaims)
Samp_Auto<- sample(x=AutoClaims$PAID,size = 200, replace= FALSE)
likeli.gamma<- function(par,x){
  shape <- par[1]
  rate <- par[2]
  logli <- sum(dgamma(x,shape = shape,rate = rate,log = TRUE))
  return(-logli)
}
MLE.gam<-optim(par = c(2,5),fn=likeli.gamma, method = "Nelder-Mead",
               x=Samp_Auto)
MLE.gam$par
Mom.gamma <- function(x){
  alpha.hat <- ((mean(x))^2)/var(x)
  beta.hat <- mean(x)/var(x)
  Tab <- c(alpha.hat,beta.hat)
  names(Tab)<- c("Alpha","Beta")
  return(Tab)
}
mom_est<-Mom.gamma(Samp_Auto)
shape_mle <- MLE.gam$par[1]
rate_mle <- MLE.gam$par[2]

shape_mom <- mom_est["Alpha"]
rate_mom <- mom_est["Beta"]
hist(Samp_Auto,xlab = "Claim Amount (PAID)",probability = TRUE,main = "")
x_vals <- sort(Samp_Auto)
lines(x_vals, dgamma(x_vals, shape = shape_mom, rate = rate_mom),
      col = "red", lwd = 2, lty = 1)
lines(x_vals, dgamma(x_vals, shape = shape_mle, rate = rate_mle),
      col = "blue", lwd = 2, lty = 1)
######Question2###########################################
BiasMSE.fun <- function(theta.hat,theta)
{
  Bias<-mean(theta.hat)-theta
  MSE <- mean((theta.hat-theta)^2)
  tab2 <- c(Bias,MSE)
  names(tab2)<- c("Bias","MSE")
  return(tab2)
}

set.seed(123) # For reproducibility
B <- 1000    # Number of bootstrap samples
n <- 200      # Sample size

# Store the original sample
Data6 <- sample(x = AutoClaims$PAID, size = n, replace = FALSE)

# Get the "true" parameters from original sample (assumed true for bootstrapping)
true_mle <- optim(par = c(2,5), fn = likeli.gamma, 
                  method = "Nelder-Mead", x = Data6)$par
true_mom <- Mom.gamma(Data6)

mle_estimates <- matrix(NA, nrow = B, ncol = 2)
mom_estimates <- matrix(NA, nrow = B, ncol = 2)

# Bootstrap loop
for (i in 1:B) {
  boot_sample <- sample(Samp_Auto, size = n, replace = TRUE)
  
  # MLE
  mle_boot <- optim(par = c(2,5), fn = likeli.gamma,method = "Nelder-Mead", x = boot_sample)$par
  mle_estimates[i, ] <- mle_boot
  
  # MOM
  mom_boot <- Mom.gamma(boot_sample)
  mom_estimates[i, ] <- c(mom_boot["Alpha"], mom_boot["Beta"])
}
bias_mse_mle_shape <- BiasMSE.fun(mle_estimates[,1], true_mle[1])
bias_mse_mle_rate <- BiasMSE.fun(mle_estimates[,2], true_mle[2])
bias_mse_mom_shape <- BiasMSE.fun(mom_estimates[,1], true_mom["Alpha"])
bias_mse_mom_rate <- BiasMSE.fun(mom_estimates[,2], true_mom["Beta"])
results <- rbind(
  MLE_Shape = bias_mse_mle_shape,
  MLE_Rate = bias_mse_mle_rate,
  MOM_Shape = bias_mse_mom_shape,
  MOM_Rate = bias_mse_mom_rate
)
print(round(tail(results), 4))
mle_estimates[,1]
?pnorm
?NPV
NPV
install.packages("evd")
url <- "https://lstat.kuleuven.be/Wiley/Data/desmoin.txt"
wind_data <- read.table(url, header = FALSE)
####################################################################33
cash_flows <- c(-10000, -10000,-10000, 
  -8000, 28000, 38000, 33000, 23000, 13000, 8000)
NPV(cf0 = -80000,i= 0.15, cf = cash_flows,times =1:10)
?NPV
?IRR
IRR(cf0 = -80000,cf = cash_flows,times =1:10)
my_NPV <- function(cf0, cf, i, times){
  cf0+sum(cf / (1 + i) ^ times)
}
ExpQQ
library(hmmm)
data(accident)
nrow(accident)
first_sample <- c(sample(accident$Freq,size = 16,replace = TRUE))
DataSim<- sapply(1:1000,function(i){
  sample_data <- sample(accident$Freq,size = 100,replace = TRUE)
  mean(sample_data)})
den_1 <- density(DataSim)
plot(den_1,main = "",xlab = "Distribution of Sample Means",
     col = "black",ylim = c(0,0.18),lwd=2)
?sapply
sample_means <- replicate(1000, {
  sample_data <- sample(accident$Freq, size = 60, replace = TRUE)
  mean(sample_data)
})
library(mice)
head(windspeed)
windspeed$Dublin
?quantile
quantile(x= windspeed$Dublin,probs = 0.95)

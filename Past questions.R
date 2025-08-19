library(MASS)
str(Insurance)
Insurance
describe(Insurance)
sample_Insurance <- Insurance[sample(1:nrow(Insurance), 
                                     size=50, replace=FALSE), ]
head(sample_Insurance)
table(Insurance$Group)
table(Insurance$District)
table(Insurance$Age)
summary(Insurance$Holders)
summary(Insurance$Claims)
cor.test(Insurance$Claims, Insurance$Holders, 
         method = "pearson",conf.level = 0.90)
cor(Insurance$Claims, Insurance$Holders)
?cor.test
plot(Insurance$Claims, Insurance$Holders,main="",
     xlab = "Holders",ylab = "Claims")
library(psych)
?describe
describe.by(Insurance$Claims,Insurance$Age)
anova_model <- aov(Claims ~ Age, data = Insurance)
summary(anova_model)
shapiro.test(residuals(anova_model))   # Shapiro–Wilk test
qqnorm(residuals(anova_model)); qqline(residuals(anova_model))
bartlett.test(Claims ~ Age, data = Insurance)
kruskal.test(Claims ~ Age, data = Insurance)
TukeyHSD(anova_model)
one.way <- oneway(Insurance$Claims, y= Insurance$Age, 
                  posthoc = 'games-howell')
??one.way
likeli.gamma<- function(par,x){
  shape <- par[1]
  rate <- par[2]
  x <- x[x > 0]
  dens <- (rate^shape / gamma(shape)) * (x^(shape - 1)) * exp(-rate * x)
  logli <- suppressWarnings(sum(log(dens)))
  return(-logli)
}

Mom.gamma <- function(x){
  alpha.hat <- ((mean(x))^2)/var(x)
  beta.hat <- mean(x)/var(x)
  Tab <- c(alpha.hat,beta.hat)
  names(Tab)<- c("Alpha","Beta")
  return(Tab)
}
likeli.gamma.2 <- function(par, x) {
  shape <- par[1]
  rate  <- par[2]
  x <- x[x > 0]
  # log-likelihood for gamma distribution
  logli <- suppressWarnings(sum(dgamma
            (x, shape = shape, rate = rate, log = TRUE)))
  
  return(-logli)  # negative log-likelihood (for minimization)
}
Mom.claims <- Mom.gamma(Insurance$Claims)
Mom.claims.Alpha <- Mom.claims["Alpha"]
Mom.claims.Beta <- Mom.claims["Beta"]
MLE.gam <- suppressWarnings(optim(par = c(2,5),fn=likeli.gamma.2,
              method = "Nelder-Mead",x=Insurance$Claims))
MLE.gam$par
MLE.claims.Alpha <- MLE.gam$par[1]
MLE.claims.Beta <- MLE.gam$par[2]
Sort_Sample_Auto <- sort(Insurance$Claims)
hist(Samp_Auto,xlab = "Claim Amount (PAID)",probability = TRUE,main = "")
lines(Sort_Sample_Auto, dgamma(Sort_Sample_Auto, shape = Mom.claims.Alpha
                               , rate = Mom.claims.Beta),
      col = "red", lwd = 2, lty = 2)

lines(Sort_Sample_Auto,dgamma(Sort_Sample_Auto, 
                               shape = MLE.claims.Alpha, 
                               rate = MLE.claims.Beta),
      col = "blue", lwd = 2, lty = 1)


legend("topright",
       legend = c("MoM Fit", "MLE Fit"),
       col = c("red", "blue"),
       lty = c(2, 1),  # dashed for MoM, solid for MLE
       lwd = 2,
       bty = "n")
length(Sort_Sample_Auto)
length(dgamma(Sort_Sample_Auto, shape = MLE.claims.Alpha, rate = MLE.claims.Beta))
?NPV
Cashflow_A <- c(12500,100,10,200,200)
Cashflow_B <- c(-1000,0,100,200,20000)
NPV(-10000,Cashflow_A,c(1:5),0.19)
NPV(-10000,Cashflow_B,c(1:5),0.19)
?IRR
IRR(-10000,Cashflow_A,c(1:5))
IRR(-10000,Cashflow_B,c(1:5))

NetPV <- function(cf0,netcash,t,i){
   cf0 + sum(netcash/(1+i)^t)
}
NetPV(-10000,Cashflow_B,c(1:5),0.19)
 
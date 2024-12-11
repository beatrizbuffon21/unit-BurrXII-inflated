UBXII<-expression(log(
  log(tau^(-sigma)) * log(y^(-1))^(sigma - 1) / 
    (y * log(1 + log(mu^(-1))^sigma)) * 
    (1 + log(y^(-1))^sigma) ^ (log(tau) / log(1 + log(mu^(-1))^sigma)-1)
))
m1UBXII<-D(UBXII,"mu")
s1UBXII<-D(UBXII,"sigma")
ms2UBXII<-D(m1UBXII,"sigma")

UBXII<-function (mu.link = "logit", sigma.link = "log") 
{
  tau<-.5
  mstats <- checklink("mu.link", "UBXII", substitute(mu.link), 
                      c("logit", "probit", "cloglog", "cauchit", "log", "own"))
  dstats <- checklink("sigma.link", "UBXII", substitute(sigma.link), 
                      c("inverse", "log", "identity", "own"))
  structure(list(family = c("UBXII", "UBurrXII"), 
                 parameters = list(mu = TRUE, sigma = TRUE), 
                 nopar = 2, 
                 type = "Continuous", 
                 mu.link = as.character(substitute(mu.link)), 
                 sigma.link = as.character(substitute(sigma.link)), 
                 
                 mu.linkfun = mstats$linkfun, 
                 sigma.linkfun = dstats$linkfun, 
                 
                 mu.linkinv = mstats$linkinv, 
                 sigma.linkinv = dstats$linkinv, 
                 
                 mu.dr = mstats$mu.eta, 
                 sigma.dr = dstats$mu.eta, 
                 
                 dldm = function(y, mu, sigma) {#ok
                   tau<-.5
                   dldm <- eval(m1UBXII)
                   dldm
                 }, 
                 d2ldm2 = function(y,mu, sigma) {
                   tau<-.5
                   dldm <- eval(m1UBXII)
                   d2ldm2 <- -dldm * dldm
                   d2ldm2 <- ifelse(d2ldm2 < -1e-15, d2ldm2,-1e-15) 
                   d2ldm2
                 }, 
                 dldd = function(y, mu, sigma) {#ok
                   tau<-.5
                   dldd <- eval(s1UBXII)
                   dldd
                 },
                 d2ldd2 = function(y,mu, sigma) {
                   tau<-.5
                   dldd <- eval(s1UBXII)
                   d2ldd2 = -dldd * dldd
                   d2ldd2 <- ifelse(d2ldd2 < -1e-15, d2ldd2,-1e-15)  
                   d2ldd2
                 },
                 d2ldmdd = function(y,mu, sigma) {
                   tau<-.5
                   dldm <- eval(m1UBXII)
                   dldd <- eval(s1UBXII)
                   d2ldmdd <- -(dldm * dldd)
                   d2ldmdd<-ifelse(is.na(d2ldmdd)==TRUE,0,d2ldmdd)
                   d2ldmdd  
                 }, 
                 G.dev.incr = function(y, mu, sigma, w, ...) -2 * log(dUBXII(y=y, mu=mu, sigma=sigma)), 
                 rqres = expression(
                   rqres(pfun = "pUBXII",  type = "Continuous", y = y, mu = mu, sigma = sigma)
                 ),
                 
                 mu.initial = expression(mu <- rep(0.5#median(y)
                                                        ,length(y))),   
                 sigma.initial = expression(sigma<- rep(4, length(y))),
                 mu.valid = function(mu) all(mu > 0 & mu < 1), 
                 sigma.valid = function(sigma)  all(sigma > 0),
                 y.valid = function(y) all(y > 0 &  y < 1)
  ), 
  class = c("gamlss.family", "family"))
}


# density function
dUBXII<-function(y, mu = .7, sigma = 2.4, tau=.5, log = FALSE)
{
  if (any(mu <= 0) | any(mu >= 1)) stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(sigma <= 0)) stop(paste("sigma must be positive", "\n", ""))
  if (any(y <= 0) | any(y >= 1)) stop(paste("x must be between 0 and 1", "\n", ""))
  fy1 <- log(tau^(-sigma)) * log(y^(-1))^(sigma - 1) / 
    (y * log(1 + log(mu^(-1))^sigma)) * 
    (1 + log(y^(-1))^sigma) ^ (log(tau) / log(1 + log(mu^(-1))^sigma)-1)
  if(log==FALSE) fy<-fy1 else fy<-log(fy1)
  fy
}
#------------------------------------------------------------------------------------------ #ok
# cumulative distribution function
pUBXII<-function(q, mu = .7, sigma = 2.4, tau=.5, log = FALSE){
  if (any(mu <= 0) | any(mu >= 1)) stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(sigma < 0)) stop(paste("sigma must be positive", "\n", ""))
  if (any(q <= 0) | any(q >= 1)) stop(paste("x must be between 0 and 1", "\n", ""))
  cdf<- (1 + log(q^(-1))^sigma)^(log(tau) / log(1 + log(mu^(-1))^sigma))
  return(cdf)
}
#------------------------------------------------------------------------------------------ #ok
# quantile function
qUBXII<-function(u,mu,sigma, tau)
{
  #q<- mu^(log(u)/log(tau))^(1/sigma)
  q<- exp(-(-1 + u^(log(1 + log(mu^(-1))^sigma)/(log(tau))))^(1/sigma))

  q
}
#------------------------------------------------------------------------------------------
# inversion method for randon generation
rUBXII<-function(n,mu,sigma,tau)
{
  u<- runif(n)
  y<- qUBXII(u,mu =mu, sigma =sigma, tau=tau)
  y
}

tau<-.5
# # Checking the results
library(gamlss)
set.seed(10)
n<-100
#Case 1: without regressors
mu_true<-.52
sigma_true<-3
mu_result<-sigma_result<-c()
logit_link<-make.link("logit")
log_link<-make.link("log")
for (i in 1:10) {
  y<-rUBXII(n,mu_true,sigma_true,   tau=.5)
  fit1<-gamlss(y~1, family="UBXII", trace = F)
  mu_result[i]<-logit_link$linkinv(fit1$mu.coefficients)
  sigma_result[i]<-log_link$linkinv(fit1$sigma.coefficients)
}

result1<- matrix(c(mu_true, mean(mu_result),
            sigma_true, mean(sigma_result)),2,2)
colnames(result1)<-c("mu","sigma")
print(round(result1,2))

# Checking the results
set.seed(10)
n <- 1000

# Case 2: with regressors
X <- runif(n)
logit_link <- make.link("logit")
log_link <- make.link("log")
b1 <- 0.7
b2 <- 3
mu_true <- logit_link$linkinv(b1 + b2 * X)
g1 <- 0.5
g2 <- 1.5
sigma_true <- log_link$linkinv(g1 + g2 * X)
R <- 100
mu_result <- matrix(NA, R, 2)
sigma_result <- matrix(NA, R, 2)
install.packages('DT')
for (i in 1:R) {
  y <- rUBXII(n, mu_true, sigma_true, tau = 0.5)
  fit1 <- gamlss(y ~ X, sigma.formula = ~ X, family = UBXII(), trace = FALSE)
  mu_result[i, ] <- fit1$mu.coefficients
  sigma_result[i, ] <- fit1$sigma.coefficients
}

true_values <- c(b1, b2, g1, g2)
mean_values <- c(apply(mu_result, 2, mean),
                 apply(sigma_result, 2, mean))
b_values <- (true_values - mean_values) / true_values * 100
eqm_values <- c(apply(mu_result, 2, var),
                apply(sigma_result, 2, var)) + (true_values - mean_values)^2
result1 <- cbind(true_values,
                 mean_values,
                 b_values,
                 eqm_values)
colnames(result1) <- c("true value", "mean", "bias", "eqm")
rownames(result1) <- c("b1", "b2", "g1", "g2")

print(round(result1, 2))


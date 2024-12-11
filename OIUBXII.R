library(gamlss)
#------------------------------------------------------------------------------------------ 
# initial values
#------------------------------------------------------------------------------------------
mu = 0.7
sigma = 2.1
nu = 0.1
tau=.5
x=.22

#------------------------------------------------------------------------------------------
# Burrxii distribution - basic functions
#------------------------------------------------------------------------------------------
# density function for x in (0,1)
dburr<-function(y,mu=.7,sigma=2.1,tau=.5)
{
  d<-log(tau^(-sigma)) * log(y^(-1))^(sigma - 1) / 
    (y * log(1 + log(mu^(-1))^sigma)) * 
    (1 + log(y^(-1))^sigma) ^ (log(tau) / log(1 + log(mu^(-1))^sigma)-1)
  
  d
}
# checking dburr
integrate(dburr,0,1,mu=mu,sigma=sigma,tau=tau)
# cumulative distribution function for x in (0,1)
pburr<-function(y,mu=.7,sigma=2.1,tau=.5)
{
  p<- (1 + log(y^(-1))^sigma)^(log(tau) / log(1 + log(mu^(-1))^sigma))
  p
}
# checking pburr
integrate(dburr,0,x,mu=mu,sigma=sigma,tau=tau)
pburr(x,mu=mu,sigma=sigma,tau=tau)
# quantile function for x in (0,1)
qburr<-function(u,mu=.7,sigma=2.1,tau=.5)
{
  q<- exp(-(-1 + u^(log(1 + log(mu^(-1))^sigma)/(log(tau))))^(1/sigma))
  q
}
# checking qkum
u<-pburr(x,mu=mu,sigma=sigma,tau=tau)
c(x,qburr(u,mu=mu,sigma=sigma,tau=tau))
#------------------------------------------------------------------------------------------
# The One inflated BurrXII distribution - basic functions
#------------------------------------------------------------------------------------------
# density function for x in [0,1)
dOIUBXII<-function (x, mu = 0.7, sigma = 2.1, nu = 0.1, log = FALSE) 
{
  if (any(mu <= 0) | any(mu >= 1)) 
    stop(paste("mu must be beetwen 0 and 1 ", "\n", ""))
  if (any(sigma < 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(nu <= 0) | any(nu >= 1)) 
    stop(paste("nu must be beetwen 0 and 1 ", "\n", ""))
  tau=.5
  log.kum =log(log(tau^(-sigma))/(log(1 + log(mu^(-1))^sigma))) - log(x) +
    (log(tau)/(log(1 + log(mu^(-1))^sigma))-1) * log(1+ log(x^(-1))^sigma) + 
    (sigma-1)*(log(-log(x)))
  log.lik <- ifelse(x == 1, log(nu), log(1 - nu) + log.kum)
  if (log == FALSE) 
    fy <- exp(log.lik)
  else fy <- log.lik
  fy <- ifelse(x <= 0 | x > 1, 0, fy)
  fy
}
# checking dOIUBXII
dOIUBXII(1) # = nu 
dOIUBXII(x)
(1-nu)*dburr(x)
dOIUBXII(0) # 0 because it is not inflated in zero
#------------------------------------------------------------------------------------------ 
# cumulative distribution function
pOIUBXII<-function (q,  mu = 0.7, sigma = 2.1, nu = 0.1, lower.tail = TRUE, 
                   log.p = FALSE) 
{
  if (any(mu <= 0) | any(mu >= 1)) 
    stop(paste("mu must be beetwen 0 and 1 ", "\n", ""))
  if (any(sigma < 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(nu <= 0) | any(nu >= 1)) 
    stop(paste("nu must be beetwen 0 and 1 ", "\n", ""))
  cdf <- ifelse((q > 0 & q < 1), nu + (1 - nu) * (1 + log(q^(-1))^sigma)^(log(tau) / log(1 + log(mu^(-1))^sigma)),0)
  cdf <- ifelse((q == 1), nu, cdf)
  cdf <- ifelse((q <= 0), 1, cdf)
  if (lower.tail == TRUE) 
    cdf <- cdf
  else cdf = 1 - cdf
  if (log.p == FALSE) 
    cdf <- cdf
  else cdf <- log(cdf)
  cdf <- ifelse(q < 0, 0, cdf)
  cdf <- ifelse(q > 1, 1, cdf)
  cdf
}
# checking pOIUBXII
pOIUBXII(1)
pOIUBXII(x)
nu+(1-nu)*pburr(x)
pOIUBXII(0)
#------------------------------------------------------------------------------------------
# quantile function
qOIUBXII<-function (p, mu = 0.7, sigma = 2.1, nu = 0.1, lower.tail = TRUE, 
                   log.p = FALSE) 
{
  if (any(mu <= 0) | any(mu >= 1)) 
    stop(paste("mu must be beetwen 0 and 1 ", "\n", ""))
  if (any(sigma < 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(nu <= 0) | any(nu >= 1)) 
    stop(paste("nu must be beetwen 0 and 1 ", "\n", ""))
  if (log.p == TRUE) 
    p <- exp(p)
  else p <- p
  if (lower.tail == TRUE) 
    p <- p
  else p <- 1 - p
  u <- (p - nu)/(1 - nu)
  if (any(p < 0) | any(p > 1)) 
    stop(paste("p must be between 0 and 1", "\n", ""))
  suppressWarnings(
    q <- ifelse((nu >= p), 1,
                exp(-(-1 + u^(log(1 + log(mu^(-1))^sigma)/(log(tau))))^(1/sigma))
    )
  )
  q
}
# checking qOIUBXII
u=pOIUBXII(x)
qOIUBXII(u)
qburr((u - nu)/(1 - nu),mu = 0.7, sigma = 2.1)
#------------------------------------------------------------------------------------------
# inversion method for random generation
rOIUBXII<-function (n, mu = 0.5, sigma = 1, nu = 0.1) 
{
  if (any(mu <= 0) | any(mu >= 1)) 
    stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(sigma < 0)) 
    stop(paste("sigma must be positive", "\n", ""))
  if (any(nu <= 0) | any(nu >= 1)) 
    stop(paste("nu must be beetwen 0 and 1 ", "\n", ""))
  if (any(n <= 0)) 
    stop(paste("n must be a positive integer", "\n", ""))
  n <- ceiling(n)
  p <- runif(n)
  r <- qOIUBXII(p, mu = mu, sigma = sigma, nu = nu)
  r
}


#------------------------------------------------------------------------------------------
# OIUBXII in gamlss.family
#------------------------------------------------------------------------------------------
# derivatives of log.kum

#log-liklihood burrxii

burrxii<-expression(
  log(log(tau^(-sigma))/(log(1 + log(mu^(-1))^sigma))) - log(y) +
    (log(tau)/(log(1 + log(mu^(-1))^sigma))-1) * log(1+ log(y^(-1))^sigma) + 
    (sigma-1)*(log(-log(y)))
)

m1<-D(burrxii,"mu")
s1<-D(burrxii,"sigma")
ms2<-D(m1,"sigma")
#
OIUBXII<-function (mu.link = "logit", sigma.link = "log", nu.link = "logit") 
{
  mstats <- checklink("mu.link", "OIUBXII", substitute(mu.link), 
                      c("logit", "probit", "cloglog", "log", "own"))
  dstats <- checklink("sigma.link", "OIUBXII", substitute(sigma.link), 
                      c("inverse", "log", "identity"))
  vstats <- checklink("nu.link", "OIUBXII", substitute(nu.link), 
                      c("logit", "probit", "cloglog", "log", "own"))
  structure(list(family = c("OIUBXII", "One inflated BurrXII"), 
                 parameters = list(mu = TRUE, sigma = TRUE, nu = TRUE), 
                 nopar = 3, type = "Mixed", 
                 mu.link = as.character(substitute(mu.link)), 
                 sigma.link = as.character(substitute(sigma.link)), 
                 nu.link = as.character(substitute(nu.link)), 
                 mu.linkfun = mstats$linkfun, sigma.linkfun = dstats$linkfun, nu.linkfun = vstats$linkfun, 
                 mu.linkinv = mstats$linkinv, sigma.linkinv = dstats$linkinv, nu.linkinv = vstats$linkinv, 
                 mu.dr = mstats$mu.eta, sigma.dr = dstats$mu.eta, nu.dr = vstats$mu.eta, 
                 dldm = function(y, mu, sigma) {
                   tau=.5   
                   dldm <- ifelse((y == 0 | y == 1), 0, eval(m1))
                   dldm
                 }, d2ldm2 = function(y, mu, sigma) { 
                   tau=.5   
                   dldm <- eval(m1)  
                   d2ldm2 <- ifelse((y == 0 | y == 1), 0, -dldm * dldm)      
                   d2ldm2
                 }, dldd = function(y, mu, sigma) {
                   tau=.5
                   dldd <- ifelse((y == 0 | y == 1), 0, eval(s1))
                   dldd
                 }, d2ldd2 = function(y, mu, sigma) { 
                   tau=.5                  
                   dldd <- ifelse(is.nan(eval(s1)), 0, eval(s1))  
                   d2ldd2 <- ifelse((y == 0 | y == 1), 0, -(dldd * dldd))
                   d2ldd2
                 }, dldv = function(y, nu) {
                   tau=.5
                   dldv <- ifelse(y == 0 | y == 1, 1/nu, -1/(1 - nu))
                   dldv
                 }, d2ldv2 = function(nu) {
                   tau=.5
                   d2ldv2 <- -1/(nu * (1 - nu))
                   d2ldv2
                 }, d2ldmdd = function(y, mu, sigma) {
                   tau=.5   
                   dldm <- eval(m1)
                   dldd <- ifelse(is.nan(eval(s1)), 0, eval(s1))  
                   d2ldmdd <- ifelse((y == 0 | y == 1), 0, -(dldm * dldd))
                   d2ldmdd
                 }, d2ldmdv = function(y) {
                   tau=.5
                   d2ldmdv <- rep(0, length(y))
                   d2ldmdv
                 }, d2ldddv = function(y) {
                   tau=.5
                   d2ldddv <- rep(0, length(y))
                   d2ldddv
                 }, G.dev.incr = function(y, mu, sigma, nu, ...) {
                   -2 * dOIUBXII(y, mu, sigma, nu, log = TRUE)
                 }, rqres = expression({
                   uval <- ifelse(y == 0 | y == 1, nu * runif(length(y), 0, 1), 
                                  (1 - nu) * pOIUBXII(y, mu, sigma, nu))
                   rqres <- qnorm(uval)
                 }), 
                 mu.initial = expression(mu <- rep(median(y),length(y))),
                 sigma.initial = expression(sigma <- rep(3, length(y))), 
                 nu.initial = expression(nu <- rep(0.3, length(y))), 
                 mu.valid = function(mu) all(mu > 0 & mu < 1), 
                 sigma.valid = function(sigma) all(sigma > 0), 
                 nu.valid = function(nu) all(nu > 0 & nu < 1), 
                 y.valid = function(y) all(y >= 0 & y <= 1)), 
            class = c("gamlss.family", "family")
  )
}


# Checking the results
library(gamlss)
set.seed(10)
n<-50
# Case 1: without regressors
mu_true<-.8
sigma_true<-2.1
nu_true<-.2
mu_result<-sigma_result<-nu_result<-c()
for (i in 1:100) {
  y<-rIUBXII(n,mu_true,sigma_true,nu_true)
  fit1<-gamlss(y~1,family="IUBXII",trace=F)
  mu_result[i]<-fit1$mu.fv[1]
  sigma_result[i]<-fit1$sigma.fv[1]
  nu_result[i]<-fit1$nu.fv[1]
}
fit1$sigma.coefficients
result1<- matrix(c(mu_true, mean(mu_result),
                   sigma_true, mean(sigma_result),
                   nu_true, mean(nu_result)),2,3)
colnames(result1)<-c("mu","sigma","nu")
rownames(result1)<-c("true value","mean")
print(round(result1,2))

# Case 2: with regressors
set.seed(10)
n=300
X <- runif(n)
logit_link <- make.link("logit")
log_link <- make.link("log")
b1 <- 0.7
b2 <- 3
mu_true <- logit_link$linkinv(b1 + b2 * X)
g1 <- 0.2
g2 <- .7
sigma_true <- log_link$linkinv(g1 + g2 * X)
c1 <- -1.2
c2 <- .3
nu_true <- logit_link$linkinv(c1 + c2 * X) 

R <- 10
mu_result <- matrix(NA, R, 2)
sigma_result <- matrix(NA, R, 2)
nu_result <- matrix(NA, R, 2)

tau = 0.5

i=0
bug=0
while (i <= R) {
  y <- rOIUBXII(n, mu_true, sigma_true, nu_true)
  fit1 <- try(gamlss(y ~ X, sigma.formula = ~ X, 
                     nu.formula = ~ X, 
                     family = OIUBXII(), control=gamlss.control(n.cyc = 100)), silent=T)
  if(class(fit1)[1]=="try-error" || 
     fit1$converged != 1){
    bug<-bug+1
  }else{
    mu_result[i, ] <- fit1$mu.coefficients
    sigma_result[i, ] <- fit1$sigma.coefficients
    nu_result[i, ] <- fit1$nu.coefficients
    i<- i + 1
    print(i)
  }
}

valid_rows <- !is.na(mu_result[, 1]) & !is.na(sigma_result[, 1]) & !is.na(nu_result[, 1])

true_values <- c(b1, b2, g1, g2, c1, c2)
mean_values <- c(apply(mu_result[valid_rows, ], 2, mean),
                 apply(sigma_result[valid_rows, ], 2, mean), 
                 apply(nu_result[valid_rows, ], 2, mean))
b_values <- (true_values - mean_values) / true_values * 100
eqm_values <- c(apply(mu_result[valid_rows, ], 2, var),
                apply(sigma_result[valid_rows, ], 2, var),
                apply(nu_result[valid_rows, ], 2, var)) + (true_values - mean_values)^2

result1 <- cbind(true_values,
                 mean_values,
                 b_values,
                 eqm_values)
colnames(result1) <- c("true value", "mean", "bias", "eqm")
rownames(result1) <- c("b1", "b2", "g1", "g2", "c1", "c2")

print(round(result1, 4))


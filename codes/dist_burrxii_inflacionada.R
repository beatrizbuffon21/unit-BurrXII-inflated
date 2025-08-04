#------------------------------------------------------------------------------------------
# BurrXII distribution inflated - basic functions
#------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------
# probability density function
dIUBXII <- function(y, mu = .7, sigma = 5, nu = .2, tau = .7, log = FALSE) {
  if (any(y < 0) | any(y > 1)) stop(paste("x must be in [0,1) or (0,1]", "\n", ""))
   fy1 <-  sigma*log(1/tau)*(log(1/y))^(sigma-1)/
    (y*log(1+(log(1/mu))^sigma)*(1+(log(1/y))^sigma)^
       (1+log(1/tau)/log(1+(log(1/mu))^sigma)))
  fy <- ifelse(y == 0 | y == 1, nu, (1 - nu) * fy1)
  if (log) {fy <- log(fy)}
  return(fy)
}
integrate(dIUBXII,0,1) # checking the pdf

#------------------------------------------------------------------------------------------
# cumulative distribution function
pIUBXII<-function(q, mu = 0.7, sigma = 2, tau=.7, nu=.2,lower.tail = TRUE, log.p = FALSE){
  if (any(mu <= 0) | any(mu >= 1)) stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(sigma < 0))  stop(paste("sigma must be positive", "\n", "")) 
  if (any(q < 0) | any(q > 1)) stop(paste("x must be between 0 and 1", "\n", ""))
  cdf1<- (1+(log(1/q))^sigma)^(log(tau)/log(1+(log(1/mu))^sigma))
  cdf<-ifelse( q > 0 & q < 1, nu + (1-nu)*cdf1, nu)
  cdf <- ifelse( q>=1, 1, cdf)
  if(!lower.tail){cdf <- 1-cdf}
  if(log.p){cdf <- log(cdf)}
  return(cdf)
}
pIUBXII(.3)
integrate(dIUBXII,0,.3) # checking the cdf with the pdf

#------------------------------------------------------------------------------------------
# quantile function
qIUBXII<-function(p, mu, sigma, tau, nu, lower.tail = TRUE,
                  log.p = FALSE){
  if(log.p){p <- exp(p)}
  if(!lower.tail){p <- 1 - p}
  u=(p-nu)/(1-nu)
  q <- ifelse( p <= nu, 0, 
               exp(-(u^(-1/(log(1/tau)/log(1+(log(1/mu))^sigma)))-1)^(1/sigma)) #UBXII qf
  )
  q <- ifelse( p == 1, 1, q)
  q  
}  


valor=integrate(dUBXII,0,.34)
.2+.8*valor$value
pIUBXII(0.34)
.2+.8*pUBXII(0.34)
pIUBXII(.34)

rIUBXII <- function(n, mu, sigma, nu){
  n <- ceiling(n)
  p <- runif(n)
  r <- qIUBXII(p, mu, sigma, nu = nu)
  r
}

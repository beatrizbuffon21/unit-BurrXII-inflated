# density function
dUBXII_r<-function(y, mu = .7, sigma = 2.4, tau=.5, log = FALSE)
{
  if (any(mu <= 0) | any(mu >= 1)) stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(sigma <= 0)) stop(paste("sigma must be positive", "\n", ""))
  if (any(y <= 0) | any(y >= 1)) stop(paste("x must be between 0 and 1", "\n", ""))
  fy1 <- log((1-tau)^(-sigma)) * log((1-y)^(-1))^(sigma - 1) / 
    ((1-y) * log(1 + log((1-mu)^(-1))^sigma)) * 
    (1 + log((1-y)^(-1))^sigma) ^ (log(1-tau) / log(1 + log((1-mu)^(-1))^sigma)-1)
  if(log==FALSE) fy<-fy1 else fy<-log(fy1)
  fy
}
integrate(dUBXII_r,0,1) # checking the pdf

#------------------------------------------------------------------------------------------
# cumulative distribution function
pUBXII_r<-function(q, mu = .7, sigma = 2.4, tau=.5, log = FALSE){
  if (any(mu <= 0) | any(mu >= 1)) stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(sigma < 0)) stop(paste("sigma must be positive", "\n", ""))
  if (any(q <= 0) | any(q >= 1)) stop(paste("x must be between 0 and 1", "\n", ""))
  cdf<- 1-(1 + log((1-q)^(-1))^sigma)^(log(1-tau) / log(1 + log((1-mu)^(-1))^sigma))
  return(cdf)
}
pUBXII_r(.3)
integrate(dUBXII_r,0,.3) # checking the cdf with the pdf

u=1-(1 + log((1-q)^(-1))^sigma)^(log(1-tau) / log(1 + log((1-mu)^(-1))^sigma))
(1 + log((1-q)^(-1))^sigma)
(1-u)^(log(1 + log((1-mu)^(-1))^sigma)/(log(1-tau)))

log((1-q)^(-1))^sigma
(1-u)^(log(1 + log((1-mu)^(-1))^sigma)/(log(1-tau)))-1

log((1-q)^(-1))
((1-u)^(log(1 + log((1-mu)^(-1))^sigma)/(log(1-tau)))-1)^(1/sigma)


(1-q)^(-1)
exp(((1-u)^(log(1 + log((1-mu)^(-1))^sigma)/(log(1-tau)))-1)^(1/sigma))

#------------------------------------------------------------------------------------------
# quantile function
qUBXII<-function(u,mu,sigma, tau)
{
  #q<- mu^(log(u)/log(tau))^(1/sigma)
  q<- 1-exp(-((1-u)^(log(1 + log((1-mu)^(-1))^sigma)/(log(1-tau)))-1)^(1/sigma))
  
  q
}
u=pUBXII(.7)
qUBXII(u,mu=.7,sigma=2.4, tau=.5) # checking the qf with the cdf
#------------------------------------------------------------------------------------------
# inversion method for randon generation
rUW<-function(n,mu,sigma,tau=.5)
{
  u<- runif(n)
  y<- qUW(u,mu =mu, sigma =sigma)
  y
}

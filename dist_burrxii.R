#------------------------------------------------------------------------------------------
# initial values
mu = .7
sigma = 2
tau=.5
q <- 0.3


#------------------------------------------------------------------------------------------
# Burr XII distribution - basic functions
#------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------
# density function
dUBXII<-function(y, mu = .7, sigma = 2, tau=.5, log = FALSE)
{
  if (any(mu <= 0) | any(mu >= 1)) stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(sigma <= 0)) stop(paste("sigma must be positive", "\n", ""))
  #if (any(y <= 0) | any(y >= 1)) stop(paste("x must be between 0 and 1", "\n", ""))
  fy1 <- log(tau^(-sigma)) * log(y^(-1))^(sigma - 1) / 
    (y * log(1 + log(mu^(-1))^sigma)) * 
    (1 + log(y^(-1))^sigma) ^ (log(tau) / log(1 + log(mu^(-1))^sigma)-1)
  if(log==FALSE) fy<-fy1 else fy<-log(fy1)
  fy
}
integrate(dUBXII,0,1) # checking the pdf

#------------------------------------------------------------------------------------------
# cumulative distribution function
pUBXII<-function(q, mu = .7, sigma = 2, tau=.5, log = FALSE){
  if (any(mu <= 0) | any(mu >= 1)) stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(sigma < 0)) stop(paste("sigma must be positive", "\n", ""))
  if (any(q <= 0) | any(q >= 1)) stop(paste("x must be between 0 and 1", "\n", ""))
  cdf<- (1 + log(q^(-1))^sigma)^(log(tau) / log(1 + log(mu^(-1))^sigma))
  return(cdf)
}
pUBXII(.3)
integrate(dUBXII,0,.3) # checking the cdf with the pdf

#------------------------------------------------------------------------------------------
# checking the values
u=(1 + log(q^(-1))^sigma) ^ (log(tau) / log(1 + log(mu^(-1))^sigma))
1 + log(q^(-1))^sigma
u^(log(1 + log(mu^(-1))^sigma)/(log(tau)))

log(q^(-1))^sigma
u^(log(1 + log(mu^(-1))^sigma)/(log(tau)))-1

log(q^(-1))
(u^(log(1 + log(mu^(-1))^sigma)/(log(tau)))-1)^(1/sigma)

q^(-1)
exp((u^(log(1 + log(mu^(-1))^sigma)/(log(tau)))-1)^(1/sigma))

#------------------------------------------------------------------------------------------
# quantile function
qUBXII<-function(u,mu,sigma, tau)
{
  q<- exp(-(-1 + u^(log(1 + log(mu^(-1))^sigma)/(log(tau))))^(1/sigma))
  q
}
u=pUBXII(.7)
qUBXII(u,mu=.7,sigma=2.4, tau=.5) # checking the qf with the cdf

#------------------------------------------------------------------------------------------
# inversion method for randon generation
rUBXII<-function(n,mu,sigma,tau)
{
  u<- runif(n)
  y<- qUBXII(u,mu =mu, sigma =sigma, tau=tau)
  y
}

#------------------------------------------------------------------------------------------
# log-liklihood Burr XII distribution
#------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------
# initial values
mu=.7
sigma=2
tau=.5
y=.5

#------------------------------------------------------------------------------------------
# density function
dburr<-function(y, mu = .7, sigma = 2, tau=.5)
{
  d<-log(tau^(-sigma)) * log(y^(-1))^(sigma - 1) / 
    (y * log(1 + log(mu^(-1))^sigma)) * 
    (1 + log(y^(-1))^sigma) ^ (log(tau) / (log(1 + log(mu^(-1))^sigma))-1)
  d
}

#------------------------------------------------------------------------------------------
#log-liklihood
ll<-function(y, mu = .7, sigma = 2.4, tau=.5){
  log(log(tau^(-sigma))/(log(1 + log(mu^(-1))^sigma))) - log(y) +
    (log(tau)/(log(1 + log(mu^(-1))^sigma))-1) * log(1+ log(y^(-1))^sigma) + 
    (sigma-1)*(log(-log(y)))
  
} 
log(dburr(y, mu, sigma, tau))
ll(y, mu, sigma, tau)

#------------------------------------------------------------------------------------------
# checking for more values
z=c(.3,.4,.5)

log(dburr(z[1], mu, sigma, tau)*dburr(z[2], mu, sigma, tau)*dburr(z[3], mu, sigma, tau))

sum(ll(z, mu, sigma, tau))

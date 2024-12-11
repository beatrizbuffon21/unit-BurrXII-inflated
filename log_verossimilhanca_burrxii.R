## Burr XII distribution
#initial values
mu=.7
sigma=2.4
tau=.5
y=.5
# density function
dburr<-function(y, mu = .7, sigma = 2.4, tau=.5)
{
  d<-log(tau^(-sigma)) * log(y^(-1))^(sigma - 1) / 
    (y * log(1 + log(mu^(-1))^sigma)) * 
    (1 + log(y^(-1))^sigma) ^ (log(tau) / (log(1 + log(mu^(-1))^sigma))-1)
  d
}

#log-liklihood
ll<-function(y, mu = .7, sigma = 2.4, tau=.5){
  log(log(tau^(-sigma))/(log(1 + log(mu^(-1))^sigma))) - log(y) +
    (log(tau)/(log(1 + log(mu^(-1))^sigma))-1) * log(1+ log(y^(-1))^sigma) + 
    (sigma-1)*(log(-log(y)))
  
} 
log(dburr(y, mu, sigma, tau))
ll(y, mu, sigma, tau)

#checking for more values
z=c(.3,.4,.5)

log(dburr(z[1], mu, sigma, tau)*dburr(z[2], mu, sigma, tau)*dburr(z[3], mu, sigma, tau))

sum(ll(z, mu, sigma, tau))


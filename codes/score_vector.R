#------------------------------------------------------------------------------------------
# Burr XII distribution - vetor escore
#------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------ 
# initial values
mu = 0.7
sigma = 2.1
nu = 0.1
tau=.5
y=.22

#------------------------------------------------------------------------------------------ 
# calculating the analytical log-likelihood
loglik <-expression(
  log(log(tau^(-sigma))/(log(1 + log(mu^(-1))^sigma)))
  -log(y) + (log(tau)/(log(1 + log(mu^(-1))^sigma))-1) 
  * log(1+ log(y^(-1))^sigma)
  + (sigma-1) * (log(-log(y)))
)

#------------------------------------------------------------------------------------------ 
# calculating the derivative of loglik with respect to sigma (analytical from R)
D(loglik, "sigma")

#------------------------------------------------------------------------------------------  
# evaluating at the defined values to test
eval(D(loglik, "sigma"))

#------------------------------------------------------------------------------------------ 
# score vector U(sigma)
#------------------------------------------------------------------------------------------ 

parte1 <- (
  (-((log(1 / mu)^sigma * log(tau^-sigma) * log(log(1 / mu))) /((1 + log(1 / mu)^sigma) * log(1 + log(1 / mu)^sigma)^2)) -
     log(tau) / log(1 + log(1 / mu)^sigma)) * log(1 + log(1 / mu)^sigma)) / log(tau^-sigma)

eval(parte1)

parte2 <- -log(y) 
eval(parte2) 

parte3 <- -((log(1 / mu)^sigma * log(tau) * log(log(1 / mu))) / 
              ((1 + log(1 / mu)^sigma) * log(1 + log(1 / mu)^sigma)^2))
eval(parte3)

parte4 <- (log(1 / y)^sigma * log(log(1 / y))) / (1 + log(1 / y)^sigma)
eval(parte4)

parte5 <- log(-log(y))
eval(parte5)

#------------------------------------------------------------------------------------------ 
# calculating the derivative of loglik with respect to mu 
D(loglik,"mu") # analytical from R

#------------------------------------------------------------------------------------------ 
# evaluating at the defined values to test
eval(D(loglik,"mu"))

u_mu <- (sigma * log(1 / mu)^(-1 + sigma)) / (mu * (1 + log(1 / mu)^sigma) * log(1 + log(1 / mu)^sigma)) + 
  (sigma * log(1 / mu)^(-1 + sigma) * log(tau) * log(1 + log(1 / y)^sigma)) / 
  (mu * (1 + log(1 / mu)^sigma) * log(1 + log(1 / mu)^sigma)^2)
eval(u_mu)


#------------------------------------------------------------------------------------------ 
# score vector U(mu)
#------------------------------------------------------------------------------------------ 

parte1 <- (sigma * log(1 / mu)^(-1 + sigma)) /
  (mu * (1 + log(1 / mu)^sigma) * log(1 + log(1 / mu)^sigma))
eval(parte1)

parte2 <- -log(y)
eval(parte2)

parte3 <- (sigma * log(1 / mu)^( -1 + sigma) * log(tau)) /
  (mu * (1 + log(1 / mu)^sigma) * log(1 + log(1 / mu)^sigma)^2)
eval(parte3)

parte4 <- log(1 + log(y^(-1))^sigma)
parte4 

parte5 <- (sigma - 1) * log(-log(y))
parte5 


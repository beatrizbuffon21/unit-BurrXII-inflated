#vetor escore para dist burrxii 

mu = 0.7
sigma = 2.1
nu = 0.1
tau=.5
y=.22

#calculando a log-verossimilhança analítica
loglik <-expression(
  log(log(tau^(-sigma))/(log(1 + log(mu^(-1))^sigma)))
  -log(y) + (log(tau)/(log(1 + log(mu^(-1))^sigma))-1) 
  * log(1+ log(y^(-1))^sigma)
  + (sigma-1) * (log(-log(y)))
)


# calculando a derivada da loglik com respeito a sigma
D(loglik,"sigma") # analitica do R
#avaliando nos valores definidos para testar
eval(D(loglik,"sigma"))


#conferi todas partes e os resultados na mão foi o mesmo do R, exceto para aqueles que retornam 0.
# vetor escore U(sigma)

parte1 <- (
  (-((log(1 / mu)^sigma * log(tau^-sigma) * log(log(1 / mu))) /((1 + log(1 / mu)^sigma) * log(1 + log(1 / mu)^sigma)^2)) -
     log(tau) / log(1 + log(1 / mu)^sigma)) * log(1 + log(1 / mu)^sigma)) / log(tau^-sigma)

eval(parte1)

parte2 <- -log(y) 
eval(parte2) #não retorna 0

parte3 <- -((log(1 / mu)^sigma * log(tau) * log(log(1 / mu))) / 
              ((1 + log(1 / mu)^sigma) * log(1 + log(1 / mu)^sigma)^2))
eval(parte3)

parte4 <- (log(1 / y)^sigma * log(log(1 / y))) / (1 + log(1 / y)^sigma)
eval(parte4)

parte5 <- log(-log(y))
eval(parte5)


# calculando a derivada da loglik com respeito a mu
D(loglik,"mu") # analitica do R
#avaliando nos valores definidos para testar
eval(D(loglik,"mu"))

u_mu <- (sigma * log(1 / mu)^(-1 + sigma)) / (mu * (1 + log(1 / mu)^sigma) * log(1 + log(1 / mu)^sigma)) + 
  (sigma * log(1 / mu)^(-1 + sigma) * log(tau) * log(1 + log(1 / y)^sigma)) / 
  (mu * (1 + log(1 / mu)^sigma) * log(1 + log(1 / mu)^sigma)^2)
eval(u_mu)

# vetor escore U(mu)

parte1 <- (sigma * log(1 / mu)^(-1 + sigma)) /
  (mu * (1 + log(1 / mu)^sigma) * log(1 + log(1 / mu)^sigma))
parte1

parte2 <- -log(y)
parte2 # não retorna 0

parte3 <- (sigma * log(1 / mu)^( -1 + sigma) * log(tau)) /
  (mu * (1 + log(1 / mu)^sigma) * log(1 + log(1 / mu)^sigma)^2)
parte3

parte4 <- log(1 + log(y^(-1))^sigma)
parte4 #não retorna 0

parte5 <- (sigma - 1) * log(-log(y))
parte5 #não retorna 0



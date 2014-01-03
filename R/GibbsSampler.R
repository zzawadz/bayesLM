getGibbsStats = function(model, burn = 10000, n = 100000)
{
  require(MASS)
  ntotal = burn + n
  invXX = model@invXX
  beta_hat = model@coeff
  
  sb = 0
  nobs = model@nobs
  
  parameters = matrix(nrow = ntotal, ncol = length(model@coeff)+1)
  
  y = as.vector(unlist(model@data[all.vars(model@formula)[1]]))
  X = cbind(1,matrix(unlist(model@data[all.vars(model@formula)[-1]]),ncol = length(beta_hat)-1))
  
  beta = beta_hat
  tau = 1/model@sigma
  
  for(i in 1:ntotal)
  {
    # tau
    sb = sum((y-X%*%beta)^2)
    tau=rgamma(1, nobs/2, 0.5*sb);
    
    beta = mvrnorm(1, beta_hat, 1/tau*invXX)
    parameters[i,] =c(beta,tau)
  }
  
  gibbs = new("GibbsRes", niter = n, burn = burn, parameters = parameters[-c(1:burn),], model = model)
  return(gibbs)
}



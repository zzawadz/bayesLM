getGibbsStats = function(model, burn = 1000, n = 1000)
{
  
  ntotal = burn + n
  
  #Wyciagniecie potrzebnych danych z obiektu model
  invXX = model@invXX
  beta_hat = model@coeff
  
  sb = 0
  nobs = model@nobs
  
  parameters = matrix(nrow = ntotal, ncol = length(model@coeff)+1)
  
  #Wyciagniecie y modelu
  y = as.vector(unlist(model@data[all.vars(model@formula)[1]]))
  # Wyciagniecie X-ow
  X = getModelMatrix(model)
  
  #Parametry startowe:
  beta = beta_hat
  tau = 1/model@sigma2
  
  
  # jazda wg wzorow - wylosowane tau jest uzywane
  # w losowaniu bety, ktore potem zluza
  # do wyznaczania sigmy, ktora sluzy do wyznaczania
  # tau - itd:-)
  for(i in 1:ntotal)
  {
    # tau
    sb = sum((y-X%*%beta)^2)
    tau=rgamma(1, nobs/2, 0.5*sb);
    # beta
    beta = mvrnorm(1, beta_hat, 1/tau*invXX)
    parameters[i,] =c(beta,tau)
  }
  
  # Tez reszte zapisuje w obiekcie - tylko mazywajacym sie
  # "GibbsRes"
  gibbs = new("GibbsRes", niter = n, 
              burn = burn, 
              parameters = parameters[-c(1:burn),], 
              model = model)
  return(gibbs)
}



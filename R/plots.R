setMethod("plotCoefDistributions", "BLM", function(model, nsd = 3.5, index = NULL,...)
{
  #Rysowanie rozkladow analitycznych dla parametrow.
  # jezeli index = NULL - dosmyslnie:
  # rysowane dla wszystkich parametrow
  # jezeli index = np 1 - wtedy rysowany pierwszy parametr
  # itp
  
  #Ile mamy prametrow modelu:
  n = length(model@coeff)
  
  #Nazwy parametrow  - beda nazwami na wykresie
  names = names(model@coeff)

  tmp = FALSE
  if(is.null(index)) 
  {
      par_def = par(mfrow = c(n+1,1), mar = c(2,2,2,2))
      tmp = TRUE
      index = 1:n
  }
  
  if(index[1] <= n)
  {
    for(i in index)
    {
      #rysowanie i-tego parametru:
      mu = model@coeff[i]
      prec = model@precision[i]
      df = model@df
      sd = sqrt(1/prec*(df/(df-2)))
      #Curve rysuje ten rozklad o gestosci zdefiniowanej w densityStudent:
      curve(densityStudnet(x,df=df,mu=mu,prec=prec), ylab = "",
            xlim = c(mu-nsd*sd, mu+nsd*sd), main = names[i], ...)
    }
  }
  
  
  plot_tau = FALSE
  if(tmp == FALSE && max(index) > n) plot_tau = TRUE 
  # Rozklad dla Tau - strona 4 mistrza - ostatni wzor na stronie
  # S(Beta) w nim to SSE!
  if(tmp  || plot_tau) 
  {
      mu = model@df/2/(0.5*model@sigma2*model@df)
      sd = sqrt(model@df/2/(0.5*model@sigma2*model@df)^2)
      sd = sd*1.5
      sse  = model@sigma2*model@df # Domyslnie  sigma2 - to SSE/(T-k)
                                 # Tutaj skaluje by bylo samo SSE
      curve(dgamma(x,model@df/2,0.5*sse), xlim = c(mu-nsd*sd, mu+nsd*sd), main = "Tau", ...)
  }
  
  if(tmp) par(par_def)
})

setMethod("plotCoefGibbsHist", "GibbsRes", function(gibbs, nsd = 3.5, index = NULL,...)
{
  n = ncol(gibbs@parameters)
  par_def = par(mfrow = c(n,1), mar = c(2,2,2,2))
  index = 1:n
  parameters = gibbs@parameters
  names = c(getCoeffNames(gibbs),"Tau")
  
  # Dodaje rozklady wyznaczone analitycznie:
  for(i in 1:n)
  {
    hist(parameters[,i], freq = FALSE, main = names[i])
    plotCoefDistributions(gibbs@model, nsd=nsd,index=i, add = TRUE, ...)
  }
 
})



#  x = 1:100
#  y = 1:100+rnorm(100)
#  data = data.frame(x,y)
#  model = fitBLM(y~x, data)
# 
# gibbs = getGibbsStats(model)
# 
#  plotCoefGibbsHist(gibbs)

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
      par_def = par(mfrow = c(n,1), mar = c(2,2,2,2))
      tmp = TRUE
      index = 1:n
  }
      
  for(i in index)
  {
    #rysowanie i-tego parametru:
    mu = model@coeff[i]
    prec = model@precision[i]
    df = model@df
    sd = sqrt(1/prec*(df/(df-2)))
    #Curve rysuje ten rozklad o gestosci zdefiniowanej w densityStudent:
    curve(densityStudnet(x,df=df,mu=mu,prec=prec), ylab = "",
          xlim = c(mu-nsd*sd, mu+nsd*sd), main = names[i],...)
  }
  
  if(tmp) par(par_def)
})

setMethod("plotCoefGibbsHist", "GibbsRes", function(gibbs, nsd = 3.5, index = NULL,...)
{
  n = ncol(gibbs@parameters)
  par_def = par(mfrow = c(n,1), mar = c(2,2,2,2))
  index = 1:n
  parameters = gibbs@parameters
  
  for(i in 1:n)
  {
    hist(parameters[,i], freq = FALSE)
    if(i < n)plotCoefDistributions(gibbs@model, nsd=nsd,index=i, add = TRUE)
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

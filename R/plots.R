#########################

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

####################

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
  par(par_def)
})



#################### 
setGeneric("plotHPD", function(model, q = 0.95, index = NULL, nsd = 3.5) standardGeneric("plotHPD"))
setMethod("plotHPD", "BLM", function(model, q = 0.95,index = NULL, nsd = 3.5)
{
  if(is.null(index)) index = 1:length(model@coeff)
  names = getCoeffNames(model)
  if(is.character(index)) index = which(index == names)
  
  hpd = getHPD(model,q=q)
  for(i in index)
  {
    mu = model@coeff[i]
    prec = model@precision[i]
    df = model@df
    sd = sqrt(1/prec*(df/(df-2)))
    #Curve rysuje ten rozklad o gestosci zdefiniowanej w densityStudent:
    
    i_hpd = hpd[i,]
    
    hpd_range = seq(i_hpd[1],i_hpd[2],length.out=300)
    
    curve(densityStudnet(x,df=df,mu=mu,prec=prec), ylab = "",
          xlim = c(mu-nsd*sd, mu+nsd*sd), main = names[i])
    
    
    hpd_value = densityStudnet(hpd_range,df=df,mu=mu,prec=prec)
    hpd_value = c(hpd_value, rep(0,length(hpd_value)))
    hpd_range_pol = c(hpd_range,rev(hpd_range))
    col=rgb(red=1,green=0,blue=0,alpha=0.6)
    polygon(hpd_range_pol, hpd_value,col = col)
  }
    
    
})

#plotHPD(model,index="(Intercept)")


setMethod("getSummaryStatistics", "BLM", function(model)
{
  t_n = names(model@coeff)
  
  cat(paste(t_n,paste(round(model@coeff,5), " sd:", round(getParametersSD(model),5))      , sep = ":  "),sep="\n")
  
  sd = getParametersSD(model)
  HPD = getBLMHPD(model)
  
})


getParametersSD = function(model)
{
  v2 = model@df/(model@df-2)*model@invXX*model@sigma2
  sqrt(diag(v2))
}


setGeneric("getHPD", function(model, q = 0.05) standardGeneric("getHPD"))
setMethod("getHPD", "BLM", function(model, q = 0.05)
{
  q = q/2
  sd  = getParametersSD(model)
  tmp_n = names(model@coeff)
  
  #Ponizej jest kawalek kodu ktory sprawia ze gdzies placze maly
  # programista. Jednak ten kawalek zwraca HPD!!!
  tmp = t(t(t(qt(c(q,1-q),df=model@df)))%*%sd) + model@coeff
  rownames(tmp) = tmp_n
  tmp
})

setMethod("getHPD", "GibbsRes", function(model, q = 0.05)
{
  q = q/2
  tmp_n = names(model@model@coeff)
  # Przepraszam za apply - ale ja je lubie
  # I polecam sie zapoznac
  tmp = t(apply(gibbs@parameters[,-ncol(gibbs@parameters)],2
                ,quantile,c(q,1-q)))
  rownames(tmp) = tmp_n
  tmp
})




# Srednie:
setGeneric("getTauMean", function(model) standardGeneric("getTauMean"))
setMethod("getTauMean", "BLM",function(model) 
{
  model@df/2/(0.5*model@sigma2*model@df)
})

setMethod("getTauMean", "GibbsRes",function(model) 
{
  mean(model@parameters[,ncol(model@parameters)])
})

#Mediany

setGeneric("getTauMedian", function(model) standardGeneric("getTauMedian"))
setMethod("getTauMedian", "BLM",function(model) 
{
  sb = model@sigma2*model@df
  df = model@df
  median(rgamma(50000, df/2, 0.5*sb))
})

setMethod("getTauMedian", "GibbsRes",function(model) 
{
  median(model@parameters[,ncol(model@parameters)])
})



# SD
setGeneric("getTauSD", function(model) standardGeneric("getTauSD"))
setMethod("getTauSD", "BLM",function(model) 
{
  sqrt(model@df/2/(0.5*model@sigma2*model@df)^2)
})

setMethod("getTauSD", "GibbsRes",function(model) 
{
  sd(model@parameters[,ncol(model@parameters)])
})

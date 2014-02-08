densityStudnet = function(x, df, mu, prec)
{
  (gamma((df+1)/2)/gamma(df/2)) * (prec/pi/df)^(0.5)*(1+(prec*(x-mu)^2)/df)^(-(df+1)/2)
}


setMethod("getModelMatrix", "BLM", function(model) model.matrix(model@formula, model@data))



setGeneric("getCoeffNames", function(model) standardGeneric("getCoeffNames"))
setMethod("getCoeffNames", "BLM",function(model) names(model@coeff))
setMethod("getCoeffNames", "GibbsRes",function(model) getCoeffNames(model@model))

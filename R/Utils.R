densityStudnet = function(x, df, mu, prec)
{
  (gamma((df+1)/2)/gamma(df/2)) * (prec/pi/df)^(0.5)*(1+(prec*(x-mu)^2)/df)^(-(df+1)/2)
}

.withIntercept = function(model) attr(terms.formula(model@formula),"intercept")==1

setMethod("getModelMatrix", "BLM", function(model) model.matrix(model@formula, model@data))

setGeneric("getDependentVariable", function(model) standardGeneric("getDependentVariable"))
setMethod("getDependentVariable", "BLM", function(model) as.vector(unlist(model@data[all.vars(model@formula)[1]])))
 



setGeneric("getCoeffNames", function(model) standardGeneric("getCoeffNames"))
setMethod("getCoeffNames", "BLM",function(model) names(model@coeff))
setMethod("getCoeffNames", "GibbsRes",function(model) getCoeffNames(model@model))

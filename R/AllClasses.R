setClass("BLM",slots=c(formula = "formula", data = "data.frame", coeff = "numeric", sigma2 = "numeric", df = "numeric", precision = "numeric", invXX = "matrix", X = "matrix", nobs = "numeric"))
setClass("GibbsRes", slots = c(niter = "numeric", burn = "numeric", parameters = "matrix", model = "BLM"))


setGeneric("plotCoefDistributions", function(model, nsd = 3.5, index = NULL, ...) standardGeneric("plotCoefDistributions"))
setGeneric("getModelMatrix", function(model) standardGeneric("getModelMatrix"))
setGeneric("plotCoefGibbsHist", function(gibbs, nsd = 3.5, index = NULL,...) standardGeneric("plotCoefGibbsHist"))


setGeneric("getSummaryStatistics", function(model) standardGeneric("getSummaryStatistics"))

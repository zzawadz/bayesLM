

setMethod("getSummaryStatistics", "BLM", function(model)
{
  
  t_n = names(model@coeff)
  cat(paste(t_n,round(model@coeff,4), sep = ":  "),sep="\n")
  
})

require(bayesLM)


x = 1:100
y = 5*x+6+rnorm(100)

dane = data.frame(y,x)

#Zaczynamy liczyc
require(bayesLM, quietly=TRUE) #1
model = fitBLM(y~x, dane)      #2
plotCoefDistributions(model)   #3


getSummaryStatistics(model)

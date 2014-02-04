
setMethod("getSummaryStatistics", "BLM", function(model)
{
  
  t_n = names(model@coeff)
  cat(paste(t_n,round(model@coeff,4), sep = ":  "),sep="\n")
  
})

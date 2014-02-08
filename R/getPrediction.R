

getPrediction = function(xf, model)
{
  if(!is.matrix(xf)){  xf = matrix(xf,nrow = 1)}
  
  if(.withIntercept(model)) xf = cbind(1,xf)
  
  sd = sqrt(diag(model@sigma2*(diag(nrow(xf))+xf%*%model@invXX%*%t(xf))))
  
  new("BLMPrediction", xf%*%model@coeff, xf = xf, model = model, sd = sd)
}


getPredHPD = function(pred, q = 0.95)
{
  q = (1-q)/2
  as.numeric(pred)+t(t(pred@sd))%*%qt(c(q,1-q), df = pred@model@df)
}

setMethod("abline", "BLM", function(a,...)
{
  abline(model@coeff,...)
})

setMethod("plot", "BLMPrediction", function(x,q = 0.95,...) 
{
  q = (1-q)/2
  
  pred = x
  x = getModelMatrix(pred@model)[,-1]
  y = getDependentVariable(pred@model)
  xf = pred@xf; if(ncol(xf)==2) xf = xf[,2]
  plot(x,y, xlim = extendrange(c(x,xf)), ylim=extendrange(c(y,pred)), ...)
  
  conf = as.numeric(pred)+t(t(pred@sd))%*%qt(c(q,1-q), df = pred@model@df) 
  segments(x0=xf,x1=xf,y0=conf[,1], y1 = conf[,2],  col = "grey",...)
  abline(model, ...)
  points(xf,pred, col = "red", pch = 18, ...)
})

setMethod("plot", "BLM", function(x, q = 0.95,...)
{
  model = x
  x = getModelMatrix(model)[,-1]
  y = getDependentVariable(model)
  plot(x,y,...)
  abline(model,...)
  xf = extendrange(x,f=0.2); xf = seq(xf[1], xf[2], length.out=300)
  pred = getPrediction(xf, model)
  yf = as.numeric(pred)
  pred_hpd = getPredHPD(pred)
  yfp = c(pred_hpd[,1], rev(pred_hpd[,2]))
  xfp = c(xf,rev(xf))
  polygon(xfp,yfp,col=rgb(1,0,0,0.3))
  
})
fitBLM = function(formula, data)
{
  fit = lm(formula, data = data)
  sigma = summary(fit)$sigma^2
  X = cbind(1,as.matrix(data[all.vars(formula)[-1]]))
  invXX = solve(t(X)%*%X)
  precision = 1/as.numeric(diag((sigma*invXX)))
  
  model = new("BLM",formula = formula, data = data, coeff = fit$coefficients, sigma = sigma, df = fit$df.residual, precision = precision, invXX = invXX, nobs = nrow(data))
  return(model)
}



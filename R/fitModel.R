fitBLM = function(formula, data)
{
  # Wpierw obliczamy oszacowania MNK parametrow
  fit = lm(formula, data = data)
  # Zapisujemy wariancje resztowa modelu MNK
  # pozniej moze sie gdzies przydac (np w samplerze Gibbsa)
  sigma2 = summary(fit)$sigma^2
  
  #Tworzymy macierz zmiennych objasniajacych X
  X = model.matrix(formula, data) 
  # Zapisujemy macierz (X'X)^(-1) - rowniez na pozniej
  invXX = solve(t(X)%*%X)
  # Precyzja:
  precision = 1/as.numeric(diag((sigma2*invXX)))
  
  # W ten sposob tworzy sie nowy obiekt klasy BLM:
  model = new("BLM",formula = formula, 
              data = data, 
              coeff = fit$coefficients, 
              sigma2 = sigma2, 
              df = fit$df.residual, 
              precision = precision, 
              invXX = invXX, 
              nobs = nrow(data))
  return(model)
}


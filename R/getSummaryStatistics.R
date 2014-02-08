### Przyjeta konwencja:
### Naglowek funckji
### setMethod("getStatystyka", "BLM", ...
### oznacza ze funckja zwraca Statystyke rozkladu
### wyznaczonego analitycznie
### natomiast:
### setMethod("getStatystyka", "GibbsRes", ...
### zwraca statystyke obliczona na podstawie Gibbs
### Mam nadzieje ze to dobra konwencja:-)
### I jasna.

### Wartości oczekiwane, srednie,

# Zwraca wartosc oczekiwana rozkladu:
#ExpVal - od expected Value
setGeneric("getCoeffExpVal", function(model, type = mean) standardGeneric("getCoeffExpVal"))
setMethod("getCoeffExpVal", "BLM", function(model)
{
  model@coeff
})
setMethod("getCoeffExpVal", "GibbsRes", function(model, type = mean)
{
  # Pobranie z obiektu gibbs proby - bez TAU!
  param = model@parameters[, -ncol(model@parameters)]
  tmp = apply(param,2,type)
  #Dodanie nazw
  names(tmp) = getCoeffNames(model)
  tmp
})

### odchylenie standardowe parametrow modelu - bez tau:

setGeneric("getCoeffSD", function(model) standardGeneric("getCoeffSD"))

setMethod("getCoeffSD", "BLM", function(model)
{
  # Strona 6 - wzor trzeci od konca na waraiancje
  v2 = model@df/(model@df-2)*model@invXX*model@sigma2
  tmp = sqrt(diag(v2))
  names(tmp) = getCoeffNames(model)
  tmp
})

setMethod("getCoeffSD", "GibbsRes", function(model)
{
  # wycinam tau
  v2 = cov(model@parameters[, -ncol(model@parameters)])
  tmp = sqrt(diag(v2))
  names(tmp) = getCoeffNames(model)
  tmp
})

### Przedzialy HPD:

setGeneric("getHPD", function(model, q = 0.05) standardGeneric("getHPD"))
setMethod("getHPD", "BLM", function(model, q = 0.95)
{
  q = (1-q)/2
  sd  = getCoeffSD(model)
  tmp_n = names(model@coeff)
  
  #Ponizej jest kawalek kodu ktory sprawia ze gdzies placze maly
  # programista. Jednak ten kawalek zwraca HPD!!!
  tmp = t(t(t(qt(c(q,1-q),df=model@df)))%*%sd) + model@coeff
  rownames(tmp) = tmp_n
  tmp
})

setMethod("getHPD", "GibbsRes", function(model, q = 0.05)
{
  q = q/2
  tmp_n = names(model@model@coeff)
  # Przepraszam za apply - ale ja je lubie
  # I polecam sie zapoznac
  tmp = t(apply(model@parameters[,-ncol(model@parameters)],2
                ,quantile,c(q,1-q)))
  rownames(tmp) = tmp_n
  tmp
})




# Macierze korelacji a'posterori: 

setGeneric("getCorMatrix", function(model) standardGeneric("getCorMatrix"))
setMethod("getCorMatrix","BLM", function(model)
{
  # Macierz korelacji a'posterori - analitycznie
  # Wzory mistrza - strona 6
  # wpierw policzona macierz kowariancji
  # nastepnie przeksztalcona i obliczona
  # macierz korelacji 
  
  sigma2 = model@sigma2
  invXX  = model@invXX
  df = model@df
  cov = df/(df-2)*sigma2*invXX
  v = solve(sqrt(diag(diag(cov))))
  cor = v%*%cov%*%v
  
  # Przygotowanie nazw dla macierzy:
  names = getCoeffNames(model)
  rownames(cor) = names
  colnames(cor) = names
  return(cor)
})

setMethod("getCorMatrix","GibbsRes", function(model)
{
  
  # Przygotowanie nazw dla macierzy:
  names = getCoeffNames(model)
  
  # W gibbs@parameters siedzi proba losowa
  # W ostatniej kolumnie jest tau - ktore usuwam
  # w zadaniu maceirz korealcji ma byc bez Tau
  cor = cor(model@parameters[, -ncol(model@parameters)])
  rownames(cor) = names
  colnames(cor) = names
  cor
})




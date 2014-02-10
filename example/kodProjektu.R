#Odkomentowac do instalacji pakietu:
#install.packages("HistData")
#install.packages("devtools")
#require(devtools)
#install_github("bayesLM", username = "zzawadz") # instaluje bezposrednio z internetu
#require(bayesLM)

require(HistData)
#head(GaltonFamilies)
#GaltonFamilies
family = (GaltonFamilies)



# Transformacja plci na numery:
# 0 - mezczyzni
# 1 - kobiety
tmp = rep(0,length(family$gender))
tmp[family$gender =="female"] = 1
family[,"gender"] = tmp


family = family[sample(1:nrow(family),size=300,replace=FALSE),]
# transformacja na cm z cali
family[,c("father","mother","childHeight")] = family[,c("father","mother","childHeight")]*2.54

#model
model = fitBLM(childHeight~father+mother+gender, family)
summary(model)
#próbnik gibsa
gibbs = getGibbsStats(model, burn = 15000, n = 100000)
#histogramy
plotCoefGibbsHist(gibbs, lwd = 2)

##statystyki dla parametrów
#wartości oczekiwane
getCoeffExpVal(model)
getCoeffExpVal(gibbs)

#mediany
getCoeffExpVal(model, type = median)
getCoeffExpVal(gibbs, type = median)

#odchylenia standardowe
getCoeffSD(model)
getCoeffSD(gibbs)
#HPD
getHPD(model)
getHPD(gibbs)
#macierze koleracji
getCorMatrix(model)
getCorMatrix(gibbs)

##statystyki dla tau
getTauMean(model)
getTauMedian(model)
getTauSD(model)
getTauMean(gibbs)
getTauMedian(gibbs)
getTauSD(gibbs)

#wykresy HPD
plotHPD(model, q = 0.95, index = "(Intercept)")
plotHPD(model, q = 0.95, index = "father")
plotHPD(model, q = 0.95, index = "mother")
plotHPD(model, q = 0.95, index = "gender")

#install.packages("HistData")
require(HistData)
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



model = fitBLM(childHeight~father+mother+gender, family)
plotCoefDistributions(model)
plotHPD(model,index="mother")
plotHPD(model,index="father")
plotHPD(model,index="gender")
plotHPD(model,index="(Intercept)")

# HPD dla predykcji:
getCoeffNames(model)
# Podaje sie w kolejnosci ojciec matka, gender
pred = getPrediction(c(170,160,0),model)
getPredHPD(pred)

### Gibbs
gibbs = getGibbsStats(model, burn = 1e4,n=2e4)
plotCoefGibbsHist(gibbs)


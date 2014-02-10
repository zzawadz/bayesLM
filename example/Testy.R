require(bayesLM)
#install.packages("HistData")
### To tak dla zabawy:-)
require(HistData)
head(GaltonFamilies)
model = fitBLM(childHeight ~ father, GaltonFamilies)

family = unique(GaltonFamilies$family)
dataF = t(sapply(family, function(x) unlist(GaltonFamilies[which(GaltonFamilies$family==x)[1],c("father","mother")])))

dataF = data.frame(dataF*2.54)

model = fitBLM(mother~father,dataF)
plot(model)
#plotCoefDistributions(model)
plotHPD(model,index="father")

gibbs = getGibbsStats(model,burn=1e4,1e5)
plotCoefGibbsHist(gibbs,lwd = 5)


getCoeffExpVal(model)
getCoeffExpVal(gibbs)

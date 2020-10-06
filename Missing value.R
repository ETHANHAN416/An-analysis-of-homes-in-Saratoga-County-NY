d<-read.delim("C:/Users/416et/Desktop/real-estate-sample-1200.txt",header = T); #original data
dataFrame<-as.data.frame(d)
library(simFrame)
nac<-NAControl(NArate=0.2) #set missing rate
Newdata<-setNA(dataFrame,nac) #get new data(with missing data) 
miss <- function(x){sum(is.na(x))/length(x)} #check missing rate
#apply(Newdata,2,miss)
OmitData<-Newdata[complete.cases(Newdata[ , c(1,5)]),]#delete data if Central.Air or Sale price is missing.
apply(OmitData,2,miss)#check our Omni data.
LogPrice=log(OmitData$Sale.Price) #set log sale price.

OmitData['LogPrice'] = LogPrice

caPrice = subset(OmitData, Central.Air == "Yes")$LogPrice #log sala price with Central.Air
NcaPrice = subset(OmitData, Central.Air == "No")$LogPrice#log sala price without Central.Air
summary(caPrice)
sd(caPrice)
summary(NcaPrice)
sd(NcaPrice)

size=dim(OmitData)[1]
#this approximates the permutation distribution by taking 2000 labeled
#samples and calculating the difference in means for each
allsize = OmitData$LogPrice
casize=length(caPrice)
pdist = c()
pval = 0
for (k in 1:10000) {
  perm = sample(1:size,casize,replace = FALSE) #choose half to label as 1
  x = mean(allsize[perm])-mean(allsize[-perm])
  pdist = c(pdist,x)
  #the approximate p-value is based on the proportion of sample
  #differences more extreme than the observed difference in means
  if (x < mean(NcaPrice)-mean(caPrice)) {
    pval = pval+1
  }
}
pval = pval/10000 #calculate approximate p-value
hist(pdist, breaks = 200, main = "Approximate Permutation Distribution"
     , xlab = "Differences for the sample permutations")
abline(v = mean(NcaPrice)-mean(caPrice), col = "blue", lwd = 2)

#The official permutation test
library(coin)
independence_test(log(Sale.Price)~Central.Air, data = OmitData, alternative = "less")

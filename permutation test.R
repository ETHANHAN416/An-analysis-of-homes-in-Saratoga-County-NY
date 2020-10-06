res <-read.delim("C:/Users/416et/Desktop/real-estate-sample-1200.txt",header = T); #original data
lprice = log(res$Sale.Price)
res['lprice'] = lprice
#This code is for the initial permutation test
ca = subset(res, Central.Air == "Yes")
nca = subset(res, Central.Air == "No")
caPrice = ca$lprice
ncaPrice = nca$lprice
summary(caPrice)
sd(caPrice)
summary(ncaPrice)
sd(ncaPrice)
#this approximates the permutation distribution by taking 10000 labeled
#samples and calculating the difference in means for each
pdist = c()
pval = 0

for (k in 1:10000) {
  perm = sample(1:1200,485,replace = FALSE) #choose some to label as 1
  x = mean(lprice[perm])-mean(lprice[-perm])
  pdist = c(pdist,x)
  #the approximate p-value is based on the proportion of sample
  #differences more extreme than the observed difference in means
  if (x < mean(ncaPrice)-mean(caPrice)) {
    pval = pval+1
  }
}
pval = pval/10000 #calculate approximate p-value
hist(pdist, breaks = 200, main = "Approximate Permutation Distribution"
     , xlab = "Differences for the sample permutations")
abline(v = mean(ncaPrice)-mean(caPrice), col = "blue", lwd = 2)
#This histogram has a wider range so you can see the blue line
hist(pdist, breaks = 200, main = "Approximate Permutation Distribution"
     , xlab = "Differences for the sample permutations", xlim = c(-.5,.2))
abline(v = mean(ncaPrice)-mean(caPrice), col = "blue", lwd = 2)

#The official permutation test
library(coin)
independence_test(lprice~Central.Air, data = res, alternative = "less")

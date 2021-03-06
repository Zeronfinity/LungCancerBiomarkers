setwd("E:/Academic/4-1/Thesis/Serum and Plasma Metabolomic/plasma/")
rm(list=ls())

data <- read.csv("plusmaLC.csv", row.names=1)

alpha <- 0.05

cnt <- 0

names <- vector('character')
pvals <- vector('numeric')

for (i in 1:nrow(data)){
  vec1 = as.numeric(data[i:i,1:41])
  vec2 = as.numeric(data[i:i,42:82])
  
  vec1 = log2(vec1)
  vec2 = log2(vec2)
  
  tt = t.test(vec1, vec2)
  names <- c(names, row.names(data)[i])
  pvals <- c(pvals, tt$p.value)
}

padjusted = p.adjust(pvals, method = "BH")

resTT = data.frame(row.names=names, "p-value"=pvals, "Adjusted p-value"=padjusted)
save(resTT,file="tstats.rdata")

kept = padjusted < 0.05

res2 = data.frame("Metabolites"=names[kept], "p-value"=pvals[kept], "Adjusted p-value"=padjusted[kept])

cat(nrow(res2))


#write.csv(resTT, file="plasma_t_test_result_all.csv")
#write.csv(res2, file="plasma_t_test_result.csv")

#ttMeta <- names[kept]
#save(ttMeta, resTT, file = "dem_t_test.RData")
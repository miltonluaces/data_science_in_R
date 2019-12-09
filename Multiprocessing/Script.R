library(parallel)
library(MASS)


# Test function
f = function(data, nStart)
    kmeans(x=data, centers=4, nstart=nStart)

ns = rep(100, 40)
#f(data=Boston, nStart=ns)
deltaSeq = system.time((res = lapply(ns, f, data=Boston)))
#print(res)


# Multiprocessing
nCores = detectCores()
clust = makeCluster(nCores)

LocalBoston = Boston
deltaMPr = system.time((res = parLapply(clust, ns, f, data = LocalBoston)))
#print(res)

print(paste("nCores : ", nCores))
print(paste("Sequential ", format(deltaSeq['elapsed'], digits = 2)))
print(paste("Multiprocess ", format(deltaMPr['elapsed'], digits = 2)))
print(paste("Multiprocess is ", format(deltaSeq['elapsed'] / deltaMPr['elapsed'], digits = 2), " times faster."))


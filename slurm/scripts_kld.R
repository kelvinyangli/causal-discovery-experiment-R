# scripts for computing kld 

library(gRain)

library(bnlearn)

source('Rcode/orderFiles.R')

source('Rcode/kld.R')

source('Rcode/mutualInfo.R')

source('Rcode/getIndexForKLD.R')

autoKLD("maxNumParents", "mmhc", 20, TRUE)
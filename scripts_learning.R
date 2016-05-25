# scripts for structure learning using mmhc, aic, bic, bde, k2, pc 

library(bnlearn)

library(pcalg)

source("Rcode/orderFiles.R")

source("Rcode/pcAlgorithm.R")

source("Rcode/toClassBN")

source("Rcode/autoLearn.R")


autoLearn(currentDirectory = "numNodes", learningMethod = "mmhc", numIterations = 20, debug = FALSE)
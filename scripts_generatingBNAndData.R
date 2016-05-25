# scripts for generating BNs and data 

library(bnlearn)

library(gtools)

source("Rcode/autoGenerateBN.R")

source("Rcode/toNumeric.R")

numNodesList = c(2, 3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 100, 200, 300, 500)

sapply(numNodesList, generateBNAndData, numNodes = 30, maxNumParents = 4, maxNumValues = 3, 
       concentration = 20, numInstances = 30000, numInstancesForTesting = 100000, 
       numIterations = 20, currentDirectory = "numNodes")


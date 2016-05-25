
# take node ordering from directory
# 
currentDirectory = s
list.files(paste0(currentDirectory, "/Ordering/"))


sapply(c("aic", "bic", "bde", "k2", "k2WithNodeOrdering", "pc"), autoLearn, 
       currentDirectory = "numNodes", numIterations = 20, debug = FALSE)

sapply(c("mmhc", "aic", "bic", "bde", "k2", "k2WithNodeOrdering", "pc"), autoLearn, 
       currentDirectory = "maxNumValues", numIterations = 20, debug = FALSE)

sapply(c("mmhc", "aic", "bic", "bde", "k2", "k2WithNodeOrdering", "pc"), autoLearn, 
       currentDirectory = "maxNumParents", numIterations = 20, debug = FALSE)

sapply(c("mmhc", "aic", "bic", "bde", "k2", "k2WithNodeOrdering", "pc"), autoLearn, 
       currentDirectory = "numInstances", numIterations = 20, debug = FALSE)

sapply(c("mmhc", "aic", "bic", "bde", "k2", "k2WithNodeOrdering", "pc"), autoLearn, 
       currentDirectory = "concentration", numIterations = 20, debug = FALSE)


sapply(c("aic", "bic", "bde", "k2"), autoLearnWithMWST, currentDirectory = "maxNumParents", numIterations = 20, alpha = 0.05, debug = FALSE)
sapply(c("aic", "bic", "bde", "k2"), autoLearnWithMWST, currentDirectory = "maxNumValues", numIterations = 20, alpha = 0.05, debug = FALSE)
sapply(c("aic", "bic", "bde", "k2"), autoLearnWithMWST, currentDirectory = "numInstances", numIterations = 20, alpha = 0.05, debug = FALSE)
sapply(c("aic", "bic", "bde", "k2"), autoLearnWithMWST, currentDirectory = "numNodes", numIterations = 20, alpha = 0.05, debug = FALSE)

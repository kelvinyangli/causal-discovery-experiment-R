# scripts for get mean prediction for each learning method 
# also get mean best (reference) prediction, where the best prediction is computed in terms of the true models
# also get the worst prediction, where the worst prediction is computed from random guessing, i.e 
# with uniformly distributed probabilities 
# 

currentDirectory = "numNodes"

measure = "bir"

# mean prediction for each method
sapply(c("mmhc", "aic", "bic", "bde", "k2", "k2WithNodeOrdering"), getMeanPrediction, 
       currentDirectory = currentDirectory, measure = measure, numIterations = 20)

# mean best prediction
getMeanReference(currentDirectory, 20)


######################### random guessing ##########################
# get arity for each data set

datasets = list.files(paste0(currentDirectory, "/Datasets/Testing"))
datasets = orderFiles(datasets, currentDirectory, numIterations)

for (i in 1:length(datasets)) {
  
  data = readRDS(paste0(currentDirectory, "/Datasets/Testing/", datasets[i]))
  
  arities = rep(2, ncol(data))
  
  for (j in 1:ncol(data)) arities[j] = nlevels(data[,j])  
  
  write.csv(arities, paste0(currentDirectory, "/Datasets/Arities/", datasets[i], ".csv"), row.names = FALSE)
  
}

randomGuessingRMSEs(currentDirectory, 20)
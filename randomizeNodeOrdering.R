# randomize variable ordering 

randomizeNodeOrdering = function(currentDirectory) {
  
  allTrainingData = list.files(paste0(currentDirectory, "/Datasets/Training")) # list all training data in folder
  allTrainingData = orderFiles(allTrainingData, currentDirectory, numIterations) # re-order all files
  
  for (i in 1:length(allTrainingData)) {
    
    data = readRDS(paste0(currentDirectory, "/Datasets/Training/", allTrainingData[i]))
    randomizedOrder = sample(1:ncol(data))
    data = data[randomizedOrder] 
    saveRDS(data, paste0(currentDirectory, "/Datasets/Training/", allTrainingData[i]))
    
    dataTesting = readRDS(paste0(currentDirectory, "/Datasets/Testing/", allTrainingData[i]))
    dataTesting = dataTesting[randomizedOrder]
    saveRDS(dataTesting, paste0(currentDirectory, "/Datasets/Testing/", allTrainingData[i]))
    
    dataTestingNumeric = readRDS(paste0(currentDirectory, "/Datasets/Numeric/", allTrainingData[i]))
    dataTestingNumeric = dataTestingNumeric[randomizedOrder]
    saveRDS(dataTestingNumeric, paste0(currentDirectory, "/Datasets/Numeric/", allTrainingData[i]))
    
  }
  
}







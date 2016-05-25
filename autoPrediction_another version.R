# compute rmse and bir for all learned cpts
autoPrediction = function(currentDirectory, learningMethod, numIterations, maxNumParents, maxNumValues, 
                          testingDataNumericList, debug = FALSE) {
  
  # learned cpts
  allLearnedCPTs = list.files(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod)) # load all learned cpts
  allLearnedCPTs = orderFiles(allLearnedCPTs, currentDirectory, numIterations) # order all cpts
  
  for (i in 1:length(testingDataNumericList)) { # for each test set
    
    dataTestingNumeric = testingDataNumericList[[i]][1:1000,] # use 1000 for now
    
    cptsLearned = readRDS(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", allLearnedCPTs[i]))
    
    df = prediction(cptsLearned, dataTestingNumeric, maxNumParents, maxNumValues, debug = debug)
    
    write.csv(df$rmse, paste0(currentDirectory, "/Evaluations/rmse/", learningMethod, "/", allLearnedCPTs[i], ".csv"), row.names = FALSE)
    
    write.csv(df$bir, paste0(currentDirectory, "/Evaluations/bir/", learningMethod, "/", allLearnedCPTs[i], ".csv"), row.names = FALSE)
    
  }
  
}


referencePrediction = function(currentDirectory, maxNumParents, maxNumValues, allTrueCPTsList, testingDataNumericList, 
                               allTestSets, debug = FALSE) {
  
  for (i in 1:length(testingDataNumericList)) { # for each true cpts 
    
    dataTestingNumeric = testingDataNumericList[[i]][1:1000,]
    
    cptsTrue = allTrueCPTsList[[i]]
    
    df = prediction(cptsTrue, dataTestingNumeric, maxNumParents, maxNumValues, debug = debug)
    
    write.csv(df, paste0(currentDirectory, "/Evaluations/reference rmse and bir/", allTestSets[i], ".csv"), row.names = FALSE)
    
  }
  
}


# compute rmse and bir for all learned cpts with fixed dag 
autoPredictionFixedDag = function(currentDirectory, learningMethod, numIterations, maxNumParents, maxNumValues, 
                                  trainingDataList, testingDataNumericList, debug = FALSE) {
  
  dataTraining = trainingDataList[[1]]
  dataTestingNumeric = testingDataNumericList[[i]][1:1000,] # only take 1000 samples for testing
  
  # learned cpts
  allLearnedCPTs = list.files(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod)) # load all learned cpts
  allLearnedCPTs = orderFiles(allLearnedCPTs, currentDirectory, numIterations) # order all cpts
  
  
  for (i in 1:length(allLearnedCPTs)) {
    
    cptsLearned = readRDS(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", allLearnedCPTs[i]))
    
    df = prediction(cptsLearned, dataTestingNumeric, maxNumParents, maxNumValues, debug = debug)
    
    write.csv(df$rmse, paste0(currentDirectory, "/Evaluations/rmse/", learningMethod, "/", allLearnedCPTs[j], ".csv"), row.names = FALSE)
    
    write.csv(df$bir, paste0(currentDirectory, "/Evaluations/bir/", learningMethod, "/", allLearnedCPTs[j], ".csv"), row.names = FALSE)
    
  }
  
}

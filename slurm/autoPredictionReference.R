##################################################################################
# compute rmse and bir by constructing a prediction model 
# increasing prediction accuracy indicates a decreasing rmse and increasing bir
##################################################################################

source("Rcode/orderFiles.R")

source("Rcode/autoPrediction.R")
#######################################################################################################
# auto prediction starts here 
#######################################################################################################
autoPredictionReference = function(currentDirectory, numIterations, maxNumParents, maxNumValues, debug = FALSE) {
  
  # list and order all data sets
  allDataSets = list.files(paste0(currentDirectory, "/Datasets/Numeric"))
  allDataSets = orderFiles(allDataSets, currentDirectory, numIterations)
  
  trueCPTs = list.files(paste0(currentDirectory, "/True networks/CPTs"))
  trueCPTs = orderFiles(trueCPTs, currentDirectory, numIterations)
  
  taskid <- Sys.getenv("SLURM_ARRAY_TASK_ID") 
  
  fileIndex = strtoi(taskid) 
  
  cat ("Our TaskID is ",fileIndex," and our filename is ",allDataSets[fileIndex],"\n")
  
  # read data
  data = readRDS(paste0(currentDirectory, "/Datasets/Numeric/", allDataSets[fileIndex]))
  
  # convert data from data.frame to matrix to speed up 
  data = as.matrix(data[1:10000,])
  
  #test write output to a Lustre File System
  DIR <- "/home/kli/p2016040001/Experiments_31032016/maxNumParents/Evaluations/reference/"
  
  # read learned cpts
  cptsTrue = readRDS(paste0(currentDirectory, "/True networks/CPTs/", trueCPTs[fileIndex]))
  
  # compute actual rmse and bir using 10000 samples from data
  actual = prediction(cptsTrue, data, maxNumParents, maxNumValues, debug = debug)
  
  write.csv(actual, paste0(DIR, allDataSets[fileIndex], ".csv"), row.names = FALSE)

}






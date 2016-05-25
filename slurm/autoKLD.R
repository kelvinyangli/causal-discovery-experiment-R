autoKLD = function(currentDirectory, learningMethod, numIterations = 20, debug = FALSE) {
  
  allTrueCPTs = list.files(paste0(currentDirectory, "/True networks/CPTs"))
  allTrueCPTs = orderFiles(allTrueCPTs, currentDirectory, numIterations)
  
  allLearnedCPTs = list.files(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod))
  allLearnedCPTs = orderFiles(allLearnedCPTs, currentDirectory, numIterations)
  
  taskID = Sys.getenv("SLURM_ARRAY_TASK_ID")
  
  fileIndex = strtoi(taskID)
  
  cptsTrue = readRDS(paste0(currentDirectory, "/True networks/CPTs/", allTrueCPTs[fileIndex]))
  
  cptsLearned = readRDS(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", allLearnedCPTs[fileIndex]))
  
  klDivergence = kld(cptsTrue, cptsLearned, debug = debug)[[1]] # compute kld between true and learned
    
  write.csv(klDivergence, paste0(currentDirectory, "/Evaluations/kld/", learningMethod, "/", allTrueCPTs[fileIndex], ".csv"), row.names = FALSE)
  
}
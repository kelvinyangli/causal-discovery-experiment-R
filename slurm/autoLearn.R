################################# Structure learning ####################################
# current learning algorithms including 
# bnlearn::mmhc, aic, bic, bde, k2
# other algorithms that will be included in are 
# pcalg::pc, ges
#########################################################################################

autoLearn = function(currentDirectory, learningMethod, numIterations, debug = FALSE) {
  
  allTrainingData = list.files(paste0(currentDirectory, "/Datasets/Training")) # list all training data in folder
  allTrainingData = orderFiles(allTrainingData, currentDirectory, numIterations) # re-order all files
  
  taskID = Sys.getenv("SLURM_ARRAY_TASK_ID")
  
  fileIndex = strtoi(taskID)
  
  data = readRDS(paste0(currentDirectory, "/Datasets/Training/", allTrainingData[fileIndex]))
  
  # learn structures from data
  
  if (debug) cat("* learning", fileIndex, "\n")
  
  if (learningMethod == "mmhc") { # mmhc algorithm
    
    dagLearned = mmhc(data)
  
  } else if (learningMethod == "pc") {
   
    dagLearned = pcAlgorithm(data)
    
    # transform cpdag in class pcalg to dag in bn
    dagLearned = toClassBN(dagLearned)
    
  } else { # K2, BDe, AIC, BIC all use hill-climbing for seaching
    
    dagLearned = hc(x = data, score = learningMethod)
    
  } 
  
  # estimate cpts using bn.fit with method = bayes to avoid 0 in cpts values
  
  cptsLearned = bn.fit(dagLearned, data, method = "bayes") 
  
  # save structure to .dot, cpts to .net files
  
  if (debug) cat("** saving", fileIndex, "\n")
  
  write.dot(paste0(currentDirectory, "/Learned networks/Structures/", learningMethod, "/", allTrainingData[fileIndex], ".dot"), dagLearned)
  
  # write.net(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", fileName, "_", seed, ".net"), cptsLearned)
  saveRDS(cptsLearned, paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", allTrainingData[fileIndex]))
  
}

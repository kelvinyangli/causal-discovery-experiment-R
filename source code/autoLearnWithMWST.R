# learn structure using aic, bic, bde, and k2 with node ordering by mwst from matlab

autoLearnWithMWST = function(currentDirectory, learningMethod, numIterations, alpha = 0.05, debug = FALSE) {
  
  allTrainingData = list.files(paste0(currentDirectory, "/Datasets/Training")) # list all training data in folder
  allTrainingData = orderFiles(allTrainingData, currentDirectory, numIterations) # re-order all files
  
  orderings = list.files(paste0(currentDirectory, "/Ordering/"))
  orderings = orderFiles(orderings, currentDirectory, numIterations)
  
  for (i in 1:length(allTrainingData)) {
    
    data = readRDS(paste0(currentDirectory, "/Datasets/Training/", allTrainingData[i]))
    
    ordering = read.csv(paste0(currentDirectory, "/Ordering/", orderings[i]), header = FALSE)
    
    ordering = t(ordering)
    
    data = data[,ordering] # take ordering from MWST learned in matlab's bnt
    
    # learn structures from data
    
    if (debug) cat("* learning", i, "\n")
    
    dagLearned = hc(x = data, score = learningMethod)
      
    # estimate cpts using bn.fit with method = bayes to avoid 0 in cpts values
    
    cptsLearned = bn.fit(dagLearned, data, method = "bayes") 
    
    # save structure to .dot, cpts to .net files
    
    if (debug) cat("** saving", i, "\n")
    
    write.dot(paste0(currentDirectory, "/Learned networks/Structures/", learningMethod, "/", allTrainingData[i], ".dot"), dagLearned)
    
    # write.net(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", fileName, "_", seed, ".net"), cptsLearned)
    saveRDS(cptsLearned, paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", allTrainingData[i]))
    
  } # end for i  
  
}

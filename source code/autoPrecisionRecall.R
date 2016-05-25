# count the tp, fp and fn 
# use bnlearn::compare

autoAccuracy = function(currentDirectory, learningMethod, numIterations, allTrueDagsList, debug = FALSE) {
  
  allLearnedCPTs = list.files(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod))
  allLearnedCPTs = orderFiles(allLearnedCPTs, currentDirectory, numIterations)
  
  numTP = vector(length = length(allLearnedCPTs))
  numFP = numTP
  numFN = numTP
  
  for (i in 1:length(allTrueDagsList)) {
    
    dagTrue = allTrueDagsList[[i]] # convert cpts to dag
    
    if (debug) cat("* calculating accuracy for the", j, "learned dag \n")
    
    cptsLearned = readRDS(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", allLearnedCPTs[i]))
    dagLearned = model2network(modelstring(cptsLearned)) # convert cpts to dag
    
    accuracy = bnlearn::compare(dagTrue, dagLearned) # compare the learned dag with the true dag
    
    numTP[i] = accuracy$tp # true positive
    numFP[i] = accuracy$fp # false positive
    numFN[i] = accuracy$fn # false negative
      
  }
  
  # save counting results to directory
  write.csv(numTP, paste0(currentDirectory, "/Evaluations/accuracy/", learningMethod, "_TP.csv"), row.names = FALSE)
  write.csv(numFP, paste0(currentDirectory, "/Evaluations/accuracy/", learningMethod, "_FP.csv"), row.names = FALSE)
  write.csv(numFN, paste0(currentDirectory, "/Evaluations/accuracy/", learningMethod, "_FN.csv"), row.names = FALSE)
  
}

autoAccuracyFixedDag = function(currentDirectory, learningMethod, numIterations, allTrueDagsList, debug = FALSE) {
  
  allLearnedCPTs = list.files(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod))
  allLearnedCPTs = orderFiles(allLearnedCPTs, currentDirectory, numIterations)
  
  numTP = vector(length = length(allLearnedCPTs))
  numFP = numTP
  numFN = numTP
  
  dagTrue = allTrueDagsList[[1]] 
  
  for (j in 1:length(allLearnedCPTs)) {
    
    if (debug) cat("* calculating accuracy for the", j, "learned dag \n")
    
    cptsLearned = readRDS(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", allLearnedCPTs[j]))
    dagLearned = model2network(modelstring(cptsLearned)) # convert cpts to dag
    
    accuracy = bnlearn::compare(dagTrue, dagLearned) # compare the learned dag with the true dag
    
    numTP[j] = accuracy$tp # true positive
    numFP[j] = accuracy$fp # false positive
    numFN[j] = accuracy$fn # false negative
    
  }
  
  # save counting results to directory
  write.csv(numTP, paste0(currentDirectory, "/Evaluations/accuracy/", learningMethod, "_TP.csv"), row.names = FALSE)
  write.csv(numFP, paste0(currentDirectory, "/Evaluations/accuracy/", learningMethod, "_FP.csv"), row.names = FALSE)
  write.csv(numFN, paste0(currentDirectory, "/Evaluations/accuracy/", learningMethod, "_FN.csv"), row.names = FALSE)
  
}

# compute precision and recall from accuracy above
autoPrecisionRecall = function(currentDirectory, numIterations) {
  
  allTPs = list.files(paste0(currentDirectory, "/Evaluations/accuracy"), pattern = "_TP.csv")
  allFPs = list.files(paste0(currentDirectory, "/Evaluations/accuracy"), pattern = "_FP.csv")
  allFNs = list.files(paste0(currentDirectory, "/Evaluations/accuracy"), pattern = "_FN.csv")
  
  for (i in 1:length(allTPs)) {
    
    TP = read.csv(paste0(currentDirectory, "/Evaluations/accuracy/", allTPs[i]), header = TRUE)
    FP = read.csv(paste0(currentDirectory, "/Evaluations/accuracy/", allFPs[i]), header = TRUE)
    FN = read.csv(paste0(currentDirectory, "/Evaluations/accuracy/", allFNs[i]), header = TRUE)
    
    precision = TP/(TP + FP)
    
    precision[is.na(precision)] = 0 # replace NA by 0
    
    recall = TP/(TP + FN)
    
    recall[is.na(recall)] = 0 # replace NA by 0
    
    learningMethod = strsplit(allTPs[i], split = "\\_")[[1]][1]
    
    write.csv(precision, paste0(currentDirectory, "/Evaluations/precision/", learningMethod, ".csv"), row.names = FALSE)
    
    write.csv(recall, paste0(currentDirectory, "/Evaluations/recall/", learningMethod, ".csv"), row.names = FALSE)
    
    autoStats(currentDirectory, learningMethod, "precision", as.vector(t(precision)), numIterations, alpha = 0.05)
    
    autoStats(currentDirectory, learningMethod, "recall", as.vector(t(recall)), numIterations, alpha = 0.05)
  }
  
}


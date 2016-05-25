# compute f measure from accuracy above
autoFMeasure = function(currentDirectory, numIterations) {
  
  allTPs = list.files(paste0(currentDirectory, "/Evaluations/accuracy"), pattern = "_TP.csv")
  allFPs = list.files(paste0(currentDirectory, "/Evaluations/accuracy"), pattern = "_FP.csv")
  allFNs = list.files(paste0(currentDirectory, "/Evaluations/accuracy"), pattern = "_FN.csv")
  
  for (i in 1:length(allTPs)) {
    
    TP = read.csv(paste0(currentDirectory, "/Evaluations/accuracy/", allTPs[i]), header = TRUE)
    FP = read.csv(paste0(currentDirectory, "/Evaluations/accuracy/", allFPs[i]), header = TRUE)
    FN = read.csv(paste0(currentDirectory, "/Evaluations/accuracy/", allFNs[i]), header = TRUE)
    
    precision = TP/(TP + FP)
    
    recall = TP/(TP + FN)
    
    fMeasure = 2*(precision * recall)/(precision + recall)
    
    fMeasure[is.na(fMeasure)] = 0 # replace NA with 0 
    
    learningMethod = strsplit(allTPs[i], split = "\\_")[[1]][1] # get learning method from file name
    
    write.csv(fMeasure, paste0(currentDirectory, "/Evaluations/f measure/", learningMethod, ".csv"), row.names = FALSE)
    
    autoStats(currentDirectory, learningMethod, "f measure", as.vector(t(fMeasure)), numIterations, alpha = 0.05)
  
  }
  
}
autoKLDApprox = function(currentDirectory, learningMethod, numIterations = 20, debug = FALSE) {
  
  datasets = list.files(paste0(currentDirectory, "/Datasets/Training"))
  datasets = orderFiles(datasets, currentDirectory, numIterations)
  
  allLearnedCPTs = list.files(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod))
  allLearnedCPTs = orderFiles(allLearnedCPTs, currentDirectory, numIterations)
  
  klDivergence = rep(0, length(allLearnedCPTs))
  
  for (i in 1:length(allLearnedCPTs)) {
  #for (i in 181:length(allLearnedCPTs)) { # compute kld b/w each true and learned cpts
    
    if (debug) if ((i %% numIterations) == 0) cat("computing kld when", currentDirectory, "=", i/numIterations, "\n")
    
    data = readRDS(paste0(currentDirectory, "/Datasets/Training/", datasets[i]))
    
    cptsLearned = readRDS(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", allLearnedCPTs[i]))
    
    klDivergence[i] = kldApprox(data, cptsLearned, debug = FALSE)[[1]] # compute kld between true and learned
    
    #write.csv(klDivergence, paste0(currentDirectory, "/Evaluations/kld/", learningMethod, "/", allTrueCPTs[i], ".csv"), row.names = FALSE)
  } # end for i
  
  write.csv(klDivergence, paste0(currentDirectory, "/Evaluations/kld2/", learningMethod, ".csv"), row.names = FALSE)
  
  autoStats(currentDirectory, learningMethod, "kld2", klDivergence, numIterations)
  
}

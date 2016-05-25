##########################################################################################
# compute kld 
# R cannot compute kld for network with 50 nodes due to memory issue
##########################################################################################

# source('C:/Users/Administrator/Dropbox/PhD@Monash/R/Code/Experiments/source code/kld.R')

# source('C:/Users/Administrator/Dropbox/PhD@Monash/R/Code/Experiments/source code/autoStats.R')

# source('C:/Users/Administrator/Dropbox/PhD@Monash/R/Code/Experiments/source code/orderFiles.R')

# source('C:/Users/Administrator/Dropbox/PhD@Monash/R/Code/Experiments/source code/mutualInfo.R')

# source('C:/Users/Administrator/Dropbox/PhD@Monash/R/Code/Experiments/source code/getIndexForKLD.R')

autoKLD = function(currentDirectory, learningMethod, numIterations = 20, debug = FALSE) {
  
  allTrueCPTs = list.files(paste0(currentDirectory, "/True networks/CPTs"))
  allTrueCPTs = orderFiles(allTrueCPTs, currentDirectory, numIterations)
  
  allLearnedCPTs = list.files(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod))
  allLearnedCPTs = orderFiles(allLearnedCPTs, currentDirectory, numIterations)
  
  klDivergence = rep(0, length(allLearnedCPTs))
  
  for (i in 1:length(allLearnedCPTs)) {
  #for (i in 1:140) { # compute kld b/w each true and learned cpts
    
    if (debug) if ((i %% numIterations) == 0) cat("computing kld when", currentDirectory, "=", i/numIterations, "\n")

    cptsTrue = readRDS(paste0(currentDirectory, "/True networks/CPTs/", allTrueCPTs[i]))
    
    cptsLearned = readRDS(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", allLearnedCPTs[i]))
    
    klDivergence[i] = kld(cptsTrue, cptsLearned, debug = FALSE)[[1]] # compute kld between true and learned
    
    #write.csv(klDivergence, paste0(currentDirectory, "/Evaluations/kld/", learningMethod, "/", allTrueCPTs[i], ".csv"), row.names = FALSE)
  } # end for i
  
  write.csv(klDivergence, paste0(currentDirectory, "/Evaluations/kld/", learningMethod, ".csv"), row.names = FALSE)
  
  autoStats(currentDirectory, learningMethod, "kld", klDivergence, numIterations)
  
}


# compute kld for up to max 7 parents












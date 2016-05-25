# compute shd/edit distance for cpdags dags, skeletons
# bnlearn::shd for cpdags
# bnlearn::hamming for skeletons
# hammingDags for dags (own function)

# compute the hamming distance between dags
# shd between cpdags, hamming between skeletons

# check if arcs x and y are same, reversed, or different
checkArc = function(x, y) {
  
  if (prod(x == y)) {
    
    allEqual = 1
    
  } else if (prod(x == rev(y))) {
    
    allEqual = -1
    
  } else {
    
    allEqual = 0
    
  }
  
  return(allEqual)
  
}

hammingDags = function(learned, true) {
  
  arcsLearned = directed.arcs(learned) 
  
  arcsTrue = directed.arcs(true)
  
  addition = 0
  deletion = 0
  reversion = 0
  
  # checkTrue = 0 means arc in true not in learned
  # hence addition add 1
  # checkTrue = -1 means arc in both but wrong direction
  # hence reversion add 1
  
  if ((nrow(arcsTrue) == 0) &&(nrow(arcsLearned) > 0)) {
    
    # if dagTrue is empty but not dagLearned 
    deletion = nrow(arcsLearned)
    
  } else if ((nrow(arcsTrue) > 0) && (nrow(arcsLearned) == 0)) {
    
    # if dagLearned is empty but not dagTrue
    addition = nrow(arcsTrue)
    
  } else if ((nrow(arcsTrue) > 0) && (nrow(arcsLearned) > 0)) {
    
    # if both dags are not empty
    for (i in 1:nrow(arcsTrue)) {
      
      # check if each arc in true appears in learned with the correct or reversed direction
      # as there can be only one 1 or -1, then sum up all results
      checkTrue = sum(apply(arcsLearned, 1, checkArc, x = arcsTrue[i,]))
      
      if (checkTrue == 0) {
        
        addition = addition + 1
        
      } else if (checkTrue == -1) {
        
        reversion = reversion + 1
        
      }
      
    } # end for i
    
    for (j in 1:nrow(arcsLearned)) {
      
      # check if each arc in learned appears in true
      # as there can be only one 1 or -1, sum up all results
      checkLearned = sum(apply(arcsTrue, 1, checkArc, x = arcsLearned[j,]))
      
      if (checkLearned == 0) deletion = deletion + 1
      
    } # end for j
    
  }
  
  return(sum(addition, deletion, reversion))
  
}

autoEditDistance = function(currentDirectory, learningMethod, numIterations, allTrueDagsList, debug = FALSE) {
  
  allLearnedCPTs = list.files(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod))
  allLearnedCPTs = orderFiles(allLearnedCPTs, currentDirectory, numIterations)
  
  # 3 empty score vectors
  shdCpdags = vector(length = length(allLearnedCPTs)) 
  shdSkeletons = shdCpdags
  shdDags = shdCpdags
  
  for (i in 1:length(allTrueDagsList)) {
    
    dagTrue = allTrueDagsList[[i]]
    
    if (debug) cat("* calculating SHD for the ", j, "file \n")
    
    cptsLearned = readRDS(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", allLearnedCPTs[i]))
    dagLearned = model2network(modelstring(cptsLearned))
    
    # shd for cpdags
    shdCpdags[i] = bnlearn::shd(learned = dagLearned, true = dagTrue)
    
    # shd for skeletons
    shdSkeletons[i] = hamming(learned = dagLearned, true = dagTrue)
    
    # shd for dags
    shdDags[i] = hammingDags(learned = dagLearned, true = dagTrue)
    
  }
  
  write.csv(shdCpdags, paste0(currentDirectory, "/Evaluations/shd cpdags/", learningMethod, ".csv"), row.names = FALSE)
  
  write.csv(shdDags, paste0(currentDirectory, "/Evaluations/shd dags/", learningMethod, ".csv"), row.names = FALSE)
  
  write.csv(shdSkeletons, paste0(currentDirectory, "/Evaluations/shd skeletons/", learningMethod, ".csv"), row.names = FALSE)
  
  autoStats(currentDirectory, learningMethod, "shd cpdags", shdCpdags, numIterations, alpha = 0.05)
  
  autoStats(currentDirectory, learningMethod, "shd dags", shdDags, numIterations, alpha = 0.05)
  
  autoStats(currentDirectory, learningMethod, "shd skeletons", shdSkeletons, numIterations, alpha = 0.05)
  
}

autoEditDistanceFixedDag = function(currentDirectory, learningMethod, numIterations, allTrueDagsList, debug = FALSE) {
  
  allLearnedCPTs = list.files(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod))
  allLearnedCPTs = orderFiles(allLearnedCPTs, currentDirectory, numIterations)
  
  # 3 empty score vectors
  shdCpdags = vector(length = length(allLearnedCPTs)) 
  shdSkeletons = shdCpdags
  shdDags = shdCpdags
  
  dagTrue = allTrueDagsList[[1]]
  
  for (j in 1:length(allLearnedCPTs)) {
    
    if (debug) cat("* calculating SHD for the ", j, "file \n")
    
    cptsLearned = readRDS(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", allLearnedCPTs[j]))
    dagLearned = model2network(modelstring(cptsLearned))
    
    # shd for cpdags
    shdCpdags[j] = bnlearn::shd(learned = dagLearned, true = dagTrue)
    
    # shd for skeletons
    shdSkeletons[j] = hamming(learned = dagLearned, true = dagTrue)
    
    # shd for dags
    shdDags[j] = hammingDags(learned = dagLearned, true = dagTrue)
    
  }
    
  write.csv(shdCpdags, paste0(currentDirectory, "/Evaluations/shd cpdags/", learningMethod, ".csv"), row.names = FALSE)
  
  write.csv(shdDags, paste0(currentDirectory, "/Evaluations/shd dags/", learningMethod, ".csv"), row.names = FALSE)
  
  write.csv(shdSkeletons, paste0(currentDirectory, "/Evaluations/shd skeletons/", learningMethod, ".csv"), row.names = FALSE)
  
  autoStats(currentDirectory, learningMethod, "shd cpdags", shdCpdags, numIterations, alpha = 0.05)
  
  autoStats(currentDirectory, learningMethod, "shd dags", shdDags, numIterations, alpha = 0.05)
  
  autoStats(currentDirectory, learningMethod, "shd skeletons", shdSkeletons, numIterations, alpha = 0.05)
  
}




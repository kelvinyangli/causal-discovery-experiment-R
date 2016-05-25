kldApprox = function(data, cptsLearned, debug = FALSE) {
  
  allNodes = names(data)
  
  miSum = 0
  
  for (i in 1:length(allNodes)) {
    
    node = allNodes[i]
    
    parents = cptsLearned[[node]]$parents
    
    if (length(parents) > 0) { # only computing mi when parents(node) is non-empty
      
      if (debug) cat("* approximating mi for node", i, "\n")
      
      miSum = miSum + mutualInfoApprox(node, parents, data)
      
    } # end if 
    
  } # end for i
  
  return(miSum)
  
}
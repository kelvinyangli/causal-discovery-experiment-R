# try marginalise probability

f = function(cpts) {
  
  allNodes = bnlearn::nodes(cpts)
  
  cardinalities = rep(2, length(allNodes)) # store the cardinality of each node
  for (ii in 1:length(allNodes)) {
    cardinalities[ii] = nrow(cpts[[ii]]$prob)
  }
  
  #marginalProbList = list() # list of marginal probability for each node
  #marginalProbList[[allNodes[1]]] = cpts[[1]]$prob # first node doesn't need marginalise 
  
  marginalMatrix = matrix(0, ncol = length(allNodes), nrow = max(cardinalities), dimnames = list(LETTERS[1:max(cardinalities)], allNodes))
  marginalMatrix[,1] = cpts[[1]]$prob
  
  for (i in 2:length(allNodes)) {
    node = allNodes[[i]]
    cpt = cpts[[i]]$prob
    cardinality = nrow(cpt)
    parents = bnlearn::parents(cpts, node)
    prob = rep(0, cardinality)
    
    # if node has only one parent
    if (length(parents) < 1) {
      
      # if there is no parents 
      prob = cpt
      
    } else if (length(parents) < 2) {
      
      # if there is only one parent
      probParents = marginalProbList[[parents]]
      
      for (j in 1:(cardinality - 1)) { # iterate through n-1 values
        prob[j] = sum(cpt[j,]*probParents)
      }
      
      prob[cardinality] = 1 - sum(prob)
      
      #marginalProb = array(prob, dim = c(1, cardinality), dimnames = list(NULL, rownames(cpt)))
      
    } else {
      
      # if there is more than 1 parent
      # get the prob for each parent
      probParents = list()
      for (y in parents) {
        probParents[[y]] = marginalMatrix[,y]
      }
      # end else
    }
    
    marginalMatrix[,i] = prob # store probabilities into matrix
    
  }
  
  return(marginalProbList)
}


getJoint = function(cpt, probParents) {
  
}




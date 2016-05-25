condProb2 = function(dataTestingNumeric, dataPointIndex, target, cardinalities, MBs, parents, cpts, dimensions, 
                     childParentsIndex, tempPredicted) {
  
  targetValue = dataTestingNumeric[dataPointIndex, target] # store original value for target 
  
  sumBeforeNormalize = 0 
  
  for (i in 1:cardinalities[target]) { # compute joint for each value of the target
    
    tempPredicted[i] = 1 # reset needed tempPredicted to 1 in order to do multiplications
    
    dataTestingNumeric[dataPointIndex, target] = i # change current value in data point to i
      
    for(j in 1:length(MBs[[target]])) { # compute conditional for each node within in mb given its parents 
      
      childParentsIndex[1] = dataTestingNumeric[dataPointIndex, MBs[[target]][j]] # get value for current node
      
      if (length(parents[[MBs[[target]][j]]]) < 1) { # if a node has no parents, its conditional given its parents is the same 
        # as its marginal
        
        tempPredicted[i] = tempPredicted[i] * cpts[[MBs[[target]][j]]][childParentsIndex[1]]
        
      } else { # if a node has parents, get conditional from cpt using getIndex
        
        for (jj in 1:length(parents[[MBs[[target]][j]]])) { # for each parent of current node
          
          # get parent's value
          childParentsIndex[jj + 1] = dataTestingNumeric[dataPointIndex, parents[[MBs[[target]][j]]][jj]]
          
        }
        
        tempPredicted[i] = 
          tempPredicted[i] * cpts[[MBs[[target]][j]]][getIndex(MBs[[target]][j], dimensions, childParentsIndex)]
        
      } # end else
      
    } # end for j
    
    # multiply tempPredicted[i] by cond prob of targetNode given its parents
    if (length(parents[[target]]) < 1) { # if the target has no parents
      
      tempPredicted[i] = tempPredicted[i] * cpts[[target]][i]
      
    } else { # if it has parents
      
      childParentsIndex[1] = i
      
      for (jj in 1:length(parents[[target]])) {
        
        childParentsIndex[[jj + 1]] = dataTestingNumeric[dataPointIndex, parents[[target]][jj]]
        
      }
      
      tempPredicted[i] = tempPredicted[i] * cpts[[target]][getIndex(target, dimensions, childParentsIndex)]
      
    }
    
    sumBeforeNormalize = sumBeforeNormalize + tempPredicted[i]
    
  } # end for i
  
  # normalize tempPredicted to get cond prob of target given its MB
  for (l in 1:cardinalities[target]) {
    
    tempPredicted[l] = tempPredicted[l] / sumBeforeNormalize
    
  }
  
  dataTestingNumeric[dataPointIndex, target] = targetValue # change the value back to original 
  
  return(tempPredicted)
  
}

condProb2 = cmpfun(condProb2)
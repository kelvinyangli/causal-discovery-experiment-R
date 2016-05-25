# compute expected square error and bir for a single data point 
expPred = function(dataTestingNumeric, dataPointIndex, target, cardinalities, priors, tempPredicted) {
  
  se = 0 
  bir = 0 
  
  for (i in 1:(cardinalities[target])) {
    
    for (j in 1:cardinalities[target]) {
      
      if (j == i) {
        
        se = se + (1 - tempPredicted[j])^2
        
        bir = bir + log(tempPredicted[i]/priors[[target]][i])
        
      } else {
        
        se = se + tempPredicted[j]^2
        
        bir = bir + log((1 - tempPredicted[i])/(1 - priors[[target]][i]))
        
      }
      
    }
    
  } # end for 
  
  # average over all values of target
  se = se / cardinalities[target]
  
  bir = bir / cardinalities[target]
  
  df = data.frame(se = se, bir = bir)
  
  return(df)
  
}


# compute rmse and  bir for entire test set
expPrediction = function(cptsLearned, dataTraining, dataTestingNumeric, maxNumParents, maxNumValues, target, debug = FALSE) {
  
  nodes = colnames(dataTestingNumeric)
  parents = list()
  MBs = list()
  cpts = list()
  priors = list()
  cardinalities = c()
  dimensions = list()
  
  # get all required information first before loops
  for (x in nodes) {
    
    parents[[x]] = cptsLearned[[x]]$parents
    MBs[[x]] = bnlearn::mb(cptsLearned, x)
    cpts[[x]] = cptsLearned[[x]]$prob
    dimensions[[x]] = dim(cpts[[x]])
    priors[[x]] = table(dataTraining[,x])/nrow(dataTraining)
    cardinalities[x] = length(priors[[x]])
    
  }
  
  # empty vector to store a node and its parents' index for taking value from cpt
  childParentsIndex = vector(length = maxNumParents + 1)
  
  # empty vecto to store conditional probabilities of a target given its parents
  tempPredicted = vector(length = maxNumValues)
  
  se = 0 # initial square error 
  bir = 0 # initial bir 
  
  for (i in 1:nrow(dataTestingNumeric)) {
    
    if (debug) cat("* computing rmse and bir for row", i, "\n")
    
    if (length(MBs[[target]]) < 1) { # if target has empty mb, conditional is maginal
      
      for (k in 1:cardinalities[target]) { # replace values in tempPredicted by cond probabilities
        
        tempPredicted[k] = cpts[[target]][k]
        
      }
      
    } else if ((length(MBs[[target]]) > 0) && (length(MBs[[target]]) == length(parents[[target]]))) { 
      # if length(mb) > 0 and mb = parents, conditional is same as cpt
      
      for (l in 1:length(parents[[target]])) { # store indexes for all parents values
        
        childParentsIndex[l + 1] = dataTestingNumeric[i, parents[[target]][l]] 
        
      }
      
      # target value always starts from "A", i.e. 1
      childParentsIndex[1] = 1
      
      # correponding cpt index 
      cptIndex = getIndex(target, dimensions, childParentsIndex)
      
      # predicted cond prob for the case when target takes the 1st value "A" given above parents values
      tempPredicted[1] = cpts[[target]][cptIndex]
      
      for (j in 2:cardinalities[target]) { # for other values of the target, simply go to the next (j-1) entries in cpt
        
        cptIndex = cptIndex + (j - 1)
        
        tempPredicted[j] = cpts[[target]][cptIndex]
        
      }
      
    } else { # for all other conditions, compute the product of cond prob of a node given its parents, 
      # where this node is within mb(target)
      
      tempPredicted = 
        condProb(dataTestingNumeric = dataTestingNumeric, dataPointIndex = i, target = target, 
                 cardinalities = cardinalities, MBs = MBs, parents = parents, cpts = cpts, 
                 dimensions = dimensions, childParentsIndex = childParentsIndex, tempPredicted = tempPredicted)
      
    } # end else 
    
    # compute the average square error and bir over all values of target
    result = expPred(dataTestingNumeric = dataTestingNumeric, dataPointIndex = i, target = target, 
                     cardinalities = cardinalities, 
                     priors = priors, tempPredicted = tempPredicted)
    
    # sum up all average square errors and bir 
    se = se + result$se
    
    bir = bir + result$bir
    
  } # end for i
  
  df = data.frame(expRMSE = sqrt(se/nrow(dataTestingNumeric)), expBIR = bir/nrow(dataTestingNumeric))
  
  return(df)
  
}


expPred = cmpfun(expPred)
expPrediction = cmpfun(expPrediction)


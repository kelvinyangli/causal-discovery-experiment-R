##################################################################################
# compute rmse and bir by constructing a prediction model 
# increasing prediction accuracy indicates a decreasing rmse and increasing bir
##################################################################################

#source('C:/Users/Administrator/Dropbox/PhD@Monash/R/Code/Experiments/source code/orderFiles.R')

#source('C:/Users/Administrator/Dropbox/PhD@Monash/R/Code/Experiments/source code/toNumeric.R')

#source('C:/Users/Administrator/Dropbox/PhD@Monash/R/Code/Experiments/source code/getIndexForPrediction.R')

# compute cond prob of target given other nodes
# this is achieved by computing the joint of all nodes then nomalise 
# the joint is computed by product of conditional probabilities of each node given its parents
condProb = function(dataTestingNumeric, dataPointIndex, target, cardinalities, parents, childrens, cpts, dimensions, 
                    childParentsIndex, tempPredicted) {
  
  originalTargetValue = dataTestingNumeric[dataPointIndex, target] # store original value for target 
  
  sumBeforeNormalize = 0 
  
  for (i in 1:cardinalities[target]) { # compute joint for each value of the target
    
    tempPredicted[i] = 1 # reset needed tempPredicted to 1 in order to do multiplications
    
    dataTestingNumeric[dataPointIndex, target] = i # change current value in data point to i
    
    # find children of tareget
    for (j in 1:length(childrens[[target]])) {
      
      node = childrens[[target]][j]
      
      childParentsIndex[1] = dataTestingNumeric[dataPointIndex, node] # get value for current node
      
      for (jj in 1:length(parents[[node]])) { # for each parent of current node
        
        # get parent's value
        childParentsIndex[jj + 1] = dataTestingNumeric[dataPointIndex, parents[[node]][jj]]
        
      }
      
      tempPredicted[i] = 
        tempPredicted[i] * cpts[[node]][getIndex(node, dimensions, childParentsIndex)]
      
    }
    
    # multiply tempPredicted[i] by cond prob of targetNode given its parents
    if (length(parents[[target]]) < 1) { # if the target has no parents
      
      tempPredicted[i] = tempPredicted[i] * cpts[[target]][i]
      
    } else { # if it has parents
      
      childParentsIndex[1] = i
      
      for (jj in 1:length(parents[[target]])) {
        
        childParentsIndex[[jj + 1]] = dataTestingNumeric[dataPointIndex, parents[[target]][jj]]
        
      } # end for jj
      
      tempPredicted[i] = tempPredicted[i] * cpts[[target]][getIndex(target, dimensions, childParentsIndex)]
      
    } # end else
    
    sumBeforeNormalize = sumBeforeNormalize + tempPredicted[i]
    
  } # end for i
  
  # normalize tempPredicted to get cond prob of target given its MB
  for (l in 1:cardinalities[target]) {
    
    tempPredicted[l] = tempPredicted[l] / sumBeforeNormalize
    
  }
  
  dataTestingNumeric[dataPointIndex, target] = originalTargetValue # change the value back to original 
  
  return(tempPredicted)
  
}


# compute mean square error and bir for a single data point over all target values
pred = function(dataTestingNumeric, dataPointIndex, target, cardinalities, priors, tempPredicted) {
  
  # indicator function for the actual value in test set
  indicator = dataTestingNumeric[dataPointIndex, target]
  
  se = 0 
  bir = 0 
  
  for (i in 1:(cardinalities[target])) {
    
    if (i == indicator) { # if correct prediction happens on ith position
      
      se = se + (1 - tempPredicted[i])^2
      
      bir = bir + log(tempPredicted[i]/priors[[target]][i])
      
    } else { # else ith position is the wrong prediction
      
      se = se + tempPredicted[i]^2
      
      bir = bir + log((1 - tempPredicted[i])/(1 - priors[[target]][i]))
      
    } # end else 
    
  } # end for 
  
  # average over all values of target
  se = se / cardinalities[target]
  
  bir = bir / cardinalities[target]

  df = data.frame(se = se, bir = bir)
  
  return(df)
  
}


# compute rmse and  bir for entire test set
# targetPercentage is the percentage of target nodes to the number of nodes 
# value between 0 and 1
# 0 indicates 1 target and 1 indicates all nodes
prediction = function(cptsLearned, dataTestingNumeric, maxNumParents, maxNumValues, debug = FALSE) {
  
  nodes = colnames(dataTestingNumeric)
  parents = list()
  childrens = list()
  cpts = list()
  priors = list()
  cardinalities = c()
  dimensions = list()
  
  # get all required information first before loops
  for (x in nodes) {
    
    parents[[x]] = cptsLearned[[x]]$parents
    childrens[[x]] = cptsLearned[[x]]$children
    cpts[[x]] = cptsLearned[[x]]$prob
    dimensions[[x]] = dim(cpts[[x]])
    priors[[x]] = table(dataTestingNumeric[,x])/nrow(dataTestingNumeric) # estimate prior from test set
    cardinalities[x] = length(priors[[x]])
    
  }
  
  numNodes = length(nodes)
  
  # empty vector to store a node and its parents' index for taking value from cpt
  childParentsIndex = vector(length = maxNumParents + 1)
  
  # empty vecto to store conditional probabilities of a target given its parents
  tempPredicted = vector(length = maxNumValues)
  
  # vector to accumulate se and bir for each variable
  se = rep(0, numNodes) 
  
  bir = se
  
  # compute rmse and bir for each data point 
  for (i in 1:nrow(dataTestingNumeric)) {
    
    if (debug) {
      
      if (i == 0.5*10000) cat("50% is done \n")
      else if (i == 10000) cat("100% is done \n")
    }
    
    for (ii in 1:numNodes) { # compute rmse and bir for all variables for ith data point
      
      target = nodes[ii]
      
      if ((length(parents[[target]]) < 1) && (length(childrens[[target]]) < 1)) { 
        # if target has empty mb, conditional is maginal
        
        for (k in 1:cardinalities[target]) { # replace values in tempPredicted by cond probabilities
          
          tempPredicted[k] = cpts[[target]][k]
          
        }
        
      } else if ((length(parents[[target]]) > 0) && (length(childrens[[target]]) < 1)) { 
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
          
          cptIndex = cptIndex + 1
          
          tempPredicted[j] = cpts[[target]][cptIndex]
          
        }
        
      } else { # for all other conditions, compute the product of cond prob of a node given its parents, 
        # where this node is within mb(target)
        
        tempPredicted = 
          condProb(dataTestingNumeric = dataTestingNumeric, dataPointIndex = i, target = target, 
                   cardinalities = cardinalities, parents = parents, childrens = childrens, cpts = cpts, 
                   dimensions = dimensions, childParentsIndex = childParentsIndex, tempPredicted = tempPredicted)
        
      } # end else 
      
      
      # compute the average square error and bir over all values of target
      result = pred(dataTestingNumeric = dataTestingNumeric, dataPointIndex = i, target = target, 
                    cardinalities = cardinalities, 
                    priors = priors, tempPredicted = tempPredicted)
      
      # sum up all average square errors and bir 
      se[ii] = se[ii] + result$se
      
      bir[ii] = bir[ii] + result$bir
      
    } # end for ii 
      
  } # end for i
  
  df = data.frame(rmse = sqrt(se/nrow(dataTestingNumeric)), bir = bir/nrow(dataTestingNumeric))
  
  return(df)
  
}


# compiler all functions
#toNumeric = cmpfun(toNumeric)
#getIndex = cmpfun(getIndex)
#condProb = cmpfun(condProb)
#pred = cmpfun(pred)
#prediction = cmpfun(prediction)


#######################################################################################################
# auto prediction starts here 
#######################################################################################################
autoPrediction = function(currentDirectory, numIterations, maxNumParents, maxNumValues, debug = FALSE) {
  
  # list and order all true cpts
  trueCPTs = list.files(paste0(currentDirectory, "/True networks/CPTs"))
  trueCPTs = orderFiles(trueCPTs, currentDirectory, numIterations)
  
  # list and order all learned cpts
  # since learned cpts using different methods are saved with the same name but under different folders, so it's ok to 
  # just list the learned cpts from one method and use them for all other methods
  learnedCPTs = list.files(paste0(currentDirectory, "/Learned networks/CPTs/mmhc"))
  learnedCPTs = orderFiles(learnedCPTs, currentDirectory, numIterations)
  
  # list and order all data sets
  allDataSets = list.files(paste0(currentDirectory, "/Datasets/Numeric"))
  allDataSets = orderFiles(allDataSets, currentDirectory, numIterations)
  
  # list all learning methods 
  learningMethods = c("mmhc", "aic", "bic", "bde", "k2")
  
  # compute the reference and actual rmse for each data set
  for (i in 1:length(allDataSets)) {
    
    # read data
    data = readRDS(paste0(currentDirectory, "/Datasets/Numeric/", allDataSets[i]))
    
    # read true cpts
    cptsTrue = readRDS(paste0(currentDirectory, "/True networks/CPTs/", trueCPTs[i]))
    
    # compute reference rmse and bir
    reference = prediction(cptsTrue, data[1:10000,], maxNumParents, maxNumValues, debug = debug)
    
    # save results to a csv file
    write.csv(reference, paste0(currentDirectory, "/Evaluations/reference/", trueCPTs[i], ".csv"), row.names = FALSE)
    
    # compute actual rmse and bir for each learning method
    for (j in 1:length(learningMethods)) {
      
      # read learned cpts
      cptsLearned = readRDS(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethods[j], "/", learnedCPTs[i]))
      
      # compute actual rmse and bir using 10000 samples from data
      actual = prediction(cptsLearned, data[1:10000,], maxNumParents, maxNumValues, debug = debug)
      
      write.csv(actual$rmse, paste0(currentDirectory, "/Evaluations/rmse/", learningMethods[j], "/", trueCPTs[i], ".csv"), row.names = FALSE)
      
      write.csv(actual$bir, paste0(currentDirectory, "/Evaluations/bir/", learningMethods[j], "/", trueCPTs[i], ".csv"), row.names = FALSE)
      
    }
    
  }
  
}



# compile functions
#autoPrediction = cmpfun(autoPrediction)





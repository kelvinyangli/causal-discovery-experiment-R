##################################### Root mean squre error #############################################
# library(gRain)

# use gRain::querrygrain
rmse = function(testingData, cptsLearned, targetNode, debug = FALSE) {
  allNodes = bnlearn::nodes(cptsLearned)
  actualValues = testingData[,targetNode]
  attributeLevels = levels(testingData[,targetNode])
  numLevels = nlevels(testingData[,targetNode])
  squareError = rep(0, nrow(testingData))
  targetNodeMB = mb(cptsLearned, targetNode) # find mb(targetNode)
  if (debug) cat("propagating cpt \n")
  if (length(targetNodeMB) < 1) {
    conditionalLearned = cptsLearned[[targetNode]]$prob
  } else {
    conditionalLearned = querygrain(as.grain(cptsLearned), nodes = c(targetNode, targetNodeMB), type = "conditional")
  }
  
  for (i in 1:nrow(testingData)) {
    if (debug) cat("calculating", i, "\n")
    if (length(targetNodeMB) < 1) {
      posteriorLearned = conditionalLearned[attributeLevels]
    } else {
      posteriorLearned = rep(0, numLevels)
      tempData = testingData[i, c(targetNodeMB, targetNode)]
      for (j in 1:(numLevels - 1)) {
        tempData[targetNode] = attributeLevels[j]
        posteriorLearned[j] = do.call("[", c(list(conditionalLearned), as.list(tempData)))
      }
      posteriorLearned[numLevels] = 1 - sum(posteriorLearned)
    }
    binaryRepresent = as.numeric(attributeLevels %in% actualValues[i])
    posteriorError = binaryRepresent - posteriorLearned
    squareError[i] = sum(posteriorError^2)
  }
  return(sqrt(mean(squareError)))
}

# iterate through test set
# testingData is multi-dimensional data frame
rmse2 = function(testingData, cptsLearned, targetNode, debug = FALSE) {
  allNodes = bnlearn::nodes(cptsLearned) 
  actualValues = testingData[,targetNode] # actual value of the target node in test set
  attributeLevels = levels(testingData[,targetNode]) # attribute levels of target node
  numLevels = nlevels(testingData[,targetNode]) # number of levels
  squareError = rep(0, nrow(testingData))
  posteriorLearned = rep(0, numLevels)
  
  for (i in 1:nrow(testingData)) {
    for (j in 1:numLevels) {
      if (debug) cat("calculating", c(i, j), "\n")
      posteriorLearned[j] = jointProb(targetNode, attributeLevels[j], testingData[i,], cptsLearned)
    }
    posteriorLearned = posteriorLearned/sum(posteriorLearned)
    binaryRepresent = as.numeric(attributeLevels %in% actualValues[i])
    posteriorError = binaryRepresent - posteriorLearned
    squareError[i] = sum(posteriorError^2)
  }
  return(sqrt(mean(squareError)))
}

# iterate through test set
# testingData is 1 row data frame
rmse3 = function(testingData, cptsLearned, targetNode, debug = FALSE) {
  allNodes = bnlearn::nodes(cptsLearned) 
  actualValues = testingData[,targetNode] # actual value of the target node in test set
  attributeLevels = levels(testingData[,targetNode]) # attribute levels of target node
  numLevels = nlevels(testingData[,targetNode]) # number of levels
  squareError = rep(0, 1)
  posteriorLearned = jointProb2(targetNode, attributeLevels, testingData, cptsLearned)
  posteriorLearned = posteriorLearned/sum(posteriorLearned)
  binaryRepresent = as.numeric(attributeLevels %in% actualValues)
  posteriorError = binaryRepresent - posteriorLearned
  squareError = sum(posteriorError^2)
  return(squareError)
}

# calculate the joint probability given a distribution
jointProb = function(targetNode, targetNodeValue, dataPoint, cpts) {
  allNodes = nodes(cpts) 
  dataPoint[targetNode] = targetNodeValue
  condProbList = rep(0, length(allNodes))
  for (i in 1:length(allNodes)) {
    currentNode = allNodes[i]
    currentParents = cpts[[i]]$parents
    currentCPT = cpts[[i]]$prob
    tempData = dataPoint[c(currentNode, currentParents)]
    condProbList[i] = do.call("[", c(list(currentCPT), as.list(tempData)))
  }
  return(prod(condProbList))
}

# compute joint prob for the same dataPoint except change target node value
jointProb2 = function(targetNode, attributeLevels, dataPoint, cpts) {
  allNodes = nodes(cpts) 
  dataPoint[targetNode] = attributeLevels[1] # assign targetNodeValue to dataPoint
  condProbList = rep(0, length(allNodes))
  joint = rep(0, numLevels)
  
  # for the first targetNode attribute 
  for (i in 1:length(allNodes)) {
    currentNode = allNodes[i]
    currentParents = cpts[[i]]$parents
    currentCPT = cpts[[i]]$prob
    tempData = dataPoint[c(currentNode, currentParents)]
    condProbList[i] = do.call("[", c(list(currentCPT), as.list(tempData)))
  }
  
  joint[1] = prod(condProbList)
  
  for (j in 2:numLevels) {
    dataPoint[targetNode] = attributeLevels[j] # assign jth attribute to dataPoint
    for (i in 1:length(allNodes)) {
      currentNode = allNodes[i]
      currentParents = cpts[[i]]$parents
      # if current node is target or it has target as one of its parents re-compute cond prob
      if ((currentNode == targetNode) | (targetNode %in% currentParents)) {
        currentCPT = cpts[[i]]$prob
        tempData = dataPoint[c(currentNode, currentParents)]
        condProbList[i] = do.call("[", c(list(currentCPT), as.list(tempData)))
      }
    }
    joint[j] = prod(condProbList)
  }
  return(joint)
}




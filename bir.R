################################ Bayesian Information Reward (BIR) ######################################
# library(gRain)
bir = function(testingData, cptsLearned, targetNode, debug = FALSE) {
  allNodes = bnlearn::nodes(cptsLearned)
  actualValues = testingData[,targetNode]
  attributeLevels = levels(testingData[, targetNode])
  numLevles = nlevels(testingData[, targetNode])
  infoReward = rep(0, nrow(testingData))
  #prior = freqs(table(actualValues))[attributeLevels]
  prior = freqs(table(actualValues))
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
      posteriorLearned = rep(0, numLevles)
      tempData = testingData[i, c(targetNodeMB, targetNode)]
      for (j in 1:(numLevles - 1)) {
        tempData[targetNode] = attributeLevels[j]
        posteriorLearned[j] = do.call('[', c(list(conditionalLearned), tempData))
      }
      posteriorLearned[numLevles] = 1 - sum(posteriorLearned)
    }
    binaryRepresent = as.numeric(attributeLevels %in% actualValues[i])
    I = rep(0, numLevles)
    if (numLevles == 2) {
      infoReward[i] = 2*ifelse(binaryRepresent[1], log(posteriorLearned[1]/prior[1]), log((1 - posteriorLearned[1])/(1 - prior[1])))
    } else {
      for (k in 1:length(binaryRepresent)) {
        infoReward[i] = infoReward[i] + ifelse(binaryRepresent[k], log(posteriorLearned[k]/prior[k]), log((1 - posteriorLearned[k])/(1 - prior[k])))
      }  
    }
  }
  return(mean(infoReward))
}

# calculate joint probability for bir2
jointProb = function(targetNode, targetNodeValue, dataPoint, cpts) {
  allNodes = bnlearn::nodes(cpts) 
  dataPoint[targetNode] = targetNodeValue
  condProbList = rep(0, length(allNodes))
  for (i in 1:length(allNodes)) {
    currentNode = allNodes[i]
    currentParents = cpts[[i]]$parents
    currentCPT = cpts[[i]]$prob
    tempData = dataPoint[1, c(currentNode, currentParents)]
    condProbList[i] = do.call("[", c(list(currentCPT), as.list(tempData)))
  }
  prod(condProbList)
}

bir2 = function(testingData, cptsLearned, targetNode, debug = FALSE) {
  allNodes = bnlearn::nodes(cptsLearned)
  actualValues = testingData[,targetNode]
  attributeLevels = levels(testingData[, targetNode])
  numLevles = nlevels(testingData[, targetNode])
  infoReward = rep(0, nrow(testingData))
  prior = freqs(table(actualValues))
  
  for (i in 1:nrow(testingData)) {
    posteriorLearned = rep(0, numLevles)
    for (j in 1:numLevles) {
      if (debug) cat("calculating", c(i, j), "\n")
      posteriorLearned[j] = jointProb(targetNode, attributeLevels[j], testingData[i,], cptsLearned)
    }
    posteriorLearned = posteriorLearned/sum(posteriorLearned)
    binaryRepresent = as.numeric(attributeLevels %in% actualValues[i])
    I = rep(0, numLevles)
    if (numLevles == 2) {
      infoReward[i] = 2*ifelse(binaryRepresent[1], log(posteriorLearned[1]/prior[1]), log((1 - posteriorLearned[1])/(1 - prior[1])))
    } else {
      for (k in 1:length(binaryRepresent)) {
        infoReward[i] = infoReward[i] + ifelse(binaryRepresent[k], log(posteriorLearned[k]/prior[k]), log((1 - posteriorLearned[k])/(1 - prior[k])))
      }  
    }
  }
  return(mean(infoReward))
}

prior = freqs(table(testingData[,targetNode]))
bir3 = function(testingData, cptsLearned, targetNode, prior, debug = FALSE) {
  allNodes = bnlearn::nodes(cptsLearned)
  actualValues = testingData[,targetNode]
  attributeLevels = levels(testingData[, targetNode])
  numLevles = nlevels(testingData[, targetNode])
  infoReward = rep(0, nrow(testingData))
  
  for (i in 1:nrow(testingData)) {
    posteriorLearned = rep(0, numLevles)
    for (j in 1:numLevles) {
      if (debug) cat("calculating", c(i, j), "\n")
      posteriorLearned[j] = jointProb(targetNode, attributeLevels[j], testingData[i,], cptsLearned)
    }
    posteriorLearned = posteriorLearned/sum(posteriorLearned)
    binaryRepresent = as.numeric(attributeLevels %in% actualValues[i])
    I = rep(0, numLevles)
    if (numLevles == 2) {
      infoReward[i] = 2*ifelse(binaryRepresent[1], log(posteriorLearned[1]/prior[1]), log((1 - posteriorLearned[1])/(1 - prior[1])))
    } else {
      for (k in 1:numLevles) {
        infoReward[i] = infoReward[i] + ifelse(binaryRepresent[k], log(posteriorLearned[k]/prior[k]), log((1 - posteriorLearned[k])/(1 - prior[k])))
      }  
    }
  }
  return(mean(infoReward))
}
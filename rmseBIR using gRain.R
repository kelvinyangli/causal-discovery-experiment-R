# rmse and bir using gRain::querygrain
rmseBIR = function(testingData, cptsLearned, targetNode, debug = FALSE) {
  allNodes = bnlearn::nodes(cptsLearned)
  actualValues = testingData[,targetNode]
  targetNodeValues = unique(testingData[,targetNode])
  targetNodeCardinality = length(targetNodeValues)
  infoReward = rep(0, nrow(testingData)) # empty bir scores
  prior = freqs(table(actualValues))[targetNodeValues] # prior probability
  squareError = rep(0, nrow(testingData)) # empty rmse scores
  targetNodeMB = mb(cptsLearned, targetNode) # find mb(targetNode)
  if (debug) cat("propagating cpt \n")
  if (length(targetNodeMB) < 1) {
    conditionalLearned = cptsLearned[[targetNode]]$prob
  } else {
    conditionalLearned = querygrain(as.grain(cptsLearned), nodes = c(targetNode, targetNodeMB), type = "conditional")
  }
  
  # iterate through test set
  for (i in 1:nrow(testingData)) {
    if (debug) cat("calculating", i, "\n")
    if (length(targetNodeMB) < 1) {
      posteriorLearned = conditionalLearned[targetNodeValues]
    } else {
      posteriorLearned = rep(0, targetNodeCardinality)
      tempData = testingData[i, c(targetNodeMB, targetNode)]
      for (j in 1:(targetNodeCardinality - 1)) {
        tempData[targetNode] = targetNodeValues[j]
        posteriorLearned[j] = do.call('[', c(list(conditionalLearned), tempData))
      }
      posteriorLearned[targetNodeCardinality] = 1 - sum(posteriorLearned)
    }
    binaryRepresent = as.numeric(targetNodeValues %in% actualValues[i])
    
    # computing BIR
    I = rep(0, targetNodeCardinality)
    if (targetNodeCardinality == 2) {
      infoReward[i] = 2*ifelse(binaryRepresent[1], log(posteriorLearned[1]/prior[1]), log((1 - posteriorLearned[1])/(1 - prior[1])))
    } else {
      for (k in 1:length(binaryRepresent)) {
        infoReward[i] = infoReward[i] + ifelse(binaryRepresent[k], log(posteriorLearned[k]/prior[k]), log((1 - posteriorLearned[k])/(1 - prior[k])))
      }  
    }
    
    # computing RMSE
    posteriorError = binaryRepresent - posteriorLearned
    squareError[i] = sum(posteriorError^2)
  }
  
  df = data.frame(bir = mean(infoReward), rmse = sqrt(mean(squareError)))
  return(df)
}
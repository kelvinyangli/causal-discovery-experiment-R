##################################################################################
# compute rmse and bir by constructing a prediction model 
# increasing prediction accuracy indicates a decreasing rmse and increasing bir
##################################################################################

# joint probability of a data point given cpts
jointProb = function(targetNode, attributeLevels, dataPoint, cpts) {
  allNodes = nodes(cpts) 
  condProbList = rep(0, length(allNodes)) # conditional prob of a node given its parents
  joint = rep(0, length(attributeLevels)) # each index of joint is the product of condProbList
  
  # for the first attribute 
  dataPoint[targetNode] = attributeLevels[1] # assign 1st value to target in dataPoint
  for (i in 1:length(allNodes)) {
    node = allNodes[i]
    parents = cpts[[i]]$parents
    cpt = cpts[[i]]$prob
    tempData = dataPoint[c(node, parents)]
    condProbList[i] = do.call("[", c(list(cpt), as.list(tempData)))
  }
  
  joint[1] = prod(condProbList)
  
  # from the 2nd value onwards, retain previous cond prob, only revise target or those adjacent to 
  # target
  for (j in 2:length(attributeLevels)) { 
    dataPoint[targetNode] = attributeLevels[j] # assign jth attribute to dataPoint
    for (k in 1:length(allNodes)) {
      node = allNodes[k]
      parents = cpts[[k]]$parents
      # only revise cond prob if node=target or target is a parent of node
      if ((node == targetNode) | (targetNode %in% parents)) {
        cpt = cpts[[k]]$prob
        tempData = dataPoint[c(node, parents)]
        condProbList[k] = do.call("[", c(list(cpt), as.list(tempData)))
      }
    }
    joint[j] = prod(condProbList)
  }
  return(joint)
}

# bir between the actual and predicted probabilities
# prior is variable frequency in training data
bir = function(actual, predicted, prior) {
  index = which(actual == 1)
  reward = log(predicted[index]/prior[index]) # if binary then bir is this
  if (length(actual) > 2) { # if cardinality is greater than 2 then take the average
    reward = (reward + sum(log((1 - predicted[-index])/(1 - prior[-index]))))/length(actual)
  } 
  return(reward)
}
  
# actual rmse and bir of a target given the learned cpts
prediction.slow = function(dataPoint, cptsLearned, targetNode, prior) {
  attributeLevels = levels(dataPoint[,targetNode]) # attribute levels of the target
  nLevels = length(attributeLevels)
  MB = bnlearn::mb(cptsLearned, targetNode) # markov blanket of target
  parents = cptsLearned[[targetNode]]$parents
  # if mb(target) = 0 then predicted = marginal
  if (length(MB) < 1) {
    predicted = cptsLearned[[targetNode]]$prob
  } else if (prod(MB %in% parents)) {
    # if mb(target) = parents(target) then predicted = marginalize cpt
    cpt = cptsLearned[[targetNode]]$prob
    predicted = rep(0, nLevels)
    for (i in 1:nLevels) {
      predicted[i] = do.call("[", c(list(cpt), as.list(c(attributeLevels[i], dataPoint[parents]))))
    }
  } else {
    # else compute predicted from joint then marginalize 
    predicted = jointProb(targetNode, attributeLevels, dataPoint, cptsLearned)
    predicted = predicted/sum(predicted) # normalize to obtain conditional
  }
  
  actual = as.numeric(attributeLevels %in% dataPoint[,targetNode])
  
  # bir
  bir = bir(actual, predicted, prior)
  # rmse
  # rmse = sqrt(mean((actual - predicted)^2))
  squareError = mean((actual - predicted)^2) # take mean se over target cardinality
  # rmse = Metrics::rmse(actual, predicted)
  
  df = data.frame(squareError = squareError, bir = bir)
  return(df)
}

# expected rmse and bir of a target given the true cpts
expPrediction = function(dataPoint, cptsTrue, targetNode, prior) {
  attributeLevels = levels(dataPoint[,targetNode]) # attribute levels of the target
  nLevels = length(attributeLevels)
  MB = bnlearn::mb(cptsTrue, targetNode) # markov blanket of target
  parents = cptsTrue[[targetNode]]$parents
  # if mb(target) = 0 then actual = marginal
  if (length(MB) < 1) {
    actual = cptsTrue[[targetNode]]$prob
  } else if (prod(MB %in% parents)) {
    # if mb(target) = parents(target) then actual = marginalize cpt
    cpt = cptsTrue[[targetNode]]$prob
    actual = rep(0, nLevels)
    for (i in 1:nLevels) {
      actual[i] = do.call("[", c(list(cpt), as.list(c(attributeLevels[i], dataPoint[parents]))))
    }
  } else {
    # else compute actual from joint then marginalize 
    actual = jointProb(targetNode, attributeLevels, dataPoint, cptsTrue)
    actual = actual/sum(actual) # normalize to obtain conditional
  }
  expSE = 0 
  expBIR = 0 
  
  # expected rmse and bir based on actual probabilities
  for (i in 1:nLevels) {
    expSE = expSE + actual[i]*((1 - actual[i])^2 + sum(actual[-i]^2))/nLevels # take mean over target cardinality
    expBIR = expBIR + actual[i]*(log(actual[i]/prior[i]) + sum(log((1 - actual[-i])/(1 - prior[-i]))))/nLevels
  }
  df = data.frame(expSE = expSE, expBIR = expBIR)
  return(df)
}

# compute rmse and bir for a learning method given in a single dimension
# dimension is either numNodes, maxNumParents, maxNumValues, associationStrength, numInstances
autoPrediction = function(currentDirectory, learningMethod, numIterations, debug = FALSE) {
  # training data
  trainingSets = list.files(paste0(currentDirectory, "/Datasets"), pattern = "_training") # load all training sets
  trainingSets = orderFiles(trainingSets, currentDirectory, 1) # order all training sets
  
  # testing data
  testSets = list.files(paste0(currentDirectory, "/Datasets"), pattern = "_testing") # load all test sets
  testSets = orderFiles(testSets, currentDirectory, 1) # order all test sets
  
  # true cpts 
  allTrueCPTs = list.files(paste0(currentDirectory, "/True networks/CPTs/")) # load all learned cpts
  allTrueCPTs = orderFiles(allTrueCPTs, currentDirectory, 1) # order all cpts
  
  # learned cpts
  allLearnedCPTs = list.files(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod)) # load all learned cpts
  allLearnedCPTs = orderFiles(allLearnedCPTs, currentDirectory, numIterations) # order all cpts
  
  allRMSEs = rep(0, length(allLearnedCPTs)) # initial empty vector of rmse
  allBIRs = allRMSEs 
  allExpRMSEs = allRMSEs
  allExpBIRs = allRMSEs
  diffRMSEs = allRMSEs
  diffBIRs = allRMSEs 
  
  for (i in 1:length(testSets)) {
    
    data = read.csv(paste0(currentDirectory, "/Datasets/", trainingSets[i]), header = TRUE)
    
    testingData = read.csv(paste0(currentDirectory, "/Datasets/", testSets[i]), header = TRUE)
    
    cptsTrue = read.net(paste0(currentDirectory, "/True networks/CPTs/", allTrueCPTs[i]))
    
    allNodes = colnames(testingData)
    
    # variable frequency in training data
    freqList = list()
    for (node in allNodes) {
      freqList[[node]] = freqs(table(data[, node]))
    }
    
    RMSEs = rep(0, nrow(testingData)) # empty vector to store rmses for all data points in test set
    BIRs = RMSEs
    expSEs = RMSEs  
    expBIRs = RMSEs 
    
    for (j in ((i-1)*numIterations + 1):(i*numIterations)) { # for each learned cpts
      
      cptsLearned = read.net(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", allLearnedCPTs[j]))
      #compute and store MBs for all nodes
      
      for (k in 1:nrow(testingData)) { # compute rmse and bir for all data points
        
        if (debug) cat("** computing rmse & bir for learned model", j, "row", k, "\n")
        
        targetNode = sample(allNodes, 1) # sample target
        prior = freqList[[targetNode]] # use frequncy for bir
        
        # expected rmse and bir based on true cpts
        expResults = expPrediction(testingData[k,], cptsTrue, targetNode, prior)
        expSEs[k] = expResults$expSE
        expBIRs[k] = expResults$expBIR
        
        # actual rmse and bir based on learned cpts
        actualResults = prediction(testingData[k,], cptsLearned, targetNode, prior)
        RMSEs[k] = actualResults$squareError # store rmse 
        BIRs[k] = actualResults$bir
      }
      
      # average relative difference between the actual and expected
      diffRMSEs[j] = mean(RMSEs - expSEs)
      diffBIRs[j] = mean(expBIRs - BIRs)
      
      allRMSEs[j] = sqrt(mean(RMSEs)) # mean of actual rmse over all data points and all nodes for the jth learned model
      allBIRs[j] = mean(BIRs) 
      
      allExpRMSEs[j] = sqrt(mean(expSEs)) # mean of expected rmse over all data points and all nodes for the jth learned model
      allExpBIRs[j] = mean(expBIRs)
    }
  }
  
  write.csv(allRMSEs, paste0(currentDirectory, "/Evaluations/rmse/", learningMethod, ".csv"), row.names = FALSE)
  write.csv(allBIRs, paste0(currentDirectory, "/Evaluations/bir/", learningMethod, ".csv"), row.names = FALSE)
  
  write.csv(allExpRMSEs, paste0(currentDirectory, "/Evaluations/expected rmse/", learningMethod, ".csv"), row.names = FALSE)
  write.csv(allExpBIRs, paste0(currentDirectory, "/Evaluations/expected bir/", learningMethod, ".csv"), row.names = FALSE)
  
  write.csv(diffRMSEs, paste0(currentDirectory, "/Evaluations/relative rmse/", learningMethod, ".csv"), row.names = FALSE)
  write.csv(diffBIRs, paste0(currentDirectory, "/Evaluations/relative bir/", learningMethod, ".csv"), row.names = FALSE)
  
  autoStats(currentDirectory, learningMethod, "rmse", allRMSEs, numIterations, alpha = 0.05)
  autoStats(currentDirectory, learningMethod, "bir", allBIRs, numIterations, alpha = 0.05)
  
  autoStats(currentDirectory, learningMethod, "expected rmse", allExpRMSEs, numIterations, alpha = 0.05)
  autoStats(currentDirectory, learningMethod, "expected bir", allExpBIRs, numIterations, alpha = 0.05)
  
  autoStats(currentDirectory, learningMethod, "relative rmse", diffRMSEs, numIterations, alpha = 0.05)
  autoStats(currentDirectory, learningMethod, "relative bir", diffBIRs, numIterations, alpha = 0.05)
}

# find all possible combinations of variables
allCombs = function(cpts) {
  allNodes = bnlearn::nodes(cpts)
  nLevels = rep(2, length(cpts))
  values = list()
  for (ii in 1:length(cpts)) {
    level = nrow(cpts[[ii]]$prob)
    nLevels[ii] = level
    values[[ii]] = LETTERS[1:level]
  }
  
  names(values) = allNodes
  allCombs = expand.grid(values)
  return(allCombs)
}


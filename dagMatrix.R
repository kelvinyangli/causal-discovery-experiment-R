
# generate a matrix to represent dag
generateDagMatrix = function(numNodes, maxNumParents) {
  
  allNodes = paste0("V", 1:numNodes)
  
  # generate a matrix to store arcs
  mtx = matrix(0, nrow = numNodes, ncol = numNodes, dimnames = list(allNodes, allNodes))
  
  # mtx contains 0 and 1, where 0 means no arc b/w two varialbes 
  # take the node ordering V1, V2, ...
  # hence mtx is an upper triangular matrix and the diagonal is 0
  for (i in 2:ncol(mtx)) {
    
    # numParents is the minimum between a sampled value from the number of preceeding nodes and maxNumParents
    numParents = sample(0:min(i - 1, maxNumParents), 1)
    
    parentsIndecies = sample(1:(i - 1), numParents) # sample parents from preceding nodes
    
    # fill matrix entry with 1 column by column
    mtx[parentsIndecies, i] = 1
    
  }
  
  return(mtx)
  
}

getIndex = function(cptDimension, parentsValues, numParents) {
  
  index = 0 
  
  for (i in 1:(numParents - 1)) {
    
    index = (index + (parentsValues[i] - 1)) * cptDimension[i + 1]
    
  }
  
  index = index + parentsValues[numParents]
  
  return(index)
  
}



# sample a cpt for each node from a symmetric dirichlet distribution with 
# a concentration parameter being set to the default value of 1
cptsDirichlet = function(dagMatrix, maxNumValues, concentration = 1) {
  
  cptsList = list() # empty list to store cpt for each node
  
  # sample arities 
  if (maxNumValues == 2) {
    
    arities = rep(2, ncol(dagMatrix))
    
  } else {
    
    arities = sample(2:maxNumValues, ncol(dagMatrix), replace = TRUE)
    
  } # end if
    
  # sample cpt for each node
  for (i in 1:ncol(dagMatrix)) {
    
    # alpha is the concentration parameter for a symmetric dirichelt dist
    alpha = rep(concentration, arities[i])
    
    parentsIndecies = which(dagMatrix[,i] == 1)
    
    numParentsInstiations = 1
    
    if (length(parentsIndecies) > 0) { # if current node has parents
      
      numParentsInstiations = prod(arities[parentsIndecies])
      
    } # end if 
    
    
    sampledCPT = rdirichlet(numParentsInstiations, alpha)
    
    cptsList[[i]] = t(sampledCPT) # take the transpose of the sampled cpt
    
  } # end for i
  
  names(cptsList) = colnames(dagMatrix)
  
  return(cptsList)
  
}


sampleData = function(dagMatrix, cpts, sampleSize) {
  
  mtx = matrix(nrow = sampleSize, ncol = ncol(dagMatrix), dimnames = list(NULL, colnames(dagMatrix)))
  
  parentsIndecies = list() 
  arities = rep(0, ncol(dagMatrix))
  numParents = rep(0, ncol(dagMatrix))
  
  # get the following information first 
  # parents indecies
  # arity 
  # number of parents 
  for (k in 1:ncol(dagMatrix)) {
    
    parentsIndecies[[k]] = which(dagMatrix[, k] == 1)
    arities[k] = nrow(cpts[[k]])
    numParents[k] = length(parentsIndecies[[k]])
    
  }
  
  ## start sampling data from cpt
  for (i in 1:nrow(mtx)) { # sample data from cpts for each row i
    
    for (j in 1:ncol(mtx)) { # sample data for each node j in row i
      
      
      if (numParents[j] < 1) { # if a node has no parent
        
        mtx[i, j] = sample(x = 1:arities[j], size = 1, prob = cpts[[j]])
        
      } else {
        
        
        
      } # end if ... else ...
      
      
    } # end for j
    
  } # end for i
  
}

# sample data for node with at least one parent from node cpt
f = function(dagMatrix, cpts, parentsIndecies, arities, numParents, mtx, rowIndex, nodeIndex) {
  
  # cpt dimension can be pre-computed and given as an input of this function
  cptDimension = arities[c(nodeIndex, parentsIndecies[[nodeIndex]])]
  
  if (numParents[nodeIndex] == 1) {
    
    cpts[[nodeIndex]][, mtx[rowIndex, parentsIndecies[[nodeIndex]]]]
    
  } else {
    
    getIndex(cptDimension, mtx[rowIndex, parentsIndecies[[nodeIndex]]], numParents[nodeIndex])
    
  }
  cpts[[nodeIndex]]
  
    
  
    
    
  
  
}



# compute the maximum length of a cpt for nodes
maxCPTLength = function(dagMatrix, cardinalities) {
  maxLength = 2
  for (i in 2:ncol(dagMatrix)) {
    currentLength = prod(cardinalities[dagMatrix[,i] == 1], cardinalities[i])
    if (currentLength > maxLength) {
      maxLength = currentLength
    }
  }
  
  return(maxLength)
}

# generate a matrix, in which each col contains the cpt entries for a node
generateCptsMatrix = function(dagMatrix, cardinalities, concentration) {
  
  # calculate the longest cpt
  maxLength = maxCPTLength(dagMatrix, cardinalities)
  
  alpha = rep(concentration, maxLength) # concentration parameter
  
  cptsMatrix = matrix(0, ncol = ncol(dagMatrix), nrow = maxLength)
  
  for (i in 1:ncol(dagMatrix)) {
    
    parents = which(dagMatrix[,i] == 1)
    
    if (length(parents) < 1) {
      sampledCPT = rdirichlet(1, alpha[1:cardinalities[i]])
    } else {
      sampledCPT = rdirichlet(prod(cardinalities[parents]), alpha[1:cardinalities[i]])
    }
    
    cptsMatrix[1:length(sampledCPT),i] = t(sampledCPT)*1000
    
  }
  
  return(cptsMatrix)
}


# convert dagMatrix to bn
toBN = function(dagMatrix) {
  allNodes = colnames(dagMatrix)
  dagBN = empty.graph(allNodes) # empty graph
  #arcMatrix = matrix(0, nrow = sum(dagMatrix), ncol = 2, dimnames = list(NULL, c("from", "to")))
  for (i in 1:(nrow(dagMatrix) - 1)) {
    adjacentNodes = which(dagMatrix[i,] == 1)
    if (length(adjacentNodes) > 0) {
      # if there is an adjacent node 
      for (j in 1:length(adjacentNodes)) {
        dagBN = set.arc(dagBN, allNodes[i], allNodes[adjacentNodes[j]])
      }
    }
  }
  return(dagBN)
}


# convert cpts from matrix to bn.fit
toBNFIT = function(dagMatrix, cptsMatrix, cardinalities) {
  allNodes = colnames(dagMatrix)
  
  nodesLevels = list()
  for (j in 1:ncol(dagMatrix)) nodesLevels[[j]] = LETTERS[1:cardinalities[j]] 
  
  cptsBN = list() 
  for (i in 1:ncol(dagMatrix)) {
    nLevels = cardinalities[i]
    currentParents = which(dagMatrix[,i] == 1)
    if (length(currentParents) < 1) {
      # if no parents
      cpt = matrix(cptsMatrix[1:nLevels, i], ncol = nLevels, dimnames = list(NULL, nodesLevels[[i]]))
    } else {
      # if exists parents
      parentsLevels = cardinalities[currentParents]
      dimNames = nodesLevels[c(i, currentParents)]
      names(dimNames) = allNodes[c(i, currentParents)]
      cpt = array(cptsMatrix[1:prod(parentsLevels, nLevels), i], dim = c(nLevels, parentsLevels), dimnames = dimNames)
    }
    cptsBN[[i]] = cpt
  }
  names(cptsBN) = allNodes
  
  dag = toBN(dagMatrix) # covert dagMatrix to bn
  bnFIT = custom.fit(dag, cptsBN) # convert list to bn.fit
  return(bnFIT)
}

cardinalities = sampleCardinalities(dm, 3)


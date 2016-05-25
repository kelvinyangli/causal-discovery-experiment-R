
############################################################################################################
# generate random structure with parameters
# number of variables, maximum number of parents
############################################################################################################
generateSeed = function() {
  op = options(digits.secs = 6)
  x = gsub("[: -]", "" , Sys.time(), perl = TRUE) # remove - and : 
  x = strsplit(x, split = "[.]")[[1]][2] # get lower digits
  x = as.numeric(x) # convert char to numeric 
  return(x)
}

generateDag = function(numNodes, maxNumParents) {
  
  allNodes = paste0("V", 1:numNodes)
  
  dag = empty.graph(allNodes)
  
  if (maxNumParents > 0) { # if nodes have parents, sample parents from preceding nodes
    
    for (i in 2:length(allNodes)) {
      
      numParents = sample(0:min(maxNumParents, i - 1), 1) 
      
      parents = sample(allNodes[1:(i - 1)], numParents)
      
      if (length(parents) > 0) { # add arc only if a node has at least 1 parents
        
        for (j in 1:numParents) dag = set.arc(dag, parents[j], allNodes[i])
        
      }
       
    } # end for i 
    
  } # else return empty dag
  
  # re-order allNodes to keep the ordering consistent when generating cpts and data
  nodes(dag) = node.ordering(dag)
  return(dag)
}

############################################################################################################
# generate random CPT based on a random DAG with parameters
# DAG, maximum number of values, concentration for Dirichelet distribution
# when concentration  is large, dirichlet is equivalent to uniform distribution i.e concentration is weak
# library(gtools)
############################################################################################################

generateCPTs = function(dag, maxNumValues, concentration, debug = FALSE) {
  # the larger concentration is, the more concentration the distribution is, hence values are more close to the middle point 
  
  allNodes = bnlearn::nodes(dag)
  numNodes = length(allNodes)
  
  # sample cardinalities for nodes 
  if (maxNumValues == 2) {
    cardinalities = rep(2, numNodes)
    concentration = rep(concentration, 2) # equal concentration parameters for all values
  } else {
    cardinalities = sample(2:maxNumValues, numNodes, replace = TRUE)  
    concentration = rep(concentration, max(cardinalities)) # equal concentration parameters for all values
  }
  
  cpts = list()
  
  if (debug) cat("* sampling cpt values \n")
  
  # generate cpts for all nodes
  for (i in 1:numNodes) {
    
    parents = dag$nodes[[i]]$parents
    
    if (length(parents) < 1) {
      
      sampledCPT = rdirichlet(1, concentration[1:cardinalities[i]]) # sample single cpt from dirichlet  
      
      cpts[[i]] = array(sampledCPT, dim = c(1, cardinalities[i]), dimnames = list(NULL, LETTERS[1:cardinalities[i]]))
      
    } else {
      
      parentsIndex = which(allNodes %in% parents)
      
      sampledCPT = rdirichlet(prod(cardinalities[parentsIndex]), concentration[1:cardinalities[i]]) # sample multiple cpts from dirichlet with the above concentration
      sampledCPT = t(sampledCPT) # take the transpose
      
      dimNames = list(LETTERS[1:cardinalities[i]])
      
      for (j in 1:length(parents)) {
        dimNames[[j + 1]] = LETTERS[1:cardinalities[parentsIndex[j]]]   
      }
      
      names(dimNames) = c(allNodes[i], parents)
      
      cpts[[i]] = array(sampledCPT, dim = c(cardinalities[i], cardinalities[parentsIndex]), 
                        dimnames = dimNames)
      
    }
  }
  
  names(cpts) = allNodes
  
  if (debug) cat("* converting to bn.fit \n")
  
  bnFit = custom.fit(dag, cpts) # convert cpts into bn.fit format
  
  return(bnFit)
}

# source the function toNumeric
# source("../../Dropbox/PhD@Monash/R/code/Experiments/source code/toNumeric.R")

# generate BN and data and save to files
generateBNAndData = function(numNodes, maxNumParents, maxNumValues, concentration, numInstances, 
                             numInstancesForTesting, numIterations, currentDirectory, debug = FALSE) {
  
  if (debug) cat("* generating BN with numNodes", numNodes, "maxNumParents", maxNumParents, "maxNumValues", maxNumValues, 
                 "concentration", concentration, "numInstances", numInstances,  "\n")
  
  i = 1 
  
  while(i <= numIterations) {
    
    seed = generateSeed()
    set.seed(seed)
    
    dag = generateDag(numNodes, maxNumParents) # generate dag
    cpts = generateCPTs(dag, maxNumValues, concentration, debug = debug) # sample cpts from dirichlet distribution
    
    dataTraining = rbn(cpts, numInstances) # generate training data
    dataTesting = rbn(cpts, numInstancesForTesting) # generate testing data
    
    # randomize node ordering in sampled data
    randomizedOrder = sample(1:length(cpts))
    
    dataTraining = dataTraining[,randomizedOrder] 
    dataTesting = dataTesting[,randomizedOrder]
    
    if (debug) cat("* saving BN \n")
    
    fileName = paste(numNodes, maxNumParents, maxNumValues, concentration, numInstances, seed, sep = "_")
    
    # save dag
    write.dot(paste0(currentDirectory, "/True networks/Structures/", 
                     fileName, ".dot"), dag)
    
    # save cpts
    # write.net(paste0(currentDirectory, "/True networks/CPTs/", fileName, "_", seed, ".net"), cpts)
    saveRDS(cpts, paste0(currentDirectory, "/True networks/CPTs/", fileName, ".rds"))
    
    if (debug) cat("* saving data \n")
    
    # save training data
    # write.csv(dataTraining, paste0(currentDirectory, "/Datasets/", fileName, "_", seed, "_training.csv"), row.names = FALSE)
    saveRDS(dataTraining, paste0(currentDirectory, "/Datasets/Training/", fileName, ".rds"))
    
    # save testing data 
    # write.csv(dataTesting, paste0(currentDirectory, "/Datasets/", fileName, "_", seed, "_testing.csv"), row.names = FALSE)
    saveRDS(dataTesting, paste0(currentDirectory, "/Datasets/Testing/", fileName, ".rds"))
    
    # convert testing data to numeric fomat using toNumeric and save into Datasets/Numeric
    dataTestingNumeric = toNumeric(dataTesting)
    saveRDS(dataTestingNumeric, paste0(currentDirectory, "/Datasets/Numeric/", fileName, ".rds"))
    
    i = i + 1
    
  } # end while i 
  
}









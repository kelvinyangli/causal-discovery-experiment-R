# compute the average node density of a dag
averageDensity = function(currentDirectory, learningMethod, numIterations) {
  
  if (learningMethod == "true") {
    
    directory = paste0(currentDirectory, "/True networks/CPTs/")
    
  } else {
    
    directory = paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/")
    
  }
  
  allCPTs = list.files(directory)
  allCPTs = orderFiles(allCPTs, currentDirectory, numIterations)
  
  meanDensity = rep(0, length(allCPTs))
  
  
  for (i in 1:length(allCPTs)) { # for each dag compute the mean density
    
    totalNumNbr = 0
   
    cpts = readRDS(paste0(directory, allCPTs[i]))
    
    allNodes = bnlearn::nodes(cpts)
    numNodes = length(allNodes)
    
    for (j in 1:numNodes) { # accumulate the number of neighbours and parents
      
      totalNumNbr = totalNumNbr + length(bnlearn::nbr(cpts, allNodes[j]))
      
    } # end for j
    
    meanDensity[i] = totalNumNbr/numNodes
    
  } # end for i
  

  write.csv(meanDensity, paste0(currentDirectory, "/Evaluations/density/", learningMethod, ".csv"), row.names = FALSE)
  
  autoStats(currentDirectory, learningMethod, "density", meanDensity, numIterations, alpha = 0.05)
  
}

#sapply(c("mmhc", "aic", "bic" ,"bde", "k2Matlab", "true"), averageDensity, currentDirectory = "concentration", numIterations = 20)


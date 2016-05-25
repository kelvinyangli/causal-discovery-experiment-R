randomGuessingRMSEs = function(currentDirectory, numIterations) {
  
  dir = paste0(currentDirectory, "/Datasets/Arities/")
  allArities = list.files(dir)
  allArities = orderFiles(allArities, currentDirectory, numIterations)
  
  randomRMSEs = rep(0, length(allArities))
  
  for (i in 1:length(allArities)) { # compute the random guessing rmse and bir for each data set
    
    arities = read.csv(paste0(dir, allArities[i]), header =  TRUE)
    
    for (j in 1:nrow(arities)) { # compute rmse for each node 
      
      randomRMSEs[i] = randomRMSEs[i] + sqrt((arities[j,] - 1)/arities[j,]^2)
      
    } # end for j
    
    randomRMSEs[i] = randomRMSEs[i]/nrow(arities)
    
  } # end for i
  
  write.csv(randomRMSEs, paste0(currentDirectory, "/Evaluations/randomGuessing/randomGuessingRMSEs.csv"), row.names = FALSE)
  
  autoStats(currentDirectory, "randomGuessingRMSEs", "randomGuessing", randomRMSEs, numIterations, alpha = 0.05)
  
}



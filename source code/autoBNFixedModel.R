autoBNFixedModel = function(numNodes, maxNumParents, maxNumValues, concentration, numInstancesList, 
                            numInstancesForTesting, numIterations) {
  
  seeds = c()
  dir1 = "numInstances/True networks/"
  dir2 = "numInstances/Datasets/"
  
  for (i in 1:numIterations) {
    
    seed1 = generateSeed()
    set.seed(seed1)
    
    # generate dag and cpts
    dag = generateDag(numNodes, maxNumParents)
    cpts = generateCPTs(dag, maxNumValues, concentration)
    
    seeds = c(seeds, seed1) # save seed for dag and cpts generation
    
    # sample training and testing data 
    for (j in 1:length(numInstancesList)) { # for each desired sample size, generate data
      
      seed2 = generateSeed()
      set.seed(seed2)
      dataTraining = rbn(cpts, numInstancesList[j])
      dataTesting = rbn(cpts, numInstancesForTesting) # always sample 100000 samples for testing
      
      fileName = paste(numNodes, maxNumParents, maxNumValues, concentration, numInstancesList[j], seed2, sep = "_")
      
      # save dag
      write.dot(paste0(dir1, "Structures/", fileName, ".dot"), dag)
      
      # save cpts
      saveRDS(cpts, paste0(dir1, "CPTs/", fileName, ".rds"))
      
      # save training data
      saveRDS(dataTraining, paste0(dir2, "Training/", fileName, ".rds"))
      
      # save testing data 
      saveRDS(dataTesting, paste0(dir2, "Testing/", fileName, ".rds"))
      
      # convert testing data to numeric fomat using toNumeric and save into Datasets/Numeric
      dataTestingNumeric = toNumeric(dataTesting)
      saveRDS(dataTestingNumeric, paste0(dir2, "Numeric/", fileName, ".rds"))
      
    } # end for j
    
  } # end for i
  
  write.csv(seeds, "numInstances/modelGenerationSeeds.csv", row.names = FALSE)
  
}


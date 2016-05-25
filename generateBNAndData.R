#################################################################################################
# output file name convention 
# numNodes_manxNumParents_maxNumValues_associationStrength_numInstances_seed
# write.dsc is very slow
#################################################################################################
generateBNAndData = function(numNodes, maxNumParents, maxNumValues, associationStrength, numInstances, numIterations, currentDirectory, debug = FALSE) {
  if (debug) cat("* generating BN with", numNodes, "nodes \n")
  seed = generateSeed()
  set.seed(seed)
  dag = generateDag(numNodes, maxNumParents)
  cpts = generateCPTs(dag, maxNumValues, associationStrength)
  
  partialFileName = paste(numNodes, maxNumParents, maxNumValues, associationStrength, numInstances, sep = "_")
  
  if (debug) cat("* saving BN \n")
  write.dot(paste0(currentDirectory, "/True networks/Structures/", partialFileName, "_", seed, ".dot"), dag)
  write.dsc(paste0(currentDirectory, "/True networks/CPTs/", partialFileName, "_", seed, ".dsc"), cpts)
  
  for (i in 1:numIterations) {
    seed = generateSeed()
    set.seed(seed)
    if (debug) cat("* generating data", i, "for BN with ", numNodes, "nodes \n")
    data = rbn(cpts, numInstances)
    if (debug) cat("* saving data \n")
    write.csv(data, paste0(currentDirectory, "/Datasets/", partialFileName, "_", seed, ".csv"))
  }
}

generateBNAndData = cmpfun(generateBNAndData)

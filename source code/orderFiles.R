# re-order files as the same order appear in folder
orderFiles = function(files, currentDirectory, numIterations) {
  
  values = matrix(0, nrow = length(files), ncol = 6) # empty matrix for storing values in file name
  
  for (i in 1:length(files)) {
    
    values[i,] = na.omit(as.numeric(unlist(strsplit(files[i], "[^0-9]+")))) # extract valuesi n file name
    
  }
  
  colnames(values) = c("numNodes", "maxNumParents", "maxNumValues", "concentration", "numInstances", "seed") # assign column names to matrix
  
  if ((currentDirectory == "numNodes") | (currentDirectory == "numNodes_nonRandomizedNodeOrdering")) {
    colIndex = 1
  } else if ((currentDirectory == "maxNumParents") | (currentDirectory == "maxNumParents_nonRandomizedNodeOrdering")) {
    colIndex = 2
  } else if ((currentDirectory == "maxNumValues") | (currentDirectory == "maxNumValues_nonRandomizedNodeOrdering")) {
    colIndex = 3
  } else if ((currentDirectory == "concentration") | (currentDirectory == "concentration_nonRandomizedNodeOrdering")) {
    colIndex = 4
  } else if ((currentDirectory == "numInstances") | (currentDirectory == "numInstances_nonRandomizedNodeOrdering")) {
    colIndex = 5
  }
  
  indexesOrder = order(values[,colIndex])
  
  values = values[indexesOrder,] # order unfixed column by increasing
  
  files = files[indexesOrder]
  
  for (j in 1:(length(files)/numIterations)) {
    
    indexes = ((j - 1) * numIterations + 1):(j * numIterations)
    files[indexes] = 
      files[indexes[order(values[((j - 1) * numIterations + 1):(j * numIterations), "seed"])]]
    
  }
  
  
  return(files)
  
}
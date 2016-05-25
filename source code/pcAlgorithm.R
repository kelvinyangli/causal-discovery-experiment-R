# cd using pc
pcAlgorithm = function(data, alpha) {

  allNodes = colnames(data)
  
  nlev = rep(2, ncol(data)) # set initial arity to 2
  
  for (j in 1:ncol(data)) nlev[j] = nlevels(data[,j]) # get arities for nodes from data
  
  # convert data into 0, 1, 3, ...
  dataConverted = matrix(0, nrow = nrow(data), ncol = ncol(data))
  
  for (k in 1:ncol(data)) dataConverted[,k] = as.numeric(data[,k]) - 1
  
  colnames(dataConverted) = allNodes
  
  suffStat = list(dm = dataConverted, nlev = nlev, adaptDF = FALSE)
  
  dagPC = pc(suffStat, indepTest = disCItest, alpha = alpha, labels = allNodes)
  
  return(dagPC)
  
}






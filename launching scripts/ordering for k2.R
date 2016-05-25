# get node ordring from structure learned by MWST

currentDirectory = "numNodes"

trees = list.files(paste0(currentDirectory, "/Learned networks/Structures/MWST"))

for (k in 1:length(trees)) {
  
  mwst = read.csv(paste0(currentDirectory, "/Learned networks/Structures/MWST/", trees[k]))
  
  rownames(mwst) = colnames(mwst) # assign row names
  
  dagBN = matrix2dag(mwst)
  
  nodeOrdering = node.ordering(dagBN)
  
  indecies = match(nodeOrdering, colnames(mwst))
  
  write.csv(indecies, paste0(currentDirectory, "/Ordering/", trees[k]), row.names = FALSE)
  
}
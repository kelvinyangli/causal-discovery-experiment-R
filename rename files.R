# rename files

dir = paste0(currentDirectory, "/Datasets/Training/")

allTrainingData = list.files(dir) # list all training data in folder
allTrainingData = orderFiles(allTrainingData, currentDirectory, numIterations) # re-order all files

for (i in 1:length(allTrainingData)) {
  
  oldname = allTrainingData[i]
  newname = na.omit(as.numeric(unlist(strsplit(oldname, "[^0-9]+"))))
  newname = paste0(paste0(newname, collapse = "_"), ".rds")
  
  file.rename(from = paste0(dir, oldname), to = paste0(dir, newname))
  
}
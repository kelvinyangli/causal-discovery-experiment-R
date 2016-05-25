# scripts for precision, recall, and edit distance

currentDirectory = "numNodes"

learningMethods = c("pc")

# load all true cpts and convert to dags
allTrueCPTs = list.files(paste0(currentDirectory, "/True networks/CPTs"))

allTrueCPTs = orderFiles(allTrueCPTs, currentDirectory, 20)

allTrueCPTsList = list()

allTrueDagsList = list()

for (i in 1:length(allTrueCPTs)) {
  
  allTrueCPTsList[[i]] = readRDS(paste0(currentDirectory, "/True networks/CPTs/", allTrueCPTs[i]))
  
  allTrueDagsList[[i]] = model2network(modelstring(allTrueCPTsList[[i]]))
  
}

# accuracies
sapply(learningMethods, autoAccuracy, currentDirectory = currentDirectory, 
       numIterations = 20, allTrueDagsList = allTrueDagsList, debug = FALSE)

# precision and recall
autoPrecisionRecall(currentDirectory = currentDirectory, numIterations = 20)

# f measure
autoFMeasure(currentDirectory = currentDirectory, numIterations = 20)

# edit distance
sapply(learningMethods, autoEditDistance, currentDirectory = currentDirectory, 
       numIterations = 20, allTrueDagsList = allTrueDagsList, debug = FALSE)






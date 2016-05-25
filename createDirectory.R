# create all directories

# currentDirectories = "test"

currentDirectories = c("numNodes", "maxNumValues", "concentration", "numInstances", "maxNumParents")

#currentDirectories = c("numNodes")
for (i in 1:length(currentDirectories)) {
  learningMethods = c("mmhc", "aic", "bic", "bde", "k2", "pc", "k2WithNodeOrdering")
  
  measures = c("accuracy", "precision", "recall", "f measure", "shd cpdags", "shd dags", "shd skeletons", 
               "rmse", "reference", "bir", "kld", "randomGuessing")
  
  currentDirectory = currentDirectories[i]
  
  ###### level 1 ######
  # Experiments/numNodes
  dir.create(currentDirectory)
  
  ###### level 2 ######
  # Experiments/numNodes/Datasets
  dir.create(paste0(currentDirectory, "/Datasets"))
  
  # Experiments/numNodes/True networks
  dir.create(paste0(currentDirectory, "/True networks"))
  
  # Experiments/numNodes/Learned networks
  dir.create(paste0(currentDirectory, "/Learned networks"))
  
  # Experiments/numNodes/Evaluations
  dir.create(paste0(currentDirectory, "/Evaluations"))
  
  # Experiments/numNodes/Plots
  dir.create(paste0(currentDirectory, "/Plots"))
  
  ###### level 3 ###### 
  dir.create(paste0(currentDirectory, "/Datasets/Training"))
  dir.create(paste0(currentDirectory, "/Datasets/Testing"))
  dir.create(paste0(currentDirectory, "/Datasets/Numeric"))
  
  # Experiments/numNodes/True networks/CPTs
  dir.create(paste0(currentDirectory, "/True networks/CPTs"))
  
  # Experiments/numNodes/True networks/Structures
  dir.create(paste0(currentDirectory, "/True networks/Structures"))
  
  # Experiments/numNodes/Learned networks/CPTs
  dir.create(paste0(currentDirectory, "/Learned networks/CPTs"))
  
  # Experiments/numNodes/Learned networks/Structures
  dir.create(paste0(currentDirectory, "/Learned networks/Structures"))
  
  ###### level 3 & 4 ######
  # Experiments/numNodes/Learned networks/CPTs/mmhc
  for (j in 1:length(learningMethods)) {
    dir.create(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethods[j])) 
  }
  
  # Experiments/numNodes/Learned networks/Structures/mmhc
  for (j in 1:length(learningMethods)) {
    dir.create(paste0(currentDirectory, "/Learned networks/Structures/", learningMethods[j])) 
  }
  
  # Experiments/numNodes/Evaluations/precision/stats
  for (k in 1:length(measures)) {
    dir.create(paste0(currentDirectory, "/Evaluations/", measures[k]))
    dir.create(paste0(currentDirectory, "/Evaluations/", measures[k], "/stats"))
  }
  
  # Experiments/numNodes/Evaluations/rmse/mmhc
  for (l in 1:length(learningMethods)) {
    dir.create(paste0(currentDirectory, "/Evaluations/rmse/", learningMethods[l]))
  }
  
  for (l in 1:length(learningMethods)) {
    dir.create(paste0(currentDirectory, "/Evaluations/bir/", learningMethods[l]))
  }
  
  
}









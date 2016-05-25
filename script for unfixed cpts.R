# code for maxNumValues and concentration parameter
# fixed dag, unfixed cpts

concentration = 1

seed = generateSeed()
set.seed(seed)
dag = generateDag(30, 4)
graphviz.plot(dag)

fileName = paste0("30_4_3_", concentration, "_30000_")
write.dot(paste0("maxNumValues/True networks/Structures/", fileName, seed, ".dot"), dag)

numIterations = 20
numInstances = 30000

# maxNumValues
maxNumValuesList = 2:6

for (i in 1:length(maxNumValuesList)) {
  
  fileName = paste0("30_4_", maxNumValuesList[i], "_", concentration, "_30000_")
  
  seed = generateSeed()
  set.seed(seed)
  
  cpts = generateCPTs(dag, maxNumValuesList[i], concentration)
  
  dataTraining = rbn(cpts, numInstances*numIterations) # generate training data
  
  dataTesting = rbn(cpts, 10000) # generate testing data
  
  saveRDS(cpts, paste0(currentDirectory, "/True networks/CPTs/", fileName, seed, ".rds"))
  
  saveRDS(dataTraining, paste0(currentDirectory, "/Datasets/", fileName, seed, "_training.rds"))
  
  saveRDS(dataTesting, paste0(currentDirectory, "/Datasets/", fileName, seed, "_testing.rds"))
  
  
}

# concentration 
seed = generateSeed()
set.seed(seed)
dag = generateDag(30, 4)
graphviz.plot(dag)

fileName = paste0("30_4_3_20_30000_")
write.dot(paste0("concentration/True networks/Structures/", fileName, seed, ".dot"), dag)

concentrationList = c(1:10, seq(20, 50, by = 10), 100, 150, 200)

for (i in 1:length(concentrationList)) {
  
  fileName = paste0("30_4_3_", concentrationList[i], "_30000_")
  
  seed = generateSeed()
  set.seed(seed)
  
  cpts = generateCPTs(dag, 3, concentrationList[i])
  
  dataTraining = rbn(cpts, numInstances*numIterations) # generate training data
  
  dataTesting = rbn(cpts, 10000) # generate testing data
  
  saveRDS(cpts, paste0("concentration/True networks/CPTs/", fileName, seed, ".rds"))
  
  saveRDS(dataTraining, paste0("concentration/Datasets/", fileName, seed, "_training.rds"))
  
  saveRDS(dataTesting, paste0("concentration/Datasets/", fileName, seed, "_testing.rds"))
  
  
}



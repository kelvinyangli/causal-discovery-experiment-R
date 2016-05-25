# for fixed dag and cpts
currentDirectory = "numInstances"

numNodes = 30
maxNumParents = 4
maxNumValues = 3
concentration = 20
numInstances = 100

fileName = paste(numNodes, maxNumParents, maxNumValues, concentration, sep = "_")

# dag
seed = generateSeed()
set.seed(seed)
dag = generateDag(numNodes, maxNumParents) # generate dag
write.dot(paste0(currentDirectory, "/True networks/Structures/", 
                 fileName, "_", seed, ".dot"), dag)

# cpts
seed = generateSeed()
set.seed(seed)
cpts = generateCPTs(dag, maxNumValues, concentration, debug = FALSE) # sample cpts from dirichlet distribution
saveRDS(cpts, paste0(currentDirectory, "/True networks/CPTs/", fileName, "_", seed, ".rds"))

# data 
numIterations = 20
numInstancesList = c(seq(100, 900, 100), seq(1000, 9000, 1000), seq(10000, 100000, 10000))
cptsFile = list.files("numInstances/True networks/CPTs")
cpts = readRDS(paste0("numInstances/True networks/CPTs/", cptsFile))
for (i in 1:length(numInstancesList)) {
  seed = generateSeed()
  set.seed(seed)
  dataTraining = rbn(cpts, numInstancesList[i]*numIterations) # generate training data
  dataTesting = rbn(cpts, 10000) # generate testing data
  saveRDS(dataTraining, paste0(currentDirectory, "/Datasets/", fileName, "_", 
                               numInstancesList[i], "_", seed, "_training.rds"))
  saveRDS(dataTesting, paste0(currentDirectory, "/Datasets/", fileName, "_", 
                              numInstancesList[i], "_", seed, "_testing.rds"))
}

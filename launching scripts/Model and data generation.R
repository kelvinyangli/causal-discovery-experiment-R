# scripts for generating models and data

# numNodes
# Kel PC only able to generate up to 200 nodes
# for numNodes = 300 learning with pc is extremly slow; computing rmse/bir is also slow
numNodesList = seq(5, 25, by = 5)
sapply(numNodesList, generateBNAndData, maxNumParents = 3, maxNumValues = 4, 
       concentration = 5, numInstances = 7000, numInstancesForTesting = 10000, 
       numIterations = 20, currentDirectory = "numNodes", debug = FALSE)

# maxNumParents
maxNumParentsList = 0:4
sapply(maxNumParentsList, generateBNAndData, numNodes = 20, maxNumValues = 4, 
       concentration = 5, numInstances = 7000, numInstancesForTesting = 10000, numIterations = 20, 
       currentDirectory = "maxNumParents", debug = FALSE)

# maxNumValues
maxNumValuesList = 2:6
#autoBNForNodeArity(numNodes = 20, maxNumParents = 3, maxNumValuesList = 2:6, concentration = 20, 
#                   numInstances = 30000, numInstancesForTesting = 100000, numIterations = 20)

sapply(maxNumValuesList, generateBNAndData, numNodes = 20, maxNumParents = 3, 
       concentration = 5, numInstances = 7000, numInstancesForTesting = 10000, numIterations = 20, 
       currentDirectory = "maxNumValues", debug = FALSE)

# concentration 
strengthList = seq(1, 9, by = 2)
sapply(strengthList, generateBNAndData, numNodes = 20, maxNumParents = 3, 
       maxNumValues = 4, numInstances = 7000, numInstancesForTesting = 10000, numIterations = 20, 
       currentDirectory = "concentration", debug = FALSE)

# numInstances
numInstancesList = c(1, 4, 7, 10, 15) * 1000
#autoBNFixedModel(numNodes = 20, maxNumParents = 3, maxNumValues = 4, concentration = 5, numInstancesList, 
#                 numInstancesForTesting = 10000, numIterations = 20)

sapply(numInstancesList, generateBNAndData, numNodes = 20, maxNumParents = 3, maxNumValues = 4, concentration = 5, 
       numInstancesForTesting = 10000, numIterations = 20, currentDirectory = "numInstances", debug = FALSE)

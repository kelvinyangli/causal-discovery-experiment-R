################################# Structure learning ####################################
# current learning algorithms including 
# bnlearn::mmhc, aic, bic, bde, k2
# other algorithms that will be included in are 
# pcalg::pc, ges
#########################################################################################

# source('C:/Users/Administrator/Dropbox/PhD@Monash/R/Code/Experiments/source code/orderFiles.R')

#source('C:/Users/Administrator/Dropbox/PhD@Monash/R/Code/Experiments/source code/pcAlgorithm.R')

#source('C:/Users/Administrator/Dropbox/PhD@Monash/R/Code/Experiments/source code/toClassBN.R')

autoLearn = function(currentDirectory, learningMethod, numIterations, alpha = 0.05, debug = FALSE) {
  
  allTrainingData = list.files(paste0(currentDirectory, "/Datasets/Training")) # list all training data in folder
  allTrainingData = orderFiles(allTrainingData, currentDirectory, numIterations) # re-order all files
  
  for (i in 1:length(allTrainingData)) {
    
    data = readRDS(paste0(currentDirectory, "/Datasets/Training/", allTrainingData[i]))
    
    # learn structures from data
    
    if (debug) cat("* learning", i, "\n")
    
    if (learningMethod == "mmhc") { # mmhc algorithm
      
      dagLearned = mmhc(data, test = "x2", score = "bde")
      
    } else if (learningMethod == "pc") {
      
      dagLearned = pcAlgorithm(data, alpha = alpha)
      
      # transform cpdag in class pcalg to dag in bn
      dagLearned = toClassBN(dagLearned) 
      
    } else if (learningMethod == "k2WithNodeOrdering") { # when a pre-determined node ordering is given
      
      # give the randomized node ordering to k2
      blacklist = ordering2blacklist(names(data)) 
      
      dagLearned = hc(x = data, score = "k2", blacklist = blacklist)
      
    } else { # K2, BDe, AIC, BIC all use hill-climbing for seaching
      
      dagLearned = hc(x = data, score = learningMethod)
      
    } 
    
    # estimate cpts using bn.fit with method = bayes to avoid 0 in cpts values
    
    cptsLearned = bn.fit(dagLearned, data, method = "bayes") 
    
    # save structure to .dot, cpts to .net files
    
    if (debug) cat("** saving", i, "\n")
    
    write.dot(paste0(currentDirectory, "/Learned networks/Structures/", learningMethod, "/", allTrainingData[i], ".dot"), dagLearned)
    
    # write.net(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", fileName, "_", seed, ".net"), cptsLearned)
    saveRDS(cptsLearned, paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", allTrainingData[i]))
      
  } # end for i  
  
}

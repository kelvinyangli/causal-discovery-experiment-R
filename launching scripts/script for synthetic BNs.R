##################################### Experiment script ###########################################
# load required libraries
libraries = c("bnlearn", "pcalg", "gRain", "gtools", "entropy", "reshape2", "ggplot2")
lapply(libraries, require, character.only = TRUE)

options(scipen = 10) # print numerical values to fixed within 10 digits instead of exponential
options(digits = 3) # print to the 3rd decimal place, default is 7

sourceDir <- function(path, fileName = NULL, trace = TRUE, ...) {
  allFiles = list.files(path, pattern = fileName)
  for (file in allFiles) {
    if(trace) cat(file,":")
    source(file.path(path, file), ...)
    if(trace) cat("\n")
  }
}

# pc 
sourceDir("C:/Users/Administrator/Dropbox/PhD@Monash/R/Code/Experiments/source code")
sourceDir("C:/Users/Administrator/Dropbox/PhD@Monash/R/Code/MB discovery/mbMMLCPT/")
setwd("Experiments_04052016")
setwd("Experiments_20042016/")

# mac
sourceDir("~/Dropbox/PhD@Monash/R/Code/Experiments/source code") 
setwd("~/Desktop/Experiments_22032016")


###################################### structure learning ######################################
sapply(c("mmhc", "aic", "bic"), autoLearn, currentDirectory = "numNodes", numIterations = 20, debug = FALSE)

sapply(c("bde", "k2"), autoLearn, currentDirectory = "numNodes", numIterations = 20, debug = FALSE)

autoLearn("concentration", "mmhc", 20)


###################################### Evaluations ####################################
# compute the mean rmse and bir from results computed by cluster
getMeanPrediction("concentration", "k2Matlab", "rmse", 20)

sapply(c("aic", "bic", "bde", "k2"), getMeanPrediction, currentDirectory = "numNodes", measure = "rmse", 
       numIterations = 20, alpha = 0.05)

sapply(c("aic", "bic", "bde", "k2"), getMeanPrediction, currentDirectory = "numNodes", measure = "bir", 
       numIterations = 20, alpha = 0.05)

sapply(c("concentration", "maxNumParents", "maxNumValues", "numInstances"), getMeanPrediction, learningMethod = "pc",
       measure = "bir", numIterations = 20, alpha = 0.05)

#######################################################################################
# kld 
#######################################################################################
sapply(c("aic","bic","bde","k2","k2Matlab","mmhc","pc"), autoKLDApprox, currentDirectory = "maxNumParents", debug = TRUE)


sapply(c("concentration", "maxNumParents", "maxNumValues", "numInstances", "numNodes"), autoKLD, learningMethod = "k2Matlab",
       numIterations = 20, debug = TRUE)

autoKLD("maxNumValues", "k2Matlab", debug = TRUE)

# plot with zoomed region
figure + coord_cartesian(xlim = c(0,50))

# average density 

sapply(c("aic", "bic", "bde", "k2", "k2Matlab", "mmhc", "pc", "true"), averageDensity, currentDirectory = "numNodes", numIterations = 20)






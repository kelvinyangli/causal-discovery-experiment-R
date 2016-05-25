######################################################################################################################
# target node, Asia: either, Sachs: PKA, Insurance: ThisCarCost, Alarm: VENTALV 
#
#
#
#
#
#
#
#
#
############################################## load libraries ########################################################
setwd("Experiments/known BNs")
library(bnlearn)
library(entropy)
library(gtools)
library(gRain) 
# to install package RBGL for gRain, copy the next two lines
# source("https://bioconductor.org/biocLite.R")
# biocLite("RBGL")
library(ggplot2)
library(reshape2)

options(scipen = 10)
########################################################### data generation ###################################
generateSeed = function() {
  op = options(digits.secs = 6)
  x = gsub("[: -]", "" , Sys.time(), perl = TRUE) # remove - and : 
  x = strsplit(x, split = "[.]")[[1]][2] # get lower digits
  x = as.numeric(x) # convert char to numeric 
  return(x)
}

generateData = function(knownBN, minData, maxData, numRepeat, debug = FALSE) {
  if(!(dir.exists(paste0("Datasets/", knownBN)))) {
    dir.create(paste0("Datasets/", knownBN))
  }
  cpts = read.dsc(paste0("CPTs/", knownBN, ".dsc"))
  ratio = log10(maxData/minData)
  numInstances = c()
  for (i in 1:ratio) {
    numInstances = c(numInstances, (1:9)*minData*10^(i-1))
  }
  numInstances = c(numInstances, maxData)
  
  for (ii in 1:length(numInstances)) {
    j = 1
    while(j <= numRepeat) {
      if (debug) {
        cat("* generating dataset ", c(ii, j), "\n")
      }
      seed = generateSeed()
      set.seed(seed)
      data = rbn(cpts, numInstances[ii])
      fileName = paste0("data_", numInstances[ii], "_", seed, ".csv")
      write.csv(data, paste0("Datasets/", knownBN, "/", fileName), row.names = FALSE)
      j = j + 1
    }
  }
}

generateData2 = function(knownBN, sampleSize) {
  cpts = read.dsc(paste0("CPTs/", knownBN, ".dsc"))
  seed = generateSeed()
  set.seed(seed)
  dataPool <<- rbn(cpts, sampleSize) # dataPool is set to be a global variable
  write.csv(dataPool, paste0("Datasets/", knownBN, "_training_", seed, ".csv"), row.names = FALSE)
}

generateTestingData2 = function(knownBN, sampleSize) {
  cpts = read.dsc(paste0("CPTs/", knownBN, ".dsc"))
  seed = generateSeed()
  set.seed(seed)
  testingData = rbn(cpts, sampleSize)
  write.csv(testingData, paste0("Datasets/", knownBN, "_testing_", seed, ".csv"), row.names = FALSE)
}

################################################# order files ############################################
orderFiles2 = function(files, numRepeat) {
  numbersInName = matrix(0, nrow = length(files), ncol = 2)
  for (i in 1:length(files)) {
    numbersInName[i,] = unique(na.omit(as.numeric(unlist(strsplit(files[i], "[^0-9]+")))))
  }
  firstOrder = order(numbersInName[,1])
  numbersInName = numbersInName[firstOrder,]
  orderedFiles = files[firstOrder]
  
  for (j in 1:(length(files)/numRepeat)) {
    secondOrder = order(numbersInName[((j-1)*numRepeat+1):(j*numRepeat), 2])
    orderedFiles[((j-1)*numRepeat+1):(j*numRepeat)] = orderedFiles[((j-1)*numRepeat+1):(j*numRepeat)][secondOrder]
  }
  return(orderedFiles)
}


################################################### structure learning ###################################
# dataPoolName = list.files("Datasets/", pattern = paste0(knownBN, "_training"))
# dataPool = read.csv(paste0("Datasets/", dataPoolName), header = TRUE)
# if (!is.factor(dataPool[1, 1])) dataPool = data.frame(apply(dataPool, 2, as.factor))

numInstancesList = c(seq(100, 900, 100), seq(1000, 9000, 1000), seq(10000, 100000, 10000))

autoLearn3 = function(knownBN, dataPool, learningMethod, numInstancesList, numRepeat, debug = FALSE) {
  if(!(dir.exists(paste0("Learned networks/", knownBN, "/", learningMethod)))) {
    dir.create(paste0("Learned networks/", knownBN, "/", learningMethod))
  }
  #if (debug) cat("* reading dataPool from disk \n")
  for (i in 1:length(numInstancesList)) {
    j = 1
    while (j <= numRepeat) {
      if (debug) cat("* learning structure", c(i, j), "\n")
      seed = generateSeed()
      set.seed(seed)
      index = sample(1:nrow(dataPool), numInstancesList[i])
      data = dataPool[index,]
      if (learningMethod == "mmhc") {
        dagLearned = mmhc(data)
      } else if (learningMethod == "bde") {
        dagLearned = hc(data, score = learningMethod, max.iter = 100)
      } else {
        dagLearned = hc(data, score = learningMethod)
      }
      cptsLearned = bn.fit(dagLearned, data, method = "bayes")
      fileName = paste0("dag_", numInstancesList[i], "_", seed, ".dsc")
      if (debug) cat("* saving the ", i, "learned structure \n")
      write.dsc(paste0("Learned networks/", knownBN, "/", learningMethod, "/", fileName), cptsLearned)
      j = j + 1
    }
  }
}

################################################### TPR and FPR ###################################################
autoAccuracy2 = function(knownBN, learningMethod, numRepeat, debug = FALSE) {
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/accuracy")))) {
    dir.create(paste0("Evaluations/", knownBN, "/accuracy"))
  }
  cptsTrue = read.dsc(paste0("CPTs/", knownBN, ".dsc"))
  dagTrue = model2network(modelstring(cptsTrue))
  allCptsLearned = list.files(paste0("Learned networks/", knownBN, "/", learningMethod))
  allCptsLearned = orderFiles2(allCptsLearned, numRepeat)
  numTP = rep(0, length(allCptsLearned))
  numFP = rep(0, length(allCptsLearned))
  numFN = rep(0, length(allCptsLearned))
  for (i in 1:length(allCptsLearned)) {
    if (debug) {
      cat("* calculating TPR & FPR for the ", i, "file \n")
    }
    cptsLearned = read.dsc(paste0("Learned networks/", knownBN, "/", learningMethod, "/", allCptsLearned[i]))
    dagLearned = model2network(modelstring(cptsLearned))
    accuracy = bnlearn::compare(dagTrue, dagLearned) # use compare function from bnlearn
    numTP[i] = accuracy$tp
    numFP[i] = accuracy$fp
    numFN[i] = accuracy$fn
  }
  write.csv(numTP, paste0("Evaluations/", knownBN, "/accuracy/", learningMethod, "_TP.csv"), row.names = FALSE)
  write.csv(numFP, paste0("Evaluations/", knownBN, "/accuracy/", learningMethod, "_FP.csv"), row.names = FALSE)
  write.csv(numFN, paste0("Evaluations/", knownBN, "/accuracy/", learningMethod, "_FN.csv"), row.names = FALSE)
}

################################################### Precision and recall ###################################################
autoPrecisionRecall2 = function(knownBN, numRepeat) {
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/precision")))) dir.create(paste0("Evaluations/", knownBN, "/precision"))
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/recall")))) dir.create(paste0("Evaluations/", knownBN, "/recall"))
  
  allTPs = list.files(paste0("Evaluations/", knownBN, "/accuracy/"), pattern = "_TP.csv")
  allFPs = list.files(paste0("Evaluations/", knownBN, "/accuracy/"), pattern = "_FP.csv")
  allFNs = list.files(paste0("Evaluations/", knownBN, "/accuracy/"), pattern = "_FN.csv")
  for (i in 1:length(allTPs)) {
    TP = read.csv(paste0("Evaluations/", knownBN, "/accuracy/", allTPs[i]), header = TRUE)
    FP = read.csv(paste0("Evaluations/", knownBN, "/accuracy/", allFPs[i]), header = TRUE)
    FN = read.csv(paste0("Evaluations/", knownBN, "/accuracy/", allFNs[i]), header = TRUE)
    precision = TP/(TP + FP)
    precision[is.na(precision)] = 0 # replace NA with 0
    recall = TP/(TP + FN)
    recall[is.na(recall)] = 0 # replace NA with 0
    learningMethod = strsplit(allTPs[i], split = "\\_")[[1]][1]
    
    write.csv(precision, paste0("Evaluations/", knownBN, "/precision/", learningMethod, ".csv"), row.names = FALSE)
    write.csv(recall, paste0("Evaluations/", knownBN, "/recall/", learningMethod, ".csv"), row.names = FALSE)
    
    if(!(dir.exists(paste0("Evaluations/", knownBN, "/precision/stats")))) dir.create(paste0("Evaluations/", knownBN, "/precision/stats"))
    autoStats2(knownBN, learningMethod, "precision", as.vector(t(precision)), 20, write = TRUE)
    
    if(!(dir.exists(paste0("Evaluations/", knownBN, "/recall/stats")))) dir.create(paste0("Evaluations/", knownBN, "/recall/stats"))
    autoStats2(knownBN, learningMethod, "recall", as.vector(t(recall)), 20, write = TRUE)
  }
}

################################################### F measure ###################################################
autoFMeasure2 = function(knownBN, numRepeat) {
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/f measure")))) {
    dir.create(paste0("Evaluations/", knownBN, "/f measure"))
  }
  allTPs = list.files(paste0("Evaluations/", knownBN, "/accuracy/"), pattern = "_TP.csv")
  allFPs = list.files(paste0("Evaluations/", knownBN, "/accuracy/"), pattern = "_FP.csv")
  allFNs = list.files(paste0("Evaluations/", knownBN, "/accuracy/"), pattern = "_FN.csv")
  for (i in 1:length(allTPs)) {
    TP = read.csv(paste0("Evaluations/", knownBN, "/accuracy/", allTPs[i]), header = TRUE)
    FP = read.csv(paste0("Evaluations/", knownBN, "/accuracy/", allFPs[i]), header = TRUE)
    FN = read.csv(paste0("Evaluations/", knownBN, "/accuracy/", allFNs[i]), header = TRUE)
    precision = TP/(TP + FP)
    recall = TP/(TP + FN)
    fMeasure = 2*(precision * recall)/(precision + recall)
    fMeasure[is.na(fMeasure)] = 0 # replace NA with 0 
    
    learningMethod = strsplit(allTPs[i], split = "\\_")[[1]][1]
    write.csv(fMeasure, paste0("Evaluations/", knownBN, "/f measure/", learningMethod, ".csv"), row.names = FALSE)
    
    if(!(dir.exists(paste0("Evaluations/", knownBN, "/f measure/stats")))) dir.create(paste0("Evaluations/", knownBN, "/f measure/stats"))
    autoStats2(knownBN, learningMethod, "f measure", as.vector(t(fMeasure)), numRepeat)
  }
}

################################################### SHD ################################################### 
# calculate 3 versions of hamming distance
# bnlearn::shd define on cpdags
# bnlearn::hamming define on skeletons
# hammingDags define on dags
autoSHD2 = function(knownBN, learningMethod, numRepeat, debug = FALSE) {
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/shd cpdags")))) dir.create(paste0("Evaluations/", knownBN, "/shd cpdags"))
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/shd skeletons")))) dir.create(paste0("Evaluations/", knownBN, "/shd skeletons"))
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/shd dags")))) dir.create(paste0("Evaluations/", knownBN, "/shd dags"))
  
  cptsTrue = read.dsc(paste0("CPTs/", knownBN, ".dsc"))
  dagTrue = model2network(modelstring(cptsTrue))
  allCptsLearned = list.files(paste0("Learned networks/", knownBN, "/", learningMethod))
  allCptsLearned = orderFiles2(allCptsLearned, numRepeat)
  # 3 empty score vectors
  shdCpdags = rep(0, length(allCptsLearned)) 
  shdSkeletons = shdCpdags
  shdDags = shdCpdags
  
  for (i in 1:length(allCptsLearned)) {
    if (debug) {
      cat("* calculating SHD for the ", i, "file \n")
    }
    cptsLearned = read.dsc(paste0("Learned networks/", knownBN, "/", learningMethod, "/", allCptsLearned[i]))
    dagLearned = model2network(modelstring(cptsLearned))
    # shd
    shdCpdags[i] = bnlearn::shd(learned = dagLearned, true = dagTrue)
    # hamming
    shdSkeletons[i] = hamming(learned = dagLearned, true = dagTrue)
    # hammingDags
    shdDags[i] = hammingDags(learned = dagLearned, true = dagTrue)
  }
  
  write.csv(shdCpdags, paste0("Evaluations/", knownBN, "/shd cpdags/", learningMethod, ".csv"), row.names = FALSE)
  write.csv(shdSkeletons, paste0("Evaluations/", knownBN, "/shd skeletons/", learningMethod, ".csv"), row.names = FALSE)
  write.csv(shdDags, paste0("Evaluations/", knownBN, "/shd dags/", learningMethod, ".csv"), row.names = FALSE)
  
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/shd cpdags/stats")))) dir.create(paste0("Evaluations/", knownBN, "/shd cpdags/stats"))
  autoStats2(knownBN, learningMethod, "shd cpdags", shdCpdags, numRepeat)
  
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/shd skeletons/stats")))) dir.create(paste0("Evaluations/", knownBN, "/shd skeletons/stats"))
  autoStats2(knownBN, learningMethod, "shd skeletons", shdSkeletons, numRepeat)
  
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/shd dags/stats")))) dir.create(paste0("Evaluations/", knownBN, "/shd dags/stats"))
  autoStats2(knownBN, learningMethod, "shd dags", shdDags, numRepeat)
}

########################################## RMSE ####################################
autoRMSE2 = function(knownBN, learningMethod, targetNode, numRepeat, debug = FALSE) {
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/rmse")))) dir.create(paste0("Evaluations/", knownBN, "/rmse"))
  cptsTrue = read.dsc(paste0("CPTs/", knownBN, ".dsc"))
  testingFile = list.files("Datasets", patter = paste0(knownBN, "_testing"))
  testingData = read.csv(paste0("Datasets/", testingFile), header = TRUE)
  if (!is.factor(testingData[1, 1])) testingData = data.frame(apply(testingData, 2, as.factor))
  allCptsLearned = list.files(paste0("Learned networks/", knownBN, "/", learningMethod))
  allCptsLearned = orderFiles2(allCptsLearned, numRepeat)
  rmseScore = rep(0, length(allCptsLearned))
  for (i in 1:length(allCptsLearned)) {
    if (debug) {
      cat("* calculating RMSE for the ", i, "file \n")
    }
    cptsLearned = read.dsc(paste0("Learned networks/", knownBN, "/", learningMethod, "/", allCptsLearned[i]))
    if (length(mb(cptsLearned, targetNode)) < 9) {
      rmseScore[i] = rmse(testingData, cptsLearned, targetNode, debug = debug)
    } else {
      rmseScore[i] = rmse2(testingData, cptsLearned, targetNode, debug = debug)
    }
  }
  write.csv(rmseScore, paste0("Evaluations/", knownBN, "/rmse/", learningMethod, ".csv"), row.names = FALSE)
  
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/rmse/stats")))) dir.create(paste0("Evaluations/", knownBN, "/rmse/stats"))
  autoStats2(knownBN, learningMethod, "rmse", rmseScore, numRepeat)
}

########################################## BIR ####################################
autoBIR2 = function(knownBN, learningMethod, targetNode, numRepeat, debug = FALSE) {
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/bir")))) dir.create(paste0("Evaluations/", knownBN, "/bir"))
  cptsTrue = read.dsc(paste0("CPTs/", knownBN, ".dsc"))
  testingFile = list.files("Datasets", patter = paste0(knownBN, "_testing"))
  testingData = read.csv(paste0("Datasets/", testingFile), header = TRUE)
  if (!is.factor(testingData[1, 1])) testingData = data.frame(apply(testingData, 2, as.factor))
  allCptsLearned = list.files(paste0("Learned networks/", knownBN, "/", learningMethod))
  allCptsLearned = orderFiles2(allCptsLearned, numRepeat)
  birScore = rep(0, length(allCptsLearned))
  for (i in 1:length(allCptsLearned)) {
    if (debug) {
      cat("* calculating BIR for the ", i, "file \n")
    }
    cptsLearned = read.dsc(paste0("Learned networks/", knownBN, "/", learningMethod, "/", allCptsLearned[i]))
    if (length(mb(cptsLearned, targetNode)) < 9) {
      birScore[i] = bir(testingData, cptsLearned, targetNode, debug = debug)
    } else {
      birScore[i] = bir2(testingData, cptsLearned, targetNode, debug = debug)
    }
    
  }
  write.csv(birScore, paste0("Evaluations/", knownBN, "/bir/", learningMethod, ".csv"), row.names = FALSE)
  
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/bir/stats")))) dir.create(paste0("Evaluations/", knownBN, "/bir/stats"))
  autoStats2(knownBN, learningMethod, "bir", birScore, numRepeat)
}

autoRMSEBIR2 = function(knownBN, learningMethod, targetNode, numRepeat, debug = FALSE) {
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/bir")))) dir.create(paste0("Evaluations/", knownBN, "/bir"))
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/rmse")))) dir.create(paste0("Evaluations/", knownBN, "/rmse"))
  cptsTrue = read.dsc(paste0("CPTs/", knownBN, ".dsc"))
  testingFile = list.files("Datasets", patter = paste0(knownBN, "_testing"))
  testingData = read.csv(paste0("Datasets/", testingFile), header = TRUE)
  if (!is.factor(testingData[1, 1])) testingData = data.frame(apply(testingData, 2, as.factor))
  allCptsLearned = list.files(paste0("Learned networks/", knownBN, "/", learningMethod))
  allCptsLearned = orderFiles2(allCptsLearned, numRepeat)
  birScore = rep(0, length(allCptsLearned))
  rmseScore = rep(0, length(allCptsLearned))
  
  for (i in 1:length(allCptsLearned)) {
    if (debug) cat("* reading the ", i, "structure \n")
    cptsLearned = read.dsc(paste0("Learned networks/", knownBN, "/", learningMethod, "/", allCptsLearned[i]))
    if (debug) cat("* calculating rmse and bir for the ", i, "structure \n")
    
    if (length(mb(cptsLearned, targetNode)) < 9) {
      res = rmseBIR(testingData, cptsLearned, targetNode, debug = debug)
      birScore[i] = res$bir
      rmseScore[i] = res$rmse
    } else {
      res = rmseBIR2(testingData, cptsLearned, targetNode, debug = debug)
      birScore[i] = res$bir
      rmseScore[i] = res$rmse
    }
    
  }
  write.csv(birScore, paste0("Evaluations/", knownBN, "/bir/", learningMethod, ".csv"), row.names = FALSE)
  write.csv(rmseScore, paste0("Evaluations/", knownBN, "/rmse/", learningMethod, ".csv"), row.names = FALSE)
  
  # bir 
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/bir/stats")))) dir.create(paste0("Evaluations/", knownBN, "/bir/stats"))
  autoStats2(knownBN, learningMethod, "bir", birScore, numRepeat)

  # rmse 
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/rmse/stats")))) dir.create(paste0("Evaluations/", knownBN, "/rmse/stats"))
  autoStats2(knownBN, learningMethod, "rmse", rmseScore, numRepeat)
}
########################################## KLD ####################################
autoKLD2 = function(knownBN, learningMethod, numRepeat, debug = FALSE) {
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/kld")))) dir.create(paste0("Evaluations/", knownBN, "/kld"))
  cptsTrue = read.dsc(paste0("CPTs/", knownBN, ".dsc"))
  allCptsLearned = list.files(paste0("Learned networks/", knownBN, "/", learningMethod))
  allCptsLearned = orderFiles2(allCptsLearned, numRepeat)
  klDivergence = rep(0, length(allCptsLearned))
  for (i in 1:length(allCptsLearned)) {
    if (debug) {
      cat("* calculating the KLD for the ", i, "file \n")
    }
    cptsLearned = read.dsc(paste0("Learned networks/", knownBN, "/", learningMethod, "/", allCptsLearned[i]))
    klDivergence[i] = kld(cptsTrue, cptsLearned, debug = debug)[[1]]
  }
  write.csv(klDivergence, paste0("Evaluations/", knownBN, "/kld/", learningMethod, ".csv"), row.names = FALSE)
  
  if(!(dir.exists(paste0("Evaluations/", knownBN, "/kld/stats")))) dir.create(paste0("Evaluations/", knownBN, "/kld/stats"))
  autoStats2(knownBN, learningMethod, "kld", klDivergence, numRepeat)
}

########################################## average ####################################
autoAverage2 = function(knownBN, learningMethod, measure, vector, numRepeat, write = TRUE) {
  average = rep(0, length(vector)/numRepeat)
  for (i in 1:length(average)) {
    average[i] = mean(vector[((i-1)*numRepeat+1):(i*numRepeat)])
  }
  if (write) write.csv(average, paste0("Evaluations/", knownBN, "/", measure, "/mean/mean_", learningMethod, ".csv"), row.names = FALSE)
  return(average)
}

########################################## std ####################################
autoStd2 = function(knownBN, learningMethod, measure, vector, numRepeat, write = TRUE) {
  std = rep(0, length(vector)/numRepeat) 
  for (i in 1:length(std)) {
    std[i] = sd(vector[((i-1)*numRepeat+1):(i*numRepeat)])
  }
  if (write) write.csv(std, paste0("Evaluations/", knownBN, "/", measure, "/std/std_", learningMethod, ".csv"), row.names = FALSE)
  return(std)
}

# mean and sd together
autoStats2 = function(knownBN, learningMethod, measure, vector, numRepeat, alpha = 0.05, write = TRUE) {
  average = rep(0, length(vector)/numRepeat)
  std = average
  for (i in 1:length(average)) {
    average[i] = mean(vector[((i-1)*numRepeat+1):(i*numRepeat)])
    std[i] = sd(vector[((i-1)*numRepeat+1):(i*numRepeat)])
  }
  error = qnorm(1 - alpha)*std/sqrt(numRepeat)
  stats <<- data.frame(average, std, error)

  if (write) write.csv(stats, paste0("Evaluations/", knownBN, "/", measure, "/stats/stats_", learningMethod, ".csv"), row.names = FALSE)
}

####################################### Execution starts here ####################################

sapply(c("mmhc", "aic", "bic", "bde", "k2"), autoLearn3, knownBN = knownBN, dataPool = dataPool, numInstancesList, numRepeat = 20, debug = TRUE)
sapply(c("mmhc", "aic", "bic", "bde", "k2"), autoAccuracy2, knownBN = knownBN, numRepeat = 20, debug = TRUE)
autoPrecisionRecall2(knownBN, 20)
autoFMeasure2(knownBN, 20)
sapply(c("mmhc", "aic", "bic", "bde", "k2"), autoSHD2, knownBN = knownBN, numRepeat = 20, debug = TRUE)
sapply(c("mmhc", "aic", "bic", "bde", "k2"), autoRMSE2, knownBN = knownBN, targetNode = "PKA", numRepeat = 20, debug = TRUE)
sapply(c("mmhc", "aic", "bic", "bde", "k2"), autoKLD2, knownBN = knownBN, numRepeat = 20, debug = TRUE)

# save plots
numRepeat = 20
knownBN = "alarm"

measure = "precision"
measure = "recall"
measure = "f measure"
measure = "shd cpdags"
measure = "shd dags"
measure = "shd skeletons"
measure = "rmse"
measure = "bir"
measure = "kld"

df_k2 = read.csv(paste0("Evaluations/", knownBN, "/", measure, "/stats/stats_k2.csv"), header = TRUE)
df_mmhc = read.csv(paste0("Evaluations/", knownBN, "/", measure, "/stats/stats_mmhc.csv"), header = TRUE)
df_aic = read.csv(paste0("Evaluations/", knownBN, "/", measure, "/stats/stats_aic.csv"), header = TRUE)
df_bic = read.csv(paste0("Evaluations/", knownBN, "/", measure, "/stats/stats_bic.csv"), header = TRUE)
df_bde = read.csv(paste0("Evaluations/", knownBN, "/", measure, "/stats/stats_bde.csv"), header = TRUE)
tempMean = cbind(numInstancesList, df_k2$average, df_mmhc$average, df_aic$average, df_bic$average, df_bde$average)
colnames(tempMean) = c("numInstances", "k2", "mmhc", "aic", "bic", "bde")
tempMean = as.data.frame(tempMean) # convert matrix to data.frame
meltTempMean = melt(tempMean, id = "numInstances")
error = c(df_k2$error, df_mmhc$error, df_aic$error, df_bic$error, df_bde$error) # standard error
figure = ggplot(meltTempMean, aes(x = meltTempMean[,1], y = value, color = variable)) + 
  geom_line(data = meltTempMean) +ylab(label = measure) + xlab("numInstances")
figure = figure + geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.2)
figure

if(!(dir.exists(paste0("Plots/", knownBN)))) dir.create(paste0("Plots/", knownBN))
ggsave(filename = paste0("Plots/", knownBN, "/", measure, "_", knownBN, ".png"))
figure





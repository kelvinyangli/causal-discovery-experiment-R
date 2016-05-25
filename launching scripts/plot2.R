# scripts for plotting and save plot

#currentDirectory = "concentration"

#measure = "precision"

plotting2 = function(currentDirectory, measure, boundaries = FALSE, dropPC = FALSE) {
  
  if ((currentDirectory == "maxNumParents") | (currentDirectory == "maxNumParents_nonRandomizedNodeOrdering")) {
    ls = 0:4
  } else if ((currentDirectory == "maxNumValues") | (currentDirectory == "maxNumValues_nonRandomizedNodeOrdering")) {
    ls = 2:6
  } else if ((currentDirectory == "concentration") | (currentDirectory == "concentration_nonRandomizedNodeOrdering")) {
    ls = seq(1,9,by = 2)
  } else if ((currentDirectory == "numInstances") | (currentDirectory == "numInstances_nonRandomizedNodeOrdering")) {
    ls = c(1,4,7,10,15) * 1000
  } else if ((currentDirectory == "numNodes") | (currentDirectory == "numNodes_nonRandomizedNodeOrdering")) {
    ls = seq(5,25,by = 5)
  }
  
  if (boundaries) {
    
    if (measure == "rmse") {
      
      df_best = read.csv(paste0(currentDirectory, "/Evaluations/reference/stats/stats_refRMSEs.csv"), header = TRUE)
      df_worst = read.csv(paste0(currentDirectory, "/Evaluations/randomGuessing/stats/stats_randomGuessingRMSEs.csv"), header = TRUE)
      
    } else if (measure == "bir") {
      
      df_best = read.csv(paste0(currentDirectory, "/Evaluations/reference/stats/stats_refBIRs.csv"), header = TRUE)
      df_worst = data.frame(average = rep(0, nrow(df_best)), std = rep(0, nrow(df_best)), error = rep(0, nrow(df_best)))
      
    } 
    
  } else {
    
    df_best = NULL
    df_worst = NULL
    
  }
  
  
  df_mmhc = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_mmhc.csv"), header = TRUE)
  df_aic = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_aic.csv"), header = TRUE)
  df_bic = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_bic.csv"), header = TRUE)
  df_bde = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_bde.csv"), header = TRUE)
  df_k2Matlab = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_k2Matlab.csv"), header = TRUE)
  df_pc = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_pc.csv"), header = TRUE)
  
  tempMean = cbind(ls, df_mmhc$average, df_aic$average, df_bic$average, df_bde$average, 
                  df_k2Matlab$average, df_pc$average, df_best$average, df_worst$average)
  
  if (boundaries) {
    
    if (measure == "rmse") {
      
      colnames(tempMean) = c(currentDirectory, "mmhc", "aic", "bic", "bde", 
                             "k2Matlab", "pc", "best", "worst")
      
    } else if (measure == "bir") {
      
      colnames(tempMean) = c(currentDirectory, "mmhc", "aic", "bic", "bde", 
                             "k2Matlab", "pc", "best", "worst")
      
    } 
    
  } else {
    
    colnames(tempMean) = c(currentDirectory, "mmhc", "aic", "bic", "bde", "k2Matlab", "pc")
    
  }
  
  
  tempMean = as.data.frame(tempMean) # convert matrix to data.frame
  meltTempMean = melt(tempMean, id = currentDirectory)
  error = c(df_mmhc$error, df_aic$error, df_bic$error, df_bde$error, 
            df_k2Matlab$error, df_pc$error, df_best$error, df_worst$error) # standard error
  
  # color options
  #cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  figure = ggplot(meltTempMean, aes(x = meltTempMean[,1], y = value, color = variable)) + 
    geom_line(data = meltTempMean) + ylab(label = measure) + xlab(currentDirectory)
  
  figure = figure + geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.2) + 
    ylim(min(meltTempMean$value - error), max(meltTempMean$value + error))
  
  figure
  
  ggsave(filename = paste0(currentDirectory, "/Plots/", measure, "_", currentDirectory, ".png"))
  
}

# 

#sapply(c("precision", "recall", "f measure", "shd cpdags", "shd dags", "shd skeletons"), plotting2, currentDirectory = "concentration")
#sapply(c("maxNumParents", "maxNumValues", "numNodes", "numInstances", "concentration"), plotting2, measure = "kld")
#sapply(c("rmse", "bir"), plotting, currentDirectory = "maxNumParents_nonRandomizedNodeOrdering", boundaries = TRUE)

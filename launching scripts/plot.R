# scripts for plotting and save plot

#currentDirectory = "maxNumParents"

#measure = "precision"

plotting = function(currentDirectory, measure) {
  
  if (currentDirectory == "maxNumParents") {
    ls = 0:15
    xlabel = "Maximum number of parents"
  } else if (currentDirectory == "maxNumValues") {
    ls = 2:6
    xlabel = "Maximum arity"
  } else if (currentDirectory == "concentration") {
    ls = c(1:10, 50, 100, 150, 200)
    xlabel = "Concentration parameter in symmetric Dirichlet distribution"
  } else if (currentDirectory == "numInstances") {
    ls = c(seq(100, 900, 100), seq(1000, 9000, 1000), seq(10000, 100000, 10000))
    xlabel = "Sample size"
  } else if (currentDirectory == "numNodes") {
    ls = c(2, 3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 100, 200)
    xlabel = "Number of nodes"
  }
  
  if (measure == "rmse") {
    
    df_best = read.csv(paste0(currentDirectory, "/Evaluations/reference/stats/stats_refRMSEs.csv"), header = TRUE)
    df_worst = read.csv(paste0(currentDirectory, "/Evaluations/randomGuessing/stats/stats_randomGuessingRMSEs.csv"), header = TRUE)
    df_true = NULL
    
    colNames = c(currentDirectory,"pc", "mmhc", "hc+aic", "hc+bic", "hc+bde", "hc+k2", "k2+MWST", 
                 "best", "worst")
    
  } else if (measure == "bir") {
    
    df_best = read.csv(paste0(currentDirectory, "/Evaluations/reference/stats/stats_refBIRs.csv"), header = TRUE)
    df_worst = data.frame(average = rep(0, nrow(df_best)), std = rep(0, nrow(df_best)), error = rep(0, nrow(df_best)))
    df_true = NULL
    
    colNames = c(currentDirectory, "pc", "mmhc", "hc+aic", "hc+bic", "hc+bde", "hc+k2", "k2+MWST", 
                 "best", "worst")
    
  } else if (measure == "density") {
    
    df_true = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_true.csv"), header = TRUE)
    df_best = NULL
    df_worst = NULL
    
    colNames = c(currentDirectory,"pc", "mmhc", "hc+aic", "hc+bic", "hc+bde", "hc+k2", "k2+MWST", "True model")
    
  } else {
    
    df_best = NULL
    df_worst = NULL
    df_true = NULL
    
    colNames = c(currentDirectory, "pc", "mmhc", "hc+aic", "hc+bic", "hc+bde", "hc+k2", "k2+MWST")
    
  }
  
  
  df_mmhc = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_mmhc.csv"), header = TRUE)
  df_aic = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_aic.csv"), header = TRUE)
  df_bic = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_bic.csv"), header = TRUE)
  df_bde = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_bde.csv"), header = TRUE)
  df_k2 = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_k2.csv"), header = TRUE)
  df_k2Matlab = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_k2Matlab.csv"), header = TRUE)
  df_pc = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_pc.csv"), header = TRUE)
  
  tempMean = cbind(ls, df_pc$average, df_mmhc$average, df_aic$average, df_bic$average, df_bde$average, df_k2$average, 
                   df_k2Matlab$average, df_best$average, df_worst$average, df_true$average)
  
  colnames(tempMean) = colNames
  
  
  tempMean = as.data.frame(tempMean) # convert matrix to data.frame
  meltTempMean = melt(tempMean, id = currentDirectory)
  colnames(meltTempMean)[2] = "Methods"
  error = c(df_pc$error, df_mmhc$error, df_aic$error, df_bic$error, df_bde$error, df_k2$error, df_k2Matlab$error,
            df_best$error, df_worst$error, df_true$error) # standard error
  
  if (measure == "f measure") {
    ylabel = "F measure"
  } else if (measure == "precision") {
    ylabel = "Precision"
  } else if (measure == "recall") {
    ylabel = "Recall"
  } else if (measure == "shd cpdags") {
    ylabel = "Edit distance for CPDAG"
  } else if (measure == "shd dags") {
    ylabel = "Edit distance for DAG"
  } else if (measure == "rmse") {
    ylabel = "Root mean square error"
  } else if (measure == "bir") {
    ylabel = "Bayesian information reward"
  } else if (measure %in% c("kld", "approx kld")) {
    ylabel = "Constant - KL-divergece"
  } else if (measure == "density") {
    ylabel = "Average density"
  }
  
  figure = ggplot(meltTempMean, aes(x = meltTempMean[,1], y = value, color = Methods)) + 
    geom_line(data = meltTempMean) + ylab(label = ylabel) + xlab(xlabel)
  
  figure = figure + geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.2) + 
    ylim(min(meltTempMean$value-error), max(meltTempMean$value+error))
  
  figure
  
  ggsave(figure, filename = paste0(currentDirectory, "/Plots/", measure, "_", currentDirectory, ".png"),
         width = 20, height = 10, unit = "cm")
  
  ggsave(figure, filename = paste0("C:/Users/Administrator/Dropbox/PhD@Monash/Experimental report/figures/", measure, "_", currentDirectory, ".png"),
         width = 20, height = 10, unit = "cm")
  
}

#sapply(c("precision", "recall", "f measure", "shd cpdags", "shd dags", "rmse", "bir", "kld", "density"), plotting, 
#       currentDirectory = "numNodes")


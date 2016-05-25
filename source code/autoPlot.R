autoPlot = function(currentDirectory, measure, parameters) {
  
  if (measure == "rmse") {
    
    df_ref = read.csv(paste0(currentDirectory, "/Evaluations/reference/stats/stats_refRMSEs.csv"), header = TRUE)
    df_ran = read.csv(paste0(currentDirectory, "/Evaluations/randomGuessing/stats/stats_randomGuessingRMSEs.csv"), header = TRUE)
    
  } else if (measure == "bir") {
    
    df_ref = read.csv(paste0(currentDirectory, "/Evaluations/reference/stats/stats_refBIRs.csv"), header = TRUE)
    df_ran = data.frame(average = 0, std = 0, error = 0)
    
  } else {
    
    df_ref = NULL
    df_ran = NULL
  }
  
  df_pc = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_pc.csv"), header = TRUE)
  df_mmhc = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_mmhc.csv"), header = TRUE)
  df_aic = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_aic.csv"), header = TRUE)
  df_bic = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_bic.csv"), header = TRUE)
  df_bde = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_bde.csv"), header = TRUE)
  df_k2 = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_k2.csv"), header = TRUE)
  
  tempMean = cbind(parameters, df_pc$average, df_mmhc$average, df_aic$average, df_bic$average, 
                   df_bde$average, df_k2$average, df_ref$average, df_ran$average)
  
  if ((measure == "rmse") | (measure == "bir")) {
    
    colnames(tempMean) = c(currentDirectory, "pc", "mmhc", "aic", "bic", "bde", "k2", "lowerBound", "upperBound")
    
  } else {
    
    colnames(tempMean) = c(currentDirectory, "pc", "mmhc", "aic", "bic", "bde", "k2")
    
  }

  tempMean = as.data.frame(tempMean) # convert matrix to data.frame
  
  meltTempMean = melt(tempMean, id = currentDirectory)
  
  error = c(df_pc$error, df_mmhc$error, df_aic$error, df_bic$error, df_bde$error, df_k2$error, 
            df_ref$error, df_ran$error) # standard error

  figure = ggplot(meltTempMean, aes(x = meltTempMean[,1], y = value, color = variable)) + 
    geom_line(data = meltTempMean) +ylab(label = measure) + xlab(currentDirectory)
  
  figure = figure + geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.2) + 
    ylim(min(meltTempMean$value-error), max(meltTempMean$value+error))
  
  return(figure)
  
}


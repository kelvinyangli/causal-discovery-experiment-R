# compute the mean, sd and error for confidence interval
autoStats = function(currentDirectory, learningMethod, measure, vector, numIterations, alpha = 0.05) {
  
  average = vector(length = length(vector)/numIterations) # empty mean
  
  std = average # empty sd
  
  for (i in 1:length(average)) { 
    
    average[i] = mean(vector[((i-1)*numIterations+1):(i*numIterations)]) # compute mean 
    
    std[i] = sd(vector[((i-1)*numIterations+1):(i*numIterations)]) # compute sd
    
  }
  
  error = qnorm(1 - alpha/2)*std/sqrt(numIterations) # compute error for confidence interval
  
  stats = data.frame(average, std, error) # store statistics into data frame
  
  # save into direcotry 
  write.csv(stats, paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_", learningMethod, ".csv"), row.names = FALSE)

}


# compute average rmse over all variables for each learning method  

getMeanPrediction = function(currentDirectory, learningMethod, measure, numIterations, alpha = 0.05) {
  
  files = list.files(paste0(currentDirectory, "/Evaluations/", measure, "/", learningMethod))
  
  files = orderFiles(files, currentDirectory, numIterations)
  
  RMSEs = vector(length = length(files))
  
  for (i in 1:length(files)) { # for each file, take the average rmse over all variables
    
    rmse = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/", learningMethod, "/", files[i]), header = TRUE)
    
    RMSEs[i] = colMeans(rmse)
  
  }
  
  write.csv(RMSEs, paste0(currentDirectory, "/Evaluations/", measure, "/", learningMethod, ".csv"), row.names = FALSE)
  
  autoStats(currentDirectory, learningMethod, measure, RMSEs, numIterations, alpha = alpha)
   
}


# mean and sd and error of relative difference 
relativeDifference = function(currentDirectory, learningMethod, measure, numIterations, alpha = 0.05) {
  
  references = list.files(paste0(currentDirectory, "/Evaluations/reference rmse and bir"), pattern = ".csv")
  
  references = orderFiles(references, currentDirectory, numIterations)
  
  RMSEs = list.files(paste0(currentDirectory, "/Evaluations/", measure, "/", learningMethod))
  
  RMSEs = orderFiles(RMSEs, currentDirectory, numIterations)
  
  differences = vector(length = length(references))
  
  std = differences
  
  rmse = vector(length = length(RMSEs))
  
  individualDifference = rmse
  
  for (i in 1:length(references)) {
    
    reference = read.csv(paste0(currentDirectory, "/Evaluations/reference rmse and bir/", references[i]), header = TRUE)
    
    reference = colMeans(reference[measure])
    
    for (j in ((i - 1)*numIterations + 1):(i*numIterations)) { # for each reference there are 20 corresponding rmse and bir
      
      rmse[j] = colMeans(read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/", learningMethod, "/", RMSEs[j]), header = TRUE))
      
      individualDifference[j] = rmse[j] - reference
    }
    
    differences[i] = mean(individualDifference[((i - 1)*numIterations + 1):(i*numIterations)])
    
    std[i] = sd(individualDifference[((i - 1)*numIterations + 1):(i*numIterations)])
    
  }
  
  error = qnorm(1 - 0.05/2) * std/sqrt(numIterations)
  
  df = data.frame(average = differences, std = std, error = error)
  
  if (measure == "rmse") {
    
    path = paste0(currentDirectory, "/Evaluations/relative rmse/stats")
    
  } else if (measure == "bir") {
    
    path = paste0(currentDirectory, "/Evaluations/relative bir/stats")
    
  }
  
  write.csv(df, paste0(path, "/stats_", learningMethod, ".csv"), row.names = FALSE)
  
}


# compile function
#autoStats = cmpfun(autoStats)
#getMeanPrediction = cmpfun(getMeanPrediction)
#relativeDifference = cmpfun(relativeDifference)

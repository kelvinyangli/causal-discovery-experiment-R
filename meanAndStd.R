
computeMean = function(data, numIterations) {
  dfMean = data.frame()
  for (i in 1:(nrow(data)/numIterations)) {
    tempData = data[((i-1)*numIterations+1):(i*numIterations), -1]
    tempMean = apply(tempData, 2, mean)
    dfMean = rbind(dfMean, tempMean)
  }
  dfMean = cbind(unique(data[,1]), dfMean)
  dfMean = dfMean[order(dfMean[,1]),]
  colnames(dfMean) = colnames(data)
  return(dfMean)
}

computeStd = function(data, numIterations) {
  dfStd = data.frame()
  for (i in 1:(nrow(data)/numIterations)) {
    tempData = data[((i-1)*numIterations+1):(i*numIterations), -1]
    tempStd = apply(tempData, 2, sd)
    dfStd = rbind(dfStd, tempStd)
  }
  dfStd = cbind(unique(data[,1]), dfStd)
  dfStd = dfStd[order(dfStd[,1]),]
  colnames(dfStd) = colnames(data)
  return(dfStd)
}

meanAndStd = function(tuningParameter, numIterations) {
  allFiles = list.files(paste0(tuningParameter, "/Evaluations"), pattern = ".csv")
  for (i in 1:length(allFiles)) {
    data = read.csv(paste0(tuningParameter, "/Evaluations/", allFiles[i]), header = TRUE)
    dataMean = computeMean(data, numIterations)
    dataStd = computeStd(data, numIterations)
    write.csv(dataMean, paste0(tuningParameter, "/Means/", allFiles[i]), row.names = FALSE)
    write.csv(dataStd, paste0(tuningParameter, "/Stds/", allFiles[i]), row.names = FALSE)
  }
}










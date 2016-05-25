############################################## merge all files ###########################################
# do rowMerge first 
# then columnMerge
# rowMerge merges files under the same learning method into one file with multiple rows

rowMerge = function(tuningParameter, evaluationMeasure, learningMethod) {
  allFiles = list.files(paste0(tuningParameter, "/Evaluations/", evaluationMeasure, "/", learningMethod))
  parameterList = c("numNodes", "maxNumParents", "maxNumValues", "associationStrength", "numInstances")
  currentParameterIndex = which(parameterList == tuningParameter)
  df = data.frame()
  vectorOfParameterValues = rep(0, length(allFiles))
  for (i in 1:length(allFiles)) {
    tempData = read.csv(paste0(tuningParameter, "/Evaluations/", evaluationMeasure, "/", learningMethod, "/", allFiles[i]), header = TRUE, row.names = 1)
    parameterValue = strsplit(allFiles[i], split = "_")[[1]][currentParameterIndex + 1]
    vectorOfParameterValues[i] = as.numeric(parameterValue)
    df = rbind(df, tempData)
  }
  df = cbind(vectorOfParameterValues, df)
  colnames(df)[1] = tuningParameter
  write.csv(df, paste0(tuningParameter, "/Evaluations/", evaluationMeasure, "/", learningMethod, ".csv"), row.names = FALSE)
}

autoRowMerge = function(tuningParameter, evaluationMeasure) {
  listOfAlgorithms = c("mmhc", "aic", "bic", "bde", "k2", "chordalysisMML", "chordalysisGtest")
  for (i in 1:length(listOfAlgorithms)) {
    if(dir.exists(paste0(tuningParameter, "/Evaluations/", evaluationMeasure, "/", listOfAlgorithms[i]))) {
      rowMerge(tuningParameter, evaluationMeasure, listOfAlgorithms[i])
    }  
  }
}

# columnMerge merges files with different learning methods into one file with multiple columns 
columnMerge = function(tuningParameter, evaluationMeasure) {
  allFiles = list.files(path = paste0(tuningParameter, "/Evaluations/", evaluationMeasure), pattern = ".csv")
  columnNames = c(tuningParameter)
  if (evaluationMeasure == "precisionandrecall") {
    mtx = matrix()
    for (i in 1:length(allFiles)) {
      df = read.csv(paste0(tuningParameter, "/Evaluations/", evaluationMeasure, "/", allFiles[i]), header = TRUE)
      mtx = cbind(mtx, df[,-1])
      learningMethod = strsplit(allFiles[i], split = "\\.")[[1]][1]
      columnNames = c(columnNames, paste0(learningMethod, c("Precision", "Recall")))
    }
    mtx[,1] = df[,1]
  } else {
    mtx = c()
    for (i in 1:length(allFiles)) {
      df = read.csv(paste0(tuningParameter, "/Evaluations/", evaluationMeasure, "/", allFiles[i]), header = TRUE)
      mtx = c(mtx, df[,-1])
      learningMethod = strsplit(allFiles[i], split = "\\.")[[1]][1]
      columnNames = c(columnNames, learningMethod)
    }
    mtx = matrix(mtx, ncol = (length(columnNames) - 1))
    mtx = cbind(df[,1], mtx)
  }
  colnames(mtx) = columnNames
  write.csv(mtx, paste0(tuningParameter, "/Evaluations/", evaluationMeasure, ".csv"), row.names = FALSE)
}








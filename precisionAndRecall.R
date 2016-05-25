# get precision and recall
computePrecision = function(data) {
  precisionScores = rep(0, nrow(data)) 
  for (i in 1:nrow(data)) {
    precisionScores[i] = data[i, 2]/(data[i, 2] + data[i, 3])
  }
  return(precisionScores)
}

computeRecall = function(data) {
  recallScore = rep(0, nrow(data)) 
  for (i in 1:nrow(data)) {
    recallScore[i] = data[i, 2]/(data[i, 2] + data[i, 4])
  }
  return(recallScore)
}

precisionAndRecall = function(tuningParameter) {
  allAccuracyData = list.files(paste0(tuningParameter, "/Evaluations/Accuracy"), pattern = ".csv")
  for (i in 1:length(allAccuracyData)) {
    data = read.csv(paste0(tuningParameter, "/Evaluations/Accuracy/", allAccuracyData[i]), header = TRUE, row.names = 1)
    precisionScores = computePrecision(data)
    recallScores = computeRecall(data)
    df = data.frame(data[,1], precision = precisionScores, recall = recallScores)
    colnames(df)[1] = tuningParameter
    write.csv(df, paste0(tuningParameter, "/Evaluations/PrecisionAndRecall/", allAccuracyData[i]), row.names = FALSE)
  }
}


############################################## auto evaluations #########################################
# evaluation = {bir, rmse, sensitivity, kld, hamming}
#########################################################################################################
autoEvaluation = function(tuningParameter, evaluationMeasure, targetNodes) {
  learningMethod = c("mmhc", "aic", "bic", "bde", "k2")
  if (evaluationMeasure == "bir") {
    sapply(learningMethod, autoBIR, tuningParameter = tuningParameter, targetNodes = targetNodes)
  } else if (evaluationMeasure == "rmse") {
    sapply(learningMethod, autoRMSE, tuningParameter = tuningParameter, targetNodes = targetNodes)
  } else if (evaluationMeasure == "precisionandrecall") {
    sapply(learningMethod, autoAccuracy, tuningParameter = tuningParameter)
    precisionAndRecall(tuningParameter)
  } else if (evaluationMeasure == "kld") {
    sapply(learningMethod, autoKLD, tuningParameter = tuningParameter)
  } else if (evaluationMeasure == "shd") {
    sapply(learningMethod, autoSHD, tuningParameter = tuningParameter)
  }
}

autoEvaluation = cmpfun(autoEvaluation)




















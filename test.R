

dag = generateDag(10,4)
cpts = generateCPTs(dag,3,10)
testingData = rbn(cpts,1000)

sapply(nodes(cpts), rmse, testingData = testingData, cptsLearned = cpts)

# observation: 
# when increase sample size for test set, rmse is more accurate, but value won't change much 
# rmse with each node as target is not varying much, between 0.64 ~ 0.71

# increase variable cardinality, rmse for each node increase
# it seems the upper bound for rmse is 1 

targetNode = "V3"
rmse(testingData, cpts, targetNode)
rmse2(testingData, cpts, targetNode)

rmseBIR(testingData, cpts, targetNode)
rmseBIR2(testingData, cpts, targetNode)


# test: generate BNs with numparents = 3, 4, 5
dag3 = generateDag(10, 3)
cpts3 = generateCPTs(dag3,2,10)
data3=rbn(cpts3,10000)
testingData3 = rbn(cpts3, 1000)
dagLearned3 = mmhc(data3)
cptsLearned3 = bn.fit(dagLearned3, data3)

dag4 = generateDag(10, 4)
cpts4 = generateCPTs(dag4,2,10)
data4=rbn(cpts4,10000)
testingData4 = rbn(cpts4, 1000)
dagLearned4 = mmhc(data4)
cptsLearned4 = bn.fit(dagLearned4, data4)

dag5 = generateDag(10, 5)
cpts5 = generateCPTs(dag5,2,10)
data5=rbn(cpts5,10000)
testingData5 = rbn(cpts5, 1000)
dagLearned5 = mmhc(data5)
cptsLearned5 = bn.fit(dagLearned5, data5)

#####
maxNumParents = 1:10
numIterations = 10
#maxNumParents = rep(maxNumParents, 20)
#maxNumParents = maxNumParents[order(maxNumParents, decreasing = FALSE)]
cptsList = list()
dataList = list()
learnedcptsList = list()
testingDataList = list()
df = data.frame()
for (i in 1:length(maxNumParents)) {
  cat("* generating dag with", maxNumParents[i], "parents \n")
  dag = generateDag(20, maxNumParents[i])
  cat("* generating cpts \n")
  cpts = generateCPTs(dag, 2, 5)
  cptsList[[i]] = cpts
  cat("* generating data \n")
  data = rbn(cpts, 10000*numIterations)
  dataList[[i]] = data
  testingData = rbn(cpts, 1000)
  testingDataList[[i]] = testingData
  
  # learning
  k = 1
  while (k <= numIterations) {
    cat("* learning structure \n")
    index = sample(1:nrow(data), 10000)
    # index = ((i-1)*10000+1):(i*10000)
    dagLearned = mmhc(data[index,])
    #dagLearned = dag
    #dagLearned = hc(data, score = "aic")
    cptsLearned = bn.fit(dagLearned, data[index,], method = "bayes")
    learnedcptsList[[i]] = cptsLearned
    rmseVector = rep(0, nrow(testingData))
    birVector = rmseVector
    expRMSEVector = rmseVector # expected square error 
    expBIRVector = rmseVector # expected info reward
    
    # compute rmse and bir
    cat("* computing rmse and bir for maxNumParents", maxNumParents[i], "\n")
    for (j in 1:nrow(testingData)) {
      targetNode = sample(colnames(testingData), 1)
      prior = freqs(table(data[,targetNode])) 
      
      # expected rmse based on true cpts
      expRES = expPrediction(testingData[j,], cpts, targetNode, prior)
      expRMSEVector[j] = expRES$expSE
      expBIRVector[j] = expRES$expBIR
      
      # actual rmse based on learned cpts
      if (debug) cat("* computing rmse and bir for row", j, "\n")
      res = prediction(testingData[j,], cptsLearned, targetNode, prior)
      rmseVector[j] = res$squareError
      birVector[j] = res$bir
    }
    #editDist1 = shd(dag, dagLearned)
    #editDist2 = hammingDags(dag, dagLearned)
    #df = rbind(df, c(parents=maxNumParents[i], rmse=sqrt(mean(rmse)), bir=mean(bir), shd_cpdags=editDist1, shd_dags=editDist2))
    
    rmse = sqrt(mean(rmseVector))
    expRMSE = sqrt(mean(expRMSEVector))
    df = rbind(df, c(maxNumParents = maxNumParents[i], rmse = rmse, expRMSE = expRMSE, rmseGap = rmse - expRMSE,
                     bir = mean(birVector), expBIR = mean(expBIRVector), birGap = mean(expBIRVector - birVector)))
    
    k = k + 1
  }
  
  colnames(df) = c("maxNumParents", "rmse", "expRMSE", "rmseGap", "bir", "expBIR", "birGap")
  print(df)
}

simplePlot = function(numIterations, measure, df, new = FALSE) {
  if (new) par(mfrow = c(2,2))
  #numIterations = 10
  if (measure == "rmse") {
    vector1 = df$rmse
    vector2 = df$expRMSE
    vector3 = df$rmseGap
  } else {
    vector1 = df$bir
    vector2 = df$expBIR
    vector3 = df$birGap
  }
  
  average1 = rep(0, length(vector1)/numIterations) # empty mean
  average2 = average1
  average3 = average1
  std1 = average1 # empty sd
  std2 = average1
  std3 = average1
  
  for (i in 1:length(average1)) { 
    average1[i] = mean(vector1[((i-1)*numIterations+1):(i*numIterations)]) # compute mean 
    average2[i] = mean(vector2[((i-1)*numIterations+1):(i*numIterations)])
    average3[i] = mean(vector3[((i-1)*numIterations+1):(i*numIterations)])
    std1[i] = sd(vector1[((i-1)*numIterations+1):(i*numIterations)]) # compute sd
    std2[i] = sd(vector2[((i-1)*numIterations+1):(i*numIterations)])
    std3[i] = sd(vector3[((i-1)*numIterations+1):(i*numIterations)])
  }
  
  plot(average1, type = "b", ylim = c(min(average1, average2), max(average1, average2)), col = "red", main = measure)
  lines(average2, type = "b", col = "blue")
  
  plot(average3, type = "b", main = "relative difference")
}

error = qnorm(1 - alpha/2)*std/sqrt(numIterations) # compute error for confidence interval
stats = data.frame(average, std, error) # store statistics into data frame
tempMean = cbind(unique(maxNumParents), stats$average)
colnames(tempMean) = c("numParents", "mmhc")

tempMean = as.data.frame(tempMean) # convert matrix to data.frame
meltTempMean = melt(tempMean, id = "numParents")
error =  c(stats$error) # standard error

figure = ggplot(meltTempMean, aes(x = meltTempMean[,1], y = value, color = variable)) + 
  geom_line(data = meltTempMean) +ylab(label = "rmse") + xlab("numParents")
figure = figure + geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.2) + 
  ylim(min(meltTempMean$value-error), max(meltTempMean$value+error))
figure


testingData = rbn(cptsTrue, 10000)

se = rep(0, nrow(testingData))
for (i in 1:nrow(testingData)) {
  targetNode = sample(colnames(testingData), 1)
  #targetNode = "V2"
  prior = freqs(table(testingData[,targetNode]))
  res=rmseBIR3(testingData[i,],cpts,targetNode,prior)
  se[i] = res$squareError
}
sqrt(mean(se))

# check average number of parenets in each dag
v = rep(0, length(cptsList))
for (i in 1:length(cptsList)) {
  cpts = cptsList[[i]]
  allNodes = bnlearn::nodes(cpts)
  totalParents = rep(0, length(allNodes))
  for (j in 1:50) {
    totalParents[j] = length(bnlearn::parents(cpts, allNodes[j]))
  }
  v[i] = sum(totalParents)/50
  print(c(v[i], max(totalParents)))
}

#####################################################
# test algorithms by compare performance against a dum learned learns no edges
f = function(testingData, cptsLearned, debug = FALSE) {
  rmse = rep(0, nrow(testingData))
  bir = rmse
  
  for (j in 1:nrow(testingData)) {
    targetNode = sample(colnames(testingData), 1)
    prior = freqs(table(testingData[,targetNode]))
    if (debug) cat("* computing rmse and bir for row", j, "\n")
    res = rmseBIR3(testingData[j,], cptsLearned, targetNode, prior)
    rmse[j] = res$squareError
    bir[j] = res$infoReward
  }
  
  return(c(sqrt(mean(rmse)), mean(bir)))
}

dag = generateDag(10,5)
graphviz.plot(dag)
cpts = generateCPTs(dag, 2, 5)
data = rbn(cpts, 10000)
testingData = rbn(cpts,10000)

listLearnedDags = list()
listLearnedDags[[1]] = mmhc(data)
listLearnedDags[[2]] = hc(data, score = "aic")
listLearnedDags[[3]] = hc(data, score = "bic")
listLearnedDags[[4]] = hc(data, score = "bde")
listLearnedDags[[5]] = hc(data, score = "k2")
listLearnedDags[[6]] = empty.graph(bnlearn::nodes(dag))
listLearnedDags[[7]] = dag

listCptsLearned = list()
for (i in 1:6) listCptsLearned[[i]] = bn.fit(listLearnedDags[[i]], data, method = "bayes")
listCptsLearned[[7]] = cpts

df_rmse = matrix(0, nrow = 7, ncol = 4) 
for (j in 1:7) {
  df_rmse[j,] = c(f(testingData, listCptsLearned[[j]]), bnlearn::shd(listLearnedDags[[j]], dag), hammingDags(listLearnedDags[[j]], dag))
}
colnames(df_rmse) = c("rmse", "bir", "shd_cpdags", "shd_dags")
rownames(df_rmse) = c("mmhc","aic","bic","bde","k2","dumb","perfect")
df_rmse


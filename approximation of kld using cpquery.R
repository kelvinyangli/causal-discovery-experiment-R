# this function returns the approximation of mutual info
# it uses cpquery to return approximation of marginal distributions
miApprox = function(x, y, cpts, method = "lw") {
  dimensionNames = dimnames(cpts[[x]]$prob)
  xLevels = dimensionNames[[x]]
  yLevels = dimensionNames[-1]
  yCombinations = expand.grid(yLevels)
  sumOfMI = 0
  
  # compute mi for all values
  for (i in 1:nrow(yCombinations)) {
    for (j in 1:length(xLevels)) {
      # for x marginal
      xEvent = paste("(", x, "=='", as.character(xLevels[j]), "')", sep = "", collapse = "&")
      probOfX = cpquery(cpts, eval(parse(text = xEvent)), evidence = TRUE, method = method)
      
      # for y marginal
      yEvent = paste("(", y, "=='", sapply(yCombinations[i,], as.character), "')", sep = "", collapse = "&")
      probOfY = cpquery(cpts, eval(parse(text = yEvent)), evidence = TRUE, method = method)
      
      # for x y joint
      xyEvent = paste("(", c(x, y), "=='", sapply(c(xLevels[j], yCombinations[i,]), as.character), "')", sep = "", collapse = "&")
      probOfXY = cpquery(cpts, eval(parse(text = xyEvent)), evidence = TRUE, method = method)
      
      # if p(x,y) = 0 or p(x) = 0 or p(y) = 0 then log term is set to 0
      if (probOfXY*probOfX*probOfY == 0) {
        logTerm = 0
      } else {
        logTerm = log(probOfXY/(probOfX*probOfY))
      }
      sumOfMI = sumOfMI + probOfXY*logTerm
      #sumOfMI = c(sumOfMI, probOfXY*logTerm)
    }
  }
  return(sumOfMI)
}

kldApprox = function(cptsTrue, cptsLearned, method = "lw", debug = FALSE) {
  # the higher the value of mi the smaller the kld and hence the closer the networks are
  allNodes = bnlearn::nodes(cptsTrue)
  mi = rep(0, length(allNodes))
  for (i in 1:length(allNodes)) {
    currentNode = allNodes[i]
    parentsOfCurrentNode = cptsLearned[[currentNode]]$parents
    # only deal with nodes who has at least one parent
    if (debug) cat("* computing mi for node", i, "\n")
    if (length(parentsOfCurrentNode) > 1) {
      approxMutualInfo = miApprox(currentNode, parentsOfCurrentNode, cptsTrue, method = method)
      while (approxMutualInfo <= 0) {
        approxMutualInfo = miApprox(currentNode, parentsOfCurrentNode, cptsTrue, method = method)
      } 
      mi[i] = approxMutualInfo
    } 
    if (debug) cat("** mi is", mi[i], "\n")
  }
  return(sum(mi))
}

miApprox(currentNode, parentsOfCurrentNode, cptsTrue, method = "lw")
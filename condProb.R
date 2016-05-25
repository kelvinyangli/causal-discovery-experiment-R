# extract conditional probability from cpts
condProb = function(cpts, childNode, childNodeValue, parentsNodes, parentsNodesValues) {
  childNodeCPT = cpts[[childNode]]$prob
  childNodeParents = cpts[[childNode]]$parents
  parentsNodesValues = parentsNodesValues[match(parentsNodes,childNodeParents)]
  p = do.call(`[`, c(list(x = childNodeCPT), as.list(c(childNodeValue, parentsNodesValues))))
  return(p)
}

condProb = cmpfun(condProb)


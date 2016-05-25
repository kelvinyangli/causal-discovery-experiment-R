###########################################################################################################################
# kl divergence 
# acid and de compos 2003
###########################################################################################################################
# this is not the full kld, it is the third term in the transformed kld equation in acid and de compos 2003
# this part of kld has a negative sign in the full kld formula, hence the higher the returned value the smaller the full kld is
# and hence the closer two distributions are 
kld = function(cptsTrue, cptsLearned, debug = FALSE) {
  
  allNodes = bnlearn::nodes(cptsTrue)
  
  miSum = 0
  
  junction = gRbase::compile(as.grain(cptsTrue)) # construct junction tree using gRain package
  
  for (i in 1:length(allNodes)) {
    
    node = allNodes[i]
    
    parents = cptsLearned[[node]]$parents
    
    if (length(parents) > 0) { # only computing mi when parents(node) is non-empty
      
      if (debug) cat("* computing mi for node", i, "\n")
      
      miSum = miSum + mutualInfo(node, parents, junction)
      
    } # end if 
    
  } # end for i
  
  return(miSum)
  
}


# this is the first two terms in transformed equation
kldFixedTerms = function(cptsTrue) {
  allNodes = bnlearn::nodes(cptsTrue)
  
  junction = compile(as.grain(cptsTrue))
  joint = querygrain(junction, nodes = allNodes, type = "joint")
  
  fixed = sum(joint*log(joint)) # 1st term
  
  for (i in 1:length(allNodes)) {
    # entropy for each node
    if (length(cptsTrue[[i]]$parents) < 1) {
      marginal = cptsTrue[[i]]$prob
    } else {
      # marginalize 
      marginal = querygrain(junction, nodes = allNodes[i], type = "marginal")[[1]]
    }
    
    fixed = fixed - sum(marginal * log(marginal))
  }
  
  return(fixed)
}


# this is the sum of all three terms 
fullKLD = function(cptsTrue, cptsLearned) {
  return(kldFixedTerms(cptsTrue, cptsLearned) - kld(cptsTrue, cptsLearned))
}

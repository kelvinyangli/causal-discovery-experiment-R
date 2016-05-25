bnlearncpt2matlabcpt = function(cpt) {
  
  arity = nrow(cpt)
  cptVec = as.vector(cpt)
  
  newcpt = rep(NA, length(cptVec))
  
  quantity = (length(cptVec)/arity)
  
  for (i in 1:arity) {
    
    for (j in 1:quantity) {
      
      newcpt[(i - 1)*quantity + j] = cptVec[i + (j - 1)*arity] 
      
    } # end for j
    
  } # end for i
  
  return(newcpt)
  
}
mutualInfoApprox = function(x, y, data) {
  
  # joint distributions of x and y
  xyJoint = freqs(table(data[, c(x, y)]))
  
  # marginalise probX and probY from xyJoint 
  yJoint = freqs(table(data[, y]))
  xMarginal = freqs(table(data[, x]))
  
  # re-name the joint distribution of y
  if (length(y) > 1) {
    
    yJointNames = expand.grid(dimnames(yJoint))
    
  } else {
    
    yJointNames = names(yJoint)
    
  }
  
  # compute mutual information for each x and y instantiation
  miSUM = 0
  
  for (i in 1:length(yJoint)) { # for each parents instantiation
    
    for (j in 1:length(xMarginal)) { # for each value of x
      
      # joint distribution of x and y
      if (length(y) > 1) { # if x has more than 1 parents
        
        probOfXY = xyJoint[getIndexForKLD(dim(xyJoint), c(j, as.numeric(yJointNames[i,])))]
        
      } else { 
        
        probOfXY = xyJoint[names(xMarginal)[j], yJointNames[i]]
        
      } # end if else 
      
      # marginal distributions of x and y
      probOfX = xMarginal[j]
      probOfY = yJoint[i]
      
      # if p(x,y) = 0 or p(x) = 0 or p(y) = 0 then log term is set to 0
      if (probOfXY*probOfX*probOfY == 0) {
        
        logTerm = 0
        
      } else {
        
        logTerm = log(probOfXY/(probOfX*probOfY))
        
      } # end if else 
      
      miSUM = miSUM + probOfXY*logTerm
      #miSUM = c(miSUM, probOfXY*logTerm)
      
    } # end for each value of x
    
  } # end for each parents instantiation
  
  return(miSUM)
  
}

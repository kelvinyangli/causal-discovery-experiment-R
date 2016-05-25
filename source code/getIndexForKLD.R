# get index of target and parents values for extracting corresponding value from cpt
getIndexForKLD = function(cptDimension, childParentsIndex) {
  
  index = 0 
  
  for (i in length(cptDimension):2) {
    
    index = (index + (childParentsIndex[i] - 1)) * cptDimension[i-1]
    
  }
  
  index = index + childParentsIndex[1]
  
  return(index)
  
}

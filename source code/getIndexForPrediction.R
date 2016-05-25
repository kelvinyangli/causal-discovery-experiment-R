# get index of target and parents values for extracting corresponding value from cpt
getIndex = function(target, dimensions, childParentsIndex) {
  
  index = 0 
  
  for (i in length(dimensions[[target]]):2) {
    
    index = (index + (childParentsIndex[i] - 1)) * dimensions[[target]][i-1]
    
  }
  
  index = index + childParentsIndex[1]
  
  return(index)
  
}

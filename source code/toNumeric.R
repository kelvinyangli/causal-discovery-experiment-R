# convert data to numerical values 
toNumeric = function(data) {
  
  # data contains factor data 
  # convert to numeric value according to levels of each attribute
  
  dataNumeric = matrix(nrow = nrow(data), ncol = ncol(data), dimnames = list(NULL, colnames(data)))
  
  for (i in 1:ncol(data)) {
    
    dataNumeric[,i] = as.numeric(data[,i])
    
  }
  
  return(as.data.frame(dataNumeric))
  
}
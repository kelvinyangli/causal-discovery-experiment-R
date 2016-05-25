# for each dataset check variable levels
# if any variable has less than 2 levels then allign its levels with levels from cpts
checkVariableLevels = function(data, cpts) {
  for (i in 1:ncol(data)) {
    if (length(levels(data[[i]])) < 2) {
      if (length(cpts[[i]]$parents) < 1) {
        variableLevles = names(cpts[[i]]$prob)
      } else {
        variableLevles = colnames(cpts[[i]]$prob)
      }
      levels(data[[i]]) = variableLevles
    }
  }
  return(data)
}


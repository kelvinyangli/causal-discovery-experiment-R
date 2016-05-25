# learn structures using pc algorithm from pcalg package
# the implemented function is pc

learnWithPC = function(data) {
  nlev = rep(0, ncol(data)) # number of levels are set to 0 initially
  for (i in 1:ncol(data)) nlev[i] = nlevels(data[,i]) # count number of levels for each variable
  for (i in 1:ncol(data)) data[,i] = as.numeric(data[,i]) # convert factor data to {0, 1, 2, ...}
  data = data - 1 # discrete values start from 0
  suffStat = list(dm = data, nlev = nlev, adaptDF = FALSE) # construct suffStat for pc argument
  # apply pc
  dagPC = pc(suffStat, 
             ## independence test: G^2 statistic
             indepTest = disCItest, alpha = 0.01, labels = colnames(data), verbose = TRUE)
  return(dagPC)
}

# convert training data into numeric to be used by matlab 

currentDirectory = "numNodes"

datasets = list.files(paste0(currentDirectory, "/Datasets/Training"))

for (i in 1:length(datasets)) {
  
  data = readRDS(paste0(currentDirectory, "/Datasets/Training/", datasets[i]))
  
  # convert into numeric
  data.numeric = toNumeric(data)
  
  write.csv(data.numeric, paste0(currentDirectory, "/Datasets/Training numeric/", datasets[i], ".csv"), row.names = FALSE)
  
}


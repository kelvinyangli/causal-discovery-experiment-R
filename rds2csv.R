# convert .rds to .csv 
# 
currentDirectory = "numInstances"
files = list.files(paste0(currentDirectory, "/Datasets/Training/"))

for (i in 1:length(files)) {
  
  data = readRDS(paste0(currentDirectory, "/Datasets/Training/", files[i]))
  
  write.csv(data, paste0(currentDirectory, "/Datasets/TrainingCsv/", files[i], ".csv"), row.names = FALSE)
  
}
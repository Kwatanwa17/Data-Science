corr <- function(directory = "C:/Users/Keita/DataScience/R Programming/specdata", threshold = 0) {
  setwd(directory)
  fnames = dir(pattern = ".csv")
  dataset = lapply(fnames, function(x) read.csv(x))
  cases = complete()
  if (threshold <= max(cases$nob)) {
    sub_cases <- cases$id[cases$nobs > threshold]
    sub_dataset <- dataset[sub_cases]
    correlation <- as.numeric(vector())
    for (i in 1:length(sub_cases)) {
      data = sub_dataset[[i]]
      data = data[complete.cases(data),]
      correlation <- append(correlation, cor(data$sulfate, data$nitrate))
    }
  } 
  return(correlation)
}




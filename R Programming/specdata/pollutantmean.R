fnames <- dir(pattern = ".csv")

dataset <- lapply(fnames, function(x) read.csv(x))

x <- dataset[3:5]

li <- as.numeric(vector())

for (n in 1:length(x)) {
  frame = x[[n]]
  sul = frame$sulfate
  logic = is.na(sul)
  li <- append(li, sul[!logic])
} 

pollutantmean <- function(directory = "C:/Users/Keita/DataScience/R Programming/specdata", pollutant, id = 1:332) {
  setwd(directory)
  fnames <- dir(pattern = ".csv")
  dataset <- lapply(fnames, function(x) read.csv(x))
  sub_dataset = dataset[id]
  li <- as.numeric(vector())
  for (n in 1:length(sub_dataset)) {
    frame = sub_dataset[[n]]
    col = frame[pollutant]
    logic = is.na(col)
    li <- append(li, col[!logic])
  } 
  mean(li)
}

pollutantmean(pollutant = "sulfate", id = 3:5)

pollutantmean(pollutant = "nitrate", id = 23)

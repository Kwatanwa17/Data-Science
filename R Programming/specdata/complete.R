

complete <- function(directory = "C:/Users/Keita/DataScience/R Programming/specdata", id=1:332) {
  setwd(directory)
  fnames = dir(pattern = ".csv")
  dataset = lapply(fnames, function(x) read.csv(x))
  sub_dataset = dataset[id]
  cases = lapply(sub_dataset, complete.cases)
  number = lapply(cases, sum)
  nobs = as.numeric(number)
  data.frame(id, nobs)
}

# id <- 4:10
# fnames <- dir(pattern = ".csv")
# dataset <- lapply(fnames, function(x) read.csv(x))
# sub_dataset <- dataset[id]
# 
# cases <- lapply(sub_dataset, complete.cases)
# number <- lapply(cases, sum)
# nobs <- as.numeric(number)
# data.frame(id, nobs)

set.seed(42)
cc <- complete(id =  332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr()                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr(threshold =  129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)


cr <- corr(threshold = 2000)                
n <- length(cr)                
cr <- corr(threshold = 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

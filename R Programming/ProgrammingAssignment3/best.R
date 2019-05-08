best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[,c(2,7,11,17,23)]
  colnames(data) <- c("Hospital name","State","heart attack","heart failure","pneumonia")
  if (state %in% data$State) {
    subdata <- subset(data, data$State == state)
  } else stop("invalid state")
  
  if (outcome %in% colnames(data)) {
    subdata <- subdata[,c("Hospital name",outcome)]
  } else  stop("invalid outcome")
  
  subdata <- subset(subdata, subdata[,outcome] != "Not Available")
  lowest <- min(as.numeric(subdata[,2]))
  best_hospitals <- subset(subdata, subdata[outcome] == lowest)
  best_hospitals <- sort(best_hospitals[,1])
  best_hospitals
}

#test
# 
# state <- "TX"
# outcome <- "heart failure"
# 
# data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
# data <- data[,c(2,7,11,17,23)]
# colnames(data) <- c("Hospital name","State","heart attack","heart failure","pneumonia")
# 
# if (state %in% data$State) {
#   subdata <- subset(data, data$State == state)
# } else stop("invalid state")
# 
# if (outcome %in% colnames(data)) {
#   subdata <- subdata[,c("Hospital name",outcome)]
# } else  stop("invalid outcome")
# 
# subdata <- subset(subdata, subdata[,outcome] != "Not Available")
# 
# lowest <- min(as.numeric(subdata[,2]))
# best_hospitals <- subset(subdata, subdata[outcome] == lowest)
# best_hospitals <- sort(best_hospitals[,1])
# best_hospitals


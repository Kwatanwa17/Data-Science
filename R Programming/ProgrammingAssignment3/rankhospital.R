rankhospital <- function(state, outcome, num = "best") {
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
  subdata[,outcome] <- as.numeric(subdata[,outcome])
  ordered <- subdata[order(subdata[,outcome], pmax(subdata[,outcome],subdata$`Hospital name`)),]
  
  if (num == "best") {
    ordered[1,1]
  } else if (num == "worst") {
    ordered[nrow(subdata),1]
  } else ordered[num,1]

}

# state <- "TX"
# outcome <- "heart failure"
# num <- 4
# 
# data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
# data <- data[,c(2,7,11,17,23)]
# colnames(data) <- c("Hospital name","State","heart attack","heart failure","pneumonia")
# if (state %in% data$State) {
#   subdata <- subset(data, data$State == state)
# } else stop("invalid state")
# if (outcome %in% colnames(data)) {
#   subdata <- subdata[,c("Hospital name",outcome)]
# } else  stop("invalid outcome")
# 
# subdata <- subset(subdata, subdata[,outcome] != "Not Available")
# subdata[,outcome] <- as.numeric(subdata[,outcome])
# ordered <- subdata[order(subdata[,outcome], pmax(subdata[,outcome],subdata$`Hospital name`)),]
# 
# if (num == "best") {
#   ordered[1,1]
# } else if (num == "worst") {
#   ordered[nrow(subdata),1]
# } else ordered[num,1]

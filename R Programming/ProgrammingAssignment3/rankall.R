rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[,c(2,7,11,17,23)]
  colnames(data) <- c("hospital","state","heart attack","heart failure","pneumonia")
  
  #Selecting the columms which will be used.
  if (outcome %in% colnames(data)) {
    subdata <- data[,c("hospital","state", outcome)]
    #Data cleaning and chechking outcome value
    subdata <- subset(subdata, subdata[,outcome] != "Not Available")
  } else  stop("invalid outcome")
  
  #Changing variable classes and listing the subdata by state name
  subdata$state <- factor(subdata$state)
  subdata[,outcome] <- as.numeric(subdata[,outcome])
  statelist <- split(subdata, subdata$state)
  #Ordering each data frame by outcome
  statelist <- lapply(statelist, function(x) x[order(x[,outcome], pmax(x[,outcome],x$hospital)),])
  
  #
  resumelist <- lapply(statelist, function(x) { 
    if (num == "best") {
      x[1,1]
    } else if (num == "worst") {
      x[nrow(x),1]
    } else x[num,1]
  })
  
  state <- names(resumelist)
  hospital <- unlist(resumelist)
  result <- data.frame(hospital, state)
  result
}

# outcome <- "heart failure"
# num <- 20
# 
# data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
# data <- data[,c(2,7,11,17,23)]
# colnames(data) <- c("hospital","state","heart attack","heart failure","pneumonia")
# 
# #Selecting the columms which will be used.
# if (outcome %in% colnames(data)) {
#   subdata <- data[,c("hospital","state", outcome)]
#   #Data cleaning and chechking outcome value
#   subdata <- subset(subdata, subdata[,outcome] != "Not Available")
# } else  stop("invalid outcome")
# 
# #Changing variable classes and listing the subdata by state name
# subdata$state <- factor(subdata$state)
# subdata[,outcome] <- as.numeric(subdata[,outcome])
# statelist <- split(subdata, subdata$state)
# #Ordering each data frame by outcome
# statelist <- lapply(statelist, function(x) x[order(x[,outcome], pmax(x[,outcome],x$hospital)),])
# 
# #
# resumelist <- lapply(statelist, function(x) { 
#   if (num == "best") {
#   x[1,1]
# } else if (num == "worst") {
#   x[nrow(x),1]
# } else x[num,1]
# })
# 
# state <- names(resumelist)
# hospital <- unlist(resumelist)
# result <- data.frame(hospital, state)
# result

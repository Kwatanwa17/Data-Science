url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(url, destfile = "edu.csv", mode = "wb")

wb <- read.csv("wb.csv", blank.lines.skip = TRUE, header = FALSE, skip = 5, na.strings = "", nrows = 190, stringsAsFactors = FALSE)
wb <- wb[,c(1,2,4,5)]
colnames(wb) <- c("CountryCode", "Ranking", "CountryName", "GDP")

edu <- read.csv("edu.csv")

mergeDF <- merge(wb, edu, by = "CountryCode", all = TRUE)



count <- grep("Fiscal year end: June", mergeDF$Special.Notes)
see <- mergeDF$Special.Notes[count]
see

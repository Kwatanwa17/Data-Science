url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(url, destfile = "wb.csv", mode = "wb")

wb <- read.csv("wb.csv", blank.lines.skip = TRUE, header = FALSE, skip = 5, na.strings = "", nrows = 190, stringsAsFactors = FALSE)
wb <- wb[,c(1,2,4,5)]
colnames(wb) <- c("CountryCode", "Ranking", "CountryName", "GDP")

wb$GDP <- gsub(",","",wb$GDP)

any(is.na(wb$GDP) == TRUE)

class(wb$GDP) <- "numeric"


grep("^United", wb$CountryName)
#3

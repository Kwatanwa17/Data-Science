url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(url, destfile = "wgtp.csv", mode = "wb")

wgtp <- read.csv("wgtp.csv")

wgtp_split <- strsplit(wgtp_char, split = "wgtp" )

str <- colnames(wgtp)
wgtp_split <- strsplit(str, split = "wgtp" )

wgtp_split[123]

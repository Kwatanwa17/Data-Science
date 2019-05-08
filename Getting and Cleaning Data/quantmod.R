library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)
amzn <- as.data.frame(amzn)

rnames <- rownames(amzn)
rnames <- gsub("-","" ,rnames)
rnames <- as.Date(rnames,"%Y%m%d")
rnames <- format(rnames, "%a %b %Y")
rnames <- as.character(rnames)
rnames <- gsub(" ","" ,rnames)

y_2012 <- grep("2012",rnames)
m_2012 <- grep("ŒŽ",rnames[y_2012])



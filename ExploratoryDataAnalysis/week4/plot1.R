## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(ggplot2)

NEI <- transform(NEI, yearasfactor = factor(NEI$year))

#Plot 1

q1 <-NEI %>% group_by(yearasfactor) %>% summarise(sum(Emissions))

png(filename = "plot1.png", width = 480, height = 480)

options(scipen=100)
par(mfrow = c(1,1))
plot(q1$yearasfactor, q1$`sum(Emissions)`, type = "n", lty = 0,
     xlab = "Year",
     ylab = "Total emission",
     main = "Plot 1")
points(q1$yearasfactor, q1$`sum(Emissions)`, col = "red")
lines(q1$yearasfactor, q1$`sum(Emissions)`)

dev.off()
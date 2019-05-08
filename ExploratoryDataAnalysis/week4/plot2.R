## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(ggplot2)

NEI <- transform(NEI, yearasfactor = factor(NEI$year))

#plot 2

baltimore <- NEI %>% filter(fips == "24510")
q2 <- baltimore %>% group_by(yearasfactor) %>% summarise(sum(Emissions))

png(filename = "plot2.png", width = 480, height = 480)

options(scipen=100)
par(mfrow = c(1,1))
plot(q2$yearasfactor, q2$`sum(Emissions)`, type = "n", lty = 0,
     xlab = "Year",
     ylab = "Total emission",
     main = "Plot 2")
points(q2$yearasfactor, q2$`sum(Emissions)`, col = "red")
lines(q2$yearasfactor, q2$`sum(Emissions)`)

dev.off()
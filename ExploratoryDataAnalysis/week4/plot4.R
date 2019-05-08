## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(ggplot2)

NEI <- transform(NEI, yearasfactor = factor(NEI$year))

#plot 4

Coal <- grepl("Coal", SCC$EI.Sector)
CoalSector <- SCC[Coal,]
CoalSectorSCC <- as.character(CoalSector$SCC)

CoalEmission <- subset(NEI, NEI$SCC %in% CoalSectorSCC)
q4 <- CoalEmission %>% group_by(yearasfactor) %>% 
  summarise(sum(Emissions))

png(filename = "plot4.png", width = 480, height = 480)

plot(q4$yearasfactor, q4$`sum(Emissions)`, type = "n", lty = 0,
     xlab = "Year",
     ylab = "Total emission",
     main = "Plot 4")
points(q4$yearasfactor, q4$`sum(Emissions)`, col = "red")
lines(q4$yearasfactor, q4$`sum(Emissions)`)

dev.off()

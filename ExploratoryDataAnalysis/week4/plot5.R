## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(ggplot2)

NEI <- transform(NEI, yearasfactor = factor(NEI$year))

#Plot 5

Motor <- grepl("Vehicle", SCC$EI.Sector)
MotorSector <- SCC[Motor,]
MotorSectorSCC <- as.character(MotorSector$SCC)
MotorEmissionBaltimore <- subset(baltimore, baltimore$SCC %in% MotorSectorSCC)

q5 <- MotorEmissionBaltimore %>% group_by(yearasfactor) %>% 
  summarise(sum(Emissions))

png(filename = "plot5.png", width = 480, height = 480)

plot(q5$yearasfactor, q5$`sum(Emissions)`, type = "n", lty = 0,
     xlab = "Year",
     ylab = "Total emission",
     main = "Plot 5")
points(q5$yearasfactor, q5$`sum(Emissions)`, col = "red")
lines(q5$yearasfactor, q5$`sum(Emissions)`)

dev.off()
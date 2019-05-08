## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(ggplot2)

NEI <- transform(NEI, yearasfactor = factor(NEI$year))

#plot 6

BalLA <- NEI %>% filter(fips == "24510"|fips == "06037")
BalLA$fips <- gsub(pattern = "24510", "Baltimore", BalLA$fips)
BalLA$fips <- gsub(pattern = "06037", "Los Angeles", BalLA$fips)

Motor <- grepl("Vehicle", SCC$EI.Sector)
MotorSector <- SCC[Motor,]
MotorSectorSCC <- as.character(MotorSector$SCC)

MotorEmissionBalLA <- subset(BalLA, BalLA$SCC %in% MotorSectorSCC)

q6 <- MotorEmissionBalLA %>% group_by(fips, year) %>% 
  summarise(sum(Emissions))

g2 <- ggplot(q6, aes(x = year, y = `sum(Emissions)`))
g2 <- g2 + geom_point(aes(colour = fips), size = 1, alpha = 0.5)
g2 <- g2 + geom_line(aes(colour = fips), size = 0.5)
g2 <- g2 + labs(x = "Yaer", y = "Total emission",
                title = "Plot 6") 
g2 <- g2 + theme(legend.title=element_blank())

ggsave(filename = "plot6.png", plot = g2)

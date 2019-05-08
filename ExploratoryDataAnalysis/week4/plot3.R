## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(ggplot2)

NEI <- transform(NEI, yearasfactor = factor(NEI$year))

#plot 3

baltimore <- transform(baltimore, type = factor(baltimore$type))
q3 <- baltimore %>% group_by(type, year) %>% summarise(sum(Emissions))

g <- ggplot(q3, aes(x = year, y = `sum(Emissions)`))
g <- g + geom_point(aes(colour = type), size = 1, alpha = 0.5)
g <- g + geom_line(aes(colour = type), size = 0.5)
g <- g + labs(x = "Yaer", y = "Total emission",
              title = "Plot 3") 

ggsave("plot3.png", plot = g)

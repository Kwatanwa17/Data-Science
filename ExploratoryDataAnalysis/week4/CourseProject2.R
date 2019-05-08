## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)

NEI <- transform(NEI, yearasfactor = factor(NEI$year))
q1 <-NEI %>% group_by(yearasfactor) %>% summarise(sum(Emissions))

#Plot 1
options(scipen=100)
par(mfrow = c(1,1))
plot(q1$yearasfactor, q1$`sum(Emissions)`, type = "n", lty = 0,
     xlab = "Year",
     ylab = "Total emission",
     main = "Plot 1")
points(q1$yearasfactor, q1$`sum(Emissions)`, col = "red")
lines(q1$yearasfactor, q1$`sum(Emissions)`)

#plot 2

baltimore <- NEI %>% filter(fips == "24510")
q2 <- baltimore %>% group_by(yearasfactor) %>% summarise(sum(Emissions))

options(scipen=100)
par(mfrow = c(1,1))
plot(q2$yearasfactor, q2$`sum(Emissions)`, type = "n", lty = 0,
     xlab = "Year",
     ylab = "Total emission",
     main = "Plot 2")
points(q2$yearasfactor, q2$`sum(Emissions)`, col = "red")
lines(q2$yearasfactor, q2$`sum(Emissions)`)

#plot 3

baltimore <- transform(baltimore, type = factor(baltimore$type))
q3 <- baltimore %>% group_by(type, year) %>% summarise(sum(Emissions))

library(ggplot2)

g <- ggplot(q3, aes(x = year, y = `sum(Emissions)`))
g <- g + geom_point(aes(colour = type), size = 1, alpha = 0.5)
g <- g + geom_line(aes(colour = type), size = 0.5)
g <- g + labs(x = "Yaer", y = "Total emission",
              title = "Plot 3") 

ggsave("plot3.png", width = 4, height = 4, units = "in")

#plot 4

Coal <- grepl("Coal", SCC$EI.Sector)
CoalSector <- SCC[Coal,]
CoalSectorSCC <- as.character(CoalSector$SCC)

CoalEmission <- subset(NEI, NEI$SCC %in% CoalSectorSCC)
q4 <- CoalEmission %>% group_by(yearasfactor) %>% 
  summarise(sum(Emissions))

plot(q4$yearasfactor, q4$`sum(Emissions)`, type = "n", lty = 0,
     xlab = "Year",
     ylab = "Total emission",
     main = "Plot 4")
points(q4$yearasfactor, q4$`sum(Emissions)`, col = "red")
lines(q4$yearasfactor, q4$`sum(Emissions)`)

#Plot 5

Motor <- grepl("Vehicle", SCC$EI.Sector)
MotorSector <- SCC[Motor,]
MotorSectorSCC <- as.character(MotorSector$SCC)

MotorEmissionBaltimore <- subset(baltimore, baltimore$SCC %in% MotorSectorSCC)

q5 <- MotorEmissionBaltimore %>% group_by(yearasfactor) %>% 
  summarise(sum(Emissions))

plot(q5$yearasfactor, q5$`sum(Emissions)`, type = "n", lty = 0,
     xlab = "Year",
     ylab = "Total emission",
     main = "Plot 5")
points(q5$yearasfactor, q5$`sum(Emissions)`, col = "red")
lines(q5$yearasfactor, q5$`sum(Emissions)`)

#plot 6

BalLA <- NEI %>% filter(fips == "24510"|fips == "06037")
BalLA$fips <- gsub(pattern = "24510", "Baltimore", BalLA$fips)
BalLA$fips <- gsub(pattern = "06037", "Los Angeles", BalLA$fips)
MotorEmissionBalLA <- subset(BalLA, BalLA$SCC %in% MotorSectorSCC)

q6 <- MotorEmissionBalLA %>% group_by(fips, year) %>% 
  summarise(sum(Emissions))

g2 <- ggplot(q6, aes(x = year, y = `sum(Emissions)`))
g2 <- g2 + geom_point(aes(colour = fips), size = 1, alpha = 0.5)
g2 <- g2 + geom_line(aes(colour = fips), size = 0.5)
g2 <- g2 + labs(x = "Yaer", y = "Total emission",
              title = "Plot 6") 
g2

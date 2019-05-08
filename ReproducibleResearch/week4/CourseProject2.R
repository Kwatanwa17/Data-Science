#Download the data

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, "2FStormData.csv.bz2")

StormData <- read.csv("2FStormData.csv.bz2")

#Question 1

library(dplyr)
library(ggplot2)
library(tidyr)
library(xtable)

Harmful <- StormData %>% select(EVTYPE, FATALITIES, INJURIES) %>% 
  group_by(EVTYPE) %>%
  summarise(Fatality = sum(FATALITIES),
            Injury = sum(INJURIES)) %>% 
  mutate(Total = Fatality + Injury)

Top10Harmful <- Harmful[order(Harmful$Total, decreasing = TRUE),][1:10,]

Top10HarmfulTrans <- Top10Harmful %>%
  select(-Total) %>% 
  gather(key = type, value = number, Fatality, Injury)

g <- ggplot(Top10HarmfulTrans, aes(EVTYPE, number,fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  coord_cartesian(ylim = c(0, 10000)) +
  labs(x = "Event", y = "Number of persons", title = "Top 10 Most harmful events") +
  guides(fill=guide_legend(title=""))
g

xtable(Top10Harmful, type = "html")

#Question2


PropDmg <- StormData %>% select(EVTYPE, PROPDMG) %>% 
  group_by(EVTYPE) %>% 
  summarise(Amount = sum(PROPDMG))

Top10Damage <- PropDmg[order(PropDmg$Amount, decreasing = TRUE),][1:10,]

options(scipen=100)
g2 <- ggplot(Top10Damage, aes(EVTYPE, Amount)) +
  geom_bar(stat = "identity", colour = "darkorange") +
  labs(x = "Event", y = "Amount (dollar)", title = "Top 10 Most property damage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
g2

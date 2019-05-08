#Load the data set "mtcars"
data(mtcars)
summary(mtcars)
head(mtcars)
?mtcars
#am variable 0 = automatic, 1 = manual

#Preparing for the explatory analysis
library(dplyr)
library(ggplot2)

mtcars <- mtcars %>% mutate(am2 = ifelse(am == 1, "manual", "automatic"))

#ttest
automatic_mpg <- dat %>% filter(am == 0) %>% select(mpg)
manual_mpg <- dat %>% filter(am == 1) %>% select(mpg)
t.test(automatic_mpg, manual_mpg, alternative = "two.sided", paired = FALSE, var.equal = FALSE)

am_boxplot <- ggplot(mtcars,aes(y = mpg, x = am2, fill = am2)) +
  geom_boxplot(color = "black") +
  ggtitle("Boxplot of mpg by the transmission type")
am_boxplot
#Obvious that the manual cars have heigher mean than the automatic ones
#Why do the manual cars have higher mpg ?? 

#Explanation
#Consider other variables that afect in the mpg 
mpg_wt <- ggplot(mtcars,aes(x = wt, y = mpg, fill = am2)) +
  geom_point(aes(color=am2)) +
  facet_wrap(~am2, ncol=2) +
  ggtitle("Violin plot of mpg by the weight (unit = 1000 lb)")
mpg_wt

mpg_wt_by_cyl <- ggplot(mtcars,aes(x = wt, y = mpg, fill = factor(cyl))) +
  geom_point(aes(color=factor(cyl))) +
  facet_wrap(~am2, ncol = 2) +
  ggtitle("plot of mpg by the weight (1000 lb) grouped by the number of cilinders")
mpg_wt_by_cyl
#manual data set tends to have less wt than the automatic data set ... test?

mpg_hp_by_cyl <- ggplot(mtcars,aes(x = hp, y = mpg, fill = factor(cyl))) +
  geom_point(aes(color=factor(cyl))) +
  facet_wrap(~am2, ncol = 2) +
  ggtitle("plot of mpg by hp grouped by the number of cilinders")
mpg_hp_by_cyl

g11 <- ggplot(mtcars,aes(x = factor(cyl), y = mpg, fill = am2)) +
  geom_boxplot(aes(color = am2)) +
  ggtitle("mpg vs cyl")
g11

mpg_hp <- ggplot(mtcars,aes(x = hp, y = mpg, fill = am2)) +
  geom_point(aes(color = am2)) +
  ggtitle("mpg vs hp by am2")
mpg_hp


#Gear, carburetors
table(mtcars$am2, mtcars$gear)
table(mtcars$am2, mtcars$carb)


#gear
g6 <- ggplot(mtcars,aes(x = hp, y = mpg, fill = factor(gear))) +
  geom_point(aes(color = factor(gear))) +
  facet_wrap(~am2, ncol = 2) +
  ggtitle("mpg vs hp by gear")
g6

g7 <- ggplot(mtcars,aes(x = hp, y = mpg, fill = factor(gear))) +
  geom_point(aes(color = factor(gear))) +
  ggtitle("mpg vs hp by gear")
g7

fit5 <- lm(mpg ~hp*factor(gear), data = mtcars)
summary(fit5)
#gear 4 is the best in mpg and hp in all cars

g12 <- ggplot(mtcars,aes(x = factor(gear), y = mpg, fill = am2)) +
  geom_boxplot(aes(color = am2)) +
  ggtitle("mpg vs gear")
g12
table(mtcars$gear, mtcars$am2)
#Automatic 3 or 4 gears/ Manual 4 or 5 gears

g13 <- ggplot(mtcars,aes(x = factor(carb), y = mpg, fill = am2)) +
  geom_boxplot(aes(color = am2)) +
  ggtitle("mpg vs carb")
g13
#number of carb predicts mpg and the difference at the same value of carb is present 

#carb
g8 <- ggplot(mtcars,aes(x = hp, y = mpg, fill = factor(carb))) +
  geom_point(aes(color = factor(carb))) +
  facet_wrap(~am2, ncol = 2) +
  ggtitle("mpg vs hp by carb")
g8

g9 <- ggplot(mtcars,aes(x = hp, y = mpg, fill = factor(carb))) +
  geom_point(aes(color = factor(carb))) +
  ggtitle("mpg vs hp by carb")
g9

fit6 <- lm(mpg ~hp * factor(carb), data = mtcars)
summary(fit6)

g10 <- ggplot(mtcars,aes(x = hp, y = mpg)) +
  geom_point(aes(color = factor(carb))) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~am2, ncol =2)
g10

g11 <- ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(aes(color=am2)) +
  facet_wrap(~factor(carb), ncol = 6) +
  ggtitle("mpg vs hp by carb")
g11
#Higher mpg lower hp

#Manual cars have lower carb 

#Linear model mpg vs hp
fit3 <- lm(mpg ~ hp*am2, data = mtcars)
summary(fit3)
g3 <- ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(aes(color = am2)) +
  geom_smooth(method = "lm", aes(color = am2), se = FALSE) +
  ggtitle("lm hp vs mpg")
g3

fit4 <- lm(mpg ~ hp + am2, data = mtcars)
summary(fit4)
g4 <- ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(aes(color = am2)) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("lm hp vs mpg")
g4

#Linear model
fit <- lm(am ~ mpg, data = mtcars)
summary(fit)
g <- ggplot(mtcars, aes(x = mpg, y = am)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("glm am vs mpg")
g


#Generalized inear model logistic
fit2 <- glm(am ~ mpg, data = mtcars, family = "binomial")
summary(fit2)
g <- ggplot(mtcars, aes(x = mpg, y = am)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = binomial(link = "logit")), se = FALSE) +
  ggtitle("glm am vs mpg")
g


#Logistic model
fit <- glm(mpg ~ am2, data = dat)
summary(fit)

#Transmission vs Engine
table(mtcars$am2, mtcars$vs)

g5 <- ggplot(mtcars,aes(x = hp, y = mpg, fill = factor(vs))) +
  geom_point(aes(color = factor(vs))) +
  facet_wrap(~am2, ncol = 2) +
  ggtitle("Violin plot of mpg by the weight (1000 lb) grouped by the number of cilinders")
g5



#linear model without am2
fit7 <- lm(mpg ~ hp + wt, data = mtcars)
summary(fit7)

#disp
plot(mtcars$disp, mtcars$mpg)

g10 <- ggplot(mtcars,aes(x = disp, y = mpg, fill = am2)) +
  geom_point(aes(color = am2)) +
  ggtitle("mpg vs disp")
g10



#Part 1: Simulation Exercise 

#Compare the sample mean the theoretical mean of the distribution
n <- 40
lambda <- 0.2
nosim <- 1000

set.seed(1)

Simulation <- replicate(nosim, rexp(n, lambda))
Simulation_means <- apply(Simulation, 2, mean)

Theorical <- replicate(nosim, rnorm(40, mean = 1/0.2, sd = 1/0.2))
Theorical_means <- apply(Theorical, 2, mean)

mean(Simulation_means)
mean(Theorical_means)

# dat <- data.frame(
#   x = append(Simulation_means, Theorical_means),
#   Type = rep(c("simulation","theorical"), each = nosim)
# )

# library(ggplot2)
# g <- ggplot(dat, aes(x = x, fill = Type)) +
#   geom_histogram(alpha = 0.20, binwidth = 0.3, colour = "black", aes(y = ..density..))
# g <- g + stat_function(fun = dnorm, size = 2)
# g

par(mfrow = c(1,2))
hist(Simulation_means, xlim = c(4,6))
hist(Theorical_means, xlim = c(4,6))

#Variance 
var(Simulation_means)
var(Theorical_means)

#Approximately normal
par(mfrow = c(1,1))
hist(Simulation_means,xlim = c(4.5,5.5), ylim = c(0,3.0), freq = FALSE)
curve(dnorm(x, mean = mean(Simulation_means), sd = sd(Simulation_means)),
      from = 4, to = 6,
      add = TRUE, col = "red")

#Part 2 Basic Inferential Data Analysis

library(datasets)
data("ToothGrowth")

#Summary of the data
summary(ToothGrowth)

#T test two sided test 
VC <- subset(ToothGrowth, supp == "VC")
OJ <- subset(ToothGrowth, supp == "OJ")

t.test(VC$dose, OJ$dose, paired = FALSE, var.equal = FALSE)
t.test(VC$len, OJ$len, paired = FALSE, var.equal = FALSE)

#As a result of the experiment data, there is no diference of tooth length by the supplement type (orange juice or ascorbic acid).
#On the other hand, it is quite clear that there would not be difference by levels of Vitamin C (Variable dose).
#For more information, please see R help page ("The Effect of Vitamin C on Tooth Growth in Guinea Pigs": http://www.is.titech.ac.jp/~mase/mase/html.jp/temp/ToothGrowth.jp.html).

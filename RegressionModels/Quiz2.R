#1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

fit <- lm(y~x)
summary(fit)

#2

#3
data(mtcars)
fit <- lm(mtcars$mpg ~ mtcars$wt)
summary(fit)

sumCoef <- summary(fit)$coefficients
sumCoef[1,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[1, 2]
sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2]
sumCoef[2,1] + c(-1, 1) * 3 * sumCoef[2, 2]
max(predict(fit,newdata = 3))

        
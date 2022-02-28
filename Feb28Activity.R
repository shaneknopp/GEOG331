# Utilizing iris data set
# GEOG 331 Class Activity
# SK 02/28/22

# subset for Iris virginica
flower <- iris[iris$Species == "virginica",]

# linear model relating petal length to sepal length
fit <- lm(flower$Petal.Length~flower$Sepal.Length)

# view results
summary(fit)

# create a scatter plot
plot(flower$Sepal.Length,flower$Petal.Length,
     main = "Iris virginica",
     xlab = "Sepal Length",
     ylab = "Petal Length",
     col = "purple",
     pch = 16)

# plot the residuals, stored in regression summary
plot(flower$Sepal.Length,summary(fit)$residuals,
     xlab = "Sepal Length",
     ylab = "Residuals",
     col = "purple",
     pch = 16)

# add a horizontal line to reference
abline(h=0, lty = "dashed")

# histogram of redisuals
hist(summary(fit)$residuals,
     main = "Regression Residuals",
     xlab = "Residual",
     col = "purple")

# shapiro wilks test (statistical test)
shapiro.test(summary(fit)$residuals) #indicates data is normally distributed

# qq plot
qqnorm(summary(fit)$residuals, pch = 16) 

qqline(summary(fit)$residuals, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7, pch = 16)

#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################
# Subset for Iris versicolor
flower <- iris[iris$Species == "versicolor",]
# Vectors for regression variables; index with formula i-1 in loop
parameters = list(c(flower$Sepal.Length), c(flower$Sepal.Width), #1. iris  sepal length x width
                  c(flower$Petal.Length), c(flower$Petal.Width), #2. iris  petal length x width
                  c(flower$Sepal.Length), c(flower$Petal.Length)) #3. iris sepal length x petal length

# Store regression variables in array
results = list()

# Loop for three regressions
for(i in 1:3) {
    results[[i]] <- lm(parameters[[((i*2)-1)]]~parameters[[(i*2)]])#parameters 1~2, 3~4, 5~6
}
# View results 
summary(results[[1]])
summary(results[[2]])
summary(results[[3]])


#####################################
##### Part 2: data in dplyr     #####
#####################################
# Data frame with maximum height of species
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

# Joined data frame with iris and maximum height
new_iris <- full_join(iris, height, by = 'Species')


#####################################
##### Part 3: plots in ggplot2  #####
#####################################
# Look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
scatter <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point()
# Display plot from variable
scatter

#3b. make a scatter plot with ggplot and get rid of busy grid lines
scatter_no_lines <- scatter + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# Display plot from variable
scatter_no_lines

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length
scatter_comprehensive <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
            geom_point(aes(color = Species, size = Petal.Length)) + #point size and color
            # Ref: https://ggplot2.tidyverse.org/reference/scale_manual.html
            scale_color_manual(values = c("virginica"="darkgoldenrod1", "setosa" = "blue", "versicolor" = "brown3")) +
            # Ref: https://ggplot2.tidyverse.org/reference/theme.html
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
            # Ref: https://ggplot2.tidyverse.org/reference/labs.html
            labs(x = "Sepal Length (mm)", y="Sepal Width (mm)", title = "Flower Species by Sepal Length and Width")

scatter_comprehensive
#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		
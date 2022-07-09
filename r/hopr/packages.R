# Load the ggplot2 package
library("ggplot2")

# Assign some values to vectors x and y then plot them
x <- c(-1.0, -0.8, -0.6, -0.4, -0.2, 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
y <- x^3
qplot(x = x, y = y)

x <- c(1, 2, 2, 2, 3, 3)
qplot(x, binwidth = 1)

# Use replicate and roll to generate some data
source("basics.R", local = FALSE)
replicate(3, 1 + 1)
replicate(10, roll())

rolls <- replicate(10000, roll())
qplot(rolls, binwidth = 1)

dice_probs = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8)

roll <- function() {
    die <- 1:6
    dice <- sample(die, prob = dice_probs, size = 2, replace = TRUE)
    sum(dice)
}

rolls <- replicate(10000, roll())
qplot(rolls, binwidth = 1)

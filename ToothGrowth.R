# ToothGrowth
library(dplyr)
library(ggplot2)

data("ToothGrowth")
head(ToothGrowth)
table(ToothGrowth$supp)
table(ToothGrowth$dose)
table(ToothGrowth$len)
summary(ToothGrowth)
hist(ToothGrowth$len)
lapply(ToothGrowth, class)

Tooth <- group_by(ToothGrowth, supp)

g <- ggplot(data = Tooth, aes(x = dose, y = len, colour = supp)) +
    geom_point() + 
    geom_smooth() +
    labs(title = "Tooth length with dose") +
    ylab("Tooth length")

g

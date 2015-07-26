# ToothGrowth
#Description
#
#The response is the length of odontoblasts (teeth) in 
#each of 10 guinea pigs at each of three dose levels of 
#Vitamin C (0.5, 1, and 2 mg) with 
#each of two delivery methods (orange juice or ascorbic acid).

library(dplyr)
library(ggplot2)
library(UsingR)

data("ToothGrowth")
head(ToothGrowth)
table(ToothGrowth$supp)
table(ToothGrowth$dose)
table(ToothGrowth$len)
summary(ToothGrowth)
hist(ToothGrowth$len)
lapply(ToothGrowth, class)

Supp <- data.frame(supp = c("OJ", "VC"), 
                   delivery = as.factor(c("orange juice", "ascorbic acid")))

Tooth <- left_join(ToothGrowth, Supp) %>%
    group_by(delivery)

g <- ggplot(data = Tooth, aes(x = dose, y = len, colour = delivery)) +
    geom_point() + 
    geom_smooth() +
    labs(title = "Tooth length with dose") +
    ylab("Tooth length")

g

plot(Tooth$dose, Tooth$len, col = Tooth$delivery)

table(Tooth$dose, Tooth$delivery)

Tooth$dose <- as.character(Tooth$dose)
Tooth$dose <- as.factor(Tooth$dose)
lapply(Tooth, class)
g <- ggplot(aes(y = len, x = dose, fill = delivery), data = Tooth) + geom_boxplot()
g <- g + labs(title = "Teeth length of Guinea Pigs when supplemented with Vitamin C")
g <- g + labs(x = "Dose in milligrams", y = "Tooth length")
g <- g + theme_bw()
g

#Orange Juice is better than Ascorbic Acid
t.test(filter(Tooth, delivery == "orange juice")$len, filter(Tooth, delivery == "ascorbic acid")$len)
qt(.975, 55.309)

#There is not enough evidence to eliminate the null hypothisis for orange juice
#being a better promoter of tooth growth over ascorbic acid.
#Looking at our graph however there is more going on here depending on the dose.
t.test(filter(Tooth, dose == "1")$len, filter(Tooth, dose == "0.5")$len)
qt(.975, 37.986)
# One mg of Vitamin C is clearly better than 0.5 mgs with a t stitistic of 6.5.
# We regect the null hypothisis that the same averger tooth growth is achieved by
# both doses. There is enough evidence to support the alternative hypothisis that
# 1mg gives better growth than 0.5mg. One mg gives on average 19.7 units of growth
# while 0.5 mgs gives an average of 10.6 units long.

t.test(filter(Tooth, dose == "2")$len, filter(Tooth, dose == "1")$len)
# so 2 mg is better than 1 which we know is better than 0.5.

# So what about the type of supplement?
t.test(filter(Tooth, dose == "0.5", delivery == "orange juice")$len,
       filter(Tooth, dose == "0.5", delivery == "ascorbic acid")$len)
qt(.975, 14.969)
t.test(filter(Tooth, dose == "1", delivery == "orange juice")$len,
       filter(Tooth, dose == "1", delivery == "ascorbic acid")$len)
#Clearly more length, between 2.8and 9.1 units longer when supplemented with orange juice
t.test(filter(Tooth, dose == "2", delivery == "orange juice")$len,
       filter(Tooth, dose == "2", delivery == "ascorbic acid")$len)
t.test(filter(Tooth, dose == "2", delivery == "orange juice")$len,
       filter(Tooth, dose == "2", delivery == "ascorbic acid")$len)$estimate[[1]]

#At th highest dose of vitamin there is no difference between ascorbic acid and orange juice.
#So that average is very smilar at 26.1 differing only in the second decimal place.
#There is more variabiabliity in the asorbic acid takers
sd(filter(Tooth, dose == "2", delivery == "orange juice")$len)
sd(filter(Tooth, dose == "2", delivery == "ascorbic acid")$len)
#Now we can say with confidence that at lower doses Orange Juice performs better than
#Ascorbic acid and roughly the same at the higher dose of 2mg. We can also say that
#the higher the dose the more the greater the tooth length on average.


# Simulation
library(ggplot2)

lambda = 0.2

hist(rexp(1000, lambda))
abline(v = 5)


## Now for the real distribution
# We know the average for the distribution however when we take a sample of 40 iid draws from the population what mean will we get of this sample.
# It turns out that the averages of the distribution has it's own distribution which is not the same as the one from the original population.

mns = NULL
vrs = NULL
for (i in 1 : 1000) {
    expdist <- rexp(40, lambda)
    mns = c(mns, mean(expdist))
    vrs = c(vrs, var(expdist))
}
hist(mns)
abline(v = 1/lambda, lwd = 2)
hist(vrs)
abline(v = 1/lambda^2, lwd = 2)
hist(rexp(1000, lambda))
hist(sqrt(vrs))
abline(v = 1/lambda, lwd = 2)

#Putting this all together and knowing the central limit theorem we can say that as the population size
#limits to infinity so the distribution of averages approaches the normal distribution
mns <- data.frame(mns)
meanSample <- mean(mns$mns)
sdMean <- 1/(sqrt(40) * lambda)
g <- ggplot(data = mns, aes(x = mns)) + geom_histogram(alpha = .20, binwidth = .2, colour = "black", aes(y = ..density..))
g <- g + stat_function(fun = dnorm, arg = list(mean = 1/lambda, sd = sdMean))
g <- g + geom_vline(xintercept = 1/lambda)
g <- g + geom_vline(xintercept = meanSample)
g
g <- g + 
    
```{r, echo=FALSE}
sample <- data.frame(sample)
g <- ggplot(data = sample, aes(x = sample)) + geom_histogram(alpha = .20, binwidth = .2, colour = "black", aes(y = ..density..))
g <- g + stat_function(fun = dexp, args = list(rate = lambda),size = 1.2)
g <- g + labs(title = "40 values randomly sampled times from the exponential distribution")
g <- g + geom_vline(aes(linetype = "Sample mean"), xintercept = samplemean, colour = "grey50", size = 1.2)
g <- g + geom_vline(aes(linetype = "Density mean"), xintercept = 5, size = 1.2)
g <- g + labs(x = "Time", y = "Density")
g <- g + theme_bw()

g

sample2 <- rexp(1000, rate = lambda)
p <- ggplot(data = sample, aes(x = sample2)) + geom_histogram(alpha = .20, binwidth = .2, colour = "black", aes(y = ..density..))
p <- p + stat_function(fun = dexp, args = list(rate = lambda),size = 1.5)
p <- p + coord_cartesian(ylim = c(0, 0.4))
p <- p + labs(x = "Time", y = "Density")
p <- p + theme_bw()

p


grid.arrange(g, p, ncol=2)

    
# May only want the variance of the average not the variance of the samples



hist(mns$mns)
abline(v = 1/lambda, lwd = 2)
hist(vrs)
abline(v = 1/lambda^2, lwd = 2)
hist(rexp(1000, lambda))
hist(sqrt(vrs))
abline(v = 1/lambda, lwd = 2)
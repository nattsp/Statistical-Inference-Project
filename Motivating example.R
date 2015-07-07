# Motivating example

hist(runif(1000))

mns = NULL
for (i in 1 : 1000) mns = c(mns, mean(runif(40)))
hist(mns)

hist(runif(40))

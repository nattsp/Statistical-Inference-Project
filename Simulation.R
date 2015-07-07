# Simulation

lambda = 0.2
hist(rexp(1000, lambda))

mns = NULL
for (i in 1 : 1000) mns = c(mns, mean(runif(40)))
hist(mns)

mns = NULL
for (i in 1 : 1000) {
    mns = c(mns, mean(rexp(40, lambda)))
}
hist(mns)

hist(rexp(1000, lambda))

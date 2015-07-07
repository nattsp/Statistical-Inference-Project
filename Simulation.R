# Simulation

lambda = 0.2
hist(rexp(1000, lambda))

mns = NULL
for (i in 1 : 1000) mns = c(mns, mean(runif(40)))
hist(mns)
median(mns)
mean(mns)

mns = NULL
vrs = NULL
for (i in 1 : 1000) {
    expdist <- rexp(40, lambda)
    mns = c(mns, mean(expdist))
    vrs = c(vrs, var(expdist))
}
hist(mns)
hist(vrs)

hist(rexp(1000, lambda))

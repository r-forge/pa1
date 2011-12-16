## brinson.test.R
## Yang Lu Yang.Lu@williams.edu

library(pa)

## data(s1)
## truth <- brinson(x = s1)
## data(s2)
## truth.multi <- brinson(x = s2)
## save(truth, truth.multi, file = "brinson.test.RData")

## Single-period

data(s1)
result <- brinson(x = s1)

stopifnot(all.equal(result, truth))

## Multi-period

data(s2)
result.multi <- brinson(x = s2)

stopifnot(all.equal(result.multi, truth.multi))


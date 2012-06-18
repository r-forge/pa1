## plot.test.R
## Yang Lu Yang.Lu@williams.edu

library(pa)

## data(jan)
## b1 <- brinson(x = jan)
## truth <- plot(b1)

## data(quarter)
## b2 <- brinson(x = quarter)
## truth.multi <- plot(b2)

## r2 <- regress(quarter)
## truth.multi.r2 <- plot(r2)

## save(truth, truth.multi, truth.multi.r2, file = "plot.test.RData")

load("plot.test.RData")

## Single-period
data(jan)
b1 <- brinson(x = jan)
result <- plot(b1, var = "sector", type = "exposure")
stopifnot(all.equal(result, truth))


## Multi-period

data(quarter)
b2 <- brinson(x = quarter)
result.multi <- plot(b2, var = "sector", type = "exposure")
stopifnot(all.equal(result.multi, truth.multi))

r2 <- regress(quarter)
result.multi.r2 <- plot(r2, var = "sector", type = "exposure")
stopifnot(all.equal(result.multi.r2, truth.multi.r2))


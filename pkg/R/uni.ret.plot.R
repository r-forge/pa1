################################################################################
##
## $Id: uni.ret.plot.R 909 2008-08-07 15:05:24Z satopaa $
##
## Function that plots a horizontal barchart of the One-month Forward
## Return in percents (x-axis) of different sets of stocks (y-axis).
##
################################################################################

## x is an universe of stocks. pm is a long-only portfolioMatch object
## whose holdings are included in x as well. pm.ls, on the other hand,
## is a long-short portfolioMatch object, whose holdings are also
## included in x. weight.var.uni and ret.var define columns that hold
## weights and returns in x.

uni.ret.plot <- function(x,
                         pm,
                         pm.ls,
                         weight.var.uni = "weight.uni",
                         ret.var        = "fwd.ret.1m"){

  ## Get returns from the data frame.
  
  perf.uni  <- sum(x[[ret.var]] * x[[weight.var.uni]])
  perf.port <- pm@return.orig[[1]]$total
  perf.ls   <- pm.ls@return.orig[[1]]$total

  ## Get returns from the matched portfolio.
  
  rp.ret <- sapply(pm@return.match, function(x) x$total)

  ## Get returns from the matched long-short portfolio.
  
  rp.ret.ls <- sapply(pm.ls@return.match, function(x) x$total)
  
  ## By combining a list of names of different sets of stocks and a
  ## matrix that has a single column of returns, make a structure that
  ## matches each return with its set of stocks.

  p.perf.lf2 <-
    structure(matrix(c(perf.port, 0.036, perf.uni,
                       mean(rp.ret), perf.ls, mean(rp.ret.ls)) * 100),
              dimnames = list(c("Portfolio", "S&P 500", "Universe",
                "Matching Portfolio Average",
                "Long-short Portfolio", "Matching Long-short Average"),
                c("One-month Forward Return (%)")))

  ## Reverses the order of rows of the structure.
  
  p.perf.lf2 <- p.perf.lf2[seq(nrow(p.perf.lf2), 1),,drop=FALSE]

  ## Print a horizontal barchart of the different set of stocks
  ## (y-axis) versus one-month forward return (x-axis).
  
  print(barchart(p.perf.lf2, xlab = "One-month Forward Return (%)",
                 main = "Forward Return of All Portfolios"))
}

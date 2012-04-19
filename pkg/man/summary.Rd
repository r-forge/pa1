\name{summary}
\alias{summary}
\title{Provide a summary for an object of either class brinson or class brinsonMulti}

\description{

  Summarize a portfolio and its benchmark. Present the results of
  brinson analysis based on the given portfolio. The input can be a
  class object of either \code{brinson} for a single-period data set or
  \code{brinsonMulti} for a multi-period data set.

}

\usage{ summary(object) }

\arguments{

  \item{object}{ An object of either class \code{brinson} or class
    \code{brinsonMulti}.}
  
}

\value{

  Return relevant information about a portfolio and its benchmark. It
  includes their exposures based on input categories and the results of
  brinson analysis. The input object is of class \code{brinson} or
  \code{brinsonMulti}. 
}

\author{Yang Lu \email{Yang.Lu@williams.edu}}

\examples{

## Single-period brinson analysis
data(jan)
p1 <- brinson(x = jan, date.var = "date", cat.var = "sector", bench.weight = "benchmark", 
    portfolio.weight = "portfolio", ret.var = "return")
summary(p1)

## Multi-period brinson analysis
data(quarter)
p2 <- brinson(x = quarter, date.var = "date", cat.var = "sector",
bench.weight = "benchmark", portfolio.weight = "portfolio", ret.var = "return")
summary(p2)

}
\keyword{file}
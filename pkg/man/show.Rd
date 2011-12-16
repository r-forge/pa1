\name{show}
\alias{show}
\title{Provide a concise summary for an object of either class brinson
  or class brinsonMulti}

\description{

  Summarize a portfolio including the time period, number of stocks in
  the portfolio and in the benchmark. The input can be a class object of
  either \code{brinson} for a single-period data set or
  \code{brinsonMulti} for a multi-period data set.
 
}

\usage{ show(object) }

\arguments{

  \item{object}{ An object of either class \code{brinson} or class
    \code{brinsonMulti}.}
  
}

\value{

  Return information of a portfolio including the time period, number of
  stocks in the portfolio and in the benchmark. The input can be a class
  object of either \code{brinson} for a single-period data set or
  \code{brinsonMulti} for a multi-period data set.

}

\author{Yang Lu \email{Yang.Lu@williams.edu}}

\examples{

data(s1)

## Single-period brinson analysis

p1 <- brinson(x = s1, date.var = "date", sector.var = "sector",
                    bench.weight = "benchmark", portfolio.weight =
                    "portfolio", ret.var = "fwd.ret.1m")

show(p1)

## Multi-period brinson analysis

data(s2)

p2 <- brinson(x = s2, date.var = "date", sector.var = "sector",
                    bench.weight = "benchmark", portfolio.weight =
                    "portfolio", ret.var = "fwd.ret.1m")

show(p2)

}
\keyword{file}
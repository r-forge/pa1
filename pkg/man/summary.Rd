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
  includes their exposures based on sector and the results of brinson
  analysis. The input object is of class \code{brinson} or
  \code{brinsonMulti}.

}

\author{Yang Lu \email{Yang.Lu@williams.edu}}

\examples{

data(s1)

## Single-period brinson analysis

p1 <- brinson(x = s1, date.var = "date", sector.var = "sector",
                    bench.weight = "benchmark", portfolio.weight =
                    "portfolio", ret.var = "fwd.ret.1m")

summary(p1)

## Multi-period brinson analysis

data(s2)

p2 <- brinson(x = s2, date.var = "date", sector.var = "sector",
                    bench.weight = "benchmark", portfolio.weight =
                    "portfolio", ret.var = "fwd.ret.1m")

summary(p2)

}
\keyword{file}
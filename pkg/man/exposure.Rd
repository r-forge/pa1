\name{exposure}
\alias{exposure}
\title{Calculate and display the sector exposure of a portfolio}

\description{

  Calculate and display the sector exposure of a portfolio. Both the
  portfolio and benchmark exposures are displayed.
  
}

\usage{ exposure(object) }

\arguments{

  \item{object}{ An object of either class \code{brinson} or class
    \code{brinsonMulti}.}
  
}

\value{

  Return a matrix if the input object is of class \code{brinson} and
  return a list if the input object is of class \code{brinsonMulti}.

}

\author{Yang Lu \email{Yang.Lu@williams.edu}}

\examples{

data(s1)

## Single-period brinson analysis

p1 <- brinson(x = s1, date.var = "date", sector.var = "sector",
                    bench.weight = "benchmark", portfolio.weight =
                    "portfolio", ret.var = "fwd.ret.1m")

exposure(p1)

## Multi-period brinson analysis

data(s2)

p2 <- brinson(x = s2, date.var = "date", sector.var = "sector",
                    bench.weight = "benchmark", portfolio.weight =
                    "portfolio", ret.var = "fwd.ret.1m")

exposure(p2)

}
\keyword{file}
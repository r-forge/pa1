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
  \item{var}{ Specify category of exposure to be shown. }
}

\value{

  Return a matrix if the input object is of class \code{brinson} and
  return a list if the input object is of class \code{brinsonMulti}.

}

\author{Yang Lu \email{Yang.Lu@williams.edu}}

\examples{

## Single-period brinson analysis
data(jan)
p1 <- brinson(x = jan, date.var = "date", cat.var = "sector", bench.weight = "benchmark", 
    portfolio.weight = "portfolio", ret.var = "return")

exposure(p1)

## Multi-period brinson analysis
data(quarter)
p2 <- brinson(x = quarter, date.var = "date", cat.var = "sector",
bench.weight = "benchmark", portfolio.weight = "portfolio", ret.var = "return")
exposure(p2)

}
\keyword{file}
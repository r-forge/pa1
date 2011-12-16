\name{brinson}
\alias{brinson}
\title{Creating an object of either class brinson or class brinsonMulti}

\description{ Conduct brinson analysis for equity portfolio
  attribution. Create a class object of either \code{brinson} for a
  single-period data set or \code{brinsonMulti} for a multi-period data
  set.  }

\usage{ brinson(x, date.var = "date", sector.var = "sector",
bench.weight = "benchmark", portfolio.weight = "portfolio", ret.var =
"fwd.ret.1m")}

\arguments{
  \item{x}{ A data frame containing the data from which
    brinson analysis will be formed.}
  \item{date.var}{A character vector which indicates the name
    of the column in \code{x} to be used as a date for each
    observation. If the unique number of \code{date.var} is one,
    a class object of \code{brinson} will be formed. Otherwise, a class
    object of \code{brinsonMulti} will be formed.}
  \item{sector.var}{A character vector which indicates the name of the
    column in \code{x} to be used as sector variable.}
  \item{bench.weight}{A character vector which indicates the name of the
    column or columns in \code{x} to be used as benchmark weight.}
  \item{portfolio.weight}{A character vector which indicates the name of the
    column or columns in \code{x} to be used as portfolio weight.}
  \item{ret.var}{A character vector which indicates the name of the
    column in \code{x} to be used as return variable.}
}

\value{

  Return an object of class \code{brinson} when there is only one unique
  date in the data frame \code{x}. Otherwise, an object of class
  \code{brinsonMulti} is returned.

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
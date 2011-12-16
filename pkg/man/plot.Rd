\name{plot}
\alias{plot}
\title{Plot either the exposure or the return of a brinson or a
  brinsonMulti object}

\description{

  This method plots the exposures or returns comparison graph of a
  portfolio and its benchmark.

}

\usage{ plot(object, type = "exposure") }

\arguments{
  \item{object}{ The input object on which the plot is based. It can be
    either a \code{brinson} object or a \code{brinsonMulti} object. }
  \item{type}{A character vector which indicates the type of plot to be
  shown. It can be either "exposure" or "returns". By default, it shows
  the exposure comparison plot.}
}

\value{

  Return a bar plot if the input is a \code{brinson} object. Return a
  faceted bar plot if the input is a \code{brinsonMulti} object.

}

\author{Yang Lu \email{Yang.Lu@williams.edu}}

\examples{

data(s1)

## Single-period brinson analysis

p1 <- brinson(x = s1, date.var = "date", sector.var = "sector",
                    bench.weight = "benchmark", portfolio.weight =
                    "portfolio", ret.var = "fwd.ret.1m")

plot(object = p1, type = "exposure")

plot(object = p1, type = "returns")

}
\keyword{file}
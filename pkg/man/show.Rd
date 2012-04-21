\name{show}
\alias{show}
\title{
  Provide a concise summary for an object of analysis.
}
\description{
  Provide a concise summary for an object of one of the following
  classes, brinson, brinsonMulti, regression, and regressionMulti. 
}
\usage{
show(object)
}
\arguments{

  \item{object}{ An object of either class \code{brinson}, class
    \code{brinsonMulti}, \code{regression}, or \code{regressionMulti}.}
  
}

\value{
  
  Return relevant information about a portfolio and its benchmark. It
  includes their exposures and returns. The input object is of class
  \code{brinson}, \code{brinsonMulti}, \code{regression}, or
  \code{regressionMulti}.
  
}

\author{Yang Lu \email{Yang.Lu@williams.edu}}

\examples{

## Single-period brinson analysis

data(jan)

p1 <- brinson(x = jan, date.var = "date", cat.var = "sector",
    bench.weight = "benchmark", portfolio.weight = "portfolio", ret.var
    = "return")

show(p1)


}

\keyword{file}

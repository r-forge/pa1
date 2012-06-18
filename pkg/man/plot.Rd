\name{plot}
\alias{plot}
\title{Plot either the exposure or the return of a brinson or a
  brinsonMulti object}

\description{

  This method plots the exposure or return comparison graph of a
  portfolio and its benchmark.

}

\usage{ plot(object, type, ...) }

\arguments{
  
  \item{object}{The input object on which the plot is based. It can be
    one of the four class objects including \code{brinson},
    \code{brinsonMulti}, \code{regression}, and \code{regressionMulti}. } 
  \item{type}{A character vector which indicates the type of plot to be
    shown. It can be either \code{exposure} or \code{returns}. By default,
    it shows the exposure comparison plot. }
  \item{...}{Other options including \code{var} which refers to the name
    of the variable whose exposure to be shown. This input has no effect
    when the \code{type} is \code{return}. The default is
    \code{sector}.}
  }


\value{

  Return a bar plot if the input is an object of class
  \code{brinson}. Return a faceted bar plot if the input is an object of
  class \code{brinsonMulti}.

  Return a trend line if the input is an object of class
  \code{regressionMulti}. Show no graph for an object of class
  \code{regression}. 

}

\author{Yang Lu \email{Yang.Lu@williams.edu}}

\examples{

## Single-period brinson analysis

data(jan)

p1 <- brinson(x = jan, date.var = "date", cat.var = "sector",
bench.weight = "benchmark", portfolio.weight = "portfolio", ret.var =
"return")

plot(p1, type = "return")

## Multi-period brinson analysis

data(quarter)

p2 <- brinson(x = quarter, date.var = "date", cat.var = "sector",
bench.weight = "benchmark", portfolio.weight = "portfolio", ret.var =
"return")

plot(p2, var = "size", type = "exposure")

}
\keyword{file}
\name{brinsonMulti-class}
\docType{class}
\alias{brinsonMulti-class}

\alias{show,brinsonMulti-method}
\alias{summary,brinsonMulti-method}
\alias{exposure,brinsonMulti-method}
\alias{returns,brinsonMulti-method}
\alias{plot,brinsonMulti-method}

\title{Class "brinsonMulti"}
\description{ Class "brinsonMulti" holds the results of an original
  portfolio, its benchmark, and the results of brinson analysis of a
  multi-period portfolio. }

\section{Slots}{
  \describe{
    \item{\code{date.var}:}{ Object of class character storing all the
      dates occurred in the universe data frame.}
    \item{\code{sector.var}:}{ Object of class character storing the
      sector variable name in the universe data frame.}
    \item{\code{bench.weight}:}{ Object of class character storing the
      benchmark weight variable name in the universe data frame.}
    \item{\code{portfolio.weight}:}{ Object of class character storing the
      portfolio weight variable name in the universe data frame.}
    \item{\code{ret.var}:}{ Object of class character storing the return
      variable name in the universe data frame.}
    \item{\code{weight.port}:}{ Object of class matrix storing the
      sector weights of the original portfolio.}
    \item{\code{weight.bench}:}{ Object of class matrix storing the
      sector weights of the benchmark portfolio.}
    \item{\code{ret.port}:}{ Object of class matrix storing the
      sector returns of the original portfolio.}
    \item{\code{ret.bench}:}{ Object of class matrix storing the
      sector returns of the benchmark portfolio.}
    \item{\code{brinson.mat}:}{ Object of class matrix storing the information
      of the brinson matrix across period. }
    \item{\code{universe}:}{ Data frame storing the universe
      environment. }
    
  }
}

\section{Methods}{
\describe{
  \item{show}{\code{signature(object = "brinson")}. Summarize
    the essential information about the portfolio.}
  \item{summary}{\code{signature(object = "brinson")}. Summarize
    the portfolio and the brinson analysis.}
  \item{exposure}{\code{signature(object = "brinson")}. Calculate and
    display the sector exposure of a portfolio. }
  \item{returns}{\code{signature(object = "brinson")}. Calculate the
    contribution of various effects based on the brinson model. }
  \item{plot}{\code{signature(object = "brinson")}. Plot the exposure or
    the return of a portfolio. }
}
}

\author{  Yang Lu }

\examples{

data(s2)

## Multi-period brinson analysis

p2 <- brinson(x = s2, date.var = "date", sector.var = "sector",
                    bench.weight = "benchmark", portfolio.weight =
                    "portfolio", ret.var = "fwd.ret.1m")

summary(p2)

exposure(p2)

returns(p2)

plot(p2, type = "exposure")

plot(p2, type = "returns")

}


\keyword{classes}